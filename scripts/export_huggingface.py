#!/usr/bin/env python
"""Export the NER-UK 2.0 corpus (Brat standoff format) to Hugging Face friendly files.

Produces token-level IOB2 data in two formats:
  - JSONL (one document per line, human-diffable, tracked in git)
  - Parquet (loadable directly by `datasets` / the HF dataset viewer)

The official train/test split from v2.0/data/dev-test-split.txt is preserved
(DEV section -> train split, TEST section -> test split). Nested entities are
removed with the same `remove_inner` strategy used to produce v2.0/iob.

The output is deterministic: running the script twice on the same corpus
produces byte-identical files. CI uses `--check` to regenerate the export into
a temporary directory and compare it against the committed one, so the
Hugging Face export cannot drift from the Brat source.
"""

import argparse
import filecmp
import json
import pathlib
import sys
import tempfile
from os.path import splitext

import pyarrow as pa
import pyarrow.parquet as pq

from ner_utils import convert_bsf, OverlapStrategy

ENTITY_TYPES = [
    "ART",
    "DATE",
    "DOC",
    "JOB",
    "LOC",
    "MISC",
    "MON",
    "ORG",
    "PCT",
    "PERIOD",
    "PERS",
    "QUANT",
    "TIME",
]

IOB2_LABELS = ["O"] + [f"{prefix}-{tag}" for tag in ENTITY_TYPES for prefix in ("B", "I")]

SPLIT_NAMES = ["train", "test"]


def read_split_file(split_path: pathlib.Path) -> dict:
    """Map document id -> split name from dev-test-split.txt.

    The DEV section is the recommended training set, TEST is the held-out set.
    """
    split_of_doc = {}
    current = None
    for ln in split_path.read_text().splitlines():
        ln = ln.strip()
        if ln == "DEV":
            current = "train"
        elif ln == "TEST":
            current = "test"
        elif ln:
            if current is None:
                raise ValueError(f"Document {ln} listed before any DEV/TEST header")
            split_of_doc[ln] = current
    return split_of_doc


def iob_string_to_tokens(iob_data: str) -> tuple:
    """Convert the `convert_bsf` output ("token TAG" per line) to parallel lists."""
    tokens, tags = [], []
    for line in iob_data.split("\n"):
        line = line.strip()
        if not line:
            continue
        parts = line.split(" ")
        if len(parts) != 2:
            raise ValueError(f"Unparseable IOB line: {line!r}")
        tokens.append(parts[0])
        tags.append(parts[1])
    return tokens, tags


def build_records(src_dir: pathlib.Path, split_path: pathlib.Path) -> dict:
    split_of_doc = read_split_file(split_path)
    records = {name: [] for name in SPLIT_NAMES}

    txt_files = sorted(p for p in src_dir.glob("**/*.txt") if p.resolve() != split_path.resolve())
    if not txt_files:
        raise SystemExit(f"No .txt files found under {src_dir}")

    seen_docs = set()
    for txt_path in txt_files:
        ann_path = pathlib.Path(splitext(txt_path)[0] + ".ann")
        if not ann_path.exists():
            raise SystemExit(f"Missing annotation file for {txt_path}")

        doc_id = txt_path.stem
        seen_docs.add(doc_id)
        if doc_id not in split_of_doc:
            raise SystemExit(f"Document {doc_id} is not listed in {split_path}")

        iob_data = convert_bsf(
            txt_path.read_text(),
            ann_path.read_text(),
            converter="iob",
            overlap_strategy=OverlapStrategy.REMOVE_INNER,
        )
        tokens, tags = iob_string_to_tokens(iob_data)
        unknown = set(tags) - set(IOB2_LABELS)
        if unknown:
            raise SystemExit(f"Unknown labels {unknown} in {ann_path}")

        records[split_of_doc[doc_id]].append(
            {
                "document_id": doc_id,
                "source": txt_path.parent.name,
                "tokens": tokens,
                "ner_tags": tags,
            }
        )

    missing = set(split_of_doc) - seen_docs
    if missing:
        raise SystemExit(f"Documents listed in {split_path} but not found: {sorted(missing)}")

    return records


PARQUET_SCHEMA = pa.schema(
    [
        pa.field("document_id", pa.string()),
        pa.field("source", pa.string()),
        pa.field("tokens", pa.list_(pa.string())),
        pa.field("ner_tags", pa.list_(pa.string())),
    ]
)


def write_export(records: dict, dst_dir: pathlib.Path) -> None:
    data_dir = dst_dir / "data"
    jsonl_dir = dst_dir / "jsonl"
    data_dir.mkdir(parents=True, exist_ok=True)
    jsonl_dir.mkdir(parents=True, exist_ok=True)

    for split in SPLIT_NAMES:
        rows = records[split]
        jsonl_path = jsonl_dir / f"{split}.jsonl"
        with jsonl_path.open("w", encoding="utf-8") as f:
            for row in rows:
                f.write(json.dumps(row, ensure_ascii=False) + "\n")

        table = pa.Table.from_pylist(rows, schema=PARQUET_SCHEMA)
        pq.write_table(
            table,
            data_dir / f"{split}.parquet",
            compression="snappy",
            # fixed metadata so that the output is byte-stable across runs
            store_schema=True,
        )


def print_stats(records: dict) -> None:
    for split in SPLIT_NAMES:
        rows = records[split]
        n_tokens = sum(len(r["tokens"]) for r in rows)
        n_entities = sum(sum(1 for t in r["ner_tags"] if t.startswith("B-")) for r in rows)
        print(f"{split}: {len(rows)} documents, {n_tokens} tokens, {n_entities} entities")


def check_export(records: dict, dst_dir: pathlib.Path) -> None:
    """Regenerate the export into a temp dir and compare with the committed one."""
    problems = []
    with tempfile.TemporaryDirectory() as tmp:
        tmp_dir = pathlib.Path(tmp)
        write_export(records, tmp_dir)

        for split in SPLIT_NAMES:
            committed_jsonl = dst_dir / "jsonl" / f"{split}.jsonl"
            fresh_jsonl = tmp_dir / "jsonl" / f"{split}.jsonl"
            if not committed_jsonl.exists():
                problems.append(f"{committed_jsonl} is missing")
            elif not filecmp.cmp(committed_jsonl, fresh_jsonl, shallow=False):
                problems.append(f"{committed_jsonl} differs from freshly generated export")

            committed_parquet = dst_dir / "data" / f"{split}.parquet"
            if not committed_parquet.exists():
                problems.append(f"{committed_parquet} is missing")
            else:
                # compare parquet by content, not bytes, to stay robust across
                # pyarrow versions
                committed_table = pq.read_table(committed_parquet)
                fresh_table = pq.read_table(tmp_dir / "data" / f"{split}.parquet")
                if not committed_table.equals(fresh_table):
                    problems.append(f"{committed_parquet} differs from freshly generated export")

    if problems:
        for p in problems:
            print(f"ERROR: {p}", file=sys.stderr)
        print(
            "\nThe committed Hugging Face export is out of date. "
            "Regenerate it with:\n  python scripts/export_huggingface.py",
            file=sys.stderr,
        )
        raise SystemExit(1)
    print("Hugging Face export is up to date.")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    repo_root = pathlib.Path(__file__).resolve().parent.parent
    parser.add_argument(
        "--src_dataset",
        type=pathlib.Path,
        default=repo_root / "v2.0" / "data",
        help="Directory with the Brat standoff corpus",
    )
    parser.add_argument(
        "--split_file",
        type=pathlib.Path,
        default=repo_root / "v2.0" / "data" / "dev-test-split.txt",
        help="Official train/test split file",
    )
    parser.add_argument(
        "--dst",
        type=pathlib.Path,
        default=repo_root / "huggingface",
        help="Output directory for the Hugging Face export",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Do not write anything; verify the committed export matches the corpus",
    )
    args = parser.parse_args()

    records = build_records(args.src_dataset, args.split_file)
    print_stats(records)

    if args.check:
        check_export(records, args.dst)
    else:
        write_export(records, args.dst)
        print(f"Export written to {args.dst}")


if __name__ == "__main__":
    main()
