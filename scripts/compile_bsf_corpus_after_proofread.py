"""
Okay, the whole folder is a mess already, and we going to add a little bit more.
"""
from typing import List
import argparse
from pathlib import Path
import glob
import shutil
from collections import Counter


def deduplicate(fname: Path) -> str:
    """
    Some texts exported from the proof-reading tool have duplicates its content
    duplicated again and again. This function removes the duplicates.
    """
    text = fname.open("r", encoding="utf-8").read()
    lines = text.split("\n")

    if lines and lines[0].strip() == "":
        print(f"Empty first line {fname}")
        return "\n".join(lines)

    try:
        duplicate_loc = lines[1:].index(lines[0])

    except ValueError:
        print(f"No duplicates found in {fname}")
        return "\n".join(lines)

    return "\n".join(lines[:duplicate_loc + 1])


if __name__ == "__main__":
    parser: argparse.ArgumentParser = argparse.ArgumentParser(
        description="A script to compile a corpus after the proof-reading into the same "
        "format as we have now in the repo"
    )

    parser.add_argument("input_dir", type=Path, help="A dir with annotations and texts")
    parser.add_argument("output_dir", type=Path, help="A dir to store exported files")

    tags_count: Counter = Counter()
    files_converted: int = 0
    args: argparse.Namespace = parser.parse_args()
    args.output_dir.mkdir(exist_ok=True, parents=True)

    for txt_file in map(Path, glob.iglob(str(args.input_dir / "*.txt"))):
        fname: str = txt_file.name
        file_id: str = txt_file.stem

        with (args.output_dir / f"{file_id}.txt").open("w") as fp_out:
            text = deduplicate(txt_file)
            fp_out.write(text)

        ann_files = glob.glob(str(args.input_dir / file_id / "*.ann"))

        if len(ann_files) != 1:
            print(f"Oh crap, cannot find right annotation file for the {file_id}: {len(ann_files)} files found")

        shutil.copy(ann_files[0], args.output_dir / f"{file_id}.ann")

    print(f"Total number of files converted: {files_converted}")
    print(f"Total number of tags: {sum(tags_count.values())}")
    print("\n".join(f"{k}: {v}" for k, v in tags_count.most_common()))
