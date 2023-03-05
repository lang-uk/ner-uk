"""
Okay, the whole folder is a mess already, and we going to add a little bit more.
"""

import argparse
from pathlib import Path
import glob
import shutil
from collections import Counter

from ner_utils import read_proofreaded_bsf_data


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
        shutil.copy(txt_file, args.output_dir)

        ann_files = glob.glob(str(args.input_dir / file_id / "*.ann"))

        if len(ann_files) != 1:
            print(f"Oh crap, cannot find right annotation file for the {file_id}: {len(ann_files)} files found")

        files_converted += 1
        with (args.output_dir / f"{file_id}.ann").open("w") as fp_out:
            for bsf in read_proofreaded_bsf_data(Path(ann_files[0])):
                # id, tag, start_idx, end_idx, token
                fp_out.write(f"T{bsf.id}\t{bsf.tag}\t{bsf.start_idx}\t{bsf.end_idx}\t{bsf.token}\n")
                tags_count.update([bsf.tag])

    print(f"Total number of files converted: {files_converted}")
    print(f"Total number of tags: {sum(tags_count.values())}")
    print("\n".join(f"{k}: {v}" for k, v in tags_count.most_common()))
