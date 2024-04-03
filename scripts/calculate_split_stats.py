import argparse
import glob
from pathlib import Path
from collections import defaultdict, Counter
from ner_utils import read_proofreaded_bsf_data, read_train_test_split

if __name__ == "__main__":
    stats = defaultdict(Counter)

    parser = argparse.ArgumentParser(
        description="A script to align annotations after the proof-reading"
    )
    parser.add_argument("input_dir", type=Path, help="A dir with annotations and texts")
    parser.add_argument(
        "split_file", type=Path, help="A file with train/dev/test split"
    )

    args = parser.parse_args()
    dev_split, test_split = read_train_test_split(args.split_file)

    for txt_file in map(
        Path,
        glob.iglob(str(args.input_dir / "**/*.txt")),
    ):
        # print(f"Processing {txt_file}")
        file_id: str = txt_file.stem
        orig_ann_file = txt_file.with_suffix(".ann")

        if not orig_ann_file.exists():
            print(f"No annotation file found for {file_id}")
            continue

        if file_id in dev_split:
            split_name = "dev"
        elif file_id in test_split:
            split_name = "test"
        else:
            print(f"File {file_id} is not in the split file")
            continue

        split_stats = stats[split_name]

        for bsf in read_proofreaded_bsf_data(orig_ann_file):
            split_stats["total_entities"] += 1
            split_stats[bsf.tag] += 1

    for split_name, split_stats in stats.items():
        print(f"Split: {split_name}")
        for tag, count in split_stats.items():
            print(f"{tag}: {count}")
        print()
