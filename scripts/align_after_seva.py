from typing import List, Tuple, Set
import glob
import shutil
from pathlib import Path
from collections import Counter
import argparse

from ner_utils import BsfInfo, bsf_to_str, read_proofreaded_bsf_data


def realign_annotations(text: str, bsfs: List[BsfInfo]) -> List[BsfInfo]:
    """
    Given the list of BSF annotations and the text, realign the annotations
    to the text. This is necessary because the proof-reading tool may have
    changed the text and the annotations are no longer aligned with the text.
    """

    result: List[BsfInfo] = []
    offset = 0
    offsets = Counter()

    prev_position = 0
    for bsf in bsfs:
        context = bsf.comment.replace("[[", "").replace("]]", "")
        wrapped_token = f"[[{ bsf.token }]]"

        if wrapped_token not in bsf.comment:
            raise ValueError(
                f"Hard to deal with token the context: {wrapped_token} in {bsf.comment}"
            )

        position = text[offset:].find(context)
        if position == -1:
            raise ValueError(
                f"Token not found: {context}, wrapped: {wrapped_token}: {text[offset:offset+100]}"
            )

        in_context_offset = bsf.comment.find(wrapped_token)
        if in_context_offset == -1:
            raise ValueError(f"Token not found in the context: {wrapped_token}")

        test_extract = text[
            offset
            + position
            + in_context_offset : offset
            + position
            + in_context_offset
            + len(bsf.token)
        ]
        if test_extract != bsf.token:
            raise ValueError(
                f"Token not found in the context: {test_extract} != {bsf.token}"
            )

        new_start = offset + position + in_context_offset
        result.append(
            BsfInfo(
                id=bsf.id,
                token=bsf.token,
                start_idx=new_start,
                end_idx=new_start + len(bsf.token),
                tag=bsf.tag,
            )
        )

        if new_start < prev_position:
            print(f"Non-ordered tokens {bsf.token}")

        if bsf.start_idx > new_start:
            offsets.update([-1])
        elif bsf.start_idx < new_start:
            offsets.update([1])

        prev_position = new_start

        offset += position

    if len(offsets) > 1:
        print(f"Offsets: {offsets}")

    return result


def calculate_overlaps(bsfs: List[BsfInfo]) -> Tuple[int, int]:
    """
    Calculates number of full overlaps and partial overlaps in the list of BsfInfo
    Args:
        bsfs: list of BsfInfo
    Returns:
        tuple of full overlaps and partial overlaps
    Examples:
    >>> calculate_overlaps([BsfInfo(id=1, token="a", start_idx=0, end_idx=1, tag="tag1"), BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2")])
    (0, 0)
    >>> calculate_overlaps([BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2"), BsfInfo(id=1, token="a", start_idx=0, end_idx=1, tag="tag1")])
    (0, 0)
    >>> calculate_overlaps([BsfInfo(id=1, token="ab", start_idx=0, end_idx=2, tag="tag1"), BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2")])
    (1, 0)
    >>> calculate_overlaps([BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2"), BsfInfo(id=1, token="ab", start_idx=0, end_idx=2, tag="tag1")])
    (1, 0)
    >>> calculate_overlaps([BsfInfo(id=1, token="abc", start_idx=0, end_idx=3, tag="tag1"), BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2")])
    (1, 0)
    >>> calculate_overlaps([BsfInfo(id=2, token="b", start_idx=1, end_idx=2, tag="tag2"), BsfInfo(id=1, token="abc", start_idx=0, end_idx=3, tag="tag1")])
    (1, 0)
    >>> calculate_overlaps([BsfInfo(id=1, token="ab", start_idx=0, end_idx=2, tag="tag1"), BsfInfo(id=2, token="bc", start_idx=1, end_idx=3, tag="tag2")])
    (0, 2)
    >>> calculate_overlaps([BsfInfo(id=2, token="bc", start_idx=1, end_idx=3, tag="tag2"), BsfInfo(id=1, token="ab", start_idx=0, end_idx=2, tag="tag1")])
    (0, 2)
    """

    full_overlaps = 0
    partial_overlaps = 0

    for i, bsf1 in enumerate(bsfs):
        for j, bsf2 in enumerate(bsfs):
            if i == j:
                continue

            start_in = bsf1.start_idx >= bsf2.start_idx and bsf1.start_idx < bsf2.end_idx
            end_in = bsf1.end_idx > bsf2.start_idx and bsf1.end_idx <= bsf2.end_idx

            reverse_start_in = (
                bsf2.start_idx >= bsf1.start_idx and bsf2.start_idx < bsf1.end_idx
            )
            reverse_end_in = bsf2.end_idx > bsf1.start_idx and bsf2.end_idx <= bsf1.end_idx

            if start_in and end_in:
                full_overlaps += 1
            elif (start_in or end_in) and not (reverse_start_in and reverse_end_in):
                partial_overlaps += 1
                print(f"Partial overlap: {bsf1} and {bsf2}")

    return full_overlaps, partial_overlaps


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="A script to align annotations after the proof-reading"
    )
    parser.add_argument("input_dir", type=Path, help="A dir with annotations and texts")
    parser.add_argument("output_dir", type=Path, help="A dir to store exported files")
    args = parser.parse_args()
    total_files = 0
    files_converted = 0
    failed_files = 0
    tags_count: Counter = Counter()
    files_with_full_overlap: Set[str] = set()
    files_with_partial_overlap: Set[str] = set()
    total_full_overlaps = 0
    total_partial_overlaps = 0

    for txt_file in map(
        Path,
        glob.iglob(str(args.input_dir / "*.txt")),
    ):
        total_files += 1
        # print(f"Processing {txt_file}")
        fname: str = txt_file.name
        file_id: str = txt_file.stem
        orig_ann_file = args.input_dir / f"{file_id}.ann"

        ann_file = args.output_dir / f"{file_id}.ann"

        if not orig_ann_file.exists():
            print(f"No annotation file found for {file_id}")
            failed_files += 1
            continue

        bsfs = list(read_proofreaded_bsf_data(orig_ann_file))
        try:
            text = txt_file.open("r", encoding="utf-8").read()
            bsfs = realign_annotations(text, bsfs)
        except ValueError as e:
            print(f"Error processing {file_id}: {e}")
            failed_files += 1
            continue

        shutil.copy(txt_file, args.output_dir / f"{file_id}.txt")
        files_converted += 1

        full_overlaps, partial_overlaps = calculate_overlaps(bsfs)
        if partial_overlaps:
            files_with_partial_overlap.add(file_id)
            total_partial_overlaps += partial_overlaps
            print(
                f"Overlaps in {file_id}: full: {full_overlaps}, partial: {partial_overlaps}"
            )
        
        if full_overlaps:
            files_with_full_overlap.add(file_id)
            total_full_overlaps += full_overlaps

        with ann_file.open("w", encoding="utf-8") as fp_out:
            for bsf in bsfs:
                tags_count.update([bsf.tag])
                fp_out.write(bsf_to_str(bsf) + "\n")

    print(
        f"Total files: {total_files}, failed files: {failed_files}, files converted: {files_converted}"
    )
    print(f"Tags count:")
    for tag, count in tags_count.most_common():
        print(f"\t{tag}: {count}")

    print(f"Files with full overlaps: {len(files_with_full_overlap)}")
    print(f"Files with partial overlaps: {len(files_with_partial_overlap)}")
    print(f"Total full overlaps: {total_full_overlaps}")
    print(f"Total partial overlaps: {total_partial_overlaps}")
