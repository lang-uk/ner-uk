from typing import List, Generator
import glob
import shutil
from pathlib import Path
from collections import Counter
import argparse

from tqdm import tqdm

from ner_utils import parse_bsf, BsfInfo, bsf_to_str, read_proofreaded_bsf_data


def find_substring_occurrences(s: str, sub: str) -> Generator[int, None, None]:
    """
    Find all occurrences of the substring `sub` in the string `s`.
    Args:
        s: The string to search in
        sub: The substring to search for
    Returns:
        A generator that yields the start position of each occurrence of `sub` in `s`

    Example:
    >>> list(find_substring_occurrences('this is a test text', 'test'))
    [10]
    >>> list(find_substring_occurrences('repeating repeating repeating', 'eating'))
    [3, 13, 23]
    """
    start = 0  # Initial position to start searching from
    while True:
        start = s.find(sub, start)
        if start == -1:  # If `sub` is not found, `find()` returns -1
            break
        yield start  # Yield the current start position
        start += 1  # Move to the next position to continue searching


def find_closest_occurrence(s: str, sub: str, start_idx: int) -> int:
    """
    Find the closest occurrence of the substring `sub` in the string `s` to the `start_idx`.
    Args:
        s: The string to search in
        sub: The substring to search for
        start_idx: The position to search around
    Returns:
        The start position of the closest occurrence of `sub` to `start_idx` in `s`
        or -1 if not found
    Example:
    >>> find_closest_occurrence('here is a simple example example', 'example', 10)
    17
    >>> find_closest_occurrence('here is a simple example example', 'example', 27)
    25
    >>> find_closest_occurrence('nothing matches', 'example', 5)
    -1
    """

    occurences = list(find_substring_occurrences(s, sub))

    if not occurences:
        return -1

    return min(occurences, key=lambda x: abs(x - start_idx))


def align_annotations(text_file: Path, annotation_file: Path) -> List[BsfInfo]:
    """
    Given the path to .txt and .ann files, align annotations with tokens.

    Args:
        text_file: Path to .txt file
        annotation_file: Path to .ann file
    Returns:
        List of aligned BsfInfo objects
    """

    with text_file.open("r", encoding="utf-8") as f:
        orig_txt: str = f.read()
    with annotation_file.open("r", encoding="utf-8") as f:
        ann: str = f.read()

    text = orig_txt.replace("\n", " ")
    offset = 0
    result: List[BsfInfo] = []
    prev_token: Optional[BsfInfo] = None
    for bsf in parse_bsf(ann):
        position = find_closest_occurrence(text, bsf.token, bsf.start_idx - offset)

        if position == -1:
            raise ValueError(f"Token not found: {bsf.token}")

        new_start_idx = position + offset

        if new_start_idx - bsf.start_idx != 0:
            print(bsf.token, new_start_idx - bsf.start_idx)
        # if abs(new_start_idx - bsf.start_idx) > 150:
        #     print(
        #         f"Token: {bsf.token} is not aligned with annotation: {bsf.start_idx} != {new_start_idx}"
        #     )
        # else:
        #     print(
        #         f"Token: {bsf.token} is aligned with annotation: {bsf.start_idx} => {new_start_idx}"
        #     )

        result.append(
            BsfInfo(
                id=bsf.id,
                token=bsf.token,
                start_idx=new_start_idx,
                end_idx=new_start_idx + len(bsf.token),
                tag=bsf.tag,
            )
        )

        if prev_token and prev_token.start_idx <= bsf.start_idx < prev_token.end_idx:
            # print(f"Overlapping tokens {prev_token.token} and {bsf.token}")
            text = text[position:]
            offset += position
        else:
            text = text[position + len(bsf.token) :]
            offset += position + len(bsf.token)
        prev_token = bsf

    return result


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
    # align_annotations(
    #     text_file=Path("../data_2.0/0ac5140eb732.txt"),
    #     annotation_file=Path("../data_2.0/0ac5140eb732.ann"),
    # )
