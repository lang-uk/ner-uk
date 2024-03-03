from typing import Dict, Tuple
from pathlib import Path
from glob import glob
from ner_utils import read_proofreaded_bsf_data
from itertools import combinations


def calculate_iaa(file1: Path, file2: Path) -> tuple[float, float, float]:
    annotator1_entities = set(
        (x.tag, x.start_idx, x.end_idx) for x in read_proofreaded_bsf_data(Path(file1))
    )
    annotator2_entities = set(
        (x.tag, x.start_idx, x.end_idx) for x in read_proofreaded_bsf_data(Path(file2))
    )

    true_positives = len(annotator1_entities.intersection(annotator2_entities))
    annotator1_false_positives = len(annotator1_entities - annotator2_entities)
    annotator2_false_positives = len(annotator2_entities - annotator1_entities)
    false_negatives = annotator1_false_positives  # Assuming both annotators should identify the same entities

    precision = (
        true_positives / (true_positives + annotator1_false_positives)
        if true_positives + annotator1_false_positives > 0
        else 0
    )
    recall = (
        true_positives / (true_positives + false_negatives)
        if true_positives + false_negatives > 0
        else 0
    )
    f1_score = (
        2 * (precision * recall) / (precision + recall) if precision + recall > 0 else 0
    )

    return f1_score, true_positives, true_positives + annotator1_false_positives


if __name__ == "__main__":
    f_ones: Dict[Tuple[str, str], float] = {}
    tps, alls = 0, 0
    for folder in glob("../databank/seva/**/*/"):
        files = list(Path(folder).glob("*.ann"))

        for file1, file2 in combinations(files, 2):
            f1, tp, all_ = calculate_iaa(file1, file2)

            f_ones[(file1, file2)] = f1
            tps += tp
            alls += all_
            # print(
            #     f"{file1} vs {file2}: {calculate_iaa(file1, file2)}"
            # )
    
    print("Number of comparisons:", len(f_ones))
    print("Total true positives:", tps)
    print("Total all:", alls)
    print("Average precision:", tps / alls)
    print("Average F1-score:", sum(f_ones.values()) / len(f_ones))
