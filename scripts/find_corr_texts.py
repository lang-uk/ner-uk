from typing import Dict, List, Any
from random import shuffle, seed
import re
from pathlib import Path
from glob import glob
from hashlib import sha256


def read_split_file(file_path: Path) -> Dict[str, List]:
    """
    Read the split file
    Args:
        file_path: path to the file
    Returns:
        dictionary with DEV and TEST splits
    """

    res: Dict[str, List] = {
        "DEV": [],
        "TEST": [],
    }

    is_dev = False
    is_test = False
    with file_path.open("r", encoding="utf-8") as f:
        lines = f.read().split("\n")
        for l in lines:
            if l == "DEV":
                is_dev = True
                is_test = False
                continue
            elif l == "TEST":
                is_dev = False
                is_test = True
                continue
            elif l == "":
                continue
            if is_dev:
                res["DEV"].append(f"{l}.txt")
            elif is_test:
                res["TEST"].append(f"{l}.txt")

    return res


def get_hash(file_path: Path) -> str:
    """
    Get the hash of a file
    :param file_path: path to the file
    :return: hash of the file
    """
    with file_path.open("r") as f:
        return sha256(re.sub(r"\s+", "", f.read().lower()).encode()).hexdigest()


def get_key(a_dict: Dict[str, Any], val: Any) -> str:
    for key, value in a_dict.items():
        if val == value:
            return key

    raise ValueError("Value not found")


files_1 = {
    fname.name: get_hash(Path(fname))
    for fname in map(Path, glob("../data/*.txt"))
    if ".tok." in fname.name
}

files_2 = {
    fname.stem: get_hash(Path(fname))
    for fname in map(Path, glob("../databank/dima/**/*.txt"))
}

print(f"Old files: {len(files_1)}, new files: {len(files_2)}")

split = read_split_file(Path("../doc/dev-test-split.txt"))

new_split: Dict[str, List] = {
    "DEV": [],
    "TEST": [],
}

print("Mapping old split to new files")
for split_name, split_files in split.items():
    print(f"{split_name}: {len(split_files)}")
    for f in split_files:
        if f not in files_1:
            print(f"File {f} is new")

        new_split[split_name].append(get_key(a_dict=files_2, val=files_1[f]))

new_files: List[str] = []

for f in files_2:
    try:
        get_key(a_dict=files_1, val=files_2[f])
    except ValueError:
        new_files.append(f)

print(f"New files to add to the split: {len(new_files)}")
seed(42)

shuffle(new_files)
test_size = round(len(new_files) * 0.3)
new_split["TEST"].extend(new_files[:test_size])
new_split["DEV"].extend(new_files[test_size:])

test_size = len(new_split["TEST"])
dev_size = len(new_split["DEV"])

print(
    f"New split: DEV: {dev_size}, TEST: {test_size}, ratio: "
    f"{(dev_size / (dev_size + test_size)):.3f} : {(test_size / (dev_size + test_size)):.3f}"
)

print("Storing new split")
with open("../databank/output/dev-test-split.txt", "w", encoding="utf-8") as f:
    f.write("DEV\n")
    f.write("\n".join(new_split["DEV"]))
    f.write("\n\nTEST\n")
    f.write("\n".join(new_split["TEST"]))

# print(new_split)
# print(read_split_file(Path("../doc/dev-test-split.txt")))

# for hash_new, file_new in files_2.items():
#     if hash_new in files_1:
#         print(f"File {file_new} is a duplicate of {files_1[hash_new]}")
#     else:
#         print(f"File {file_new} is new")
