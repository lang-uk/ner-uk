import re
from pathlib import Path
from glob import glob
from hashlib import sha256


def get_hash(file_path: Path) -> str:
    """
    Get the hash of a file
    :param file_path: path to the file
    :return: hash of the file
    """
    with file_path.open("r") as f:
        return sha256(re.sub(r"\s+", "", f.read().lower()).encode()).hexdigest()


files_1 = {
    get_hash(Path(fname)): fname
    for fname in glob("../data/*.txt")
    if ".tok." in fname
}

files_2 = {get_hash(Path(fname)): fname for fname in glob("../data_2.0/*.txt")}

print(f"Old files: {len(files_1)}, new files: {len(files_2)}")

for hash_new, file_new in files_2.items():
    if hash_new in files_1:
        print(f"File {file_new} is a duplicate of {files_1[hash_new]}")
    else:
        print(f"File {file_new} is new")
