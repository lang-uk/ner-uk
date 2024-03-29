import argparse
import os

from ner_utils import read_data_to_iob, read_train_test_split
import stanza
from mitie import named_entity_extractor
from sklearn.metrics import classification_report
from tqdm import tqdm


def process_with_stanza(model_path: str, data_x: list[str]) -> list[list[str]]:
    """
    Process `data_x` with Stanza pipeline. Data must be pre tokenized.
    :param model_path: path to stanza model file
    :param data_x: list of tokenized data set to run evaluation on
    :returns: labels in iob format. 2d array corresponding to data_x structure. (we preserve split by documents)
    """
    print('Initializing Stanza pipeline')
    stanza.download('uk')
    nlp = stanza.Pipeline('uk', processors='tokenize,pos,lemma',
                          ner_model_path=model_path,
                          ner_forward_charlm_path="", ner_backward_charlm_path="", tokenize_pretokenized=True)

    y_res_stanza = []
    for x in tqdm(data_x, total=len(data_x)):
        #     print(x)
        doc = nlp(x)

        iob = []
        for t in doc.iter_tokens():
            token_str = t.ner
            if token_str.startswith('E'):
                token_str = 'I' + token_str[1:]
            elif token_str.startswith('S'):
                token_str = 'B' + token_str[1:]
            iob.append(token_str)

        y_res_stanza.append(iob)

    print(f'Stanza total set length: {len(y_res_stanza)}')
    return y_res_stanza


def process_with_mitie(model_path: str, data_x: list[str]) -> list[list[str]]:
    """
    Process data in `data_x` using Mitie model provided via `model_path`.
    :param model_path: path to a file with Mitie model
    :param data_x: data samples to run model on
    :return: iob representation of model output. List items correspond to documents in data_x
    """
    print('Loading MITIE model')
    model = named_entity_extractor(model_path)

    print('Processing documents with Mitie model')
    ent_lst = []
    for x in tqdm(data_x, total=len(data_x)):
        entities = model.extract_entities(x.split())  # (range, tag, score)
        ent_lst.append(entities)

    # convert to iob
    y_res = []
    for xi in range(len(data_x)):
        x = data_x[xi]
        ents = ent_lst[xi]

        tokens = x.split()
        ent_i = 0
        iob = []
        for ti in range(len(tokens)):
            if ent_i >= len(ents):
                iob.append('O')
                continue

            rng, tag, _ = ents[ent_i]
            start_i, end_i = rng.start, rng.stop
            if ent_i >= len(ents) or ti < start_i:
                iob.append('O')
            elif ti == start_i:
                iob.append('B-' + tag)
                if start_i == end_i - 1:
                    ent_i = ent_i + 1
            elif start_i < ti < end_i:
                iob.append('I-' + tag)
                if ti == end_i - 1:
                    ent_i = ent_i + 1
        y_res.append(iob)
    return y_res


def print_report(y_gt: list[list[str]], y: list[list[str]], report_name: str = ''):
    """
    Print classification report using sklearn classification_report.
    Input parameters will be converted to a flat structure prior to calculating a report.
    :param y_gt: ground truth labels
    :param y: calculated labels
    :param report_name: Name to printed in the header of the console output
    """
    y_gt_flat = [item for sublist in y_gt for item in sublist]
    y_flat = [item for sublist in y for item in sublist]

    print(f'Classification report: {report_name}')
    print(classification_report(y_gt_flat, y_flat))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Evaluate NER models on TEST data set from "train-test-split.txt". Supported models: Mitie, Stanza')

    parser.add_argument('--mitie', type=str, help='Path to trained MITIE model.')
    parser.add_argument('--stanza', type=str, help='Path to trained Staza model.')
    parser.add_argument('--split_file', type=str, default='doc/dev-test-split.txt',
                        help='Path to txt file with Train/Test split (optional).')

    parser.print_usage()

    args = parser.parse_args()

    if not args.mitie and not args.stanza:
        print('At least one model must be specified to run this script: --mitie, --stanza')
    elif not os.path.exists('data'):
        print("Error: data folder not found. Make sure you are running this script from the ner-uk project root")
    else:
        _, test_files = read_train_test_split(args.split_file)
        print('Reading test data from files')
        x_test, y_test = read_data_to_iob(test_files)

        if args.mitie:
            y_mitie = process_with_mitie(args.mitie, x_test)

        if args.stanza:
            y_stanza = process_with_stanza(args.stanza, x_test)

        # printing reports
        if args.mitie:
            print_report(y_test, y_mitie, 'Mitie')
        if args.stanza:
            print_report(y_test, y_stanza, 'Stanza')
