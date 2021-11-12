import unittest
from scripts.convert_markup import convert_bsf_2_vulyk, parse_bsf, BsfInfo
import json


class TestBsf2Vulyk(unittest.TestCase):
    def setUp(self) -> None:
        self.vulyk_base = {
            "modifications": [ ],
            "equivs": [ ],
            "protocol": 1,
            "ctime": 0,
            "triggers": [ ],
            "text": "",
            "source_files": ["ann", "txt" ],
            "messages": [ ],
            "sentence_offsets": [],
            "comments": [ ],
            "entities": [],
            "mtime": 0,
            "relations": [ ],
            "token_offsets": [],
            "action": "getDocument",
            "normalizations": [ ],
            "attributes": [ ],
            "events": [ ],
            "document": "",
            "collection": "/"
        }

    def test_empty_vulyk(self):
        data = ''
        bsf_markup = ''
        expected = self.vulyk_base
        self.assertEqual(expected, convert_bsf_2_vulyk(data, bsf_markup))

    def test_no_ents(self):
        data = 'Текст без сутностей'
        bsf_markup = ''
        self.vulyk_base["text"] = data
        self.assertEqual([], convert_bsf_2_vulyk(data, bsf_markup)["entities"])

    def test_tok_idx(self):
        data = """розпорядження землями
в межах , 

визначених"""
        bsf_markup = ""
        tok_idx = [[0, 13], [14, 21],
                   [22, 23], [24, 29], [30, 31], [34, 44]]
        self.vulyk_base["text"] = data
        self.assertEqual(tok_idx, convert_bsf_2_vulyk(data, bsf_markup)["token_offsets"])

    def test_sentence_offset(self):
        data = """Речення номер 1 .

Рядок другий"""
        bsf_markup = ""
        sent_idx = [[0, 17], [18, 18], [19, 31]]
        result = convert_bsf_2_vulyk(data, bsf_markup)
        self.assertEqual(sent_idx, result["sentence_offsets"])
        self.assertEqual([], result["entities"])

    def test_entities_offset(self):
        data = """Речення з Токен . 
токен Другий """
        bsf_markup = """T1 ORG 11 15 Токен
T2 MISC 25 31 Другий"""
        expected = [["T1", "ORG", [[11, 15]]], ["T2", "MISC", [[25, 31]]]]
        result = convert_bsf_2_vulyk(data, bsf_markup)
        self.assertEqual(expected, result["entities"])


if __name__ == '__main__':
    unittest.main()
