from bsf_beios.bsf_to_beios import BsfInfo, parse_bsf
from mitie import *

cpu_threads = 4


# Read dev/test split from config
dev_files, test_files = [], []
container = dev_files
with open('../doc/dev-test-split.txt', 'r') as f:
    for ln in f:
        ln = ln.strip()
        if ln == 'DEV':
            container = dev_files
        elif ln == 'TEST':
            container = test_files
        elif ln == '':
            pass
        else:
            container.append(ln)
            
print(f'Loaded corpus file split configuration (documents): DEV={len(dev_files)}, TEST={len(test_files)}')


# convert char offset in ner-uk markup to token based MITIE markup
# and create MITIE samples
base_path = '../data/'
samples = []
for f_name in dev_files:
    # read ann
    with open (base_path + f_name + '.ann', 'r') as f:
        annotations = parse_bsf(f.read())
    # read tokens
    with open (base_path + f_name + '.txt', 'r') as f:
        tok_txt = f.read()
    
    tokens = tok_txt.split()
#     print(annotations)
#     print(tokens)
    
    # convert char offset to token offset
    tok_ann = []
    tok_idx = 0

    ann: BsfInfo
    for ann in annotations:
        tok_start = 0
        in_token = False
        for i in range(tok_idx, len(tokens)):
            tok_idx = i + 1            
            if not in_token and ann.token.startswith(tokens[i]):
                tok_start = i
                tok_end = i + 1
                in_token = (len(ann.token) != len(tokens[i]))
#                 print(f't={ann.token}, tok_start={tok_start}, tok_end={tok_end}, in_token={in_token}, tok_idx={tok_idx}')
                if len(ann.token) == len(tokens[i]):
                    break
            elif in_token and ann.token.endswith(tokens[i]):
                tok_end = i+1
                in_token = False
                break
        tok_ann.append(BsfInfo(ann.id, ann.tag, tok_start, tok_end, ann.token))
        
#     print(tok_ann)
    # Create MITIE sample 
    sample = ner_training_instance(tokens)
    for t_ann in tok_ann:
        sample.add_entity(xrange(t_ann.start_idx, t_ann.end_idx), t_ann.tag)
    samples.append(sample)
        
print(f'Converted to MITIE format. Sample documents {len(samples)}')


## Training

trainer = ner_trainer("../../artifacts/mitie/total_word_feature_extractor.dat")

for s in samples:
    trainer.add(s)

trainer.num_threads = cpu_threads

print("Launching training process... hold on tight... gonna be slow")
# takes long here
ner = trainer.train()

ner.save_to_disk("../../artifacts/mitie/ner_model.dat")

