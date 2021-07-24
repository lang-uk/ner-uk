#!/bin/sh

workspace='workspace/stanza'
wordvec_file_name='wordvec_stanza.uk.pt'
wordvec_path=../$wordvec_file_name #relative to stanza-git folder

if [ ! -d $workspace ]
then
    echo 'Creating workspace folder ' + $workspace
    mkdir -p $workspace
fi

# clone stanza git repo to workspace folder
git clone https://github.com/gawy/stanza.git --branch ner-languk-def-split --single-branch $workspace/stanza-git
pip3 install $workspace/stanza-git

# download stanza pretrained vectors if those are not provided via cmd line
if [ ! -f $workspace/$wordvec_file_name ] 
then 
    echo "$workspace/wordvec_stanza.uk.pt not found. Trying to download."
    curl https://dl.dropboxusercontent.com/s/13vrv639z1u42qn/stanza-wordvec-uk.pt.zip?dl=0 --output $workspace/wordvec_stanza.uk.pt.zip
    unzip $workspace/wordvec_stanza.uk.pt.zip -d $workspace
    mv $workspace/uk.pt $workspace/$wordvec_file_name
    rm $workspace/wordvec_stanza.uk.pt.zip
    wordvec_path=../$wordvec_file_name
fi

mkdir -p $workspace/lang-uk
ln -s `pwd` $workspace/lang-uk/ner-uk

# NER_BASE must be set to ner-uk project root. Stanza will try to look for $NER_BASE/lang-uk/ner-uk/data
cd $workspace/stanza-git && NERBASE=`pwd`/.. python3 stanza/utils/training/run_ner.py uk-languk --wordvec_pretrain_file $wordvec_path --save_dir `pwd`/..
