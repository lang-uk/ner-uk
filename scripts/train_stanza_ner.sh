#!/bin/sh

# Launch training process for Stanza model.
# This script will try checkout Stanza repo and will install it as local pip package.
# If word vectors are not provided with '-w' option, the script will also download the default version for Stanza.
# To avoid issues with paths and dir structure, the script must be run from a root of project 'scripts/train_stanza_ner.sh'

# processing arguments

for i in "$@"; do
  case $i in
    -w=*|--word_vec=*)
      wordvec_param="${i#*=}"
      shift # past argument=value
      ;;
    
    *)
      # unknown option
      echo "Parameter '$i' is not supported skipping it."
      ;;
  esac
done


workspace='workspace/stanza'
wordvec_file_name='wordvec_stanza.uk.pt'
wordvec_path=${wordvec_param:-"../$wordvec_file_name"} #relative to stanza-git folder

echo "Word vectors to be read from '$wordvec_path' relative to 'stanza-git' folder."

if [ ! -d $workspace ]
then
    echo 'Creating workspace folder ' + $workspace
    mkdir -p $workspace
fi

# clone stanza git repo to workspace folder
git clone https://github.com/stanfordnlp/stanza.git $workspace/stanza-git
pip3 install $workspace/stanza-git

# download stanza pretrained vectors if those are not provided via cmd line
if [ ! -f "$wordvec_path" ]
then 
    echo "$wordvec_path not found. Trying to download default vectors for stanza."
    curl "https://lang.org.ua/static/downloads/ner-aux/stanza-wordvec-uk.pt.zip" --output "$workspace/wordvec_stanza.uk.pt.zip"
    unzip $workspace/wordvec_stanza.uk.pt.zip -d $workspace
    mv $workspace/uk.pt $workspace/$wordvec_file_name
    rm $workspace/wordvec_stanza.uk.pt.zip
    wordvec_path=../$wordvec_file_name
fi

# Create a link to lang-uk root folder inside stanza workspace.
# Stanza expects lang-uk repo to be checked out on the same level as it is.
mkdir -p $workspace/lang-uk
ln -s "$(pwd)" $workspace/lang-uk/ner-uk

# NER_BASE must be set to ner-uk project root. Stanza will try to look for $NER_BASE/lang-uk/ner-uk/data
cd $workspace/stanza-git && NERBASE="$(pwd)/.." python3 stanza/utils/training/run_ner.py uk-languk --wordvec_pretrain_file $wordvec_path --save_dir "$(pwd)/.."
