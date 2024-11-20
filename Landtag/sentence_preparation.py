import pandas as pd
import numpy as np
import datetime
import re

import spacy
from tqdm import tqdm

# load Spacy
# if model not yet installed, run: python -m spacy download de_core_news_md

nlp = spacy.load("de_core_news_md")
nlp.disable_pipes(
    [
        "tok2vec",
        "tagger",
        "morphologizer",
        "parser",
        "lemmatizer",
        "attribute_ruler",
        "ner",
    ]
)
nlp.enable_pipe("senter")

def speech_to_sentences(df):
    """
    Convert a df with a column "speech" to a df with all sentences.
    """

    # iterate over all speeches
    sent_id = -1
    all_sents = []

    for index, speech in tqdm(df.iterrows(), total=df.shape[0]):
        # extract doc-level information
        text = speech.speech
    #    text = clean_speech(text)
        doc = nlp(text)

        # get sentences of doc to iterate over
        sents = list(doc.sents)
        n_sentences = len(list(doc.sents))

        # skip very short speeches
        if n_sentences < 3:
            continue

        # iterate over sentences + add 1 row per sentence to all_sents
        for sent_no, sent in enumerate(sents, 1):
            sent_id += 1
            # do not use first and last sentence
            if not 1 < sent_no < n_sentences:
                continue

            sent_length = len([tok for tok in sent if not tok.is_punct])
            # skip sentences with less than 3 words
            if sent_length < 5:
                continue

            data = {
                "sent_id": sent_id,
                "speech_id": index,
                "name": speech.speaker,
                "electoral_term": speech.wp,
                "party": speech.party,
                "role": speech.role,
                "date": speech.date,
                "session": speech.session,
                "sentence_no": sent_no,
                "sentence_length": sent_length,
                "sentence": str(sent),
                "issue": speech.issue,
                "issue_total_speeches": speech.issue_total_speeches
            }
            ['speaker', 'party', "speech", "seq", "wp", "role", "date", "issue", "issue_total_speeches"]


            all_sents.append(data)

    return pd.DataFrame(all_sents)

def clean_sents(sentence_df):
    # Clean bad endings
    sentence_df.loc[sentence_df.sentence.str.contains("^(– |- )", case=True), "sentence"] = sentence_df.loc[sentence_df.sentence.str.contains("^(– |- )", case=True), "sentence"].str.split(" ", n=1, expand=True)[1] # Fix sentences that start with – and a space

    # Fix Brackets in speaker name
    sentence_df["name"] = sentence_df["name"].str.replace("[(|)]", "",regex=True)

    # Make all references to CO2 spelled identical
    sentence_df["sentence"] = sentence_df["sentence"].str.replace(r"[C|c][O|o]\s?2", 'CO2 ', regex=True)
    sentence_df["sentence"] = sentence_df["sentence"].str.replace(r"CO2  ", 'CO2 ', regex=True)
    sentence_df["sentence"] = sentence_df["sentence"].str.replace(r'CO2 -', 'CO2-', regex=True)

    # Fix speakers included in speeches
    for i in range(sentence_df.shape[0]):
        u = sentence_df.iloc[i]
        sname = u["name"]
        regexp = re.compile(fr'\w{sname} ')
        if regexp.search(u["sentence"]):
            df.iloc[i,10] = df.iloc[i].sentence.replace(f"{sname} ", '')

    # remove some sentences with greeting and some issue which could potentially contribute to biased results
    sentence_df = sentence_df[
        ~sentence_df.sentence.str.contains(
            "^((meine )?sehr (geehrt|verehrt)|liebe|(meine )?damen und)", case=False
        )  # introductions
        & ~sentence_df.sentence.str.contains("(:|;)$", case=False)  # bad endings
        & ~sentence_df.sentence.str.contains("^([a-z]|-|–)", case=True)  # lowercase sent starts / dashes
    ]

    # remove sentences with a colon as it's often included in sentences with quotes which lead to misclassification
    sentence_df = sentence_df[ ~(sentence_df.sentence.str.contains(": "))]

    return sentence_df
