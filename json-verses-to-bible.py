#!/usr/bin/env python3

# Convert JSON list of verse objects to single, hierarchical Bible
# object.

import json
import sys

from collections import OrderedDict

filename = sys.argv[1]

with open(filename) as f:
    verses = json.load(f)

bible = OrderedDict()

current_book = OrderedDict()
current_book_name= None
current_chapter = OrderedDict()
current_chapter_number = None

for verse in verses:

    if ((not int(verse['chapter']) == current_chapter_number)
        or (not verse['book'] == current_book_name)):
        # New chapter

        if len(current_chapter) > 0:
            # Add old chapter
            current_book[current_chapter_number] = current_chapter

        current_chapter_number = int(verse['chapter'])
        current_chapter = OrderedDict()

    if not verse['book'] == current_book_name:
        # New book

        if len(current_book) > 0:
            # Add old book
            bible[current_book_name] = current_book

        current_book_name = verse['book']
        current_book = OrderedDict()

    current_chapter[int(verse['verse'])] = verse['text']

# Add last book
current_book[current_chapter_number] = current_chapter
bible[current_book_name] = current_book

print(json.dumps(bible, indent=2))
