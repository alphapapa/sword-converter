#!/usr/bin/env python3

# * Imports

# ** Standard library

import json
import logging as log
import re
import sqlite3

from collections import OrderedDict

# ** 3rd-party

import click
from click_default_group import DefaultGroup

# * Constants

# ** Key regexps

KEY_BOOK_CHAPTER_VERSE_REGEXP = re.compile(r"([^:]+) +(\d+):(\d+)")
KEY_BOOK_CHAPTER_REGEXP = re.compile(r"([^:]+) +(\d+)")
KEY_BOOK_REGEXP = re.compile(r"([^:]+)")

# ** Queries

SELECT_BOOK = "SELECT * FROM verses WHERE book=?"
SELECT_CHAPTER = "SELECT * FROM verses WHERE book=? AND chapter=?"
SELECT_VERSE = "SELECT * FROM verses WHERE book=? AND chapter=? AND verse=?"

# * Functions

def json_to_dict_hierarchical(json):
    """Convert hierarchical JSON object to one-object-per-verse format."""

    pass

def split_key(key):
    "Return book, chapter, and verse for KEY."

    # I wish there were a more elegant way to do this, but Python isn't Lisp.
    m = KEY_BOOK_CHAPTER_VERSE_REGEXP.match(key)
    if m:
        # Book
        return (m.group(1), m.group(2), m.group(3))
    else:
        m = KEY_BOOK_CHAPTER_REGEXP.match(key)
        if m:
            return (m.group(1), m.group(2), None)
        else:
            m = KEY_BOOK_REGEXP.match(key)
            if m:
                return (m.group(1), None, None)

# ** Query

def query_json(filename, key):
    "Query JSON FILENAME for KEY."

    (book, chapter, verse) = split_key(key)

    # Load JSON
    with open(filename) as f:
        verses = json.load(f)

    # Find matches
    if verse:
        result = [v for v in verses
                  if v['book'] == book
                  if v['chapter'] == chapter
                  if v['verse'] == verse]
    elif chapter:
        result = [v for v in verses
                  if v['book'] == book
                  if v['chapter'] == chapter]
    else:
        result = [v for v in verses
                  if v['book'] == book]

    return result

def query_sqlite(filename, key):
    "Query SQLite database FILENAME for KEY."

    (book, chapter, verse) = split_key(key)

    # Connect to database
    con = sqlite3.connect(filename)
    c = con.cursor()
    c.row_factory = sqlite3.Row

    # Run query
    if verse:
        c.execute(SELECT_VERSE, (book, chapter, verse))
    elif chapter:
        c.execute(SELECT_CHAPTER, (book, chapter))
    elif book:
        c.execute(SELECT_BOOK, (book,))  # Note the comma, which is required (thanks, Python)

    return c.fetchall()

# ** Rendering

def render_plain(rows):
    for row in rows:
        print("%s %s:%s: %s" % (row['book'], row['chapter'], row['verse'], row['text']))

def render_json(rows):
    print(json.dumps(list(row_to_dict(row) for row in rows), indent=2))

def row_to_dict(row):
    # NOTE: In Python 3.6, key order is preserved: <https://www.python.org/dev/peps/pep-0468/>
    return OrderedDict(book=row['book'], chapter=row['chapter'], verse=row['verse'], text=row['text'])

# * Click

@click.group(cls=DefaultGroup, default='query')
@click.option('-v', '--verbose', count=True)
def cli(verbose):

    # Setup logging
    if verbose >= 2:
        LOG_LEVEL = log.DEBUG
    elif verbose == 1:
        LOG_LEVEL = log.INFO
    else:
        LOG_LEVEL = log.WARNING

    log.basicConfig(level=LOG_LEVEL, format="%(levelname)s: %(message)s")

# ** Commands

# *** query

@click.command()
@click.argument('filename', type=click.Path(exists=True))
@click.argument('key', type=str)
@click.option('--output', type=str, default='plain')
def query(filename, key, output):
    """Query FILENAME (SQLite database or JSON file) for KEY (e.g. "Genesis 1:1")."""

    # Run query
    if filename.endswith('json'):
        result = query_json(filename, key)
    elif filename.endswith('sqlite'):
        result = query_sqlite(filename, key)

    # Render result
    if result:
        if output == 'plain':
            render_plain(result)

        elif output == 'json':
            render_json(result)

# * Main

if __name__ == "__main__":
    cli.add_command(query)

    cli()
