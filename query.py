#!/usr/bin/env python3

# * Imports

# ** Standard library

import itertools
import json
import logging as log
import random
import re
import sqlite3

from collections import OrderedDict

# ** 3rd-party

from blessings import Terminal

import click
from click_default_group import DefaultGroup

# * Constants

FTS_OPERATORS = {"AND", "OR", "NOT"}

TERM = Terminal()
REPLACEMENT_COLORS = [TERM.red, TERM.green, TERM.yellow, TERM.blue, TERM.magenta, TERM.cyan]
random.shuffle(REPLACEMENT_COLORS)
COLOR_REPLACEMENTS = itertools.cycle([color + r"\g<0>" + TERM.normal
                                      for color in REPLACEMENT_COLORS])

# ** Key regexps

KEY_BOOK_CHAPTER_VERSE_REGEXP = re.compile(r"([^:]+) +(\d+):(\d+)")
KEY_BOOK_CHAPTER_REGEXP = re.compile(r"([^:]+) +(\d+)")
KEY_BOOK_REGEXP = re.compile(r"([^:]+)")

# ** Queries

SELECT_BOOK = "SELECT * FROM verses WHERE book LIKE ?"
SELECT_CHAPTER = "SELECT * FROM verses WHERE book LIKE ? AND chapter=?"
SELECT_VERSE = "SELECT * FROM verses WHERE book LIKE ? AND chapter=? AND verse=?"

SEARCH = "SELECT * FROM verses WHERE text MATCH ?"

# * Functions

def json_to_dict_hierarchical(json):
    """Convert hierarchical JSON object to one-object-per-verse format."""

    pass

def split_key(key):
    "Return book, chapter, and verse for KEY."

    # NOTE: chapter and verse must be converted to integers, because,
    # depending on how the table was created, SQLite does not always
    # accept strings for these numbers.  (The non-FTS table accepted
    # them, but the FTS table requires them to actually be integers.)

    # I wish there were a more elegant way to do this, but Python isn't Lisp.
    m = KEY_BOOK_CHAPTER_VERSE_REGEXP.match(key)
    if m:
        # Book
        return (m.group(1), int(m.group(2)), int(m.group(3)))
    else:
        m = KEY_BOOK_CHAPTER_REGEXP.match(key)
        if m:
            return (m.group(1), int(m.group(2)), None)
        else:
            m = KEY_BOOK_REGEXP.match(key)
            if m:
                return (m.group(1), None, None)

# ** Query

def lookup_json(filename, key):
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

def lookup_sqlite(filename, key):
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
    result = c.fetchall()

    if not result:
        # Try again with wildcard after book name
        book = book + '%'
        if verse:
            c.execute(SELECT_VERSE, (book, chapter, verse))
        elif chapter:
            c.execute(SELECT_CHAPTER, (book, chapter))
        elif book:
            c.execute(SELECT_BOOK, (book,))
        result = c.fetchall()

    return result

# ** Search

def search_json(filename, query):
    "Search JSON FILENAME for QUERY."

    # NOTE: Does not currently support boolean matching like the
    # SQLite function.

    # Split query into keywords
    query = query.split()

    # Load JSON
    with open(filename) as f:
        verses = json.load(f)

    # Find matches
    result = []
    for v in verses:
        found = True
        for key in query:
            if not key in v['text']:
               found = False
               break
        if found:
            result.append(v)

    return result

def search_sqlite(filename, key):
    "Search SQLite database FILENAME for KEY."

    # Connect to database
    con = sqlite3.connect(filename)
    c = con.cursor()
    c.row_factory = sqlite3.Row

    # Run query
    c.execute(SEARCH, (key,))

    return c.fetchall()

# ** Rendering

def render_plain(rows, keywords=None, color_map=None):
    "Print ROWS.  If COLOR_MAP is True, with book/chapter/verse and KEYWORDS highlighted according to it."

    # Remove search operators
    if keywords:
        keywords = filter_keywords(keywords)

    # Print rows
    for row in rows:
        if keywords:
            text = colorize_matches(row['text'], keywords=keywords, color_map=color_map)
        else:
            text = row['text']

        click.secho("%s %s:%s: " % (row['book'], row['chapter'], row['verse']), bold=True, nl=False, color=color_map is not None)
        click.echo(text)

def colorize_matches(s, keywords=None, color_map=None):
    "Return string S with KEYWORDS highlighted."

    for keyword in keywords:
        replacement = color_map[keyword.lower()]
        s = re.sub(keyword, replacement, s, flags=re.IGNORECASE)

    return s

def filter_keywords(keywords):
    "Return KEYWORDS without SQLite FTS4 special operators."

    keywords = set(keywords)
    keywords = keywords.difference(FTS_OPERATORS)

    # Remove words starting with a minus sign.  NOTE: This is correct
    # for FTS basic syntax, but I'm not sure about the extended
    # syntax.  I'm guessing it's uncommon for SQLite to be compiled
    # with the extended syntax, and even if extended syntax is on,
    # it's not likely anyone will search for "-word" and not intend to
    # exclude it.
    keywords = [word for word in keywords
                if not word.startswith("-")]

    return keywords

def render_json(rows):
    "Print ROWS as JSON."

    print(json.dumps(list(row_to_dict(row) for row in rows), indent=2))

def row_to_dict(row):
    "Return ROW as an OrderedDict."

    # NOTE: In Python 3.6, key order is preserved: <https://www.python.org/dev/peps/pep-0468/>
    return OrderedDict(book=row['book'], chapter=row['chapter'], verse=row['verse'], text=row['text'])

# * Click

@click.group(cls=DefaultGroup, default='lookup')
@click.option('-v', '--verbose', count=True)
def cli(verbose):
    "Main CLI function."

    # Setup logging
    if verbose >= 2:
        LOG_LEVEL = log.DEBUG
    elif verbose == 1:
        LOG_LEVEL = log.INFO
    else:
        LOG_LEVEL = log.WARNING

    log.basicConfig(level=LOG_LEVEL, format="%(levelname)s: %(message)s")

# ** Commands

# *** lookup

@click.command()
@click.argument('filename', type=click.Path(exists=True))
@click.argument('key', type=str, nargs=-1)
@click.option('--output', type=str, default='plain')
def lookup(filename, key, output):
    """Lookup KEY (e.g. "Genesis 1:1") in FILENAME (SQLite database or JSON file)."""

    key = " ".join(key)

    # Run query
    if filename.endswith('json'):
        result = lookup_json(filename, key)
    elif filename.endswith('sqlite'):
        result = lookup_sqlite(filename, key)

    # Render result
    if result:
        if output == 'plain':
            render_plain(result)

        elif output == 'json':
            render_json(result)

    else:
        raise click.UsageError("%s not found" % key)

# *** search

@click.command()
@click.argument('filename', type=click.Path(exists=True))
@click.argument('keywords', type=str, nargs=-1)
@click.option('--output', type=click.Choice(['plain', 'json']), default='plain')
def search(filename, keywords, output):
    """Search for KEYWORDS in FILENAME (SQLite database or JSON file)."""

    # Normalize query into a string
    query = " ".join(keywords)
    # Split query into keywords for later
    keywords = filter_keywords(query.split())

    # Run query
    if filename.endswith('json'):
        result = search_json(filename, query)
    elif filename.endswith('sqlite'):
        result = search_sqlite(filename, query)

    # Render result
    if result:
        # Prepare colors
        color_map = {}
        for word in keywords:
            color_map[word.lower()] = next(COLOR_REPLACEMENTS)

        if output == 'plain':
            render_plain(result, keywords=keywords, color_map=color_map)
        elif output == 'json':
            render_json(result)
    else:
        raise click.UsageError("No matches found for: %s" % query)

# * Main

if __name__ == "__main__":
    cli.add_command(lookup)
    cli.add_command(search)

    cli()
