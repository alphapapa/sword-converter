#!/usr/bin/env python3

# * Imports

# ** Standard library

import json
import logging as log
import os
import sqlite3

# ** 3rd-party

import click

# * Constants

# ** Database queries

CREATE_TABLE = """CREATE TABLE verses (
book TEXT,
chapter INTEGER,
verse INTEGER,
text TEXT,
PRIMARY KEY(book, chapter, verse))
WITHOUT ROWID;"""

CREATE_TABLE_FTS = """CREATE VIRTUAL TABLE verses USING fts4 (
book TEXT,
chapter INTEGER,
verse INTEGER,
text TEXT)"""

INSERT_VERSES = "INSERT INTO verses(book, chapter, verse, text) VALUES (?, ?, ?, ?)"

# * Classes

class VersesIterator():
    # https://docs.python.org/2/library/sqlite3.html#sqlite3.Cursor.executemany
    def __init__(self, data):
        self.iter = iter(list(data))

    def __iter__(self):
        return self

    def next(self):
        current = next(self.iter)

        return (current['book'], current['chapter'], current['verse'], current['text'])

      # Seems that both __next__ and next are required, but they should do the same thing...?
    __next__ = next

# * Functions

def json_to_dict_hierarchical(json):
    """Convert hierarchical JSON object to one-object-per-verse format."""

    pass

# * Click

@click.group()
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
    log.debug("Logging level: %s" % LOG_LEVEL)

# ** Commands

# *** json-to-sqlite

@click.command()
@click.argument('json_filename', type=str)
@click.argument('db_filename', type=str)
@click.option('--force', is_flag=True)
def convert(json_filename, db_filename, force=False):
    """Convert JSON file to SQLite database."""

    # Check for existing database
    if os.path.exists(db_filename):
        if force:
            os.remove(db_filename)
        else:
            raise Exception("Database already exists, --force not used")

    # Read JSON file
    with open(json_filename) as f:
        verses = json.load(f)

    # Some modules (e.g. SBLGNT) don't have the whole Bible.  We
    # should probably have the SWORD-JSON converter skip those, but
    # until then, we can filter them easily here.
    verses = [v for v in verses
              if v['text']]

    # Create database and insert verses
    con = sqlite3.connect(db_filename)
    with con:
        con.execute(CREATE_TABLE_FTS)
        con.executemany(INSERT_VERSES, VersesIterator(verses))
        con.execute("INSERT INTO verses(verses) VALUES('optimize')")
        con.execute("VACUUM verses")

# * Main

if __name__ == "__main__":
    cli.add_command(convert)

    cli()
