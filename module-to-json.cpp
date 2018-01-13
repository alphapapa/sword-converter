// This converts an entire SWORD Bible module (not sure about other
// module types) to JSON.

// NOTE: This file is a modified version of lookup.cpp from Sword
// 1.8.0.  Unless and until I figure out how to build this outside of
// the Sword source tree, or as a different file, this file should be
// copied over lookup.cpp and then built as that program.

// TODO: Might be able to use stripText() function, which is mentioned in swmodule.h:

// /** Produces plain text, without markup, of the module entry at the supplied key
//  * @param tmpKey desired module entry
//  * @return result buffer
//  */
// virtual const char *stripText(const SWKey *tmpKey);

/******************************************************************************
 *
 *  lookup.cpp -	Simple example of how to retrieve an entry from a
 *			SWORD module
 *
 * $Id: lookup.cpp 3483 2017-06-25 15:19:34Z scribe $
 *
 * Copyright 1997-2013 CrossWire Bible Society (http://www.crosswire.org)
 *	CrossWire Bible Society
 *	P. O. Box 2528
 *	Tempe, AZ  85280-2528
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation version 2.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 */

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <swmgr.h>
#include <swmodule.h>
#include <swfilter.h>
#include <markupfiltmgr.h>
#include <versekey.h>

using sword::SWMgr;
using sword::MarkupFilterMgr;
using sword::SWModule;
using sword::FMT_WEBIF;
using sword::FMT_HTMLHREF;
using sword::FMT_XHTML;
using sword::FMT_RTF;
using sword::FMT_LATEX;
using sword::FMT_PLAIN;
using sword::ModMap;
using sword::AttributeTypeList;
using sword::AttributeList;
using sword::AttributeValue;
using sword::VerseKey;
using sword::FilterList;

static inline void
replace_all (std::string &str, const std::string& from, const std::string& to)
{
  // https://stackoverflow.com/a/24315631
  size_t start_pos = 0;
  while((start_pos = str.find(from, start_pos)) != std::string::npos) {
    str.replace(start_pos, from.length(), to);
    start_pos += to.length(); // Handles case where 'to' is a substring of 'from'
  }
}

static inline void
escape_text (std::string &s)
{
  // Escape quotes
  replace_all(s, "\"", "\\\"");

  // Escape newlines *literally*, because JSON officially does not
  // allow "control characters" (e.g. newlines) in strings
  replace_all(s, "\n", "\\n");
}

static inline void
json (VerseKey *vk, SWModule *target)
{
  std::string text = std::string(target->renderText().trim());
  escape_text(text);
  std::cout << "{"
            << "\"book\": \"" << vk->getBookName() << "\", "
            << "\"chapter\": " << vk->getChapter() << ", "
            << "\"verse\": " << vk->getVerse() << ", "
            << "\"text\": \"" << text << "\""
            << "},\n";
}

int main(int argc, char **argv)
{

  SWMgr manager(new MarkupFilterMgr(FMT_PLAIN));
  SWModule *target;

  if (argc != 3) {
    fprintf(stderr, "\nusage: %s <modname> <\"lookup key\">\n"
            "\tExample: lookup KJV \"James 1:19\"\n\n", argv[0]);

    exit(-1);
  }

  target = manager.getModule(argv[1]);
  if (!target) {
    fprintf(stderr, "Could not find module [%s].  Available modules:\n", argv[1]);
    ModMap::iterator it;
    for (it = manager.Modules.begin(); it != manager.Modules.end(); ++it) {
      fprintf(stderr, "[%s]\t - %s\n", (*it).second->getName(), (*it).second->getDescription());
    }
    exit(-1);
  }

  // turn all filters to default values
  manager.setGlobalOption("Headings", "Off");
  manager.setGlobalOption("Strong's Numbers", "Off");
  manager.setGlobalOption("Lemmas", "Off");

  VerseKey *vk = dynamic_cast<VerseKey *>(target->getKey());

  if (vk) {
    //	vk->AutoNormalize(false);
    vk->setIntros(true);
    // vk->setText(argv[2]);
    vk->setText("Gen 1:1");
  }
  else {
    target->setKey(argv[2]);
  }

  // NOTE: Not sure if necessary, but it's in lookup.cpp, so it
  // probably is.
  target->renderText();		// force an entry lookup to resolve key to something in the index

  // Start JSON array
  std::cout << "[\n";

  // Output one JSON object per verse
  long last_index;
  while (last_index != vk->getIndex()) {
    if (! vk->getVerse() == 0) {
      // Skip verse "0"
      json(vk, target);
    }
    last_index = vk->getIndex();
    vk->increment();
  }

  // End JSON array
  std::cout << "\n]";

  return 0;
}
