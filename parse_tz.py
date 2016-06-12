#!/usr/bin/env python

import json
import re
import sys


TZRE = re.compile(r'\| *timezone1? *= *\[*([^|\]]+)\]*\s*\|')

NAME_RE = re.compile(r'\| *name *= *([^|\]]+)\s*\|')


redirects = []

tzs = set()


for arg in sys.argv[1:]:
    with open(arg) as json_file:
        cities = json.load(json_file)
        for page in cities['query']['pages'].values():
            title = page['title']
            content = page['revisions'][0]['*']

            if title == 'St. Louis':
                city = title
                state = 'Missouri'
            elif title == 'New York City':
                city = state = 'New York'
            elif title == 'Houston':
                city = title
                state = 'Texas'
            else:
                try:
                    city, state = title.split(', ')
                except ValueError:
                    name = NAME_RE.search(content).group(1)
                    try:
                        city, state = name.strip().split(', ')
                    except ValueError:
                        print(content)
                        print(name)
                        raise ValueError("No name in {}".format(title))

            if content.startswith('#REDIRECT'):
                redirects.append(re.search(r'\[\[([^\]]+)\]\]', content).group(1))
                continue
            else:
                tz = TZRE.search(content)
                try:
                    tz = tz.group(1).rstrip()
                except AttributeError:
                    print(content)
                    raise ValueError("Wrong content in {}".format(title))

            tz = re.sub(' \(.+', '', tz)
            tz = re.sub('North American ', '', tz)
            tz = re.sub(' Time Zone', '', tz)
            tz = re.sub(' Time', '', tz)
            tz = re.sub(' Standard', '', tz)
            tz = re.sub('-.+', '', tz)

            if tz == "CST":
                tz = "Central"
            elif tz == "PST":
                tz = "Pacific"

            tz = tz.lower()

            tzs.add(tz)

            print('usTZ "{city}" "{state}" = {tz}'.format(**locals()))

# print(tzs)

print('|'.join(redirects))
