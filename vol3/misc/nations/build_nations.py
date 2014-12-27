# Quick Python script to aggregate some Wikipedia data.
__author__ = 'jheaton'

import codecs
import re
import csv

def extract(s,first,second) :
    i = s.find(first)
    if i==None:
        return
    i = s.find(first,i+1)
    j = s.find(second,i+1);
    return s[i+len(first):j]

def extract1st(s,first,second) :
    i = s.find(first)
    if i==None:
        return
    j = s.find(second,i);
    return s[i+len(first):j]

def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

# Read the GDP
gdp_map = {}

with codecs.open("/Users/jheaton/data/aifh/gdp.txt", "r", encoding='utf-8') as f:
    start = False
    count = 0
    for line in f:
        if "||" in line and "{{" in line:
            if start:
                country = extract1st(line, "{{flag|", "}}")
                gdp = extract1st(line,"{{nts|", "}}")
                gdp = gdp.replace(',', '')
                gdp_map[country.lower()] = gdp

        if "{| class=\"wikitable sortable\"" in line:
            count = count + 1
            if count==4: start = True

        if "| colspan=3" in line: start = False


# Read the lifepsan
lifespan_map = {}

with codecs.open("/Users/jheaton/data/aifh/lifespan.txt", "r", encoding='utf-8') as f:
    for line in f:
        if line.startswith("|}"):
            break
        if "|" in line and "||" in line:
            country = extract1st(line, "{{flag|", "}}")
            expect = extract(line, "||", "||")
            lifespan_map[country.lower()] = expect.strip()

# Read the literacy
literacy_map = {}

with codecs.open("/Users/jheaton/data/aifh/lit.txt", "r", encoding='utf-8') as f:
    for line in f:
        if line.startswith("|}"):
            break
        if "|" in line and "||" in line:
            country = extract1st(line, "{{flag|", "}}")
            literacy = extract(line, "||", "||")
            literacy = literacy.replace('%','')
            if is_number(literacy):
                literacy = str(round(float(literacy)/100.0,4))
            literacy_map[country.lower()] = literacy.strip()

# Read the country codes

with codecs.open('/Users/jheaton/data/aifh/out.csv', 'w', encoding='utf-8') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['code','country','gdp','lifespan','literacy'])
    start = False
    with codecs.open("/Users/jheaton/data/aifh/countries.txt", "r", encoding='utf-8') as f:
        for line in f:
            if "wikitable sortable" in line:
                start = True
            elif line.startswith("|}"):
                start = False

            if start:
                if line.startswith("|") and line.count("[[")==1:
                    country = re.search("%s(.*)%s" % ("\[\[", "\]\]"), line).group(1)
                elif "<tt>" in line:
                    code = extract(line,"<tt>","</tt>")
                    key = country.lower()
                    if key in gdp_map and key in lifespan_map and key in literacy_map:
                        l = [code,country,gdp_map[key],lifespan_map[key],literacy_map[key]]
                        writer.writerow(l)