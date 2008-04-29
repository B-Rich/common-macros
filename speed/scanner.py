import string, time

def parsetext (filename):
    file = open(filename)
    dict = {}
    while 1:
        line = file.readline()
        if line == "":
            break
        splt = string.split(line, "~")
        if len(splt) != 3:
            print line
        else:
            (id, attribute, value) = splt
            id = string.strip(id)
            attribute = string.strip(attribute)
            value = string.strip(value)
            if not dict.has_key(id):
                dict[id] = {}
            field_dict = dict[id]
            field_dict[attribute] = value
    file.close()
    None

import sys

parsetext(sys.argv[1])
