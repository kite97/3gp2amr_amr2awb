#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from sys import argv

if __name__ == "__main__":
    data = open(argv[1], "r")
    open("summary", "a").write('filename\tresult\tduration\tformatTag\tsampleRate\tbitsPerSample\tvalue\tdetail\t\n')
    for line in data:
        case = eval(line)
        open("summary", "a").write("%s\t%s\t%s\t%d\t%d\t%d\t%s\t%s\n" %(case['filename'], case['result'], case['duration'], case['formatTag'], case['sampleRate'], case['bitsPerSample'], case['value'], case['detail']))
    data.close()
