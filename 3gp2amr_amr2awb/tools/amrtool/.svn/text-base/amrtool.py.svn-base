#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from os import path
from os import walk
from os import listdir
from os import system
from struct import unpack
import argparse

AMR_MAGIC_NUMBER = "#!AMR"
AMRWB_MAGIC_NUMBER = "#!AMR-WB"
AMR_RATE = [4.75, 5.15, 5.9, 6.7, 7.4, 7.95, 10.2, 12.2]
AMRWB_RATE = [6.6, 8.85, 12.65, 14.25, 15.85, 18.25, 19.85, 23.05, 23.85]

def get_ft_info(rawfile, align):
    FT = -1
    data = open(rawfile, "rb").read()
    if (int(align) == 0):
        fmt = "BB%ds"%(len(data) - 2)
        fst_byte,sec_byte,_ = unpack(fmt, data)
        FT = ((fst_byte & 7) << 1) + (sec_byte >> 7)
    elif (int(align) == 1):
        fmt = "BB%ds"%(len(data) - 2)
        _,sec_byte,_ = unpack(fmt, data)
        FT = sec_byte >> 3
    else:
        print("octet-align=%s error"%align)
    return FT

def get_amr_info(amrfile):
    content = open(amrfile, "rb").read()
    fmt = ">5sB%ds"%(len(content) - 6)
    head,_,data = unpack(fmt, content)
    if (head == AMR_MAGIC_NUMBER):
        fmt = ">8sB%ds"%(len(content) - 9)
        head,_,rest= unpack(fmt, content)
        if (head == AMRWB_MAGIC_NUMBER):
            data = rest
            amrtype = "wb"
        else:
            amrtype = "nb"
        fmt = "B%ds"%(len(data) - 1)
        head,_= unpack(fmt, data)
        FT = head >> 3
        return amrtype, FT
    else:
        return "", 0

def dec_amr_raw(rawfile, amrtype, align):
    dstpcm = ""
    if (amrtype == "nb"):
        dstpcm = path.splitext(rawfile)[0] + ".8000.pcm"
        system("./amrnb_dec_raw %s %s %s"%(rawfile, dstpcm, align))
        system("sox -t raw -w -r 8000 -c 1 -s %s -U %s.wav"%(dstpcm, path.splitext(rawfile)[0]))
    elif (amrtype == "wb"):
        dstpcm = path.splitext(rawfile)[0] + ".16000.pcm"
        system("./amrwb_dec_raw %s %s %s"%(rawfile, dstpcm, align))
        system("sox -t raw -w -r 16000 -c 1 -s %s -U %s.wav"%(dstpcm, path.splitext(rawfile)[0]))
    else:
        print("amrtype=%s error"%amrtype)
    system("rm %s"%dstpcm)

def dec_amr(amrfile):
    amrtype, _ = get_amr_info(amrfile)
    dstpcm = ""
    if (amrtype == "nb"):
        dstpcm = path.splitext(amrfile)[0] + ".8000.pcm"
        system("./amrnb_dec %s %s"%(amrfile, dstpcm))
    elif (amrtype == "wb"):
        dstpcm = path.splitext(amrfile)[0] + ".16000.pcm"
        system("./amrwb_dec %s %s"%(amrfile, dstpcm))
    else:
        print("%s format error"%amrfile)
    return dstpcm

def enc_amr(pcmfile, amrtype, frametype):
    has_temp_file = False
    try:
        rate = int(path.splitext(path.splitext(pcmfile)[0])[1][1:])
    except ValueError:
        print "%s rate is unknown"%pcmfile
        return
    title = path.splitext(path.splitext(pcmfile)[0])[0]
    if (amrtype == "nb"):
        if (rate != 8000):
            system("sox -t raw -w -r %d -c 1 -s %s -t raw -w -r 8000 -c 1 -s temp.8000.pcm"%(rate, pcmfile))
            pcmfile = "temp.8000.pcm"
            has_temp_file = True
        if (len(frametype) == 0):
            for i in range(8):
                system("./amrnb_enc %s %s.%.2f.amr %d"%(pcmfile, title, AMR_RATE[i], i))
        else:
            for i in frametype:
                if (i < 0 or i > 8):
                    print "amrnb ft should between 0 and 8"
                    continue
                system("./amrnb_enc %s %s.%.2f.amr %d"%(pcmfile, title, AMR_RATE[i], i))
    if (amrtype == "wb"):
        if (rate != 16000):
            system("sox -t raw -w -r %d -c 1 -s %s -t raw -w -r 16000 -c 1 -s temp.16000.pcm"%(rate, pcmfile))
            pcmfile = "temp.16000.pcm"
            has_temp_file = True
        if (len(frametype) == 0):
            for i in range(9):
                system("./amrwb_enc %s %s.%.2f.awb %d"%(pcmfile, title, AMRWB_RATE[i], i))
        else:
            for i in frametype:
                if (i < 0 or i > 9):
                    print "amrnb ft should between 0 and 9"
                    continue
                system("./amrwb_enc %s %s.%.2f.awb %d"%(pcmfile, title, AMRWB_RATE[i], i))
    if (has_temp_file):
        system("rm temp*.pcm")

def get_amr_list(folder):
    amrfilelist = get_file_list("amr", folder)
    wbfilelist = get_file_list("awb", folder)
    if (len(wbfilelist) != 0):
        amrfilelist.append(wbfilelist)
    return amrfilelist

def get_file_list(ext, folder):
    filelist = []
    for root,dirs,files in walk(folder):
        filelist = filelist + [root+'/'+file for file in files if path.splitext(file)[1][1:] == ext]
    return filelist

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('file', nargs='*', help='amrfile(s) or pcmfile(s)')
    parser.add_argument('-r', nargs='?', action='store', dest='folder', default='-', help='recursive folder to get inputfiles, folder is current path when value is none')
    parser.add_argument('--dec', action='store_true', help='decode amr')
    parser.add_argument('--enc', action='store_true', help='encode amr')
    parser.add_argument('--trans', action='store_true', help='transcoding amr')
    parser.add_argument('--type', nargs=1, action='store', help='default value is wb')
    parser.add_argument('--mode', nargs='+', action='store', help='default value is all possible values')
    parser.add_argument('--raw', action='store_true', help='decode raw data')
    parser.add_argument('--octet-align', nargs=1, action='store', help='default value is 1(1-oct;0-eff)')
    args = parser.parse_args()
    amrfilelist = []
    rawfilelist = []
    pcmfilelist = []
    FT = []
    amrtype = "wb"
    octet_align = 1
    if (args.mode != None):
        FT = [int(ft) for ft in args.mode]
    if (args.type != None):
        amrtype = args.type[0]
    if (args.octet_align != None):
        octet_align = args.octet_align[0]

    if (len(args.file) == 0):
        if (args.folder == None):
            if (args.enc):
                pcmfilelist = get_file_list("pcm", ".")
            else:
                amrfilelist = get_amr_list(".")
                rawfilelist = get_file_list("raw", ".")
        elif (args.folder == "-"):
            if (args.enc):
                pcmfilelist = [pcmfile for pcmfile in listdir(".") if (path.isfile(pcmfile) and pcmfile[-3:] == "pcm")]
            else:
                amrfilelist = [amrfile for amrfile in listdir(".") if (path.isfile(amrfile) and (amrfile[-3:] == "amr" or amrfile[-3:] == "awb"))]
                rawfilelist = [rawfile for rawfile in listdir(".") if (path.isfile(rawfile) and (rawfile[-3:] == "raw"))]
        else:
            if (args.enc):
                pcmfilelist = get_file_list("pcm", args.folder)
            else:
                amrfilelist = get_amr_list(args.folder)
                rawfilelist = get_file_list("raw", args.folder)
    else:
        if (args.enc):
            pcmfilelist = [pcmfile for pcmfile in args.file if pcmfile[-3:] == "pcm"]
        else:
            amrfilelist = [amrfile for amrfile in args.file if amrfile[-3:] == "amr" or amrfile[-3:] == "awb"]
            rawfilelist = [rawfile for rawfile in args.file if amrfile[-3:] == "raw"]
    if (args.dec):
        if (args.raw):
            print rawfilelist
            for rawfile in rawfilelist:
                dec_amr_raw(rawfile, amrtype, octet_align)
        else:
            print amrfilelist
            for amrfile in amrfilelist:
                dec_amr(amrfile)
    elif (args.enc):
        print pcmfilelist
        for pcmfile in pcmfilelist:
            enc_amr(pcmfile, amrtype, FT)
    elif (args.trans):
        print amrfilelist
        for amrfile in amrfilelist:
            midpcm = dec_amr(amrfile)
            enc_amr(midpcm, amrtype, FT)
    else:
        if (args.raw):
            print rawfilelist
            for rawfile in rawfilelist:
                ft = get_ft_info(rawfile, octet_align)
                print "%-30s FT=%d"%(rawfile, ft)
        else:
            print amrfilelist
            for amrfile in amrfilelist:
                type, ft = get_amr_info(amrfile)
                if (type != ""):
                    print "%-30s %s, FT=%d"%(amrfile, type, ft)
                else:
                    print "%-30s format error"%(amrfile, type, ft)
