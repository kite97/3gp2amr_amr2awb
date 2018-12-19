#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from os import path
from os import walk
from os import listdir
from sys import exit
from struct import unpack
import argparse

Version = 0
Flags = ""
AudioDataLen = 0
AudioDataOffset = 0
EntryCount = 0
MediaType = ""
AudioType = ""
SrcFile = ""
Starts = 0
FourSpaces = "    "
Display = False
Fail_Destfile = 0
Fail_Offset= 0
Fail_Missing = 0
Fail_AudioType= 0

def parse_box(data, align):
    if (data == ""):
        return
    fmt = ">I4s%ds" %(len(data) - 8)
    BoxLen, Box, Content = unpack(fmt, data)
    fullboxs = ["mvhd", "elst", "mdhd", "hdlr", "smhd", "dref", "stsd", "stsc", "stsz", "stco", "stts", "tkhd"]
    if Box in fullboxs:
        parse_fullbox(data, align)
    elif (BoxLen == 0):
        parse_content(Box, Content, Version, Flags, len(data), align)
    elif (BoxLen == 1):
        fmt = ">I4sQ%ds" %(len(data) - 16)
        _, Box, BoxLen, ContentRest = unpack(fmt, data)
        ContentLen = BoxLen - 16
        fmt = ">%ds%ds"%(ContentLen, len(ContentRest) - ContentLen)
        Content, Rest = unpack(fmt, ContentRest)
        parse_content(Box, Content, Version, Flags, BoxLen, align)
        parse_box(Rest, align)
    else:
        ContentRest = Content
        ContentLen = BoxLen - 8
        fmt = ">%ds%ds"%(ContentLen, len(ContentRest) - ContentLen)
        Content, Rest = unpack(fmt, ContentRest)
        parse_content(Box, Content, Version, Flags, BoxLen, align)
        parse_box(Rest, align)

def parse_fullbox(data, align):
    global Version
    global Flags
    fmt = ">I4sB3s%ds" %(len(data) - 12)
    BoxLen, Box, Version, Flags, Content = unpack(fmt, data)
    if (BoxLen == 0):
        parse_content(Box, Content, Version, Flags, len(Content), align)
    elif (BoxLen == 1):
        fmt = ">I4sQB3s%ds" %(len(data) - 20)
        _, Box, BoxLen, Version, Flags, ContentRest = unpack(fmt, data)
        ContentLen = BoxLen - 20
        fmt = ">%ds%ds"%(ContentLen, len(ContentRest) - ContentLen)
        Content, Rest = unpack(fmt, ContentRest)
        parse_content(Box, Content, Version, Flags, BoxLen, align)
        parse_box(Rest, align)
    else:
        ContentRest = Content
        ContentLen = BoxLen - 12
        fmt = ">%ds%ds"%(ContentLen, len(ContentRest) - ContentLen)
        Content, Rest = unpack(fmt, ContentRest)
        parse_content(Box, Content, Version, Flags, BoxLen, align)
        parse_box(Rest, align)

def screen_display(info):
    if Display:
        print info

def parse_content(Box, Content, Version, Flags, BoxLen, align):
    global Starts
    screen_display("%sBox %s @ %d of size: %d, @ ends %d"%(align, Box, Starts, BoxLen, Starts+BoxLen))
    Containers = ["trak", "moov", "minf", "dinf", "edts", "mdia", "stbl"]
    if Box in Containers:
        Starts += 8
    else:
        Starts += BoxLen
    if Box in Containers:
        parse_box(Content, align+FourSpaces)
    elif (Box == "ftyp"):
        fmt = ">4sI%ds" %(len(Content) - 8)
        MajorBrand, MinorVersion, Rest = unpack(fmt, Content)
        CompatibleBrands = []
        while (len(Rest) > 4):
            fmt = ">4s%ds"%(len(Rest) - 4)
            Brand, Rest = unpack(fmt, Rest)
            CompatibleBrands.append(Brand)
        CompatibleBrands.append(Rest)
        screen_display("%s#MajorBrand=%s, MinorVersion=%d, CompatibleBrands=%s"%(align,MajorBrand,MinorVersion,CompatibleBrands))
    elif (Box == "mvhd"):
        if (Version == 0):
            fmt = ">IIII%ds"%(len(Content) - 16)
        elif (Version == 1):
            fmt = ">QQIQ%ds"%(len(Content) - 28)
        _,_,_,Duration,_ = unpack(fmt, Content)
        screen_display("%s#Duration=%d"%(align, Duration))
    elif (Box == "elst"):
        fmt = ">I%ds"%(len(Content) - 4)
        elst_EntryCount, _ = unpack(fmt, Content)
        screen_display("%s#EntryCount=%d"%(align, elst_EntryCount))
    elif (Box == "hdlr"):
        global MediaType
        fmt = ">I4s12s%ds"%(len(Content) - 20)
        _,HeaderType,_,Name = unpack(fmt, Content)
        screen_display("%s#HeaderType=%s, Name=%s"%(align, HeaderType, Name.strip()))
        if (HeaderType == "soun"):
            MediaType = "audio"
        elif (HeaderType == "vide"):
            MediaType = "video"
    elif (Box == "stsd"):
        fmt = ">I%ds"%(len(Content) - 4)
        _, Rest = unpack(fmt, Content)
        Starts = Starts - BoxLen + 16
        parse_box(Rest, align+FourSpaces)
    elif (Box == "sawb"):
        global AudioType
        AudioType = "amrwb"
        screen_display("%s#AudioType=amrwb"%(align))
    elif (Box == "samr"):
        AudioType = "amrnb"
        screen_display("%s#AudioType=amrnb"%(align))
    elif (Box == "stts"):
        entryCount, Rest = unpack(">I%ds"%(len(Content)-4), Content)
        screen_display("%s#entryCount=%d"%(align, entryCount))
        while (len(Rest) > 8):
            sampleCount, sampleDelta, Rest = unpack(">II%ds"%(len(Rest)-8), Rest)
            screen_display("%s#sampleCount=%-5d, sampleDelta=%-5d"%(align, sampleCount, sampleDelta))
        sampleCount, sampleDelta= unpack(">II", Rest)
        screen_display("%s#sampleCount=%-5d, sampleDelta=%-5d"%(align, sampleCount, sampleDelta))
    elif (Box == "stsc"):
        _, FirstChunk, SamplePerChunk, SampleDescIndex = unpack(">IIII", Content)
        screen_display("%s#FirstChunk=%d, SamplePerChunk=%d, SampleDescIndex=%d"%(align,FirstChunk,SamplePerChunk,SampleDescIndex))
    elif (Box == "stsz"):
        global AudioDataLen
        fmt = ">II%ds"%(len(Content) - 8)
        SampleSize, SampleCount, Rest = unpack(fmt, Content)
        EntrySizes = []
        if (MediaType == "audio"):
            if (SampleSize == 0):
                EntrySizes = split_to_int(Rest)
                AudioDataLen = sum(EntrySizes)
                screen_display("%s#AudioDataLen=%d(sum(EntrySizes))"%(align, AudioDataLen))
            else:
                AudioDataLen = SampleSize * SampleCount
                screen_display("%s#AudioDataLen=%d(SampleSize*SampleCount)"%(align, AudioDataLen))
            screen_display("%s#SampleSize=%d, SampleCount=%d, EntrySizes=%s"%(align, SampleSize, SampleCount,EntrySizes))
    elif (Box == "stco"):
        global AudioDataOffset
        global EntryCount
        fmt = ">I%ds"%(len(Content) - 4)
        EntryCount, Rest = unpack(fmt, Content)
        Entrys = split_to_int(Rest.strip())
        screen_display("%s#EntryCount=%d, Entrys=%s"%(align, EntryCount, Entrys))
        if (MediaType == "audio"):
            AudioDataOffset = Entrys[0]
            screen_display("%s#AudioDataOffset=%d"%(align, AudioDataOffset))

def split_to_int(Rest):
    Elements = []
    while (len(Rest) > 4):
        fmt = ">I%ds"%(len(Rest) - 4)
        Element, Rest = unpack(fmt, Rest)
        Elements.append(Element)
    Element = unpack(">I", Rest)
    Elements.append(int("%d"%Element))
    return Elements

def parse_3gp(file):
    global SrcFile
    SrcFile = file
    parse_box(open(file, "rb").read(), "")

def data2file(head, data, dstfile, force):
    global Fail_Destfile
    if not force:
        if path.exists(dstfile):
            screen_display("%s already exist"%dstfile)
            Fail_Destfile += 1
            return False,"%s already exist"%dstfile
    open(dstfile,"wb").write(head + data)
    return True,""

def _3gp2amr(file, ext, ExtStandard, force):
    global Fail_Offset
    global Fail_Missing
    global Fail_AudioType
    if (EntryCount != 1):
        screen_display("Error: OffsetSize != 1")
        Fail_Offset += 1
        return False,"OffsetSize!=1"
    if (AudioDataLen == 0):
        screen_display("Error: missing mandatory box stsz")
        Fail_Missing += 1
        return False,"missing mandatory box stsz"
    if (AudioDataOffset == 0):
        screen_display("Error: missing mandatory box stco")
        Fail_Missing += 1
        return False,"missing mandatory box stco"
    if (AudioType != "amrwb" and AudioType != "amrnb"):
        screen_display("Error: AudioType is not amrnb/amrwb")
        Fail_AudioType += 1
        return False,"AudioType is not amrnb/amrwb"
    Data = open(file, "rb").read()
    RestLen = len(Data) - AudioDataOffset - AudioDataLen
    fmt = "%ds%ds%ds"%(AudioDataOffset, AudioDataLen, RestLen)
    _, AudioData, _ = unpack(fmt, Data)
    if (AudioType == "amrwb"):
        if ExtStandard:
            dstfile = SrcFile.replace("3gp","awb")
        else:
            dstfile = SrcFile.replace("3gp", ext[0])
        return data2file("#!AMR-WB\n", AudioData, dstfile, force)
    elif (AudioType == "amrnb"):
        if ExtStandard:
            dstfile = SrcFile.replace("3gp","amr")
        else:
            dstfile = SrcFile.replace("3gp", ext[0])
        return data2file("#!AMR\n", AudioData, dstfile, force)

def get_3gp_list(folder):
    filelist = []
    for root,dirs,files in walk(folder):
        filelist = filelist + [root+'/'+file for file in files if file[-3:] == "3gp"]
    return filelist

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('file', nargs='*', help='3gpfile(s)')
    parser.add_argument('-f', action='store_true', dest='force', help='if destination file exist, rewrite it')
    parser.add_argument('-r', nargs='?', action='store', dest='folder', default='-', help='recursive folder to get 3gpfiles, folder is current path when value is none')
    parser.add_argument('--log', nargs=1, action='store', help='logfile is result.log when without --log')
    parser.add_argument('--ext', nargs=1, action='store', help='follow standard[*.awb(amrwb)/*.amr(amrnb)] when without --ext')
    parser.add_argument('--info', action='store_true', help='show 3gpfile info but not convert it into amr')
    parser.add_argument('--debug', action='store_true', help='show debug info')
    args = parser.parse_args()
    if (args.log == None):
        logfile = "result.log"
    else:
        logfile = args.log[0]
    if not path.exists(logfile):
        open(logfile,"a").write("%-40s\t%-10s\t%-10s\t%-10s\t%-s\n"%("filename","result","audiotype","datalen","reason"))
    if (args.ext == None):
        ExtStandard = True
    else:
        ExtStandard = False
    if (args.info or args.debug):
        Display = True
    if (len(args.file) == 0):
        if (args.folder == None):
            _3gpfilelist = get_3gp_list(".")
        elif (args.folder == "-"):
            _3gpfilelist = [_3gpfile for _3gpfile in listdir(".") if (path.isfile(_3gpfile) and _3gpfile[-3:] == "3gp")]
        else:
            _3gpfilelist = get_3gp_list(args.folder)
    else:
        _3gpfilelist = [_3gpfile for _3gpfile in args.file if _3gpfile[-3:] == "3gp"]
    totalNum = 0
    sucNum = 0
    failNum = 0
    for _3gpfile in _3gpfilelist:
        screen_display("parse %s starts"%_3gpfile)
        screen_display("*****************************************************************************")
        parse_3gp(_3gpfile)
        if not args.info:
            totalNum += 1
            result, reason = _3gp2amr(_3gpfile, args.ext, ExtStandard, args.force)
            if (result):
                open(logfile,"a").write("%-40s\t%-10s\t%-10s\t%-10d\n"%(_3gpfile,"success",AudioType,AudioDataLen))
                screen_display("convert %s success"%_3gpfile)
                sucNum += 1
            else:
                open(logfile,"a").write("%-40s\t%-10s\t%-10s\t%-10d\t%-s\n"%(_3gpfile,"failed",AudioType,AudioDataLen,reason))
                screen_display("convert %s failed"%_3gpfile)
                failNum += 1
        screen_display("=============================================================================\n")
    if not args.info:
        print "the total num of 3gpfile: %d"%totalNum
        print "success: %d"%sucNum
        print "failed:  %d"%failNum
        print "dstfile already exist:  %d"%Fail_Destfile
        print "unsupport offsetSize>1: %d"%Fail_Offset
        print "missing mandatory box:  %d"%Fail_Missing
        print "AudioType is not amr:   %d"%Fail_AudioType
        open(logfile,"a").write("\n")
