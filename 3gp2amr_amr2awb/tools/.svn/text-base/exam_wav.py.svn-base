#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from os import path
from os import walk
from sys import argv
from sys import exit
from struct import unpack

VALUE = ""
DETAIL = ""

def parse_riff_chunk(data, result):
    global VALUE
    global DETAIL
    VALUE = ""
    DETAIL = ""
    fmt = "<4si4s%ds" %(len(data) - 12)
    ChunkID,ChunkSize,WaveID,wave_chunks = unpack(fmt, data)
    if ChunkID != "RIFF":
        result = "error"
        VALUE += "RIFF_ChunkID = %s;"%ChunkID
        DETAIL += "Error:RIFF_ChunkID should be RIFF;"
    if ChunkSize != 4 + len(wave_chunks):
        VALUE += "RIFF_ChunkSize = %d, RIFF_RealSize = %d;"%(ChunkSize, 4+len(wave_chunks))
        DETAIL += "Info:RIFF_ChunkSize should be equal to RIFF_RealSize;"
    if WaveID != "WAVE":
        result = "error"
        VALUE += "WAVE_ChunkID = %s;"%WaveID
        DETAIL += "Error:WAVE_ChunkID should be WAVE;"
    return wave_chunks, result

def parse_fmt_chunk(data, result):
    global VALUE
    global DETAIL
    fmt = "<4sihhiihh%ds" %(len(data) - 24)
    ChunkID,ChunkSize,FormatTag,Channel,SampleRate,DataRate,BlockSize,BitsPerSample,rest = unpack(fmt, data)
    if ChunkID != "fmt ":
        result = "error"
        VALUE += "fmt_ChunkID = %s;"%ChunkID
        DETAIL += "Error:fmt_ChunkID should be fmt ;"
    if ChunkSize != 16 and ChunkSize != 18:
        result = "error"
        VALUE += "fmt_ChunkSize = %d;"%ChunkSize
        DETAIL += "Error:fmt_ChunkSize should be 16 or 18;"
    if FormatTag != 1 and FormatTag != 6 and FormatTag != 7:
        result = "error"
        VALUE += "FormatTag = %d;"%FormatTag
        DETAIL += "Error:FormatTag should be 1(pcm) or 6(alaw) or 7(ulaw);"
    if Channel != 1: 
        result = "error"
        VALUE += "Channel = %d;"%Channel
        DETAIL += "Error:Channel should be 1;"
    if SampleRate != 8000:
        result = "error"
        VALUE += "SampleRate = %d;"%SampleRate
        DETAIL += "Error:SampleRate should be 8000;"
    if BitsPerSample != 8 and BitsPerSample != 16:
        result = "error"
        VALUE += "BitsPerSample = %d;"%BitsPerSample
        DETAIL += "Error:BitsPerSample should be 8 or 16;"
    if ChunkSize == 16 and (FormatTag == 6 or FormatTag == 7):
        VALUE += "fmt_ChunkSize = 16, FormatTag = %d;"%FormatTag
        DETAIL += "Info:non_pcm without cbSize and fact chunk;"
    if ChunkSize == 18 and FormatTag == 1:
        VALUE += "fmt_ChunkSize = 18, FormatTag = 1;"
        DETAIL += "Info:pcm has cbSize;"
    if (FormatTag == 6 or FormatTag == 7) and BitsPerSample != 8:
        result = "error"
        VALUE += "FormatTag = %d; BitsPerSample = %d;"%(FormatTag,BitsPerSample)
        DETAIL += "Error:BitsPerSample of non_pcm must be 8;"
    if DataRate != SampleRate * Channel * BitsPerSample / 8:
        DETAIL += "Info:DateRate unexpected;"
    if BlockSize != Channel * BitsPerSample / 8:
        DETAIL += "Info:BlockSize unexpected;"
        
    if ChunkSize == 18:
        cbSize,chunks = unpack("<h%ds"%(len(rest)-2), rest)
        if cbSize == 0x6164 or cbSize == 0x6166:
            DETAIL += "Info:fmt_ChunkSize=18, but fmt_Chunk without cbSize;" #imp does not support this case
        else:
            rest = chunks
    some_ChunkID,_ = unpack("<4s%ds"%(len(rest)-4), rest)
    if some_ChunkID == "fact":
        fact_chunk,data_chunk = unpack("<12s%ds"%(len(rest)-12), rest)
        return data_chunk, result, FormatTag, SampleRate, BitsPerSample, Channel
    else:
        return rest, result, FormatTag, SampleRate, BitsPerSample, Channel

def parse_data_chunk(data, result):
    global VALUE
    global DETAIL
    ChunkID,ChunkSize,Samples = unpack("4si%ds" %(len(data) - 8), data)
    if ChunkID != "data":
        result = "error"
        VALUE += "data_ChunkID = %s;"%ChunkID
        DETAIL += "Error:data_ChunkID should be data;"
    if ChunkSize != len(Samples) and ChunkSize != len(Samples) - 1:
        VALUE += "data_ChunkSize = %d; data_RealSize = %d;"%(ChunkSize, len(Samples))
        DETAIL += "Info:data_ChunkSize should be equal to data_RealSize(-1);"
    length = (ChunkSize <= len(Samples)) and ChunkSize or len(Samples)
    return result, length

def get_duration(data, sampleRate, bitsPerSample, channel):
    if sampleRate != 0 and bitsPerSample != 0 and channel !=0:
        return ("%d:%.3f" %(float(data)/channel/sampleRate/(float(bitsPerSample)/8)/60
                              , float(data)/channel/sampleRate/(float(bitsPerSample)/8)%60)).rjust(9)
    else:
        return "-".rjust(9)
    
def parse_wave(wavefile):
    if wavefile[-3:] != "wav":
        print "%s is not a wave file!" %wavefile
        exit(0)
    data = open(wavefile, "rb").read()
    if len(data) < 44:
        return {'filename':wavefile, 'result':'error', 'value':'', 'detail':'wave head broken', 'duration':'-', 'formatTag':0, 'sampleRate':0, 'bitsPerSample':0}
    wave_chunks, result = parse_riff_chunk(data, "")
    data_chunk, result, formatTag, sampleRate, bitsPerSample, channel = parse_fmt_chunk(wave_chunks, result)
    result, data_length = parse_data_chunk(data_chunk, result)
    duration = get_duration(data_length, sampleRate, bitsPerSample, channel)
    if result == "":
        return {'filename':wavefile, 'result':'correct', 'value':VALUE, 'detail':DETAIL, 'duration':duration, 'formatTag':formatTag, 'sampleRate':sampleRate, 'bitsPerSample':bitsPerSample}
    else:
        return {'filename':wavefile, 'result':result, 'value':VALUE, 'detail':DETAIL, 'duration':duration, 'formatTag':formatTag, 'sampleRate':sampleRate, 'bitsPerSample':bitsPerSample}

def get_wave_list(folder):
    for root,dirs,files in walk(folder):
        return [root+'/'+file for file in files if file[-3:] == "wav"]

def parse_waves(src_folder, dst_file):
    for wavefile in get_wave_list(src_folder):
        open(dst_file, "a").write(repr(parse_wave(wavefile))+'\n')
            
if __name__ == "__main__":
    if path.isfile(argv[1]):
        print repr(parse_wave(argv[1]))+'\n'
    else:
        parse_waves(argv[1], argv[2])
        print "exam over!\nexam %d wav files total!"% len(get_wave_list(argv[1]))
