%% Generated by the Erlang ASN.1 compiler version:3.0.2
%% Purpose: Erlang record definitions for each named and unnamed
%% SEQUENCE and SET, and macro definitions for each value
%% definition,in module RcsRpsMsg



-ifndef(_RCSRPSMSG_HRL_).
-define(_RCSRPSMSG_HRL_, true).

-record('RcsRpsMessage',{
transactionID, contextID, terminationID, endpointID, callID, command}).

-record('TerminationStruct',{
rpsNo, boardNo, channelNo}).

-record('IntraChannel',{
channelid, channelswitch}).

-record('IntraChannelReply',{
channelid, channelswitch}).

-record('ResInfo',{
totalRes, usingRes, badUseNo, totalConf, usingConf, identifyID}).

-record('IntraAdd',{
remoteSDP = asn1_NOVALUE, ntJt = asn1_NOVALUE, statistics = asn1_NOVALUE, roomID = asn1_NOVALUE, cic = asn1_NOVALUE, bear = asn1_NOVALUE, remoteAddr = asn1_NOVALUE, mediafileAddr = asn1_NOVALUE}).

-record('IntraIP4Addr',{
type, ip, port}).

-record('IntraModify',{
remoteSDP = asn1_NOVALUE, timerlength}).

-record('IntraModifyConfereeAttr',{
roomID, attribute, activeTalker = asn1_NOVALUE, type = asn1_NOVALUE, minSize = asn1_NOVALUE, dualvideo = asn1_NOVALUE, reserve = asn1_NOVALUE, audiomix = asn1_NOVALUE, videolayout = asn1_NOVALUE}).

-record('IntraConfPara',{
roomID, sumNum = asn1_NOVALUE, roomSize = asn1_NOVALUE, deleteWhen = asn1_NOVALUE, term, volumeControl = asn1_NOVALUE, toneClamping, confMode = asn1_NOVALUE, dualvideo = asn1_NOVALUE, reserve = asn1_NOVALUE, audiomix = asn1_NOVALUE, videolayout = asn1_NOVALUE, minSize = asn1_NOVALUE}).

-record('VolumeControlCode',{
up = asn1_NOVALUE, down = asn1_NOVALUE, reset = asn1_NOVALUE}).

-record('IntraConfReserve',{
required = asn1_NOVALUE, number = asn1_NOVALUE, type = asn1_NOVALUE, start, dur = asn1_NOVALUE}).

-record('IntraConfAudioMix',{
id = asn1_NOVALUE, samplerate = asn1_NOVALUE, nloudest = asn1_NOVALUE, asnri = asn1_NOVALUE, asnth = asn1_NOVALUE, gain = asn1_NOVALUE}).

-record('IntraConfGainAgc',{
tgtlvl, maxgain}).

-record('IntraConfGain',{
id = asn1_NOVALUE, amt = asn1_NOVALUE, agc = asn1_NOVALUE}).

-record('IntraConfVideoRoot',{
size, bgcolor = asn1_NOVALUE, bgimage = asn1_NOVALUE}).

-record('IntraConfVideoLyt',{
id = asn1_NOVALUE, type = asn1_NOVALUE, root, selector = asn1_NOVALUE, otherregion = asn1_NOVALUE}).

-record('IntraConfVideoLytSel',{
id = asn1_NOVALUE, method, status = asn1_NOVALUE, blankothers = asn1_NOVALUE, switchinterval = asn1_NOVALUE, speakerview = asn1_NOVALUE, region = asn1_NOVALUE, root = asn1_NOVALUE}).

-record('IntraRegion',{
id, left = asn1_NOVALUE, top = asn1_NOVALUE, relativesize = asn1_NOVALUE, priority = asn1_NOVALUE, title = asn1_NOVALUE, logo = asn1_NOVALUE, freeze = asn1_NOVALUE, titletextcolor = asn1_NOVALUE, titlebackgroundcolor = asn1_NOVALUE, bordercolor = asn1_NOVALUE, borderwidth = asn1_NOVALUE}).

-record('IntraConfStream',{
dir = asn1_NOVALUE, fromcompressed = asn1_NOVALUE, tocompressed = asn1_NOVALUE, fromext = asn1_NOVALUE, toext = asn1_NOVALUE}).

-record('IntraConfStreamAudioExt',{
preferred = asn1_NOVALUE, gain = asn1_NOVALUE, clamp = asn1_NOVALUE}).

-record('IntraConfClamp',{
dtmf = asn1_NOVALUE, tone = asn1_NOVALUE}).

-record('IntraConfStreamVideoExt',{
override = asn1_NOVALUE}).

-record('IntraMove',{
sourceRoomID, destRoomID, attribute = asn1_NOVALUE, echoCancle, tariffTone, administrator, activeTalker, audiostream = asn1_NOVALUE, videostream = asn1_NOVALUE}).

-record('IntraDeleteRoom',{
roomID, auId = asn1_NOVALUE, viId = asn1_NOVALUE}).

-record('IntraSignal',{
signal}).

-record('SdPc',{
type, buffer}).

-record('An',{
file1 = asn1_NOVALUE, file2 = asn1_NOVALUE, file3 = asn1_NOVALUE, file4 = asn1_NOVALUE, file5 = asn1_NOVALUE, file6 = asn1_NOVALUE, file7 = asn1_NOVALUE, file8 = asn1_NOVALUE, file9 = asn1_NOVALUE, file10 = asn1_NOVALUE}).

-record('AudioVideoFile',{
audiofile = asn1_NOVALUE, videofile = asn1_NOVALUE, audiofmt = asn1_NOVALUE, videofmt = asn1_NOVALUE}).

-record('Vn',{
file1 = asn1_NOVALUE, file2 = asn1_NOVALUE, file3 = asn1_NOVALUE, file4 = asn1_NOVALUE, file5 = asn1_NOVALUE, file6 = asn1_NOVALUE, file7 = asn1_NOVALUE, file8 = asn1_NOVALUE, file9 = asn1_NOVALUE, file10 = asn1_NOVALUE}).

-record('TTS',{
fileName = asn1_NOVALUE, text, voiceSourceID = asn1_NOVALUE, speed = asn1_NOVALUE, volume = asn1_NOVALUE, style = asn1_NOVALUE, bgSound = asn1_NOVALUE, lang = asn1_NOVALUE}).

-record('VAR',{
vartype, value, lang = asn1_NOVALUE}).

-record('AudioVideo',{
audiofile = asn1_NOVALUE, videofile = asn1_NOVALUE, audiofmt = asn1_NOVALUE, videofmt = asn1_NOVALUE}).

-record('VnTTS',{
file1 = asn1_NOVALUE, file2 = asn1_NOVALUE, file3 = asn1_NOVALUE, file4 = asn1_NOVALUE, file5 = asn1_NOVALUE, file6 = asn1_NOVALUE, file7 = asn1_NOVALUE, file8 = asn1_NOVALUE, file9 = asn1_NOVALUE, file10 = asn1_NOVALUE}).

-record('FileArry',{
file1 = asn1_NOVALUE, file2 = asn1_NOVALUE, file3 = asn1_NOVALUE, file4 = asn1_NOVALUE, file5 = asn1_NOVALUE, file6 = asn1_NOVALUE, file7 = asn1_NOVALUE, file8 = asn1_NOVALUE, file9 = asn1_NOVALUE, file10 = asn1_NOVALUE}).

-record('AuPa',{
file = asn1_NOVALUE, it = asn1_NOVALUE, iv = asn1_NOVALUE, du = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, ni = asn1_NOVALUE}).

-record('ViPa',{
file = asn1_NOVALUE, it = asn1_NOVALUE, iv = asn1_NOVALUE, du = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, mix = asn1_NOVALUE, ni = asn1_NOVALUE}).

-record('Mix',{
mixable = asn1_NOVALUE, volume = asn1_NOVALUE, audioLocal = asn1_NOVALUE, audioRemote = asn1_NOVALUE, videoLocal = asn1_NOVALUE, videoRemote = asn1_NOVALUE}).

-record('Volume',{
volumetype = asn1_NOVALUE, volume = asn1_NOVALUE}).

-record('ConfPa',{
roomID, an = asn1_NOVALUE, it = asn1_NOVALUE, iv = asn1_NOVALUE, du = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, ni = asn1_NOVALUE}).

-record('AuPc',{
ip = asn1_NOVALUE, rp = asn1_NOVALUE, nd = asn1_NOVALUE, fa = asn1_NOVALUE, sa = asn1_NOVALUE, tp = asn1_NOVALUE, ni = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, cb = asn1_NOVALUE, mx = asn1_NOVALUE, mn = asn1_NOVALUE, fdt = asn1_NOVALUE, idt = asn1_NOVALUE, edt = asn1_NOVALUE, na = asn1_NOVALUE, dp = asn1_NOVALUE, rsk = asn1_NOVALUE, rik = asn1_NOVALUE, rtk = asn1_NOVALUE, psk = asn1_NOVALUE, stk = asn1_NOVALUE, sik = asn1_NOVALUE, eik = asn1_NOVALUE, iek = asn1_NOVALUE, starttimer = asn1_NOVALUE}).

-record('ViPc',{
ip = asn1_NOVALUE, rp = asn1_NOVALUE, nd = asn1_NOVALUE, fa = asn1_NOVALUE, sa = asn1_NOVALUE, tp = asn1_NOVALUE, ni = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, cb = asn1_NOVALUE, mx = asn1_NOVALUE, mn = asn1_NOVALUE, fdt = asn1_NOVALUE, idt = asn1_NOVALUE, edt = asn1_NOVALUE, na = asn1_NOVALUE, dp = asn1_NOVALUE, rsk = asn1_NOVALUE, rik = asn1_NOVALUE, rtk = asn1_NOVALUE, psk = asn1_NOVALUE, stk = asn1_NOVALUE, sik = asn1_NOVALUE, eik = asn1_NOVALUE, iek = asn1_NOVALUE, mix = asn1_NOVALUE, starttimer = asn1_NOVALUE}).

-record('ConfPc',{
roomID, ip = asn1_NOVALUE, rp = asn1_NOVALUE, nd = asn1_NOVALUE, fa = asn1_NOVALUE, sa = asn1_NOVALUE, tp = asn1_NOVALUE, ni = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, cb = asn1_NOVALUE, mx = asn1_NOVALUE, mn = asn1_NOVALUE, fdt = asn1_NOVALUE, idt = asn1_NOVALUE, edt = asn1_NOVALUE, na = asn1_NOVALUE, dp = asn1_NOVALUE, rsk = asn1_NOVALUE, rik = asn1_NOVALUE, rtk = asn1_NOVALUE, psk = asn1_NOVALUE, stk = asn1_NOVALUE, sik = asn1_NOVALUE, eik = asn1_NOVALUE, iek = asn1_NOVALUE, starttimer = asn1_NOVALUE}).

-record('AuPr',{
ip = asn1_NOVALUE, rp = asn1_NOVALUE, ns = asn1_NOVALUE, fa = asn1_NOVALUE, sa = asn1_NOVALUE, rf = asn1_NOVALUE, ni = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, cb = asn1_NOVALUE, prt = asn1_NOVALUE, pst = asn1_NOVALUE, rlt = asn1_NOVALUE, na = asn1_NOVALUE, rsk = asn1_NOVALUE, rik = asn1_NOVALUE, rtk = asn1_NOVALUE, psk = asn1_NOVALUE, stk = asn1_NOVALUE, eik = asn1_NOVALUE, rcdfmt = asn1_NOVALUE, append = asn1_NOVALUE}).

-record('ConfPr',{
roomID, ip = asn1_NOVALUE, rp = asn1_NOVALUE, ns = asn1_NOVALUE, fa = asn1_NOVALUE, sa = asn1_NOVALUE, rf = asn1_NOVALUE, ni = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, cb = asn1_NOVALUE, prt = asn1_NOVALUE, pst = asn1_NOVALUE, rlt = asn1_NOVALUE, na = asn1_NOVALUE, rsk = asn1_NOVALUE, rik = asn1_NOVALUE, rtk = asn1_NOVALUE, psk = asn1_NOVALUE, stk = asn1_NOVALUE, eik = asn1_NOVALUE, rcdfmt = asn1_NOVALUE, append = asn1_NOVALUE}).

-record('ViPr',{
ip = asn1_NOVALUE, rp = asn1_NOVALUE, ns = asn1_NOVALUE, fa = asn1_NOVALUE, sa = asn1_NOVALUE, rf = asn1_NOVALUE, ni = asn1_NOVALUE, sp = asn1_NOVALUE, vl = asn1_NOVALUE, cb = asn1_NOVALUE, prt = asn1_NOVALUE, pst = asn1_NOVALUE, rlt = asn1_NOVALUE, na = asn1_NOVALUE, rsk = asn1_NOVALUE, rik = asn1_NOVALUE, rtk = asn1_NOVALUE, psk = asn1_NOVALUE, stk = asn1_NOVALUE, eik = asn1_NOVALUE, rcdfmt = asn1_NOVALUE, append = asn1_NOVALUE}).

-record('IntraEvent',{
roomID = asn1_NOVALUE, ntNetfail = asn1_NOVALUE, ntQualityalert = asn1_NOVALUE, rtpPltrans = asn1_NOVALUE, eventType, auOc}).

-record('NtNetfail',{
keepActive = asn1_NOVALUE, ntNetfailReturn = asn1_NOVALUE}).

-record('NtQualityalert',{
keepActive = asn1_NOVALUE, ntQalertThreshold}).

-record('RtpPltrans',{
keepActive = asn1_NOVALUE, rtpPltransReturn = asn1_NOVALUE}).

-record('AuOc',{
keepActive = asn1_NOVALUE, auOcReturn = asn1_NOVALUE}).

-record('PaReturnParam',{
returnCode, pt}).

-record('PcReturnParam',{
returnCode, ik = asn1_NOVALUE, ap = asn1_NOVALUE, na = asn1_NOVALUE, dc = asn1_NOVALUE, rc = asn1_NOVALUE}).

-record('PrReturnParam',{
returnCode, vi = asn1_NOVALUE, ap = asn1_NOVALUE, na = asn1_NOVALUE, ri = asn1_NOVALUE, rc = asn1_NOVALUE, rt}).

-record('IntraAddReply',{
addReturn, localSDP, localAddr = asn1_NOVALUE}).

-record('IntraSubReply',{
subReturn, ntDur = asn1_NOVALUE, ntOs = asn1_NOVALUE, ntOr = asn1_NOVALUE, rtpPs = asn1_NOVALUE, rtpPr = asn1_NOVALUE, rtpPl = asn1_NOVALUE, rtpJit = asn1_NOVALUE, rtpDelay = asn1_NOVALUE}).

-record('IntraModifyReply',{
modifyReturn, localSDP}).

-record('IntraStopReply',{
operationID, stopReturn, dur = asn1_NOVALUE}).

-record('IntraAddConfirm',{
remoteSDP, remoteAddr = asn1_NOVALUE}).

-record('IntraBridge',{
calledEndPointID, type = asn1_NOVALUE, stop = asn1_NOVALUE}).

-record('IntraExitRoom',{
sourceRoomID, returnCode}).

-record('IntraBytes',{
type, bytes}).

-record('IntraRtspSetup',{
streamType, mode, url = asn1_NOVALUE, transport = asn1_NOVALUE, ip = asn1_NOVALUE, port = asn1_NOVALUE, filepath = asn1_NOVALUE}).

-record('IntraRtspPlay',{
range = asn1_NOVALUE, scale = asn1_NOVALUE, speed = asn1_NOVALUE}).

-record('IntraRtspPause',{
range = asn1_NOVALUE}).

-record('IntraRtspRecord',{
range = asn1_NOVALUE, scale = asn1_NOVALUE}).

-record('IntraRtspRange',{
npt = asn1_NOVALUE, smpte = asn1_NOVALUE, abst = asn1_NOVALUE}).

-record('IntraRtspDesRep',{
result, sdp = asn1_NOVALUE}).

-record('IntraOSDAdd',{
direction, osdList}).

-record('IntraOSDDelete',{
direction, deleteLayer = asn1_NOVALUE}).

-record('IntraOSDUpdate',{
direction, updateList}).

-record('IntraOSDHide',{
direction, hideLayer = asn1_NOVALUE}).

-record('IntraOSDDisplay',{
direction, displayLayer = asn1_NOVALUE}).

-record('IntraOSDGroup',{
groupID, layerID, groupState = asn1_NOVALUE, groupAttr = asn1_NOVALUE, wallPaper = asn1_NOVALUE}).

-record('IntraOSDWallPaper',{
wallColorSchema = asn1_NOVALUE, wallColorValue = asn1_NOVALUE, wallImage = asn1_NOVALUE}).

-record('IntraOSDTextDescript',{
osdTextString, osdTextFont = asn1_NOVALUE, osdTextSize = asn1_NOVALUE, osdTextStyle = asn1_NOVALUE, osdCharInstead = asn1_NOVALUE, posTop = asn1_NOVALUE, posLeft = asn1_NOVALUE, frontColorSchema = asn1_NOVALUE, frontColorValue = asn1_NOVALUE, frontAlpha = asn1_NOVALUE, bgColorSchema = asn1_NOVALUE, bgColorValue = asn1_NOVALUE, bgAlpha = asn1_NOVALUE, charSpace = asn1_NOVALUE, lineSpace = asn1_NOVALUE, areaWidth = asn1_NOVALUE, areaHeight = asn1_NOVALUE, charCountInLine = asn1_NOVALUE, lineCount = asn1_NOVALUE, textFlash = asn1_NOVALUE, textMoving = asn1_NOVALUE}).

-record('IntraOSDImageDescript',{
imagePath, imageBgDisplay = asn1_NOVALUE, imageAlpha = asn1_NOVALUE, imageFlash = asn1_NOVALUE, imageMoving = asn1_NOVALUE, posTop = asn1_NOVALUE, posLeft = asn1_NOVALUE, zoomType = asn1_NOVALUE, imageWidth = asn1_NOVALUE, imageHeight = asn1_NOVALUE}).

-record('IntraTestAlive',{
seqNo, rcsMediaType = asn1_NOVALUE}).

-endif. %% _RCSRPSMSG_HRL_
