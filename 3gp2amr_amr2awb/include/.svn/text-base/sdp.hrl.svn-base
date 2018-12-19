-record(ext, { rate = none
             , channel = none
             , others = []

             , align = eff
             , align_mark = false
             , modeset = none
             , modeset_mark = false

             , mode_switch = 0
             , bitrate = "16.4"

             , framerate = 30
             , framerate_mark = false
             , framesize = {640, 480}
             , framesize_mark = false}).

-record(codec, {type = none, payload, ext = #ext{}}).

-record(media_descript, { port = none
                        , proto = none
                        , addrtype = inet
                        , addr = {0,0,0,0}
                        , codeclist = []
                        , digit = []
                        , sendrecv = none
                        , ptime = none
                        , maxptime = none }).

-record(sdp, { negotype = none
             , mediatype = none
             , addrtype = inet
             , addr = {0,0,0,0}

             , sendrecv = sendrecv
             , sendrecv_mark = false
             , app = []
             , text = []
             , audio = #media_descript{}
             , video = #media_descript{} }).

-define(NET, "IN").
-define(IP4, "IP4").
-define(IP6, "IP6").

-define(AUDIO, "audio").
-define(VIDEO, "video").
-define(APP, "application").
-define(TEXT, "text").

-define(V_LINE, "v=0").
-define(O_LINE, "o=- 0 0 IN").
-define(S_LINE, "s=-").
-define(C_LINE, "c=IN").
-define(T_LINE, "t=0 0").
-define(M_LINE, "m=").
-define(PROTO, "RTP/AVP").
-define(A_LINE, "a=").

-define(PCMU, "pcmu").
-define(PCMA, "pcma").
-define(G723, "G723").
-define(G729, "G729").
-define(AMR, "AMR").
-define(AMRWB, "AMR-WB").
-define(TELEPHONE, "telephone-event").
-define(EVS, "EVS").
-define(H263, "H263").
-define(H264, "H264").
