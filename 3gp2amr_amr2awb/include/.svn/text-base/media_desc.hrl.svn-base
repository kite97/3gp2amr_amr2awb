-record(audio_desc, { port
                    , codec
                    , payload
                    , digit
                    , amr_ext
                    , evs_ext}).
-record(digit_desc, { type
                    , payload = none}).

-record(media_ctrl, { type
                    , filenames = []
                    , times = 1
                    , interval = 0
                    , max_time = infinity
                    , barge}).

-record(digit_timer, { fdt = infinity
                     , idt = 4
                     , start_fdt_timer = false}).
-record(digit_dgt, { min
                   , max
                   , rik
                   , eik}).
-record(digit_pattern, { pattern }).

-record(digit_ctrl, { timer = #digit_timer{}
                    , digit = #digit_pattern{pattern = ""}
                    , media
                    , times = 1
                    , barge = false}).
-record(video_desc, { port
                    , codec
                    , payload
                    , framerate}).

-record(record_ctrl, { type
                     , file_name
                     , format
                     , media
                     , max_time = infinity
                     , barge
                     , term_key}).
