%%%-------------------------------------------------------------------
%%% @author wanglihe <wanglihe@ebupt.com>
%%% @copyright (C) 2014, wanglihe
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2014 by wanglihe <wanglihe@ebupt.com>
%%%-------------------------------------------------------------------

-record(isofile, {ftyp, moov, mdat, duration, trak = []}).
-record(trak, { media_type
              , encoding
              , sample_size
              , sample_count
              , sound_type
              , video_type
              , chunk_offset
              , sps
              , pps
              , chunk_sample_count}).
-define(NAL_UNIT_TYPE_IDR, 5).
-define(NAL_UNIT_TYPE_SEI, 6).
-define(NAL_UNIT_TYPE_SPS, 7).
-define(NAL_UNIT_TYPE_PPS, 8).
