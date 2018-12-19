%% 类型名称
-define(AUDIO_ULAW, ulaw).
-define(AUDIO_ALAW, alaw).
-define(AUDIO_G723, g723).
-define(AUDIO_G729, g729).
-define(AUDIO_PCM8K8, pcm8k8).
-define(AUDIO_PCM8K16, pcm8k16).
-define(AUDIO_PCM16K8, pcm16k8).
-define(AUDIO_PCM16K16, pcm16k16).
-define(AUDIO_VOX8K, vox8k).
-define(AUDIO_AMRNB, amrnb).
-define(AUDIO_AMRWB, amrwb).
-define(AUDIO_EVS, evs).

%% 频率
-define(AUDIO_16K, 16000).
-define(AUDIO_8K, 8000).

%% PCM位数
-define(AUDIO_8BIT, 8).
-define(AUDIO_16BIT, 16).

-define(AUDIO_8K8BIT, {?AUDIO_8K, ?AUDIO_8BIT}).
-define(AUDIO_8K16BIT, {?AUDIO_8K, ?AUDIO_16BIT}).
-define(AUDIO_16K8BIT, {?AUDIO_16K, ?AUDIO_8BIT}).
-define(AUDIO_16K16BIT, {?AUDIO_16K, ?AUDIO_16BIT}).

%% 单帧音频数据量
-define(AUDIO_ULAW_FRAME_LEN, 160).
-define(AUDIO_ALAW_FRAME_LEN, 160).
-define(AUDIO_PCM8K16_FRAME_LEN, 320).
-define(AUDIO_PCM16K16_FRAME_LEN, 640).
-define(AUDIO_G729_FRAME_LEN, 20).
