-define(REQKEYS, [ req_alloc_total
                 , req_alloc_succ
                 , req_alloc_fail

                 , req_ack_total
                 , req_ack_succ
                 , req_ack_fail

                 , req_play_total
                 , req_play_succ
                 , req_play_fail

                 , req_pc_total
                 , req_pc_succ
                 , req_pc_fail

                 , req_pr_total
                 , req_pr_succ
                 , req_pr_fail

                 , req_stop_ivr_total
                 , req_stop_ivr_succ
                 , req_stop_ivr_fail

                 , req_release_total
                 , req_release_succ
                 , req_release_fail

                 , req_create_total
                 , req_create_succ
                 , req_create_fail

                 , req_join_total
                 , req_join_succ
                 , req_join_fail

                 , req_unjoin_total
                 , req_unjoin_succ
                 , req_unjoin_fail

                 , req_conf_pa_total
                 , req_conf_pa_succ
                 , req_conf_pa_fail

                 , req_conf_stop_pa_total
                 , req_conf_stop_pa_succ
                 , req_conf_stop_pa_fail

                 , req_conf_pc_total
                 , req_conf_pc_succ
                 , req_conf_pc_fail

                 , req_conf_stop_pc_total
                 , req_conf_stop_pc_succ
                 , req_conf_stop_pc_fail

                 , req_conf_rec_total
                 , req_conf_rec_succ
                 , req_conf_rec_fail

                 , req_conf_stop_rec_total
                 , req_conf_stop_rec_succ
                 , req_conf_stop_rec_fail

                 , req_stop_unknown_total
                 , req_stop_unknown_succ
                 , req_stop_unknown_fail

                 , req_destroy_total
                 , req_destroy_succ
                 , req_destroy_fail
                ]).
-define(TIMEKEYS, [ time_alloc_expend            %处理alloc的时长
                  , time_confirm_expend          %处理confirm的时长
                  , time_play_delay              %响应play的时延
                  , time_playing                 %放音时长
                  , time_digit_collect_delay     %响应digit collect的时延
                  , time_digit_collecting        %收号时长
                  , time_record_delay            %响应录制的时延
                  , time_recording               %录制时长
                  , time_release_expend          %处理release的时长
                  ]).
-define(RTPKEYS, [ rtp_send_lip
                 , rtp_send_sendpkts
                 , rtp_send_sendbytes
                 , rtp_send_lostpkts
                 , rtp_recv_lip
                 , rtp_recv_rcvpkts
                 , rtp_recv_rcvbytes
                 , rtp_recv_lostpkts
                 ]).
-define(KPISENDKEYS, [ kpi_send_lip
                     , kpi_send_rip
                     , kpi_send_sendpkts
                     , kpi_send_sendbytes
                     , kpi_send_lostpkts
                     ]).
-define(KPIRECVKEYS, [ kpi_recv_lip
                     , kpi_recv_rip
                     , kpi_recv_rcvpkts
                     , kpi_recv_rcvbytes
                     , kpi_recv_lostpkts
                     , kpi_recv_lostpktsum
                     , kpi_recv_lostoctetsum
                     , kpi_recv_delaysum
                     , kpi_recv_delaymax
                     , kpi_recv_shakesum
                     , kpi_recv_shakemax
                     , kpi_recv_delayavg
                     , kpi_recv_shakeavg
                 ]).
-define(SESSIONKEYS, [ session_total
                     , session_caps
                     , session_succ
                     , session_fail
                     ]).
-define(WORKERKEYS, [ worker_max
                    , worker_idle
                    , worker_running
                    , worker_peak
                    , worker_blocked
                    ]).
-define(CONFKEYS, [ conf_max
                  , conf_idle
                  , conf_running
                  , conf_peak ]).

-record(status, { start_time
                , version
                , worker_max = 0
                , worker_running = 0
                , worker_peak = 0
                , worker_blocked = 0
                , conf_max = 0
                , conf_running = 0
                , conf_peak = 0
                , session_last_total = 0}).
