-record(icp_message, { length=0,
                       protocol_type,
                       frame_type,
                       sid,
                       did,
                       slot_num,
                       bph,
                       frame}).
-record(icp_frame, {areaid,
                    host,
                    sender,
                    code,
                    frame_body}).

-record(icp_frame_body, {length,
                         content}).

-record(command_alloc_req, {ref_num,
                            board_selector,
                            trunk_mid,
                            trunk_slot,
                            specialize_resource_type,
                            bc_type,
                            assoc_rs_slot,
                            nbup_mode,
                            media_attr,
                            options}).


-record(command_alloc_cnf, {ref_num,
                            ivr_slot,
                            alloc_result,
                            options}).

-record(command_release_req, {ref_num,
                              ivr_slot}).

-record(command_release_cnf, {ref_num,
                              ivr_slot,
                              release_result}).

-record(rtp_para, {id,
                   length,
                   conn_addr,
                   media_port,
                   payload_type,
                   packet_time}).

-define(IPS_200_BG_A_IVRALLOC_REQ,	607).
-define(IPS_200_BG_A_IVRALLOC_CNF,	608).
-define(IPS_200_BG_A_IVRSERVICE_REQ, 609).
-define(IPS_200_BG_A_IVRSERVICE_CNF, 610).
-define(IPS_200_BG_A_IVRRELEASE_REQ, 611).
-define(IPS_200_BG_A_IVRRELEASE_CNF, 612).
-define(IPS_200_BG_A_IVREVENT_IND,	613).
-define(IPS_200_BG_A_CONFCREATE_REQ, 600).
-define(IPS_200_BG_A_CONFCREATE_CNF, 601).
-define(IPS_200_BG_A_CONFCONTROL_REQ, 602).
-define(IPS_200_BG_A_CONFCONTROL_CNF, 603).
-define(IPS_200_BG_A_CONFDELETE_REQ, 604).
-define(IPS_200_BG_A_CONFDELETE_CNF, 605).
-define(IPS_200_BG_A_CONFEVENT_IND, 606).
-define(IPS_200_BG_A_IVREVENT_RSP,	614).
-define(IPS_200_BG_A_IVRUNISERVICE_REQ, 615).
-define(IPS_200_BG_A_BRIDGERESCONN_REQ, 622).
-define(IPS_200_BG_A_BRIDGERESCONN_CNF, 623).
-define(IPS_200_BG_A_FALLBACK_REQ, 624).
-define(IPS_200_BG_A_FALLBACK_CNF, 625).
-define(IPS_200_BG_A_MEDIARES_ALLOC_REQ, 626).
-define(IPS_200_BG_A_MEDIARES_ALLOC_CNF, 627).
-define(IPS_200_BG_A_IPCONFCREATE_REQ, 709).
-define(IPS_200_BG_A_IPCONFCREATE_CNF, 710).
-define(IPS_200_BG_A_IPCONFCONTROL_REQ, 711).
-define(IPS_200_BG_A_IPCONFCONTROL_CNF, 712).
-define(IPS_200_BG_A_IPCONFDELETE_REQ, 713).
-define(IPS_200_BG_A_IPCONFDELETE_CNF, 714).
-define(IPS_200_BG_A_IPCONFEVENT_IND, 715).
-define(IPS_200_BG_A_IPCONFEVENT_RSP, 716).

%%在icp中定义的媒体类型
-define(MEDIA_ALAW, 1).
-define(MEDIA_ULAW, 4).
-define(MEDIA_AMRNB, 5).
-define(MEDIA_AMRWB, 10).
-define(MEDIA_EVS, 71).
-define(MEDIA_H264, 8).
