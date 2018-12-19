#include "serviceInterface.hpp" 
#include "dbwrapper.hpp"
#include "rswrapper.hpp"
#include <time.h>
#include <stdlib.h>
#include <msmlInterface.hpp>
#include <sys/types.h>
#include <unistd.h>
char lib_version[] = "MRBT-AUPC-EBMS [Ver0.01 20130911(CONFIRM)]";

static long long COUNT = 0;


//超时事件定义
#define SESSION_TIMEOUT 1

//定义事件处理函数{
extern "C" char* tfsendMS(TSCSM * fsm);
extern "C" char* send200caller(TSCSM * fsm);
extern "C" char* sendInviteMGW(TSCSM * fsm);
extern "C" char* sendAckMGW(TSCSM * fsm);
extern "C" char* sendAckMS(TSCSM * fsm);
extern "C" char* sendINFOMS(TSCSM * fsm);
extern "C" char* sendInfoMGW(TSCSM * fsm);
extern "C" char* send200MS(TSCSM * fsm);
extern "C" char* send200MGW(TSCSM * fsm);
extern "C" char* todomsinviteok(TSCSM * fsm);
extern "C" char* todomgwinviteok(TSCSM * fsm);
extern "C" char* todomsinfook(TSCSM * fsm);
extern "C" char* analysismsinfo(TSCSM * fsm);
extern "C" char* analysismgwinfo(TSCSM * fsm);
extern "C" char* todoinvite(TSCSM * fsm);
extern "C" char* todoinviteok(TSCSM * fsm);
extern "C" char* todomsbye200(TSCSM * fsm);
extern "C" void randomFileName(TSCSM * fsm);
extern "C" void   settime(TSCSM * fsm , char* name);
extern "C" long long gettime(TSCSM * fsm , char* name);
extern "C" char* todorelease(TSCSM * fsm);
extern "C" char* todomsbye(TSCSM * fsm);
extern "C" char* todomgwbye(TSCSM * fsm);
extern "C" char* todocallerbye(TSCSM * fsm);
extern "C" char* todotimeout(TSCSM * fsm);
extern "C" char* todomgwinfook(TSCSM * fsm);

void sendBufferHeadertoCCB(TSCSM * fsm);
void getParamFromCCB(TSCSM * fsm);


//定义事件处理函数}


//注册事件处理函数列表
TDealFuncItem lib_cmdtable[] = {
     { "send200caller",		send200caller},
    { "tfsendMS",		tfsendMS},
    { "sendInviteMGW",		sendInviteMGW},
    { "sendAckMGW",		sendAckMGW},  
    { "sendAckMS",		sendAckMS},
    { "sendINFOMS",		sendINFOMS},
    { "sendInfoMGW",		sendInfoMGW},
    { "send200MS",		send200MS},
    { "send200MGW",		send200MGW},    
    { "todoinvite",		todoinvite},   
    { "todomsinviteok",		todomsinviteok},
    { "todomgwinviteok",		todomgwinviteok},
    { "todomsinfook",		todomsinfook},
    { "analysismsinfo",		analysismsinfo},
    { "analysismgwinfo",		analysismgwinfo},
    { "todomsbye200",		todomsbye200},
    {"todorelease",	todorelease},   
    {"todomsbye",	todomsbye},
    {"todomgwbye",	todomgwbye},
    {"todocallerbye",	todocallerbye},
    {"todotimeout",	todotimeout},
    {"todomgwinfook",	todomgwinfook},
    { 0, 0}
};

extern "C" char* todotimeout(TSCSM * fsm)
{
	int type = 	getReceiveIntMsgPara(fsm, "timeMark");
	printf("time out type is %d\n",type);
	switch (type){
		case SESSION_TIMEOUT:
		{
			printf("session timeout\n");	
			return("service.end");
		}
		default:
			return("timeout");
	}
	return("timeout");	
}

extern "C" char* todoinvite(TSCSM * fsm)
{
	//统计 InviteTimes
	return("todoinvite.end");	
}

extern "C" char* send200caller(TSCSM * fsm)
{
       char* sip_ch2_choice_request_sipversion = getReceiveStrMsgPara(fsm,"sip.ch2.choice.request.sip_version");
			 newMessage(fsm,"SIP","TRANSMIT");
			 setIntMsgPara(fsm,"sip.ch2.present",2);//必选 1: req    2:rsp
       setStrMsgPara(fsm,"sip.ch2.choice.response.sip_version",sip_ch2_choice_request_sipversion);
       setStrMsgPara(fsm,"sip.ch2.choice.response.status_code","200");
       setStrMsgPara(fsm,"sip.ch2.choice.response.reason_phrase","OK");       
       
	sendMessage(fsm,"sip.caller");
	
	return("send200caller.end");
}

extern "C" char* tfsendMS(TSCSM * fsm)
{       
	//string tempnum[16];     
	newMessage(fsm,"SIP","MS.INVITE");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	setIntMsgPara(fsm,"sip.index",0);
	setStrMsgPara(fsm,"sip.cseq.method","INVITE");
	//long num=1;
	//sprintf(tempnum,"%ld",num);
	setIntMsgPara(fsm,"sip.cseq.num",1);
	setCCBInt(fsm,"sip.ms0.sequece",1);
	sendMessage(fsm,"sip.ms");
	settime(fsm , "sendmsinvite");
	
	setTimer(fsm, SESSION_TIMEOUT, 30, SecTimer );
  statistics("MSInvites");
	
	return("tfsendMS.end");
}

extern "C" char* sendInviteMGW(TSCSM * fsm)
{
       //string tempnum[16];
	newMessage(fsm,"SIP","MS.INVITE");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	setIntMsgPara(fsm,"sip.index",1);
	setStrMsgPara(fsm,"sip.cseq.method","INVITE");
	//long num=1;
	//sprintf(tempnum,"%ld",num);
	setStrMsgPara(fsm,"sip.cseq.num","1");
	setCCBInt(fsm,"sip.ms1.sequece",1);
	char* content=getCCBStr(fsm,"ms_sdp"); 
	printf("content is %s\n",content);
	setStrMsgPara(fsm,"sip.body",content);
	char *length=getCCBStr(fsm,"ms_sdp_len");
	printf("body.length=%s\n",length);
	setStrMsgPara(fsm,"sip.content_length",length);

	char *contenttype=getCCBStr(fsm,"ms_content_type");
	printf("contenttype=%s\n",contenttype);
	setStrMsgPara(fsm,"sip.content_type.mediarange.type", contenttype);
	char *contentsubtype=getCCBStr(fsm,"ms_content_subtype");
	printf("contentsubtype=%s\n",contentsubtype);
	setStrMsgPara(fsm,"sip.content_type.mediarange.subtype",contentsubtype );
	
	sendMessage(fsm,"sip.mgw");
  statistics("MGWInvites");
	
	return("sendInviteMGW.end");
}

extern "C" char* sendAckMGW(TSCSM * fsm)
{
	newMessage(fsm,"SIP","MS.ACK");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	setStrMsgPara(fsm,"sip.cseq.method","ACK");
	//printf("in sendAckMGW,sip.cseq.method=%s\n",getSendStrMsgPara(fsm,"sip.cseq.method"));
	setStrMsgPara(fsm,"sip.cseq.num",getReceiveStrMsgPara(fsm,"sip.cseq.num"));
	printf("in sendAckMGW,sip.cseq.num=%s\n",getReceiveStrMsgPara(fsm,"sip.cseq.num"));
	setStrMsgPara(fsm,"sip.content_length","0");
	sendMessage(fsm,"sip.mgw");
	statistics("MGWAcks");
	return("sendAckMGW.end");
}

extern "C" char* sendAckMS(TSCSM * fsm)
{
       
	
	newMessage(fsm,"SIP","MS.ACK");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	setStrMsgPara(fsm,"sip.cseq.method","ACK");
	setStrMsgPara(fsm,"sip.cseq.num",getReceiveStrMsgPara(fsm,"sip.cseq.num"));
       char* content=getCCBStr(fsm,"mgw_sdp"); 
	printf("content is %s\n",content);
	setStrMsgPara(fsm,"sip.body",content);
	char *length=getCCBStr(fsm,"mgw_sdp_len");
	printf("body.length=%s\n",length);
	setStrMsgPara(fsm,"sip.content_length",length);

	
	setStrMsgPara(fsm,"sip.content_type.mediarange.type", "application");
	setStrMsgPara(fsm,"sip.content_type.mediarange.subtype","sdp" );
	sendMessage(fsm,"sip.ms");
	statistics("MSAcks");
	return("sendAckMS.end");
}

extern "C" char* sendINFOMS(TSCSM * fsm)
{
	 //play procedure 
	int error=0; 
	newMessage(fsm,"SIP","MS.INFO");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	setStrMsgPara(fsm,"sip.cseq.method","INFO");
	setStrMsgPara(fsm,"sip.cseq.num","2");

        char msmlscript[1450];
	string msml="<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                          "<msml version=\"1.1\">"
                              "<dialogstart target=\"conn:12345\" name=\"audioPlay\" mark=\"7\">"
                           	"<record maxtime=\"10s\" dest=\"file://%s\" format=\"audio/wav;codecs=g711u\">"
                                      "<play>"
                                        "<audio uri=\"file://announce.wav\"/>"
                                      "</play>"
                                   "<recordexit>"
                           	    "<send target=\"source\" event=\"done\" valuelist=\"record.len record.end\"/>"
                           	 "</recordexit>"
                           	"</record>"
                             "</dialogstart>"
                          "</msml>";
                           
	randomFileName(fsm);
	char * fileName = getCCBStr (fsm, "randomfilename");
	sprintf(msmlscript,msml.c_str(),fileName);
        int len=strlen(msmlscript);
	printf("msmlscript is %s\n",msmlscript);
	setStrMsgPara(fsm,"sip.body",msmlscript);   
        char temp[32];
        sprintf(temp,"%d",len);
        printf("temp size =  %s\n",temp);
        setStrMsgPara(fsm,"sip.content_length",temp);
        setStrMsgPara(fsm,"sip.content_type.mediarange.type", "application");
        setStrMsgPara(fsm,"sip.content_type.mediarange.subtype","msml+xml" );
        sendMessage(fsm,"sip.ms");
        settime(fsm , "sendmsinfo");
         //统计 MSInfoTimes
        statistics("MSInfos");
  
	return("sendINFOMS.end");
}

extern "C" char* sendInfoMGW(TSCSM * fsm)
{
       int error=0; 
	newMessage(fsm,"SIP","MS.INFO");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	//int sequece=getCCBInt(fsm,"sip.ms0.sequece");
	//int num=sequece+1;
	//sprintf(temp,"%ld",num);
	setStrMsgPara(fsm,"sip.cseq.method","INFO");
	setStrMsgPara(fsm,"sip.cseq.num","2");
	setStrMsgPara(fsm,"msml.play.barge","false");
	setIntMsgPara(fsm,"msml.play.reptition",1);  //not foud reptition
	setIntMsgPara(fsm,"msml.play.interval",0);	
	setStrMsgPara(fsm,"msml.play.maxtime","17s");
       //setIntMsgPara(fsm,"msml.dtmf.firstdigittimeout",10);
	//randomFileName(fsm);
        //char * fileName = getCCBStr (fsm, "randomfilename");
	setIntMsgPara(fsm,"msml.play.file.count",1);
	setStrMsgPara(fsm,"msml.play.file.0","mgwplay.wav");
	
	string msmlscript=msmlpacket(fsm,1,error);
	printf("msmlscript is %s\n",msmlscript.c_str());
	if(error!=0)
		printf("error is %d\n",error);
	else
	{
       printf("msmlscript is %s\n",msmlscript.c_str());
		   setStrMsgPara(fsm,"sip.body",(char*)(msmlscript.c_str()));	
		   char temp[32];
		   sprintf(temp,"%d",msmlscript.size());
		   printf("temp size =  %s\n",temp);
	     setStrMsgPara(fsm,"sip.content_length",temp);
		   setStrMsgPara(fsm,"sip.content_type.mediarange.type", "application");
		   setStrMsgPara(fsm,"sip.content_type.mediarange.subtype","msml+xml" );
		   sendMessage(fsm,"sip.mgw");
		   settime(fsm , "sendmgwinfo");
		   //统计 MGWInfoTimes
	     statistics("MGWInfos");		
	}
	
	return("sendInfoMGW.end");
}

extern "C" char* todomgwinfook(TSCSM * fsm)
{
        //统计 InviteTimes
        return("todomgwinfook.end");
}

extern "C" char* send200MS(TSCSM * fsm)
{
       char* sip_ch2_choice_request_sipversion = getReceiveStrMsgPara(fsm,"sip.ch2.choice.request.sip_version");
	newMessage(fsm,"SIP","MS.RESPONSE");
	setIntMsgPara(fsm,"sip.ch2.present",2);//必选 1: req    2:rsp
	setStrMsgPara(fsm,"sip.cseq.method","INFO");
	setStrMsgPara(fsm,"sip.cseq.num",getReceiveStrMsgPara(fsm,"sip.cseq.num"));
  //setStrMsgPara(fsm,"sip.ch2.choice.response.sip_version",sip_ch2_choice_request_sipversion);
  //setStrMsgPara(fsm,"sip.ch2.choice.response.status_code","200");
  //setStrMsgPara(fsm,"sip.ch2.choice.response.reason_phrase","OK");
  setStrMsgPara(fsm,"sip.content_length","0");
	setStrMsgPara(fsm,"sip.content_type.mediarange.type", "application");
	setStrMsgPara(fsm,"sip.content_type.mediarange.subtype","sdp" );
	
	sendMessage(fsm,"sip.ms");
	
	return("send200MS.end");
}

extern "C" char* send200MGW(TSCSM * fsm)
{
	char* sip_ch2_choice_request_sipversion = getReceiveStrMsgPara(fsm,"sip.ch2.choice.request.sip_version");
	newMessage(fsm,"SIP","MS.RESPONSE");
	setIntMsgPara(fsm,"sip.ch2.present",2);//必选 1: req    2:rsp
	setStrMsgPara(fsm,"sip.cseq.method","INFO");
	setStrMsgPara(fsm,"sip.cseq.num",getReceiveStrMsgPara(fsm,"sip.cseq.num"));
  //setStrMsgPara(fsm,"sip.ch2.choice.response.sip_version",sip_ch2_choice_request_sipversion);
  //setStrMsgPara(fsm,"sip.ch2.choice.response.status_code","200");
  //setStrMsgPara(fsm,"sip.ch2.choice.response.reason_phrase","OK");
	setStrMsgPara(fsm,"sip.content_length","0");
	setStrMsgPara(fsm,"sip.content_type.mediarange.type", "application");
	setStrMsgPara(fsm,"sip.content_type.mediarange.subtype","sdp" );
		   
	sendMessage(fsm,"sip.mgw");
	
	return("send200MGW.end");
}

extern "C" char* sendBYEMS(TSCSM * fsm)
{
	newMessage(fsm,"SIP","MS.BYE");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	//int sequece=getCCBInt(fsm,"sip.ms0.sequece");
	//int num=sequece+1;
	//sprintf(temp,"%ld",num);
	setStrMsgPara(fsm,"sip.cseq.method","BYE");
	setStrMsgPara(fsm,"sip.cseq.num","3");
	sendMessage(fsm,"sip.ms");

	settime(fsm , "sendbyems");
	
	return("sendBYEMS.end");
}

extern "C" char* sendBYEcaller(TSCSM * fsm)
{
	newMessage(fsm,"SIP","BYE");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	//int sequece=getCCBInt(fsm,"sip.ms0.sequece");
	//int num=sequece+1;
	//sprintf(temp,"%ld",num);
	setStrMsgPara(fsm,"sip.cseq.method","BYE");
	setStrMsgPara(fsm,"sip.cseq.num","1");
	
	sendMessage(fsm,"sip.caller");
	
	return("sendBYEcaller.end");
}

extern "C" char* sendBYEmgw(TSCSM * fsm)
{
	newMessage(fsm,"SIP","MS.BYE");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	//int sequece=getCCBInt(fsm,"sip.ms0.sequece");
	//int num=sequece+1;
	//sprintf(temp,"%ld",num);
	setStrMsgPara(fsm,"sip.cseq.method","BYE");
	setStrMsgPara(fsm,"sip.cseq.num","3");
	
	sendMessage(fsm,"sip.mgw");
	
	return("service.end");
}


extern "C" char* todomsinviteok(TSCSM * fsm)
{
	//统计 MSAnswer200Times
	long long msok = getCurrentTime_MS() ;
	long long sendmsinvite = gettime(fsm , "sendmsinvite");
	int diff = msok - sendmsinvite;
	statistics("D1", diff);
	
	statistics("MSInvite200s");
  char* content = getReceiveStrMsgPara(fsm,"sip.body"); 
	printf("content is %s\n",content);
	char *length=getReceiveStrMsgPara(fsm,"sip.content_length");
	printf("length=%s\n",length);
  setCCBStr(fsm,"ms_sdp",content);
	setCCBStr(fsm,"ms_sdp_len",length);
	char *contenttype=getReceiveStrMsgPara(fsm,"sip.content_type.mediarange.type");
	printf("contenttype=%s\n",contenttype);
	setCCBStr(fsm,"ms_content_type",contenttype);
	char *contentsubtype=getReceiveStrMsgPara(fsm,"sip.content_type.mediarange.subtype");
	printf("contentsubtype=%s\n",contentsubtype);
  setCCBStr(fsm,"ms_content_subtype",contentsubtype);
	
	setCCBInt(fsm,"ms.released_flag",0);
  printf("mgw.released_flag is %d\n",getCCBInt(fsm,"ms.released_flag"));
	return("in the end of todoinvite200!!!");
}

extern "C" char* todomgwinviteok(TSCSM * fsm)
{
	//统计 MGWAnswer200Times
	statistics("MGWInvite200s");
	
	char *content=getReceiveStrMsgPara(fsm,"sip.body");
	printf("content is %s\n",content);
	char *length=getReceiveStrMsgPara(fsm,"sip.content_length");
  printf("length=%s\n",length);
	setCCBStr(fsm,"mgw_sdp",content) ;
  setCCBStr(fsm,"mgw_sdp_len",length);
	
	setCCBInt(fsm,"mgw.released_flag",0);
	printf("mgw.released_flag is %d\n",getCCBInt(fsm,"mgw.released_flag"));
	return("in the end of todomgwinvite200!!!");
}

extern "C" char* todomsinfook(TSCSM * fsm)
{
	long long infook = getCurrentTime_MS() ;
	long long sendmsinfo = gettime(fsm , "sendmsinfo");
	int diff = infook - sendmsinfo;
	statistics("D2", diff);
	
	return("todomsinfook.end");
}

extern "C" char* analysismsinfo(TSCSM * fsm)
{	
	long long revmsinfo = getCurrentTime_MS() ;
	long long sendmgwinfo = gettime(fsm , "sendmgwinfo");
	int diff = revmsinfo - sendmgwinfo;
			
	msmlresponse msmlres;	
	char* content = getReceiveStrMsgPara(fsm,"sip.body");
	printf("in analysismsinfo,sip.body=%s\n",content);
	msmlparse(content, &msmlres);

	printf("msml eventname=%s\n",(msmlres.pevent.eventname).c_str());
	printf("msml id=%s\n",(msmlres.pevent.relatedid).c_str());
	if (msmlres.pevent.eventname=="msml.dialog.exit")
  return("MSINFO_ERROR");
	
	map<string,string>::iterator it,it1;
	for(it=msmlres.pevent.namelist.begin();it!=msmlres.pevent.namelist.end();it++)
	{
	    printf("name=%s,value=%s\n",(it->first).c_str(),(it->second).c_str());
	}

	it= msmlres.pevent.namelist.find(string("record.end"));
	if(it == msmlres.pevent.namelist.end())
	{
		SCFERROR("[ERROR][Fsm error] can not find record.end in map\n");
	}else{
		printf("[INFO] find record.end");
		if(it->second == string("record.complete.maxlength"))
		{
			printf("[INFO] find record.end\n");
			statistics("MsRecordSuccesses");
                     
			statistics("D4", diff-16000);
		}else
		{
			SCFERROR("[INFO] not find record.end");
			//add one to MsPlayFault
			statistics("MsRecordFaults");
			return("MSINFO_RecordFault");
		}
	} 
	
	return("MSINFO_1");

}

extern "C" char* analysismgwinfo(TSCSM * fsm)
{
                 
	long long revmgwinfo = getCurrentTime_MS() ;
	long long sendmgwinfo = gettime(fsm , "sendmgwinfo");
	int diff = revmgwinfo - sendmgwinfo;    
	msmlresponse msmlres;	
	char* content = getReceiveStrMsgPara(fsm,"sip.body");
	printf("in analysismgwinfo,sip.body=%s\n",content);
	msmlparse(content, &msmlres);

	printf("msml eventname=%s\n",(msmlres.pevent.eventname).c_str());
	printf("msml id=%s\n",(msmlres.pevent.relatedid).c_str());
	if (msmlres.pevent.eventname=="msml.dialog.exit")
		return("MGWINFO_ERROR");
	
	map<string,string>::iterator it,it1;
	for(it=msmlres.pevent.namelist.begin();it!=msmlres.pevent.namelist.end();it++)
	{
	    printf("name=%s,value=%s\n",(it->first).c_str(),(it->second).c_str());
	}

	it= msmlres.pevent.namelist.find(string("play.end"));
	if(it == msmlres.pevent.namelist.end())
	{
		//没找到play.end
		SCFERROR("[ERROR][Fsm error] can not find play.end in map");
	}else{
		printf("[INFO] find play.end");
		if(it->second == string("play.complete"))
		{
			printf("[INFO] find play compelte");
			//add one to MsPlaySuccess
			statistics("MgwPlaySuccesses");
      
			statistics("RevMGWInfoTime", diff/1000);
			
				
		}else
		{
			SCFERROR("[INFO] not find play compelte");
			//add one to MsPlayFault
			statistics("MgwPlayFault");
		}
	} 
	
	return("MGWINFO_SUCCESS");
}

extern "C" char* todomsbye200(TSCSM * fsm)
{
  long long msbye200 = getCurrentTime_MS() ;
	long long sendbyems = gettime(fsm , "sendbyems");
	int diff = msbye200 - sendbyems;
	statistics("MSBye200Time", diff);

	
  setCCBInt(fsm,"ms.released_flag",1);
	return("todomsbye200.end");
}

extern "C" char* todomgwbye200(TSCSM * fsm)
{
  
	return("todomgwbye200.end");
}



extern "C" char* todomsbye(TSCSM * fsm)
{
  newMessage(fsm,"SIP","MS.BYE");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	setStrMsgPara(fsm,"sip.ch2.choice.request.method","BYE");
	setStrMsgPara(fsm,"sip.cseq.method","BYE");
	setStrMsgPara(fsm,"sip.cseq.num","3");
	sendMessage(fsm,"sip.ms");
	
	long long msbye = getCurrentTime_MS();
	long long sendmsinvite = gettime(fsm , "sendmsinvite");
	int diff = msbye - sendmsinvite;
	statistics("CallLength",diff/1000);
	statistics("MSByes");
	
	setCCBInt(fsm,"ms.released_flag",1);
	todorelease(fsm);
	return("sendByeMS.end");
}

extern "C" char* todocallerbye(TSCSM * fsm)
{
  newMessage(fsm,"SIP","TRANSMIT");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	setStrMsgPara(fsm,"sip.ch2.choice.request.method","BYE");//必选 1: req    2:rsp
  setStrMsgPara(fsm,"sip.cseq.method","BYE");
	setStrMsgPara(fsm,"sip.cseq.num","2");
  sendMessage(fsm,"sip.caller");
	
	return("sendByecaller.end");
}

extern "C" char* todomgwbye(TSCSM * fsm)
{
        newMessage(fsm,"SIP","MS.BYE");
	setIntMsgPara(fsm,"sip.ch2.present",1);//必选 1: req    2:rsp
	setStrMsgPara(fsm,"sip.cseq.method","BYE");
	setStrMsgPara(fsm,"sip.cseq.num","3");
	sendMessage(fsm,"sip.mgw");
	setCCBInt(fsm,"mgw.released_flag",1);
	todorelease(fsm);
	return("sendByeMGW.end");
}


extern "C" char* todorelease(TSCSM * fsm)
{
	int flag1=getCCBInt(fsm,"ms.released_flag");
	int flag2=getCCBInt(fsm,"mgw.released_flag");
	printf("in todorelease ms.release_flag is %d,mgw.released_flag is %d\n",flag1,flag2);
	if (flag1==0 &&flag2==0)
	{
		printf("no bye sent,continue\n");
		return("continue");
	}
	if (flag1==1 &&flag2==0)
	{
		printf("sent ms bye,no mgw bye\n");
		return("continue");
	}
	if (flag1==0 &&flag2==1)
	{
		printf("sent mgw bye,no ms bye\n");
		return("continue");
	}
	if (flag1==1 &&flag2==1)
	{
		  printf("sent ms&mgw bye,finish fsm\n");
			return("service.end");
	}
	return("continue");
}

extern "C" void  randomFileName(TSCSM * fsm)
{
	char fileName[128];
	pid_t pid = getpid();
	sprintf(fileName, "%ld_%020lld.wav", (long)pid, COUNT);
	setCCBStr(fsm, "randomfilename", fileName);
	COUNT++;
	return;
}
extern "C" void   settime(TSCSM * fsm , char* name)
{
  long long t = getCurrentTime_MS();	 
	char string_t[128];
	memset(string_t,0,128);
	sprintf(string_t,"%lld",t);
	setCCBStr (fsm,name, string_t);
}
extern "C" long long  gettime(TSCSM * fsm , char* name)
{
	char* time = getCCBStr (fsm, name);
	long long time_long = atol(time);
	return time_long;
}


