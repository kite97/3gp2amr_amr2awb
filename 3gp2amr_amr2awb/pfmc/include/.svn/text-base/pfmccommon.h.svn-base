/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: pfmccommon.h,v $
 *  Last Revision        : $Revision: 1.8 $
 *  Last Revision Date   : $Date: 2012/09/21 10:10:11 $
 *  Author               :
 *  Description          :
 **********************************************************/
#ifndef _PFMCCOMMON_H
#define _PFMCCOMMON_H

#ifdef __cplusplus
extern "C" {
#endif

namespace PfmcApi
{
	/*初始化返回结果值描述
	INIT_SUCCESS = 0,
	GET_HOST_ERROR = -21,
	GET_ACCOUNT_ERROR = -22,
	GET_PID_ERROR = -23,
	TYPE_ERROR = -24,
	LOGNAME_ERROR = -25,
	INIT_UDPSENDER_ERROR = -26,
	INIT_DATAFILEMAN_ERROR = -27,
	INIT_BAKFILEMAN_ERROR = -28,
	GETENV_PFMCAPIDIR_ERROR = -29,
	
	发送Message返回结果值描述，包含校验错误类型定义
	SUCCESS = 0,
	HEADTYPE_MISSING = -1, 
	HEADVERSION_MISSING = -2,
	HOST_MISSING = -3,
	ACCOUNT_MISSING = -4,
	PID_MISSING = -5,
	TID_MISSING = -6,
	JOBID_MISSING = -7,
	ENDTIME_MISSING = -8,
	ERROR_ENDTIME = -9,
	VALUE_NOTMATCH_NAME = -10,
	ENCODE_MESSAGE_FAIL = -11,
	NEW_PFMCMESSAGE_FAIL = -12,
	SENDUDP_ERROR = -13,
	WRITEFILE_ERROR = -14,
	INIT_NOTSUCC = -15,
	OTHER_ERROR = -16
	*/

	///IP版本定义
	typedef enum { ERRORIPV=-1, IPV4=0, IPV6=1 } IPVersion;
		///保存目的IP和端口，IPv4或者IPv6版本信息    
	
	///保存IP地址、端口、IP版本
	struct UDPAddress
	{
		/// IP
		std::string ipAddress;
		/// PORT
		unsigned int udpPort;
		///IP version
		IPVersion ipVersion;
		UDPAddress(const std::string& _ipAddress, const unsigned int _udpPort, const IPVersion _ipVersion) :
			ipAddress(_ipAddress), udpPort(_udpPort), ipVersion(_ipVersion){}
	};
	
	///文件名（包含路径）的最大长度
	#define PFMC_MAX_LOGNMAE_LEN 512
	///每行日志的最大长度
	#define PFMC_LOG_LINE_MAX 10000
	///日志文件最大10M
	#define PFMC_LOG_FILE_SIZE 10*1024*1024
	///hostname最大长度
	#define PFMC_MAX_HOSTNAME_LEN 128
	///UDP消息最大60K
	#define PFMC_MAX_UDPMESSAGE_SIZE 60*1024
	///buffer长度
	#define PFMC_MAX_BUF_LEN 1024
}

#ifdef __cplusplus
   }
#endif

#endif
