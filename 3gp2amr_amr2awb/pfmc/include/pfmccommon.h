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
	/*��ʼ�����ؽ��ֵ����
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
	
	����Message���ؽ��ֵ����������У��������Ͷ���
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

	///IP�汾����
	typedef enum { ERRORIPV=-1, IPV4=0, IPV6=1 } IPVersion;
		///����Ŀ��IP�Ͷ˿ڣ�IPv4����IPv6�汾��Ϣ    
	
	///����IP��ַ���˿ڡ�IP�汾
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
	
	///�ļ���������·��������󳤶�
	#define PFMC_MAX_LOGNMAE_LEN 512
	///ÿ����־����󳤶�
	#define PFMC_LOG_LINE_MAX 10000
	///��־�ļ����10M
	#define PFMC_LOG_FILE_SIZE 10*1024*1024
	///hostname��󳤶�
	#define PFMC_MAX_HOSTNAME_LEN 128
	///UDP��Ϣ���60K
	#define PFMC_MAX_UDPMESSAGE_SIZE 60*1024
	///buffer����
	#define PFMC_MAX_BUF_LEN 1024
}

#ifdef __cplusplus
   }
#endif

#endif
