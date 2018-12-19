/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: interface.h,v $
 *  Last Revision        : $Revision: 1.10 $
 *  Last Revision Date   : $Date: 2012/09/25 03:03:55 $
 *  Author               :
 *  Description          :
 **********************************************************/
#ifndef _INTERFACE_H
#define _INTERFACE_H

#include <string>
#include <vector>
#include <sstream>
#include <pthread.h>

#ifdef __cplusplus
extern "C" {
#endif

namespace PfmcApi
{
	class PfmcMessage;
	class Interface
	{
	public:
		/**
		 * Ĭ�Ϲ��캯����Ĭ��apiTypeΪ0����Ϣ�ļ����÷�ʽ��Ĭ�ϲ���¼��־
		 */
		Interface();
		/**
		 * ���캯��������apiTypeΪ0����Ϣ�ļ����÷�ʽ 1��ֻ�ļ���ʽ��Ĭ�ϲ���¼��־
		 * @param type apiType����
		 *             0����Ϣ�ļ����÷�ʽ 1��ֻ�ļ���ʽ
		 */
		Interface(const int type);
		/**
		 * ���캯����Ĭ��apiTypeΪ0����Ϣ�ļ����÷�ʽ����¼��־
		 * @param logName ��־����������־������pfmc_api.��������.log
		 */
		Interface(const std::string& logName);
		/**
		 * ���캯��������apiTypeΪ0����Ϣ�ļ����÷�ʽ 1��ֻ�ļ���ʽ����¼��־
		 * @param type apiType����
		 *             0����Ϣ�ļ����÷�ʽ 1��ֻ�ļ���ʽ
		 * @param logName ��־����������־������pfmc_api.��������.log
		 */
		Interface(const int type , const std::string& logName);
		/**
		 * ��������
		 */
		~Interface();
		/**
		 * ��ʼ������
		 * @return 
		 *  0  SUCCESS
		 *  -21 GET_HOST_ERROR
		 *  -22 GET_ACCOUNT_ERROR
		 *  -23 GET_PID_ERROR
		 *  -24 TYPE_ERROR
		 *  -25 LOGNAME_ERROR
		 *  -26 INIT_UDPSENDER_ERROR
		 *  -27 INIT_DATAFILEMAN_ERROR
		 *  -28 INIT_BAKFILEMAN_ERROR
		 */
		int init();
		/**
		 * ����Ŀ��IP�Ͷ˿�
		 * @param ip Ŀ��IP��ַ
		 * @param port Ŀ�Ķ˿�
		 * @return �ɹ�����true,�쳣����false
		 */
		bool addDestIpAndPort(const std::string& ip, const unsigned int port);
		/**
		 * ɾ��ԭ�е�Ŀ��IP��ַ�б�
		 * @return �ɹ�����true,�쳣����false
		 */
		bool delAllDestIPAndPort();
		/**
		 * ���ñ�������Ϊ��
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setDayBak();
		/**
		 * ���ñ�������ΪСʱ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setHourBak();
		/**
		 * ��UDP��Ϣ��־����
		 * @return �ɹ�����true,�쳣����false
		 */
		bool opneUDPTrace();
		/**
		 * �ر�UDP��Ϣ��־����
		 * @return �ɹ�����true,�쳣����false
		 */
		bool closeUDPTrace();
		/**
		 * ����һ��Message��Ϣ���������ӿڹ淶��֯��Ϣ���ݣ�
		 * apiTypeΪ1��ֻ�ļ���ʽ�����ж���Ϣ��С
		 * apiTypeΪ0����Ϣ���ļ����÷�ʽ���ж���Ϣ��С������60K�����ļ���ʽ��С�ڵ���60K����UDP��Ϣ
		 * @param message һ��Message��Ϣ����
		 * @return 
		 *  0  SUCCESS
		 *  -1 MESSAGE_ERROR HEADTYPE_MISSING
		 *  -2 MESSAGE_ERROR HEADVERSION_MISSING
		 *  -3 MESSAGE_ERROR HOST_MISSING
		 *  -4 MESSAGE_ERROR ACCOUNT_MISSING
		 *  -5 MESSAGE_ERROR PID_MISSING
		 *  -6 MESSAGE_ERROR TID_MISSING
		 *  -7 MESSAGE_ERROR JOBID_MISSING
		 *  -8 MESSAGE_ERROR ENDTIME_MISSING
		 *  -9 MESSAGE_ERROR ERROR_ENDTIME
		 *  -10 MESSAGE_ERROR VALUE_NOTMATCH_NAME
		 *  -11 ENCODE_MESSAGE_FAIL
		 *  -12 NEW_PFMCMESSAGE_FAIL
		 *  -13 SENDUDP_ERROR
		 *  -14 WRITEFILE_ERROR
		 *  -15 OTHER_ERROR
		 */
		int sendMessage(PfmcMessage& message);
		/**
		 * �����ļ�����ά���ӿں�����Ҫ����õ�Ƶ��ҪС�ڱ�������
		 */
		void active();
		/**
		 * ��ȡ��ǰ��־����
		 * @returns ���ص�ǰ��־����
		 * DEBUG_LOG = 1,
		 * INFO_LOG = 2,
		 * WARN_LOG = 3,
		 * ERROR_LOG = 4,
		 **/
		int getLogLevel();
		/**
		 * ���õ�ǰ��־����
		 * @return �ɹ�����true,�쳣����false
		 **/
		bool setLogLevel(const int level);
		/**
		 * ��ȡinit��sendMessage����ֵ��Ӧ��������Ϣ
		 * @param result init��sendMessage����ֵ
		 * @param desc ����ֵ��Ӧ��������Ϣ
		 **/
		char* getResultDesc(const int result);
		///Ԥ������
		bool reserveFunc1(std::string& reserve1);
		bool reserveFunc2(std::string& reserve1, std::string& reserve2);
		bool reserveFunc3(std::string& reserve1, std::string& reserve2, std::string& reserve3);
		int reserveFunc4(std::string& reserve1);
		int reserveFunc5(std::string& reserve1, std::string& reserve2);
		int reserveFunc6(std::string& reserve1, std::string& reserve2, std::string& reserve3);
	
	private:
		int sendMessage(PfmcMessage* message);
		 
	private:
		///apiType 0����Ϣ�ļ����÷�ʽ 1��ֻ�ļ���ʽ
		const int m_apiType;
		///�Ƿ���Ҫ��¼��־��true����¼��־��false������Ҫ��¼��־
		const bool m_needLog;
		///��־���ƣ�������־������pfmc_api.��������.log,Ĭ��pfmc_api.log
		const std::string m_logName;
		///��¼��ʼ���Ƿ�ɹ�,trueΪ�ɹ���falseΪʧ��
		bool m_isInitSucc;
		///Ԥ���ֶ�
		std::string m_reserve1;
		std::string m_reserve2;
		std::string m_reserve3;
		unsigned long m_reserve4;
		unsigned long m_reserve5;
		bool m_reserve6;
		bool m_reserve7;
	};
}

#ifdef __cplusplus
   }
#endif

#endif

