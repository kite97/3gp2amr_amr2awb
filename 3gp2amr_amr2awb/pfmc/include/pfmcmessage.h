/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: pfmcmessage.h,v $
 *  Last Revision        : $Revision: 1.8 $
 *  Last Revision Date   : $Date: 2012/09/21 05:52:08 $
 *  Author               :
 *  Description          :
 **********************************************************/
#ifndef _PFMCMESSAGE_H
#define _PFMCMESSAGE_H

#include <string>
#include <vector>
#include <sstream>
#include <sys/types.h>

#ifdef __cplusplus
extern "C" {
#endif

namespace PfmcApi
{
	
	/** @brief Messageά����
	 *
	 * �ṩ����Tid������JobId����ȡJobId������ͳ�ƽ���ʱ�䡢���÷ָ���������Name�ֶΡ�����Valueֵ����
	 * �ṩ��Ϣ����У�顢��Ϣ���롢��ȡ��Ϣ��С�Ĺ���
	 */
	class PfmcMessage
	{
	public:
		/**
		 * ���캯��
		 */
		PfmcMessage();
		/**
		 * ���캯��
		 * @param jobid ����ID
		 */
		PfmcMessage(const std::string& jobid);
		/**
		 * ���캯��
		 * @param jobid ����ID
		 * @param endtime ����ͳ�ƽ���ʱ��
		 */
		PfmcMessage(const std::string& jobid, const std::string& endtime);
		/**
		 * ��������
		 */
		~PfmcMessage();
		/**
		 * ����Tidֵ
		 * @param tid �߳�idֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setTid(const unsigned long tid );
		/**
		 * ��ȡ�Ƿ���Ҫ����Tidֵ
		 * @return ����m_needTidֵ
		 */
		const bool getNeedTid() const { return m_needTid; }
		/**
		 * ��ȡTidֵ
		 * @return ����m_tidֵ
		 */
		const unsigned long getTid() const { return m_tid; }
		/**
		 * ��ȡJobIdֵ
		 * @return ����m_jobidֵ
		 */
		const std::string& getJobId() const { return m_jobId; }
		/**
		 * ����JobId��
		 * @param jobId ͳ��������
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setJobId(const std::string& jobId) { m_jobId = jobId; return true; }
		/**
		 * ��ȡJobId��
		 * @return m_jobId
		 */
		const std::string& getJobId() { return m_jobId; }
		/**
		 * ����ͳ�ƽ���ʱ��ֵ
		 * @param endTime ͳ�ƽ���ʱ��
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setEndTime(const std::string& endTime) { m_endTime = endTime; return true; }
		/**
		 * ���÷ָ���
		 * @param delimiter �ָ���ֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setDelimiter(const std::string& delimiter) { m_delimiter = delimiter; return true; }
		/**
		 * ����NameԪ��ֵ
		 * @param name NameԪ��ֵ�е�һ���ֶ�����
		 * @return �ɹ�����true,�쳣����false
		 */
		bool addName(const std::string& name);
		/**
		 * �����µ�һ��Valueֵ�����ڿ��ܴ��ڶ���ValueԪ��
		 * ��Ҫ��addNewLineForValue�������һ��Ԫ�أ�Ȼ����addValue������һ����ÿ���ֶζ�Ӧ��ֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool addNewLineForValue();
		/**
		 * ����һ��Valueֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool endLineForValue();
		/**
		 * ����һ��ValueԪ����һ���ֶζ�Ӧ��ֵ����Ҫ��ӦaddName��˳��
		 * @param value һ��ValueԪ����һ���ֶζ�Ӧ��ֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool addValue(const std::string& value);
		/**
		 * ��Ϣ�Ƿ��ѱ��룻
		 * @return �ѱ��뷵��true,δ���뷵��false
		 */
		bool isEncoded() const { return m_isEncoded; }
		/**
		 * Message��Ϣ���뺯��,����ǰУ����Ϣ���ݺ�����ֵ
		 * @return 
		 *  0  SUCCESS
		 *  -1 HEADTYPE_MISSING
		 *  -2 HEADVERSION_MISSING
		 *  -3 HOST_MISSING
		 *  -4 ACCOUNT_MISSING
		 *  -5 PID_MISSING
		 *  -6 TID_MISSING
		 *  -7 JOBID_MISSING
		 *  -8 ENDTIME_MISSING
		 *  -9 ERROR_ENDTIME
		 *  -10 VALUE_NOTMATCH_NAME
		 */
		int encodeMessage();
		/**
		 * ���Message�Ĵ�С
		 * @return 
		 *   0 ��ϢС�ڵ���60K
		 *   1 ��Ϣ����60K
		 */
		const int checkMessageSize();
		/**
		 * ������Ϣͷ��Typeֵ
		 * @param type ��Ϣͷ��Typeֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setHeadType(const std::string& type) { m_headType = type; return true; }
		/**
		 * ������Ϣͷ��Reserveֵ
		 * @param headReserve ��Ϣͷ��Reserveֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setHeadReserve(const std::string& headReserve) { m_headReserve = headReserve; return true; }
		/**
		 * ������Ϣͷ��Versionֵ
		 * @param version ��Ϣͷ��Versionֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setHeadVersion(const std::string& version) { m_headVersion = version; return true; }
		/**
		 * ������Ϣ���Hostֵ
		 * @param hostName ��Ϣ���Hostֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setHostName(const std::string& hostName) { m_hostName = hostName; return true; }
		/**
		 * ������Ϣ���Accountֵ
		 * @param accountName ��Ϣ���Accountֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setAccountName(const std::string& accountName) { m_accountName = accountName; return true; }
		/**
		 * ������Ϣ���Pidֵ
		 * @param pid ��Ϣ���Pidֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setPid(const pid_t pid) { m_pid = pid; return true; }
		/**
		 * ������Ϣ���Reserveֵ
		 * @param bodyReserve ��Ϣ���Reserveֵ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool setBodyReserve(const std::string& bodyReserve) { m_bodyReserve = bodyReserve; return true; }
		/**
		 * ��ȡ��������Ϣ����
		 * @return string ��������Ϣ����
		 */
		const std::string& getMessageContent() const { return m_content; }
		/**
		 * ʱ���ʽת��
		 * @param endTimeForTM struct tm ����ʱ��
		 * @return �ɹ�����true,�쳣����false
		 */
		 bool getEndTimeForTM(struct tm& endTimeForTM);
		 ///Ԥ������
		bool reserveFunc1(std::string& reserve1);
		bool reserveFunc2(std::string& reserve1, std::string& reserve2);
		bool reserveFunc3(std::string& reserve1, std::string& reserve2, std::string& reserve3);
		int reserveFunc4(std::string& reserve1);
		int reserveFunc5(std::string& reserve1, std::string& reserve2);
		int reserveFunc6(std::string& reserve1, std::string& reserve2, std::string& reserve3);
	
	private:
		/**
		 * ���NameԪ�����ֶθ�����ValueԪ����ֵ�����Ƿ�һ��
		 * @return �ɹ�����true,�쳣����false
		 */
		bool checkNameAndValue();
		/**
		 * ���EndTimeԪ��ֵ�Ƿ����yyyy-MM-dd HH:mm:ss�ĸ�ʽ
		 * @return �ɹ�����true,�쳣����false
		 */
		bool checkEndTimeValue();
		/**
		 * Message��ϢУ��
		 * @return 
		 *  0  SUCCESS
		 *  -1 HEADTYPE_MISSING
		 *  -2 HEADVERSION_MISSING
		 *  -3 HOST_MISSING
		 *  -4 ACCOUNT_MISSING
		 *  -5 PID_MISSING
		 *  -6 TID_MISSING
		 *  -7 JOBID_MISSING
		 *  -8 ENDTIME_MISSING
		 *  -9 ERROR_ENDTIME
		 *  -10 VALUE_NOTMATCH_NAME
		 */
		int checkMessage();
		/// ������ϢHeader
		bool encodeHead(std::ostringstream& stream);
		/// ������ϢBody
		bool encodeBody(std::ostringstream& stream);
		
	private:
		///��ϢͷTypeԪ��
		std::string m_headType;
		///��ϢͷReserveԪ��
		std::string m_headReserve;
		///��ϢͷVersionԪ��
		std::string m_headVersion;
		///��Ϣ��HostԪ��
		std::string m_hostName;
		///��Ϣ��AccountԪ��
		std::string m_accountName;
		///��Ϣ��PidԪ��
		pid_t m_pid;
		///��Ϣ��tidԪ��
		unsigned long m_tid;
		///�Ƿ���Ҫ����Tid
		bool m_needTid;
		///��Ϣ����ReserveԪ��
		std::string m_bodyReserve;
		///��Ϣ��JobIdԪ��
		std::string m_jobId;
		///��Ϣ��EndTimeԪ��
		std::string m_endTime;
		///��Ϣ��Data��Delimiter���ԣ���Ƿָ���
		std::string m_delimiter;
		///��Ϣ��NameԪ��
		std::vector< std::string > m_nameList;
		///��ʱ����һ��ValueԪ��ֵ
		std::vector<std::string> tmpValueList;
		///���tmpValueList�Ƿ���ã���addNewLineForValue()֮��endLineForValue֮ǰ��tmpValueList����
		bool m_isTmpValueListAviable;
		///��Ϣ������ValueԪ��
		std::vector< std::vector<std::string> > m_valueLineList;
		/// ��������Ϣ�ı�
		std::string m_content;
		/// �����Ϣ�Ƿ��ѱ���
		bool m_isEncoded;
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

