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
	
	/** @brief Message维护类
	 *
	 * 提供设置Tid、设置JobId、获取JobId、设置统计结束时间、设置分隔符、增加Name字段、增加Value值功能
	 * 提供消息内容校验、消息编码、获取消息大小的功能
	 */
	class PfmcMessage
	{
	public:
		/**
		 * 构造函数
		 */
		PfmcMessage();
		/**
		 * 构造函数
		 * @param jobid 任务ID
		 */
		PfmcMessage(const std::string& jobid);
		/**
		 * 构造函数
		 * @param jobid 任务ID
		 * @param endtime 任务统计结束时间
		 */
		PfmcMessage(const std::string& jobid, const std::string& endtime);
		/**
		 * 析构函数
		 */
		~PfmcMessage();
		/**
		 * 设置Tid值
		 * @param tid 线程id值
		 * @return 成功返回true,异常返回false
		 */
		bool setTid(const unsigned long tid );
		/**
		 * 获取是否需要设置Tid值
		 * @return 返回m_needTid值
		 */
		const bool getNeedTid() const { return m_needTid; }
		/**
		 * 获取Tid值
		 * @return 返回m_tid值
		 */
		const unsigned long getTid() const { return m_tid; }
		/**
		 * 获取JobId值
		 * @return 返回m_jobid值
		 */
		const std::string& getJobId() const { return m_jobId; }
		/**
		 * 设置JobId；
		 * @param jobId 统计任务编号
		 * @return 成功返回true,异常返回false
		 */
		bool setJobId(const std::string& jobId) { m_jobId = jobId; return true; }
		/**
		 * 获取JobId；
		 * @return m_jobId
		 */
		const std::string& getJobId() { return m_jobId; }
		/**
		 * 设置统计结束时间值
		 * @param endTime 统计结束时间
		 * @return 成功返回true,异常返回false
		 */
		bool setEndTime(const std::string& endTime) { m_endTime = endTime; return true; }
		/**
		 * 设置分隔符
		 * @param delimiter 分隔符值
		 * @return 成功返回true,异常返回false
		 */
		bool setDelimiter(const std::string& delimiter) { m_delimiter = delimiter; return true; }
		/**
		 * 增加Name元素值
		 * @param name Name元素值中的一个字段名称
		 * @return 成功返回true,异常返回false
		 */
		bool addName(const std::string& name);
		/**
		 * 增加新的一行Value值，由于可能存在多行Value元素
		 * 需要先addNewLineForValue标记增加一行元素，然后用addValue增加这一行内每个字段对应的值
		 * @return 成功返回true,异常返回false
		 */
		bool addNewLineForValue();
		/**
		 * 结束一行Value值
		 * @return 成功返回true,异常返回false
		 */
		bool endLineForValue();
		/**
		 * 增加一行Value元素内一个字段对应的值，需要对应addName的顺序
		 * @param value 一行Value元素内一个字段对应的值
		 * @return 成功返回true,异常返回false
		 */
		bool addValue(const std::string& value);
		/**
		 * 消息是否已编码；
		 * @return 已编码返回true,未编码返回false
		 */
		bool isEncoded() const { return m_isEncoded; }
		/**
		 * Message消息编码函数,编码前校验消息内容和内容值
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
		 * 检查Message的大小
		 * @return 
		 *   0 消息小于等于60K
		 *   1 消息大于60K
		 */
		const int checkMessageSize();
		/**
		 * 设置消息头的Type值
		 * @param type 消息头的Type值
		 * @return 成功返回true,异常返回false
		 */
		bool setHeadType(const std::string& type) { m_headType = type; return true; }
		/**
		 * 设置消息头的Reserve值
		 * @param headReserve 消息头的Reserve值
		 * @return 成功返回true,异常返回false
		 */
		bool setHeadReserve(const std::string& headReserve) { m_headReserve = headReserve; return true; }
		/**
		 * 设置消息头的Version值
		 * @param version 消息头的Version值
		 * @return 成功返回true,异常返回false
		 */
		bool setHeadVersion(const std::string& version) { m_headVersion = version; return true; }
		/**
		 * 设置消息体的Host值
		 * @param hostName 消息体的Host值
		 * @return 成功返回true,异常返回false
		 */
		bool setHostName(const std::string& hostName) { m_hostName = hostName; return true; }
		/**
		 * 设置消息体的Account值
		 * @param accountName 消息体的Account值
		 * @return 成功返回true,异常返回false
		 */
		bool setAccountName(const std::string& accountName) { m_accountName = accountName; return true; }
		/**
		 * 设置消息体的Pid值
		 * @param pid 消息体的Pid值
		 * @return 成功返回true,异常返回false
		 */
		bool setPid(const pid_t pid) { m_pid = pid; return true; }
		/**
		 * 设置消息体的Reserve值
		 * @param bodyReserve 消息体的Reserve值
		 * @return 成功返回true,异常返回false
		 */
		bool setBodyReserve(const std::string& bodyReserve) { m_bodyReserve = bodyReserve; return true; }
		/**
		 * 获取编码后的消息内容
		 * @return string 编码后的消息内容
		 */
		const std::string& getMessageContent() const { return m_content; }
		/**
		 * 时间格式转换
		 * @param endTimeForTM struct tm 结束时间
		 * @return 成功返回true,异常返回false
		 */
		 bool getEndTimeForTM(struct tm& endTimeForTM);
		 ///预留函数
		bool reserveFunc1(std::string& reserve1);
		bool reserveFunc2(std::string& reserve1, std::string& reserve2);
		bool reserveFunc3(std::string& reserve1, std::string& reserve2, std::string& reserve3);
		int reserveFunc4(std::string& reserve1);
		int reserveFunc5(std::string& reserve1, std::string& reserve2);
		int reserveFunc6(std::string& reserve1, std::string& reserve2, std::string& reserve3);
	
	private:
		/**
		 * 检查Name元素中字段个数和Value元素中值个数是否一致
		 * @return 成功返回true,异常返回false
		 */
		bool checkNameAndValue();
		/**
		 * 检查EndTime元素值是否符合yyyy-MM-dd HH:mm:ss的格式
		 * @return 成功返回true,异常返回false
		 */
		bool checkEndTimeValue();
		/**
		 * Message消息校验
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
		/// 编码消息Header
		bool encodeHead(std::ostringstream& stream);
		/// 编码消息Body
		bool encodeBody(std::ostringstream& stream);
		
	private:
		///消息头Type元素
		std::string m_headType;
		///消息头Reserve元素
		std::string m_headReserve;
		///消息头Version元素
		std::string m_headVersion;
		///消息体Host元素
		std::string m_hostName;
		///消息体Account元素
		std::string m_accountName;
		///消息体Pid元素
		pid_t m_pid;
		///消息体tid元素
		unsigned long m_tid;
		///是否需要设置Tid
		bool m_needTid;
		///消息体重Reserve元素
		std::string m_bodyReserve;
		///消息体JobId元素
		std::string m_jobId;
		///消息体EndTime元素
		std::string m_endTime;
		///消息体Data中Delimiter属性，标记分隔符
		std::string m_delimiter;
		///消息体Name元素
		std::vector< std::string > m_nameList;
		///临时保存一行Value元素值
		std::vector<std::string> tmpValueList;
		///标记tmpValueList是否可用，在addNewLineForValue()之后，endLineForValue之前，tmpValueList可用
		bool m_isTmpValueListAviable;
		///消息体所有Value元素
		std::vector< std::vector<std::string> > m_valueLineList;
		/// 编码后的消息文本
		std::string m_content;
		/// 标记消息是否已编码
		bool m_isEncoded;
		///预留字段
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

