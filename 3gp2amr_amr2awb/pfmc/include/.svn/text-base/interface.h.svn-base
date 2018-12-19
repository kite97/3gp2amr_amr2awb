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
		 * 默认构造函数，默认apiType为0：消息文件并用方式，默认不记录日志
		 */
		Interface();
		/**
		 * 构造函数，设置apiType为0：消息文件并用方式 1：只文件方式，默认不记录日志
		 * @param type apiType类型
		 *             0：消息文件并用方式 1：只文件方式
		 */
		Interface(const int type);
		/**
		 * 构造函数，默认apiType为0：消息文件并用方式，记录日志
		 * @param logName 日志名，建议日志命名：pfmc_api.进程名称.log
		 */
		Interface(const std::string& logName);
		/**
		 * 构造函数，设置apiType为0：消息文件并用方式 1：只文件方式，记录日志
		 * @param type apiType类型
		 *             0：消息文件并用方式 1：只文件方式
		 * @param logName 日志名，建议日志命名：pfmc_api.进程名称.log
		 */
		Interface(const int type , const std::string& logName);
		/**
		 * 析构函数
		 */
		~Interface();
		/**
		 * 初始化函数
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
		 * 加载目的IP和端口
		 * @param ip 目的IP地址
		 * @param port 目的端口
		 * @return 成功返回true,异常返回false
		 */
		bool addDestIpAndPort(const std::string& ip, const unsigned int port);
		/**
		 * 删除原有的目的IP地址列表
		 * @return 成功返回true,异常返回false
		 */
		bool delAllDestIPAndPort();
		/**
		 * 设置备份周期为天
		 * @return 成功返回true,异常返回false
		 */
		bool setDayBak();
		/**
		 * 设置备份周期为小时
		 * @return 成功返回true,异常返回false
		 */
		bool setHourBak();
		/**
		 * 打开UDP消息日志跟踪
		 * @return 成功返回true,异常返回false
		 */
		bool opneUDPTrace();
		/**
		 * 关闭UDP消息日志跟踪
		 * @return 成功返回true,异常返回false
		 */
		bool closeUDPTrace();
		/**
		 * 发送一个Message消息，包含按接口规范组织消息内容，
		 * apiType为1即只文件方式，不判断消息大小
		 * apiType为0即消息和文件并用方式，判断消息大小，大于60K采用文件方式，小于等于60K采用UDP消息
		 * @param message 一个Message消息内容
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
		 * 备份文件周期维护接口函数，要求调用的频度要小于备份周期
		 */
		void active();
		/**
		 * 获取当前日志级别
		 * @returns 返回当前日志级别
		 * DEBUG_LOG = 1,
		 * INFO_LOG = 2,
		 * WARN_LOG = 3,
		 * ERROR_LOG = 4,
		 **/
		int getLogLevel();
		/**
		 * 设置当前日志级别
		 * @return 成功返回true,异常返回false
		 **/
		bool setLogLevel(const int level);
		/**
		 * 获取init和sendMessage返回值对应的描述信息
		 * @param result init和sendMessage返回值
		 * @param desc 返回值对应的描述信息
		 **/
		char* getResultDesc(const int result);
		///预留函数
		bool reserveFunc1(std::string& reserve1);
		bool reserveFunc2(std::string& reserve1, std::string& reserve2);
		bool reserveFunc3(std::string& reserve1, std::string& reserve2, std::string& reserve3);
		int reserveFunc4(std::string& reserve1);
		int reserveFunc5(std::string& reserve1, std::string& reserve2);
		int reserveFunc6(std::string& reserve1, std::string& reserve2, std::string& reserve3);
	
	private:
		int sendMessage(PfmcMessage* message);
		 
	private:
		///apiType 0：消息文件并用方式 1：只文件方式
		const int m_apiType;
		///是否需要记录日志，true：记录日志，false：不需要记录日志
		const bool m_needLog;
		///日志名称，建议日志命名：pfmc_api.进程名称.log,默认pfmc_api.log
		const std::string m_logName;
		///记录初始化是否成功,true为成功，false为失败
		bool m_isInitSucc;
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

