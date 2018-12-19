/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: pfmcmanager.h,v $
 *  Last Revision        : $Revision: 1.5 $
 *  Last Revision Date   : $Date: 2012/09/20 06:46:39 $
 *  Author               :
 *  Description          :
 **********************************************************/
#ifndef _PFMCMANAGER_H
#define _PFMCMANAGER_H

#include <string>
#include <vector>
#include <sstream>
#include "pfmcmessage.h"
#include "udpsender.h"
#include "filemanager.h"

#ifdef __cplusplus
extern "C" {
#endif

namespace PfmcApi
{
	class UDPSender;
	class DataFileManager;
	class BakFileManager; 
	
	class PfmcManager
	{
	public:
		/**
		 * 析构函数
		 */
		~PfmcManager();
		/**
		 * 初始化函数,自动获取host、account、pid信息
		 * @return 
		 *  0  SUCCESS
		 *  -1 GET_HOST_ERROR
		 *  -2 GET_ACCOUNT_ERROR
		 *  -3 GET_PID_ERROR
		 *  -6 INIT_UDPSENDER_ERROR
		 *  -7 INIT_DATAFILEMAN_ERROR
		 *  -8 INIT_BAKFILEMAN_ERROR
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
		 * 设置备份周期
		 * @param bakPeriod 备份周期 单位秒
		 * @return 成功返回true,异常返回false
		 */
		bool setBakPeroid(const int bakPeriod);
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
		 * 发送一个Message消息，包含按接口规范 组织消息内容，
		 * api类型为1即只文件方式，不判断消息大小
		 * api类型为0即消息和文件并用方式，判断消息大小，大于60K采用文件方式，小于等于60K采用UDP消息
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
		int sendMessage(PfmcMessage* message);
		/**
		 * 设置apiType
		 * @param apiType apiType值
		 * @return 成功返回true,异常返回false
		 */
		bool setApiType(const int apiType) { m_apiType = apiType; return true; }
		/**
		 * 设置是否需要记录API日志
		 * @param needLog true：记录API日志；false：不记录
		 * @return 成功返回true,异常返回false
		 */
		bool setNeedLog(const bool needLog) { m_needLog = needLog; return true; }
		/**
		 * 设置日志名
		 * @param logName 日志名称
		 * @return 成功返回true,异常返回false
		 */
		bool setLogName(const std::string& logName) { m_logName = logName; return true; }
		/**
		 * 备份文件周期维护接口函数，要求调用的频度要小于备份周期
		 */
		void active();
		/**
		 * 单件类获取对象指针函数
		 * @returns 返回单件对象指针
		 **/
		static PfmcManager *getInstance();
		/**
		 * 获取当前日志级别
		 * @returns 返回当前日志级别
		 * DEBUG_LOG = 1,
		 * INFO_LOG = 2,
		 * WARN_LOG = 3,
		 * ERROR_LOG = 4,
		 **/
		int getLogLevel() const;
		/**
		 * 设置当前日志级别
		 * @return 成功返回true,异常返回false
		 **/
		bool setLogLevel(const int level);
		
	private:
		/**
		 * 构造函数
		 */
		PfmcManager();
		/**
		 * 获取统计文件名
		 * @param message Message消息
		 * @param dataFileName 统计文件名
		 * @return 成功返回true,异常返回false
		 */
		bool getDataFileName(const PfmcMessage* message, std::string& dataFileName);
		/**
		 * 将Message消息写入统计文件
		 * @param message Message消息文本内容
		 * @param fileName 统计文件名
		 * @return 成功返回true,异常返回false
		 */
		bool writeDataFile(const std::string& message, const std::string& fileName);
		/**
		 * 将Message消息写入备份文件
		 * @param message Message消息文本内容
		 * @param fileName 备份文件名
		 * @return 成功返回true,异常返回false
		 */
		bool writeBakFile(const std::string& message, const std::string& fileName);
		
	private:
		static PfmcManager *instance;
		///主机hostname
		std::string m_hostName;
		///账户名称
		std::string m_accountName;
		///进程PID
		pid_t m_pid;
		///是够跟踪UDP日志，true：记录跟踪日志；false：不记录。默认false
		bool m_UDPTrace;
		///apiType 0：消息文件并用方式 1：只文件方式。默认0
		int m_apiType;
		///是否需要记录日志，true：记录API日志；false：不记录。默认false
		bool m_needLog;
		///日志名称，建议日志命名：pfmc_api.进程名称.log,默认pfmc_api.log
		std::string m_logName;
		
		UDPSender* udpSender;
		DataFileManager* dataFileManager;
		BakFileManager* bakFileManager;
		
	};
}

#ifdef __cplusplus
   }
#endif

#endif

