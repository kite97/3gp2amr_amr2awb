/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: filemanager.h,v $
 *  Last Revision        : $Revision: 1.7 $
 *  Last Revision Date   : $Date: 2012/09/26 05:15:44 $
 *  Author               :
 *  Description          :
 **********************************************************/
#ifndef _FILEMANAGER_H
#define _FILEMANAGER_H

#include <string>
#include <vector>
#include <sstream>
#include "pfmccommon.h"

#ifdef __cplusplus
extern "C" {
#endif

namespace PfmcApi
{
	class DataFileManager
	{
	public:
		/**
		 * 构造函数
		 */
		DataFileManager();
		/**
		 * 析构函数
		 */
		~DataFileManager();
		/**
		 * 初始化函数
		 * @return 成功返回true,异常返回false
		 */
		bool init();
		/**
		 * 将消息写入统计文件
		 * @param message消息文本 
		 * @param fileName 统计文件名
		 * @return 成功返回true,异常返回false
		 */
		bool writeDataFile(const std::string& message, const std::string& fileName);
		/**
		 * 备份文件周期维护接口函数，要求高频度调用
		 * @param pid 当前进程pid
		 * @param hostName 当前进程所在主机hostname
		 */
		void active();
			
	private:
		/**
		 * 查找统计文件目录下，隐藏的和正式的文件，最后编辑时间为6天前的文件进行删除
		 */
		void delHistoryDataFile();
		
	private:
		///统计文件目录
		std::string m_dataFilePath;
		///上次删除历史文件时间
		time_t m_lastDelTime;
	};
	
	
	class BakFileManager
	{
	public:
		/**
		 * 构造函数
		 */
		BakFileManager();
		/**
		 * 析构函数
		 */
		~BakFileManager();
		/**
		 * 初始化函数
		 * @return 成功返回true,异常返回false
		 */
		bool init();
		/**
		 * 获取备份文件名
		 * @param bakFileName 备份文件名
		 * @return 成功返回true,异常返回false
		 */
		bool getBakFileName( const pid_t pid, const std::string& hostName, std::string& bakFileName);
		/**
		 * 将消息写入备份文件
		 * @param message消息文本 
		 * @param fileName 备份文件名
		 * @return 成功返回true,异常返回false
		 */
		bool writeBakFile(const std::string& message, const std::string& fileName);
		/**
		 * 设置备份周期
		 * @param bakPeriod 备份周期 单位秒
		 * @return 成功返回true,异常返回false
		 */
		bool setPeriod(const pid_t pid, const std::string& hostName, const int bakPeriod);
		/**
		 * 获取备份周期
		 * @return 成功返回m_bakPeriod
		 */
		const int getPeriod() const { return m_bakPeriod; }
		/**
		 * 将隐藏的备份文件改名为正式文件
		 * @param pid 当前进程pid
		 * @param hostName 当前进程所在主机hostname
		 */
		void bakFileCheck(const pid_t pid, const std::string& hostName);
		/**
		 * 备份文件周期维护接口函数，要求高频度调用
		 * @param pid 当前进程pid
		 * @param hostName 当前进程所在主机hostname
		 */
		void active(const pid_t pid, const std::string& hostName);
	
	private:
		/// 设置文件写锁
		static const bool setWriteLock(FILE *fp);
		/// 设置文件写解锁
		static const bool setWriteUnlock(FILE *fp);
		/**
		 * 检查文件是否存在
		 * @param fileName 文件名
		 * @return 存在返回true,不存在返回false
		 */
		bool checkExisted(const std::string& fileName);
			
		/**
		 * 将到达备份周期的备份文件改名成正式备份文件
		 * @param fileName 备份文件名
		 * @return 成功返回true,异常返回false
		 */
		bool mvToFinalBakFile(const std::string& fileName, const std::string& finalFileName);
		/**
		 * 生成空备份文件
		 * @param fileName 文件名
		 * @return 存在返回true,不存在返回false
		 */
		bool writeEmptyFile(const std::string& filePathAndName);
		/**
		 * 获取上一备份周期备份文件名
		 * @param m_pid pid值
		 * @param hostName hostname值
		 * @param bakFileName 上一备份周期备份文件名
		 * @return 成功返回true,异常返回false
		 */
		bool getLastBakFileName(const pid_t pid, const std::string& hostName, std::string& bakFileName);
		/**
		 * 查找备份文件目录下，隐藏的和正式的备份文件，最后编辑时间为6天前的文件进行删除
		 */
		void delHistoryBakFile();
	private:
		///备份文件目录
		std::string m_bakFilePath;
		///备份周期
		int m_bakPeriod;
		///上次将备份文件改为正式文件的时间
		time_t m_lastBakTime;
		///上次删除历史文件时间
		time_t m_lastDelTime;
	};
	
	/**
	 * 获取模周期为0的时间点
	 * @param nowTime 当前时间
	 * @period 周期
	 * @return 存在模周期为0的时间
	 */
	time_t getTime(const time_t nowTime, int period);
	/**
	 * 检查文件修改时间，判断是否是6天前的文件
	 * @param filename 文件名
	 * @return 是6天前文件返回true，否则返回false
	 */
	bool isHistoryFile(const std::string& filename);
	char* pfmc_del_safe_fgets(char *pBuff, int pLen, FILE *pStream);
}

#ifdef __cplusplus
   }
#endif

#endif

