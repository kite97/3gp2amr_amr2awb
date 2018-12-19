/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: filemanager.C,v $
 *  Last Revision        : $Revision: 1.23 $
 *  Last Revision Date   : $Date: 2012/12/13 02:52:56 $
 *  Author               :
 *  Description          :
 **********************************************************/
static const char rcs_id[] = "$Id: filemanager.C,v 1.23 2012/12/13 02:52:56 cvs_wg Exp $";

#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include "filemanager.h"
#include "pfmclog.h"

using namespace PfmcApi;
using PfmcLogSpace::pfmcLog;

extern char g_logPathAndName[];

DataFileManager::DataFileManager()
	:m_lastDelTime(0)
{

}

DataFileManager::~DataFileManager()
{

}

bool DataFileManager::init()
{
	if (getenv("PFMCAPIDIR") == NULL)
	{
		pfmcLog(ERROR_LOG, "DataFileManager::init() getenv PFMCAPIDIR fail");
		return false;
	}
	char tempPath[PFMC_MAX_LOGNMAE_LEN] = "";
	snprintf(tempPath, PFMC_MAX_LOGNMAE_LEN-1 , "%s/files/datafiles/", getenv("PFMCAPIDIR"));
	m_dataFilePath = tempPath;
	pfmcLog(INFO_LOG, "DataFileManager::init() init datafile path : %s", m_dataFilePath.c_str());
	return true;
}

bool DataFileManager::writeDataFile(const std::string& message, const std::string& fileName)
{
	std::string filePathAndName = m_dataFilePath + "." + fileName;
	std::string finalFile = m_dataFilePath + fileName;
	///判断隐藏文件是否存在
	int status = access(filePathAndName.c_str(), 0);
	if (status == 0)
	{
		pfmcLog(WARN_LOG, "DataFileManager::writeDataFile() %s exists", filePathAndName.c_str());
		status = unlink(filePathAndName.c_str());
		if (status < 0)
		{
			pfmcLog(ERROR_LOG, "DataFileManager::writeDataFile() [File operation fail] unlink %s error,%s",
				 filePathAndName.c_str(), strerror(errno));
		}
	}

	FILE *fp = fopen(filePathAndName.c_str(), "w");
	if (fp == NULL)
	{
		pfmcLog(ERROR_LOG, "DataFileManager::writeDataFile() [File operation fail] fopen fail");
		return false;
	}
	if (fprintf(fp, "%s", message.c_str()) < 0)
	{
		fclose(fp);
		pfmcLog(ERROR_LOG, "DataFileManager::writeDataFile() [File operation fail] fprintf fail");
		return false;
	}
	fclose(fp);
	///判断统计文件是否存在
	status = access(finalFile.c_str(), 0);
	if (status == 0)
	{
		pfmcLog(WARN_LOG, "DataFileManager::writeDataFile() %s exists", finalFile.c_str());
		status = unlink(finalFile.c_str());
		if (status < 0)
		{
			pfmcLog(ERROR_LOG, "DataFileManager::writeDataFile() [File operation fail] unlink %s error,%s",
				 finalFile.c_str(), strerror(errno));
		}
	}

	if (rename(filePathAndName.c_str(), finalFile.c_str()) < 0)
	{
		pfmcLog(ERROR_LOG, "DataFileManager::writeDataFile() [File operation fail] rename %s to %s error",
			 filePathAndName.c_str(), finalFile.c_str());
		return false;
	}

	pfmcLog(DEBUG_LOG, "DataFileManager::writeDataFile() write datafile:%s succ", finalFile.c_str());
	return true;
}

void DataFileManager::delHistoryDataFile()
{
	///查找统计文件目录下，隐藏的和正式的文件，最后编辑时间为6天前的文件进行删除
	char commAnd[PFMC_MAX_BUF_LEN] = "";
	FILE* fp;
	char buffer[PFMC_MAX_BUF_LEN] = "";
	sprintf(commAnd, "ls -al %s|grep -E 'dat'|awk '{print $NF}'", m_dataFilePath.c_str());
	pfmcLog(DEBUG_LOG, "DataFileManager::delHistoryDataFile() commAnd is %s.", commAnd );

	if (fp = popen(commAnd, "r"))
	{
		while (pfmc_del_safe_fgets(buffer, PFMC_MAX_BUF_LEN, fp) != NULL)
		{
			std::string tmpfile;
			tmpfile = buffer;
			int rc = 0;
			if ( ( rc = tmpfile.find( '\r', 0 ) ) != std::string::npos )
			{
				tmpfile[rc] = '\0';
			}
			if ( ( rc = tmpfile.find( '\n', 0 ) ) != std::string::npos )
			{
				tmpfile[rc] = '\0';
			}
			std::string tmpfilePathAndName = m_dataFilePath + tmpfile;
			if (isHistoryFile(tmpfilePathAndName))
			{
				///是6天前的历史文件，删除
				int status = unlink(tmpfilePathAndName.c_str());
				if (status < 0)
				{
					pfmcLog(ERROR_LOG, "DataFileManager::delHistoryDataFile() [File operation fail] unlink %s error,%s",
					 tmpfilePathAndName.c_str(), strerror(errno));
				}
				else
				{
					pfmcLog(INFO_LOG, "DataFileManager::delHistoryDataFile() unlink %s succ",
					 tmpfilePathAndName.c_str());
				}
			}
		}
		pclose( fp );
	}
	else
	{
		pfmcLog(ERROR_LOG, "DataFileManager::delHistoryDataFile() popen Error[%d]:%s",
			 errno, strerror( errno ) );
	}
	return;
}

void DataFileManager::active()
{
	time_t nowTime;
	time(&nowTime);
	if ((nowTime - m_lastDelTime) > 24*60*60)
	{
		///到达历史统计文件删除时间
		pfmcLog(DEBUG_LOG, "DataFileManager::active() time to delHistoryFile");
		time_t delModTime;
		delModTime = getTime(nowTime, 24*60*60);
		if (delModTime < 0)
		{
			pfmcLog(ERROR_LOG, "DataFileManager::active() get delModTime error");
			m_lastDelTime = delModTime;
			return;
		}
		m_lastDelTime = delModTime;
		///删除6天前的历史文件
		delHistoryDataFile();
	}
	return;
}

BakFileManager::BakFileManager()
	:m_bakPeriod(3600),
	 m_lastBakTime(0),
	 m_lastDelTime(0)
{
}

BakFileManager::~BakFileManager()
{

}

bool BakFileManager::init()
{
	if (getenv("PFMCAPIDIR") == NULL)
	{
		pfmcLog(ERROR_LOG, "BakFileManager::init() getenv PFMCAPIDIR fail");
		return false;
	}
	char tempPath[PFMC_MAX_LOGNMAE_LEN] = "";
	snprintf(tempPath, PFMC_MAX_LOGNMAE_LEN-1 , "%s/files/bakfiles/", getenv("PFMCAPIDIR"));
	m_bakFilePath = tempPath;
	pfmcLog(INFO_LOG, "BakFileManager::init() init bakfile path : %s bakPeriod:%d ",
		 m_bakFilePath.c_str(), m_bakPeriod);
	return true;
}

bool BakFileManager::writeBakFile(const std::string& message, const std::string& fileName)
{
	std::string filePathAndName = m_bakFilePath + "." + fileName;
	FILE *fp = fopen(filePathAndName.c_str(), "at");
	if (fp == NULL)
	{
		pfmcLog(ERROR_LOG, "DataFileManager::writeBakFile() [File operation fail] fopen fail");
		return false;
	}
	bool rt = setWriteLock(fp);
	if (!rt)
	{
		setWriteUnlock(fp);
		fclose(fp);
		pfmcLog(ERROR_LOG, "DataFileManager::writeBakFile() setWriteLock fail");
		return false;
	}
	if (fprintf(fp, "%s", message.c_str()) < 0)
	{
		setWriteUnlock(fp);
		fclose(fp);
		pfmcLog(ERROR_LOG, "DataFileManager::writeBakFile() fprintf fail");
		return false;
	}
	setWriteUnlock(fp);
	fclose(fp);
	pfmcLog(DEBUG_LOG, "DataFileManager::writeBakFile() writeBakFile succ");
	return true;
}

bool BakFileManager::getBakFileName(const pid_t pid, const std::string& hostName, std::string& bakFileName)
{
	bakFileName = "PMD.";
	bakFileName += hostName;
	bakFileName += ".";
	char pidStr[20] = "";
	sprintf(pidStr, "%lu", pid);
	bakFileName += pidStr;
	bakFileName += ".";

	time_t nowTime;
	time(&nowTime);
	time_t modTime;
	modTime = getTime(nowTime, m_bakPeriod);
	if (modTime < 0)
	{
		pfmcLog(ERROR_LOG, "BakFileManager::getBakFileName() get modTime error");
		return false;
	}
	char tmpTimeStr[5] = "";
	std::string tmpTime;

	time_t bakTime = modTime + m_bakPeriod;
	struct tm bakTimeTm;
#ifdef PFMCAPI_THREADSAFE
	localtime_r(&bakTime, &bakTimeTm);
#else
	struct tm *tmpBakTimeTm;
	tmpBakTimeTm = localtime(&bakTime);
	memcpy(&bakTimeTm, tmpBakTimeTm, sizeof(tm));
#endif
	switch (bakTimeTm.tm_wday)
	{
		case 0:
			tmpTime = "7" ;
			break;
		case 1: tmpTime = "1";
			break;
		case 2: tmpTime = "2";
			break;
		case 3: tmpTime = "3";
			break;
		case 4: tmpTime = "4";
			break;
		case 5: tmpTime = "5";
			break;
		case 6: tmpTime = "6";
			break;
	}
	sprintf(tmpTimeStr, "%02d%02d", bakTimeTm.tm_hour, bakTimeTm.tm_min);
	tmpTime += tmpTimeStr;

	bakFileName += tmpTime;
	bakFileName += ".bak";
	pfmcLog(INFO_LOG, "BakFileManager::getBakFileName() bakFileName:%s", bakFileName.c_str());
	return true;
}

bool BakFileManager::checkExisted(const std::string& fileName)
{
	FILE *fp = fopen(fileName.c_str(), "r");
	if (fp == NULL)
	{
		return false;
	}
	else
	{
		fclose(fp);
		return true;
	}
}

bool BakFileManager::getLastBakFileName(const pid_t pid, const std::string& hostName, std::string& bakFileName)
{
	bakFileName = "PMD.";
	bakFileName += hostName;
	bakFileName += ".";
	char pidStr[20] = "";
	sprintf(pidStr, "%lu", pid);
	bakFileName += pidStr;
	bakFileName += ".";

	time_t nowTime;
	time(&nowTime);
	time_t bakTime;
	bakTime = getTime(nowTime, m_bakPeriod);
	if (bakTime < 0)
	{
		pfmcLog(ERROR_LOG, "BakFileManager::getLastBakFileName() get bakTime error");
		return false;
	}

	char tmpTimeStr[5] = "";
	std::string tmpTime;
	struct tm bakTimeTm;
#ifdef PFMCAPI_THREADSAFE
	localtime_r(&bakTime, &bakTimeTm);
#else
	struct tm *tmpBakTimeTm;
	tmpBakTimeTm = localtime(&bakTime);
	memcpy(&bakTimeTm, tmpBakTimeTm, sizeof(tm));
#endif
	switch (bakTimeTm.tm_wday)
	{
		case 0:
			tmpTime = "7" ;
			break;
		case 1: tmpTime = "1";
			break;
		case 2: tmpTime = "2";
			break;
		case 3: tmpTime = "3";
			break;
		case 4: tmpTime = "4";
			break;
		case 5: tmpTime = "5";
			break;
		case 6: tmpTime = "6";
			break;
	}
	sprintf(tmpTimeStr, "%02d%02d", bakTimeTm.tm_hour, bakTimeTm.tm_min);
	tmpTime += tmpTimeStr;

	bakFileName += tmpTime;
	bakFileName += ".bak";
	pfmcLog(INFO_LOG, "BakFileManager::getLastBakFileName() bakFileName:%s", bakFileName.c_str());
	return true;
}

bool BakFileManager::writeEmptyFile(const std::string& filePathAndName)
{
	FILE *fp = fopen(filePathAndName.c_str(), "at");
	if (fp == NULL)
	{
		pfmcLog(ERROR_LOG, "BakFileManager::writeEmptyFile() [File operation fail] fopen fail ");
		return false;
	}
	bool rt = setWriteLock(fp);
	if (!rt)
	{
		setWriteUnlock(fp);
		fclose(fp);
		pfmcLog(ERROR_LOG, "BakFileManager::writeEmptyFile() setWriteLock fail ");
		return false;
	}
	if (fprintf(fp, " ") < 0)
	{
		setWriteUnlock(fp);
		fclose(fp);
		pfmcLog(ERROR_LOG, "BakFileManager::writeEmptyFile() fprintf fail ");
		return false;
	}
	setWriteUnlock(fp);
	fclose(fp);
	pfmcLog(DEBUG_LOG, "BakFileManager::writeEmptyFile() writeEmptyFile:%s ok ", filePathAndName.c_str());
	return true;
}

bool BakFileManager::setPeriod(const pid_t pid, const std::string& hostName, const int bakPeriod)
{
	if (bakPeriod > m_bakPeriod)
	{
		///获取本周期隐藏备份文件名
		std::string bakFileName;
		if (!getBakFileName(pid, hostName, bakFileName))
		{
			pfmcLog(ERROR_LOG, "BakFileManager::setBakPeroid() getBakFileName error");
		}
		pfmcLog(INFO_LOG, "BakFileManager::setBakPeroid() mv last bakfile to final before bak period changed");
		std::string filePathAndName = m_bakFilePath + "." + bakFileName;
		std::string finalFilePathAndName = m_bakFilePath + bakFileName;
		//隐藏备份文件如果存在，改名为正式备份文件
		if (checkExisted(filePathAndName))
		{
			pfmcLog(INFO_LOG, "BakFileManager::setBakPeroid() last bakfile existed");
			mvToFinalBakFile(filePathAndName, finalFilePathAndName);
		}
		else
		{
			pfmcLog(DEBUG_LOG, "BakFileManager::setBakPeroid() last bakfile not existed");
		}
	}
	m_lastBakTime = 0;
	m_bakPeriod = bakPeriod;
	return true;
}

void BakFileManager::bakFileCheck(const pid_t pid, const std::string& hostName)
{
	///将备份文件改为正式文件
	std::string lastBakFileName;
	if (!getLastBakFileName(pid, hostName, lastBakFileName))
	{
		pfmcLog(ERROR_LOG, "BakFileManager::bakFileCheck() getLastBakFileName error");
		return;
	}

	std::string filePathAndName = m_bakFilePath + "." + lastBakFileName;
	std::string finalFilePathAndName = m_bakFilePath + lastBakFileName;
	if (!checkExisted(filePathAndName))
	{
		///文件不存在，创建空文件
		if (!writeEmptyFile(finalFilePathAndName))
		{
			pfmcLog(ERROR_LOG, "BakFileManager::bakFileCheck() generate empty bakfile error");
		}
		return;
	}
	///隐藏备份文件存在，改名为正式备份文件
	mvToFinalBakFile(filePathAndName, finalFilePathAndName);
	return;
}

void BakFileManager::delHistoryBakFile()
{
	///查找备份文件目录下，隐藏的和正式的备份文件，最后编辑时间为6天前的文件进行删除
	char commAnd[PFMC_MAX_BUF_LEN] = "";
	FILE* fp;
	char buffer[PFMC_MAX_BUF_LEN] = "";
	sprintf(commAnd, "ls -alt %s|grep -E 'bak'|awk '{print $NF}'", m_bakFilePath.c_str());
	pfmcLog(DEBUG_LOG, "BakFileManager::delHistoryBakFile() commAnd is %s.", commAnd );

	if (fp = popen(commAnd, "r"))
	{
		while (pfmc_del_safe_fgets(buffer, PFMC_MAX_BUF_LEN, fp) != NULL)
		{
			std::string tmpfile;
			tmpfile = buffer;
			int rc = 0;
			if ( ( rc = tmpfile.find( '\r', 0 ) ) != std::string::npos )
			{
				tmpfile[rc] = '\0';
			}
			if ( ( rc = tmpfile.find( '\n', 0 ) ) != std::string::npos )
			{
				tmpfile[rc] = '\0';
			}
			std::string tmpfilePathAndName = m_bakFilePath + tmpfile;
			if (isHistoryFile(tmpfilePathAndName))
			{
				///是6天前的历史文件，删除
				int status = unlink(tmpfilePathAndName.c_str());
				if (status < 0)
				{
					pfmcLog(ERROR_LOG, "DataFileManager::delHistoryBakFile() unlink %s error,%s",
					 tmpfilePathAndName.c_str(), strerror(errno));
				}
				else
				{
					pfmcLog(INFO_LOG, "DataFileManager::delHistoryBakFile() unlink %s succ",
					 tmpfilePathAndName.c_str());
				}
			}
		}
		pclose( fp );
	}
	else
	{
		pfmcLog(ERROR_LOG, "BakFileManager::delHistoryBakFile() popen Error[%d]:%s",
			 errno, strerror( errno ) );
	}
	return;
}

void BakFileManager::active(const pid_t pid, const std::string& hostName)
{
	time_t nowTime;
	time(&nowTime);

	if (m_lastBakTime == 0)
	{
		time_t firstModTime;
		firstModTime = getTime(nowTime, m_bakPeriod);
		if (firstModTime < 0)
		{
			pfmcLog(ERROR_LOG, "BakFileManager::active() get firstModTime error");
			return;
		}
		m_lastBakTime = firstModTime;
	}

	if ((nowTime - m_lastBakTime) > (m_bakPeriod+60))
	{
		///到达备份周期
		pfmcLog(DEBUG_LOG, "BakFileManager::active() time to bakFileCheck");
		time_t modTime;
		modTime = getTime(nowTime, m_bakPeriod);
		if (modTime < 0)
		{
			pfmcLog(ERROR_LOG, "BakFileManager::active() get modTime error");
			return;
		}
		m_lastBakTime = modTime;
		///将备份文件改名为正式文件
		bakFileCheck(pid, hostName);
	}
	if ((nowTime - m_lastDelTime) > 24*60*60)
	{
		///到达历史文件删除时间
		pfmcLog(DEBUG_LOG, "BakFileManager::active() time to delHistoryFile");
		time_t delModTime;
		delModTime = getTime(nowTime, 24*60*60);
		if (delModTime < 0)
		{
			pfmcLog(ERROR_LOG, "BakFileManager::active() get delModTime error");
			m_lastDelTime = delModTime;
			return;
		}
		m_lastDelTime = delModTime;
		///删除6天前的历史文件
		delHistoryBakFile();
	}
	return;
}

bool BakFileManager::mvToFinalBakFile(const std::string& fileName, const std::string& finalFileName)
{
	unlink(finalFileName.c_str());
	if (rename(fileName.c_str(), finalFileName.c_str()) < 0)
	{
		pfmcLog(ERROR_LOG, "BakFileManager::mvToFinalBakFile() [File operation fail] rename %s to %s error",
			 fileName.c_str(), finalFileName.c_str());
		return false;
	}
	pfmcLog(INFO_LOG, "BakFileManager::mvToFinalBakFile() rename %s to %s ok",
			fileName.c_str(), finalFileName.c_str());
	return true;
}

const bool BakFileManager::setWriteLock(FILE *fp)
{
	int fd = fileno(fp);
	struct flock writeLock;
	writeLock.l_type = F_WRLCK;
	writeLock.l_start = 0;
	writeLock.l_whence = SEEK_SET;
	writeLock.l_len = 0;
	writeLock.l_pid = -1;
	if (::fcntl(fd, F_SETLKW, &writeLock) < 0)
	{
		return false;
	}
	return true;
}

const bool BakFileManager::setWriteUnlock(FILE *fp)
{
	int fd = fileno(fp);
	struct flock writeLock;
	writeLock.l_type = F_UNLCK;
	writeLock.l_start = 0;
	writeLock.l_whence = SEEK_SET;
	writeLock.l_len = 0;
	writeLock.l_pid = -1;
	if (::fcntl(fd, F_SETLKW, &writeLock) < 0)
	{
		return false;
	}
	return true;
}

time_t PfmcApi::getTime(const time_t nowTime, int period)
{
	///支持天及以下周期
	time_t modTime;
	if (period % (24*60*60) == 0)
	{
		struct tm timeTM;
#ifdef PFMCAPI_THREADSAFE
		localtime_r(&nowTime, &timeTM);
#else
		struct tm *tmpTimeTM;
		tmpTimeTM = localtime(&nowTime);
		memcpy(&timeTM, tmpTimeTM, sizeof(tm));
#endif
		timeTM.tm_hour = 0;
		timeTM.tm_min = 0;
		timeTM.tm_sec = 0;
		modTime = mktime(&timeTM);
		if (modTime < 0)
		{
			pfmcLog(ERROR_LOG, "getTime() mktime error ");
			return -1;
		}
	}
	else
	{
		int tmpSec = 0;
		tmpSec = nowTime % period;
		modTime = nowTime - tmpSec;
		//pfmcLog(DEBUG_LOG, "BakFileManager::getTime() period:%d, tmpSec:%d", period, tmpSec);
	}
	return modTime;
}

bool PfmcApi::isHistoryFile(const std::string& filename)
{
	///判断文件是否是6天前的历史文件
	pfmcLog(DEBUG_LOG, "isHistoryFile() filename %s ", filename.c_str());
	struct stat buf;
	if (stat(filename.c_str(), &buf) < 0)
	{
		pfmcLog(ERROR_LOG, "isHistoryFile() stat %s error %s", filename.c_str(), strerror(errno));
		return false;
	}
	time_t lastModTime = buf.st_mtime;
	time_t nowTime;
	time(&nowTime);
	if ((nowTime - lastModTime) > 6*24*60*60 )
	{
		pfmcLog(DEBUG_LOG, "isHistoryFile() find history file %s ", filename.c_str());
		return true;
	}
	return false;
}

char* PfmcApi::pfmc_del_safe_fgets( char *pBuff, int pLen, FILE *pStream )
{
	char *result;
	int count = 0;
	while ( 1 )
	{
		result = fgets( pBuff, pLen, pStream );
		if ( count > 5 )
		{
			return NULL;
		}
		if ( result != NULL )
		{
			return result;
		}
		else if ( errno == EINTR || errno == EAGAIN )
		{
			errno = 0;
			clearerr( pStream );
			count ++;
			continue;
		}
		else
		{
			return NULL;
		}
	}
}
