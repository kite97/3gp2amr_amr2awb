/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: pfmcmanager.C,v $
 *  Last Revision        : $Revision: 1.9 $
 *  Last Revision Date   : $Date: 2012/09/21 10:10:00 $
 *  Author               :
 *  Description          :
 **********************************************************/
static const char rcs_id[] = "$Id: pfmcmanager.C,v 1.9 2012/09/21 10:10:00 cvs_wg Exp $";

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>
#include "pfmcmanager.h"
#include "pfmclog.h"

using namespace PfmcApi;

char g_logPathAndName[PFMC_MAX_LOGNMAE_LEN] = "";
bool g_needLog = false;
LogLevel g_logLevel = INFO_LOG;
char g_tracePathAndName[PFMC_MAX_LOGNMAE_LEN] = "";

PfmcManager* PfmcManager::instance = 0;

PfmcManager* PfmcManager::getInstance()
{
	if (instance == 0)
	{
		instance = new PfmcManager();
	}
	return instance;
}

PfmcManager::PfmcManager()
	:m_UDPTrace(false)
{
	udpSender = NULL;
	dataFileManager = NULL;
	bakFileManager = NULL;
}

PfmcManager::~PfmcManager()
{
	if (udpSender != NULL)
	{
		delete udpSender;
	}
	if (dataFileManager != NULL)
	{
		delete dataFileManager;
	}
	if (bakFileManager != NULL)
	{
		delete bakFileManager;
	}
	if (instance != NULL)
	{
		delete instance;
	}

}

int PfmcManager::init()
{
	///日志路径初始化，TRACE日志路径初始化
	if (getenv("PFMCAPIDIR") == NULL)
	{
		///GETENV_PFMCAPIDIR_ERROR
		return -29;
	}
	g_needLog = m_needLog;
	if (m_needLog)
	{
		snprintf(g_logPathAndName, PFMC_MAX_LOGNMAE_LEN-1 , "%s/log/%s", getenv("PFMCAPIDIR"), m_logName.c_str());
		snprintf(g_tracePathAndName, PFMC_MAX_LOGNMAE_LEN-1 , "%s/log/tracelog/trace.%s", getenv("PFMCAPIDIR"), m_logName.c_str());
	}
	else
	{
		snprintf(g_tracePathAndName, PFMC_MAX_LOGNMAE_LEN-1 , "%s/log/tracelog/trace.log", getenv("PFMCAPIDIR"));
	}

	udpSender = new UDPSender();
	if (udpSender == NULL)
	{
		pfmcLog(ERROR_LOG, "PfmcManager::init() INIT_UDPSENDER_ERROR");
		///INIT_UDPSENDER_ERROR
		return -26;
	}

	dataFileManager = new DataFileManager();
	if (dataFileManager == NULL)
	{
		pfmcLog(ERROR_LOG, "PfmcManager::init() INIT_DATAFILEMAN_ERROR");
		///INIT_DATAFILEMAN_ERROR
		return -27;
	}
	if (!(dataFileManager->init()))
	{
		pfmcLog(ERROR_LOG, "PfmcManager::init() INIT_DATAFILEMAN_ERROR: dataFileManager init error");
		///INIT_DATAFILEMAN_ERROR
		return -27;
	}

	bakFileManager = new BakFileManager();
	if (bakFileManager == NULL)
	{
		pfmcLog(ERROR_LOG, "PfmcManager::init() INIT_BAKFILEMAN_ERROR");
		///INIT_BAKFILEMAN_ERROR
		return -28;
	}
	if (!(bakFileManager->init()))
	{
		pfmcLog(ERROR_LOG, "PfmcManager::init() INIT_BAKFILEMAN_ERROR: bakFileManager init error");
		///INIT_BAKFILEMAN_ERROR
		return -28;
	}

	///获取host
	char hostName[PFMC_MAX_HOSTNAME_LEN+1] = "";
	if (gethostname(hostName, sizeof(hostName)) != 0)
	{
		pfmcLog(ERROR_LOG, "PfmcManager::init() GET_HOST_ERROR");
		///GET_HOST_ERROR
		return -21;
	}
	m_hostName = hostName;
	pfmcLog(INFO_LOG, "PfmcManager::init() hostName:%s", m_hostName.c_str());

	///获取account
	struct passwd *myName = NULL;
	myName = getpwuid(geteuid());
	if (myName == NULL)
	{
		pfmcLog(ERROR_LOG, "PfmcManager::init() GET_ACCOUNT_ERROR");
		///GET_ACCOUNT_ERROR
		return -22;
	}
	m_accountName = myName->pw_name;
	pfmcLog(INFO_LOG, "PfmcManager::init() accountName:%s", m_accountName.c_str());

	///获取pid
	if ((m_pid = getpid()) == -1)
	{
		pfmcLog(ERROR_LOG, "PfmcManager::init() GET_PID_ERROR");
		///GET_PID_ERROR
		return -23;
	}
	pfmcLog(INFO_LOG, "PfmcManager::init() pid:%d", m_pid);
	pfmcLog(INFO_LOG, "PfmcManager::init() PfmcManager init succ");
	///INIT_SUCCESS
	return 0;
}

bool PfmcManager::addDestIpAndPort(const std::string& ip, const unsigned int port)
{
	return udpSender->addIp(ip, port);
}

bool PfmcManager::delAllDestIPAndPort()
{
	return udpSender->clearIPList();
}

bool PfmcManager::setBakPeroid(const int bakPeriod)
{
	return bakFileManager->setPeriod( m_pid, m_hostName, bakPeriod);
}

bool PfmcManager::opneUDPTrace()
{
	m_UDPTrace = true;
	return true;
}

bool PfmcManager::closeUDPTrace()
{
	m_UDPTrace = false;
	return true;
}

int PfmcManager::sendMessage(PfmcMessage* message)
{
	///先判断apiType，如果apitype错误，退出
	if (m_apiType != 0 && m_apiType != 1)
	{
		pfmcLog(ERROR_LOG, "PfmcManager::sendMessage() TYPE_ERROR");
		///OTHER_ERROR
		return -16;
	}
	///设置消息中的host、account、pid
	message->setHostName(m_hostName);
	message->setAccountName(m_accountName);
	message->setPid(m_pid);
#ifdef PFMCAPI_THREADSAFE
	pthread_t tmpTid;
	tmpTid = pthread_self();
	if ( tmpTid < 0 )
	{
		pfmcLog(ERROR_LOG, "PfmcManager::sendMessage() pthread_self() get tid < 0,use defalut 0");
		tmpTid = 0;
	}
	pfmcLog(DEBUG_LOG, "PfmcManager::sendMessage() pthread_self() get tid:%lu", tmpTid);
	message->setTid(tmpTid);
#endif
	///消息编码
	int encodeResult = message->encodeMessage();
	if (encodeResult < 0)
	{
		return encodeResult;
	}
	std::string content = message->getMessageContent();

	///apiType为1，只文件方式
	if (m_apiType == 1)
	{
		pfmcLog(DEBUG_LOG, "PfmcManager::sendMessage() apiType=1 datafile only");
		std::string dataFileName;
		if (!getDataFileName(message, dataFileName))
		{
			///OTHER_ERROR
			return -16;
		}
		if (!writeDataFile(content, dataFileName))
		{
			///WRITEFILE_ERROR
			return -14;
		}
		///SUCCESS
		return 0;
	}

	///apiType为0
	///判断消息大小
	pfmcLog(DEBUG_LOG, "PfmcManager::sendMessage() apiType=0");
	int rt = message->checkMessageSize();
	if (rt < 0)
	{
		pfmcLog(ERROR_LOG, "PfmcManager::sendMessage() checkMessageSize ERROR ");
		///OTHER_ERROR
		return -16;
	}

	///消息大于60K 文件方式
	if (rt == 1)
	{
		std::string dataFileName;
		if (!getDataFileName(message, dataFileName))
		{
			///OTHER_ERROR
			return -16;
		}
		if (!writeDataFile(content, dataFileName))
		{
			///WRITEFILE_ERROR
			return -14;
		}
		///SUCCESS
		return 0;
	}

	///消息小于等于60K udp消息方式
	int msgResult = udpSender->sendUDPMessage(content);
	///如果trace为打开，trace udp消息到日志
	if (m_UDPTrace)
	{
		pfmcTraceLog(content);
	}
	///记录udp消息到备份文件
	std::string bakFileName;
	if (!bakFileManager->getBakFileName(m_pid, m_hostName, bakFileName))
	{
		pfmcLog(ERROR_LOG, "PfmcManager::sendMessage() getBakFileName error");
	}
	if (!writeBakFile(content, bakFileName))
	{
		pfmcLog(ERROR_LOG, "PfmcManager::sendMessage() writeDataFile error");
	}

	if ( msgResult< 0)
	{
		///SENDUDP_ERROR
		return -13;
	}
	///SUCCESS
	return 0;
}

bool PfmcManager::getDataFileName(const PfmcMessage* message, std::string& dataFileName)
{
	dataFileName = "PMD.";
	dataFileName += m_hostName;
	dataFileName += ".";
	char pidStr[20] = "";
	sprintf(pidStr, "%lu", m_pid);
	dataFileName += pidStr;
	if (message->getNeedTid())
	{
		char tidStr[30] = "";
		sprintf(tidStr, "%lu", message->getTid());
		dataFileName += "-";
		dataFileName += tidStr;
	}
	dataFileName += ".";

	dataFileName += message->getJobId();
	dataFileName += ".";

	struct tm messageTime;
	PfmcMessage* tmpMessage = const_cast< PfmcMessage* >(message);
	if (!tmpMessage->getEndTimeForTM(messageTime))
	{
		return false;
	}
	char tmpTImeStr[5] = "";
	std::string tmpTime;
	switch (messageTime.tm_wday)
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
	sprintf(tmpTImeStr, "%02d%02d", messageTime.tm_hour, messageTime.tm_min);
	tmpTime += tmpTImeStr;

	dataFileName += tmpTime;
	dataFileName += ".dat";
	pfmcLog(INFO_LOG, "PfmcManager::getDataFileName() dataFileName:%s", dataFileName.c_str());
	return true;
}

void PfmcManager::active()
{
	if (m_apiType == 0)
	{
		bakFileManager->active(m_pid, m_hostName);
	}
	else
	{
		pfmcLog(DEBUG_LOG, "PfmcManager::active() m_apiType == 1 no need to bakFileManager");
	}
		dataFileManager->active();
	return;
}

bool PfmcManager::writeDataFile(const std::string& message, const std::string& fileName)
{
	return dataFileManager->writeDataFile(message, fileName);
}

bool PfmcManager::writeBakFile(const std::string& message, const std::string& fileName)
{
	return bakFileManager->writeBakFile(message, fileName);
}

int PfmcManager::getLogLevel() const
{
	int level = -1;
	switch(g_logLevel)
	{
		case DEBUG_LOG: level = 1 ;
			break;
		case INFO_LOG: level = 2 ;
			break;
		case WARN_LOG : level = 3 ;
			break;
		case ERROR_LOG: level = 4 ;
			break;
	}
	return level;
}

bool PfmcManager::setLogLevel(const int level)
{
	switch(level)
	{
		case 1: g_logLevel = DEBUG_LOG;
			break;
		case 2: g_logLevel = INFO_LOG;
			break;
		case 3: g_logLevel = WARN_LOG;
			break;
		case 4: g_logLevel = ERROR_LOG;
			break;
		default:
			return false;
	}
	return true;
}
