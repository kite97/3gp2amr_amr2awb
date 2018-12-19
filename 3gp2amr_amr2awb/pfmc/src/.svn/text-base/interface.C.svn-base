/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: interface.C,v $
 *  Last Revision        : $Revision: 1.11 $
 *  Last Revision Date   : $Date: 2012/09/25 03:08:26 $
 *  Author               :
 *  Description          :
 **********************************************************/
static const char rcs_id[] = "$Id: interface.C,v 1.11 2012/09/25 03:08:26 cvs_wg Exp $";

#include "interface.h"
#include "pfmcmanager.h"
#include "pfmclog.h"

using namespace PfmcApi;
extern char g_logPathAndName[];
#ifdef PFMCAPI_THREADSAFE
///互斥锁，用于多线程
pthread_mutex_t m_if_Lock = PTHREAD_MUTEX_INITIALIZER;
#endif

Interface::Interface()
	: m_apiType(0),
	  m_needLog(false),
	  m_logName(""),
	  m_isInitSucc(false)
{

}

Interface::Interface(const int type)
	: m_apiType(type),
	  m_needLog(false),
	  m_logName(""),
	  m_isInitSucc(false)
{

}

Interface::Interface(const std::string& logName)
	: m_apiType(0),
	  m_needLog(true),
	  m_logName(logName),
	  m_isInitSucc(false)
{

}

Interface::Interface(const int type , const std::string& logName)
	: m_apiType(type),
	  m_needLog(true),
	  m_logName(logName),
	  m_isInitSucc(false)
{

}

Interface::~Interface()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_destroy(&m_if_Lock);
#endif
}

int Interface::init()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif

	PfmcManager::getInstance()->setApiType(m_apiType);
	PfmcManager::getInstance()->setNeedLog(m_needLog);
	PfmcManager::getInstance()->setLogName(m_logName);

	int initRt = PfmcManager::getInstance()->init();
	if (initRt < 0)
	{
		pfmcLog(ERROR_LOG, "Interface::init() Interface init error:%d", initRt);

#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return initRt;
	}
	if (m_apiType != 0 && m_apiType != 1)
	{
		pfmcLog(ERROR_LOG, "Interface::init() Interface init error: -4 TYPE_ERROR");

#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		///TYPE_ERROR
		return -24;
	}
	if (!m_needLog)
	{
		pfmcLog(INFO_LOG, "Interface::init() Interface init succ ");
		m_isInitSucc = true;

#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		///INIT_SUCCESS
		return 0;
	}
	if (m_logName.size() == 0)
	{
		pfmcLog(ERROR_LOG, "Interface::init() Interface init error: -5 LOGNAME_ERROR");

#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		///LOGNAME_ERROR
		return -25;
	}
	m_isInitSucc = true;

#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	pfmcLog(INFO_LOG, "Interface::init() Interface init succ");
	///INIT_SUCCESS
	return 0;
}

bool Interface::addDestIpAndPort(const std::string& ip, const unsigned int port)
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return false;
	}
	if (PfmcManager::getInstance()->addDestIpAndPort(ip, port))
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return true;
	}
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return false;
}

bool Interface::delAllDestIPAndPort()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return false;
	}
	if (PfmcManager::getInstance()->delAllDestIPAndPort())
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return true;
	}
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return false;
}

bool Interface::setDayBak()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return false;
	}
	if (PfmcManager::getInstance()->setBakPeroid(24*3600))
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return true;
	}
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return false;
}

bool Interface::setHourBak()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return false;
	}
	if (PfmcManager::getInstance()->setBakPeroid(3600))
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return true;
	}
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return false;
}

bool Interface::opneUDPTrace()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return false;
	}
	if (PfmcManager::getInstance()->opneUDPTrace())
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return true;
	}
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return false;
}

bool Interface::closeUDPTrace()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return false;
	}
	if (PfmcManager::getInstance()->closeUDPTrace())
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return true;
	}
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return false;
}

int Interface::sendMessage(PfmcMessage& message)
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		///INIT_NOTSUCC
		return -15;
	}
	PfmcMessage* pfmcMsg = new PfmcMessage(message);
	if (pfmcMsg == NULL)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		///NEW_PFMCMESSAGE_FAIL
		return -12;
	}
	pfmcLog(DEBUG_LOG, "Interface::sendMessage() new PfmcMessage ok ");
	int sendRt = sendMessage(pfmcMsg);
	delete pfmcMsg;
	pfmcLog(DEBUG_LOG, "Interface::sendMessage() delete PfmcMessage ok ");
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return sendRt;
}

void Interface::active()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return;
	}
	PfmcManager::getInstance()->active();
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return;
}

int Interface::getLogLevel()
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return -1;
	}
	int rt = PfmcManager::getInstance()->getLogLevel();
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return rt;
}

bool Interface::setLogLevel(const int level)
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	///判断interface是否已经完成init，并且成功，如果未成功初始化，退出
	if (!m_isInitSucc)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return false;
	}
	if (PfmcManager::getInstance()->setLogLevel(level))
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&m_if_Lock);
#endif
		return true;
	}
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return false;
}

char* Interface::getResultDesc(const int result)
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&m_if_Lock);
#endif
	static char desc[32] = "";
	switch(result)
	{
		case 0: sprintf(desc, "%s", "SUCCESS");
			break;
		case -1: sprintf(desc, "%s", "HEADTYPE_MISSING");
			break;
		case -2: sprintf(desc, "%s", "HEADVERSION_MISSING");
			break;
		case -3: sprintf(desc, "%s", "HOST_MISSING");
			break;
		case -4: sprintf(desc, "%s", "ACCOUNT_MISSING");
			break;
		case -5: sprintf(desc, "%s", "PID_MISSING");
			break;
		case -6: sprintf(desc, "%s", "TID_MISSING");
			break;
		case -7: sprintf(desc, "%s", "JOBID_MISSING");
			break;
		case -8: sprintf(desc, "%s", "ENDTIME_MISSING");
			break;
		case -9: sprintf(desc, "%s", "ERROR_ENDTIME");
			break;
		case -10: sprintf(desc, "%s", "VALUE_NOTMATCH_NAME");
			break;
		case -11: sprintf(desc, "%s", "ENCODE_MESSAGE_FAIL");
			break;
		case -12: sprintf(desc, "%s", "NEW_PFMCMESSAGE_FAIL");
			break;
		case -13: sprintf(desc, "%s", "SENDUDP_ERROR");
			break;
		case -14: sprintf(desc, "%s", "WRITEFILE_ERROR");
			break;
		case -15: sprintf(desc, "%s", "INIT_NOTSUCC");
			break;
		case -16: sprintf(desc, "%s", "OTHER_ERROR");
			break;
		case -21: sprintf(desc, "%s", "INIT_GET_HOST_ERROR");
			break;
		case -22: sprintf(desc, "%s", "INIT_GET_ACCOUNT_ERROR");
			break;
		case -23: sprintf(desc, "%s", "INIT_GET_PID_ERROR");
			break;
		case -24: sprintf(desc, "%s", "INIT_TYPE_ERROR");
			break;
		case -25: sprintf(desc, "%s", "INIT_LOGNAME_ERROR");
			break;
		case -26: sprintf(desc, "%s", "INIT_UDPSENDER_ERROR");
			break;
		case -27: sprintf(desc, "%s", "INIT_DATAFILEMAN_ERROR");
			break;
		case -28: sprintf(desc, "%s", "INIT_BAKFILEMAN_ERROR");
			break;
		case -29: sprintf(desc, "%s", "GETENV_PFMCAPIDIR_ERROR");
			break;
		default: sprintf(desc, "%s", "GET_RESULTDESC_FAIL");
			break;
	}

#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&m_if_Lock);
#endif
	return desc;
}

int Interface::sendMessage(PfmcMessage* message)
{
	return PfmcManager::getInstance()->sendMessage(message);
}


