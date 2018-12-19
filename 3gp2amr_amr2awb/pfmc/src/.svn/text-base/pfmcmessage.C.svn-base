/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: pfmcmessage.C,v $
 *  Last Revision        : $Revision: 1.11 $
 *  Last Revision Date   : $Date: 2012/09/21 05:40:08 $
 *  Author               :
 *  Description          :
 **********************************************************/
static const char rcs_id[] = "$Id: pfmcmessage.C,v 1.11 2012/09/21 05:40:08 cvs_wg Exp $";

#include <time.h> 
#include "pfmcmessage.h"
#include "pfmccommon.h"
#include "pfmclog.h"

using namespace PfmcApi;
extern char g_logPathAndName[];
	
PfmcMessage::PfmcMessage()
	: m_headType("PMD"),
	  m_headReserve(""),
	  m_headVersion("1.0"),
	  m_needTid(false),
	  m_bodyReserve(""),
	  m_jobId(),
	  m_endTime(),
	  m_delimiter("|"),
	  m_content(),
	  m_hostName(),
	  m_accountName(),
	  m_pid(0),
	  m_tid(0),
	  m_isTmpValueListAviable(false),
	  m_isEncoded(false)
{
}

PfmcMessage::PfmcMessage(const std::string& jobid)
	: m_headType("PMD"),
	  m_headReserve(""),
	  m_headVersion("1.0"),
	  m_needTid(false),
	  m_bodyReserve(""),
	  m_jobId(jobid),
	  m_endTime(),
	  m_delimiter("|"),
	  m_content(),
	  m_hostName(),
	  m_accountName(),
	  m_pid(0),
	  m_tid(0),
	  m_isTmpValueListAviable(false),
	  m_isEncoded(false)
{
}

PfmcMessage::PfmcMessage(const std::string& jobid, const std::string& endtime)
	: m_headType("PMD"),
	  m_headReserve(""),
	  m_headVersion("1.0"),
	  m_needTid(false),
	  m_bodyReserve(""),
	  m_jobId(jobid),
	  m_endTime(endtime),
	  m_delimiter("|"),
	  m_content(),
	  m_hostName(),
	  m_accountName(),
	  m_pid(0),
	  m_tid(0),
	  m_isTmpValueListAviable(false),
	  m_isEncoded(false)
{
}

PfmcMessage::~PfmcMessage()
{
	m_nameList.clear();
	tmpValueList.clear();
	m_valueLineList.clear();
}

bool PfmcMessage::setTid(const unsigned long tid )
{
	m_needTid = true;
	m_tid = tid;
	return true;
}

bool PfmcMessage::addName(const std::string& name)
{
	m_nameList.push_back(name);
	return true;
}

bool PfmcMessage::addNewLineForValue()
{
	if (m_isTmpValueListAviable == true)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::addNewLineForValue() must end the last line");
		return false;
	}
	tmpValueList.clear();
	m_isTmpValueListAviable = true;
	pfmcLog(DEBUG_LOG, "PfmcMessage::addNewLineForValue() addNewLineForValue succ");
	return true;
}

bool PfmcMessage::endLineForValue()
{
	if (m_isTmpValueListAviable == false)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::endLineForValue() must addNewLineForValue before endLineForValue");
		return false;
	}
	m_valueLineList.push_back(tmpValueList);
	m_isTmpValueListAviable = false;
	pfmcLog(DEBUG_LOG, "PfmcMessage::endLineForValue() endLineForValue succ");
	return true;
}

bool PfmcMessage::addValue(const std::string& value)
{
	if (!m_isTmpValueListAviable)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::addValue() must addNewLineForValue before addValue");
		return false;
	}
	tmpValueList.push_back(value);
	pfmcLog(DEBUG_LOG, "PfmcMessage::addValue() addValue succ");
	return true;
}

int PfmcMessage::encodeMessage()
{
	if (m_isEncoded)
	{
		pfmcLog(INFO_LOG, "PfmcMessage::encodeMessage() PfmcMessage is encoded ");
		///SUCCESS
		return 0;
	}
	int checkResult = checkMessage();
	if (checkResult < 0)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::encodeMessage() checkMessage failed errno:%d", checkResult);
		return checkResult;
	}
	std::ostringstream messageStream;
	messageStream << "<Message>\n";
	if (!encodeHead(messageStream) || !encodeBody(messageStream))
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::encodeMessage() encodeMessage failed ");
		///ENCODE_MESSAGE_FAIL
		return -11;
	}
	messageStream << "</Message>\n";
	m_content = messageStream.str();
	m_isEncoded = true;
	pfmcLog(DEBUG_LOG, "PfmcMessage::encodeMessage() encode succ");
	///SUCCESS
	return 0;
}

const int PfmcMessage::checkMessageSize()
{
	if (!m_isEncoded)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::checkMessageSize() PfmcMessage not encode ");
		return -1;
	}
	size_t messageSize = m_content.size();
	if (messageSize > PFMC_MAX_UDPMESSAGE_SIZE)
	{
		pfmcLog(INFO_LOG, "PfmcMessage::checkMessageSize() messageSize > 60K ");
		return 1;
	}
	pfmcLog(DEBUG_LOG, "PfmcMessage::checkMessageSize() messageSize: %d ", messageSize);
	pfmcLog(INFO_LOG, "PfmcMessage::checkMessageSize() messageSize <= 60K ");
	return 0;
}

bool PfmcMessage::getEndTimeForTM(struct tm& endTimeForTM)
{
	if (m_endTime.length() != 19)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::getEndTimeForTM() EndTime length error ");
		return false;
	}
	endTimeForTM.tm_sec = atoi(m_endTime.substr(17,2).c_str());
	endTimeForTM.tm_min = atoi(m_endTime.substr(14,2).c_str());
	endTimeForTM.tm_hour = atoi(m_endTime.substr(11,2).c_str());
	endTimeForTM.tm_mday = atoi(m_endTime.substr(8,2).c_str());
	endTimeForTM.tm_mon = atoi(m_endTime.substr(5,2).c_str())-1;
	endTimeForTM.tm_year = atoi(m_endTime.substr(0,4).c_str())-1900;
	
	time_t cur_time = time(NULL);
	struct tm* time_cur = localtime(&cur_time);
	endTimeForTM.tm_isdst = time_cur->tm_isdst;
	
	if (mktime(&endTimeForTM) == (time_t)-1)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::getEndTimeForTM() EndTime format error ");
		return false;
	}
	return true;
}

bool PfmcMessage::checkNameAndValue()
{
	int numOfName = m_nameList.size();
	int numOfValue = 0;
	for (std::vector< std::vector<std::string> >::iterator it = m_valueLineList.begin(); \
		it != m_valueLineList.end(); it++)
	{
		numOfValue = (*it).size();
		if (numOfValue != numOfName)
		{
			return false;
		}
	}
	return true;
}

bool PfmcMessage::checkEndTimeValue()
{
	if (m_endTime.length() != 19)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::checkEndTimeValue() EndTime length error ");
		return false;
	}
	struct tm tmpTm;
	tmpTm.tm_sec = atoi(m_endTime.substr(17,2).c_str());
	tmpTm.tm_min = atoi(m_endTime.substr(14,2).c_str());
	tmpTm.tm_hour = atoi(m_endTime.substr(11,2).c_str());
	tmpTm.tm_mday = atoi(m_endTime.substr(8,2).c_str());
	tmpTm.tm_mon = atoi(m_endTime.substr(5,2).c_str())-1;
	tmpTm.tm_year = atoi(m_endTime.substr(0,4).c_str())-1900;
	if (mktime(&tmpTm) == (time_t)-1)
	{
		pfmcLog(ERROR_LOG, "PfmcMessage::checkEndTimeValue() EndTime format error ");
		return false;
	}
	return true;
}

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
int PfmcMessage::checkMessage()
{
	if (m_hostName.length() == 0)
	{
		///HOST_MISSING
		return -3;
	}
	if (m_accountName.length() == 0)
	{
		///ACCOUNT_MISSING
		return -4;
	}
	if (m_pid <= 0)
	{
		///PID_MISSING
		return -5;
	}
	if (m_needTid)
	{
		if (m_tid < 0)
		{
			///TID_MISSING
			return -6;
		}
	}
	if (m_jobId.length() == 0)
	{
		///JOBID_MISSING
		return -7;
	}
	if (m_endTime.length() == 0)
	{
		///ENDTIME_MISSING
		return -8;
	}
	if (!checkEndTimeValue())
	{
		///ERROR_ENDTIME
		return -9;
	}
	if (!checkNameAndValue())
	{
		//VALUE_NOTMATCH_NAME
		return -10;
	}
	///SUCCESS
	return 0;
}

bool PfmcMessage::encodeHead(std::ostringstream& stream)
{
	stream << "\t<Header>\n";
	stream << "\t\t<Type>" << m_headType << "</Type>\n";
	stream << "\t\t<Reserve>" << m_headReserve << "</Reserve>\n";
	stream << "\t\t<Version>" << m_headVersion << "</Version>\n";
	stream << "\t</Header>\n";
	return true;
}

bool PfmcMessage::encodeBody(std::ostringstream& stream)
{
	stream << "\t<Body>\n";
	stream << "\t\t<Host>" << m_hostName << "</Host>\n";
	stream << "\t\t<Account>" << m_accountName << "</Account>\n";
	stream << "\t\t<Pid>" << m_pid << "</Pid>\n";
	if (m_needTid)
	{
		stream << "\t\t<Tid>" << m_tid << "</Tid>\n";
	}
	if (m_bodyReserve.length() != 0)
	{
		stream << "\t\t<Reserve>" << m_bodyReserve << "</Reserve>\n";
	}
	stream << "\t\t<JobId>" << m_jobId << "</JobId>\n";
	stream << "\t\t<EndTime>" << m_endTime << "</EndTime>\n";
	stream << "\t\t<Data Delimiter='" << m_delimiter;
	stream << "'>\n";

	///Data元素内容
	stream << "\t\t\t<Name>";
	
	for (std::vector<std::string>::iterator it = m_nameList.begin(); it != m_nameList.end(); it++)
	{
		stream << *it;
		std::vector<std::string>::iterator nextIt;
		nextIt = it + 1;
		if (nextIt != m_nameList.end())
		{
			stream << m_delimiter;
		}
	}
	stream << "</Name>\n";
	for (std::vector< std::vector<std::string> >::iterator it = m_valueLineList.begin(); \
		it != m_valueLineList.end(); it++)
	{
		stream << "\t\t\t<Value>";
		for (std::vector<std::string>::iterator it_value = (*it).begin(); it_value != (*it).end(); it_value++ )
		{
			stream << *it_value;
			std::vector<std::string>::iterator nextItValue;
			nextItValue = it_value + 1;
			if (nextItValue != (*it).end())
			{
				stream << m_delimiter;
			}
		}
		stream << "</Value>\n";
	}
	
	stream << "\t\t</Data>\n";
	stream << "\t</Body>\n";
	return true;
}

