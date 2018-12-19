/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: pfmclog.C,v $
 *  Last Revision        : $Revision: 1.8 $
 *  Last Revision Date   : $Date: 2012/09/20 06:47:52 $
 *  Author               :
 *  Description          :
 **********************************************************/
static const char rcs_id[] = "$Id: pfmclog.C,v 1.8 2012/09/20 06:47:52 cvs_wg Exp $";

#include <time.h>
#include <errno.h>
#include <stdarg.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "pfmclog.h"
#include "pfmcmessage.h"
#include "pfmccommon.h"

extern char g_logPathAndName[];
extern bool g_needLog;
extern LogLevel g_logLevel;
extern char g_tracePathAndName[];

namespace PfmcLogSpace
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_t g_logFile_Lock = PTHREAD_MUTEX_INITIALIZER;
	pthread_mutex_t g_traceFile_Lock = PTHREAD_MUTEX_INITIALIZER;
#endif
};

void PfmcLogSpace::pfmcLog(const LogLevel level, const char* buf, ...)
{
	///g_needLog=false 不需要记录日志
	if (!g_needLog)
	{
		return;
	}
	///日志级别小于当前设置的日志级别，退出
	if (level < g_logLevel)
	{
		return;
	}
	///g_needLog=true 需要记录日志
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&g_logFile_Lock);
#endif

	FILE *fp = 0;
	fp = fopen(g_logPathAndName, "at");
	if (fp == 0)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&g_logFile_Lock);
#endif
		return;
	}
	char logMsg[PFMC_LOG_LINE_MAX+1];
	va_list arg_ptr;
	va_start(arg_ptr, buf);
	vsprintf(logMsg, buf, arg_ptr);
	va_end(arg_ptr);

	std::string levelStr;
	switch(level)
	{
		case DEBUG_LOG: levelStr = "DEBUG";
			break;
		case INFO_LOG: levelStr = "INFO";
			break;
		case WARN_LOG : levelStr = "WARN";
			break;
		case ERROR_LOG: levelStr = "ERROR";
			break;
	}

	struct tm pTm;
	time_t cur_time;
	time(&cur_time);
#ifdef PFMCAPI_THREADSAFE
	localtime_r(&cur_time, &pTm);
#else
	struct tm *tmpTm = NULL;
	tmpTm = localtime(&cur_time);
	memcpy(&pTm, tmpTm, sizeof(tm));
#endif

	fprintf(fp,
			"%04d-%02d-%02d %02d:%02d:%02d  [%s] %s\n",
			pTm.tm_year + 1900,
			pTm.tm_mon + 1,
			pTm.tm_mday,
			pTm.tm_hour,
			pTm.tm_min,
			pTm.tm_sec,
			levelStr.c_str(),logMsg);

	char cmdBuf[1000];
	if(ftell(fp) > PFMC_LOG_FILE_SIZE)
	{
		sprintf(cmdBuf, "%s.%04d%02d%02d%02d%02d",
				g_logPathAndName,
				pTm.tm_year+1900,
				pTm.tm_mon+1,
				pTm.tm_mday,
				pTm.tm_hour,
				pTm.tm_min);
		rename(g_logPathAndName, cmdBuf);
	}

	fclose(fp);
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&g_logFile_Lock);
#endif
	return;
}

void PfmcLogSpace::pfmcTraceLog(std::string& traceContent)
{
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_lock(&g_traceFile_Lock);
#endif

	FILE *fp = 0;
	fp = fopen(g_tracePathAndName, "at");
	if (fp == 0)
	{
#ifdef PFMCAPI_THREADSAFE
		pthread_mutex_unlock(&g_traceFile_Lock);
#endif
		return;
	}

	struct tm pTm;
	time_t cur_time;
	time(&cur_time);
#ifdef PFMCAPI_THREADSAFE
	localtime_r(&cur_time, &pTm);
#else
	struct tm *tmpTm = NULL;
	tmpTm = localtime(&cur_time);
	memcpy(&pTm, tmpTm, sizeof(tm));
#endif

	fprintf(fp,
			"%04d-%02d-%02d %02d:%02d:%02d\n%s\n",
			pTm.tm_year + 1900,
			pTm.tm_mon + 1,
			pTm.tm_mday,
			pTm.tm_hour,
			pTm.tm_min,
			pTm.tm_sec,
			traceContent.c_str());

	char cmdBuf[1000];
	if(ftell(fp) > PFMC_LOG_FILE_SIZE)
	{
		sprintf(cmdBuf, "%s.%04d%02d%02d%02d%02d",
				g_tracePathAndName,
				pTm.tm_year+1900,
				pTm.tm_mon+1,
				pTm.tm_mday,
				pTm.tm_hour,
				pTm.tm_min);
		rename(g_tracePathAndName, cmdBuf);
	}

	fclose(fp);
#ifdef PFMCAPI_THREADSAFE
	pthread_mutex_unlock(&g_traceFile_Lock);
#endif
	return;
}
