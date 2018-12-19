/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: pfmclog.h,v $
 *  Last Revision        : $Revision: 1.3 $
 *  Last Revision Date   : $Date: 2012/09/03 10:07:31 $
 *  Author               :
 *  Description          :
 **********************************************************/
#ifndef _PFMCLOG_H
#define _PFMCLOG_H

#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <string>

#ifdef __cplusplus
extern "C" {
#endif

namespace PfmcLogSpace
{
	///日志级别
	typedef enum {
				DEBUG_LOG = 1,
				INFO_LOG = 2,
				WARN_LOG = 3,
				ERROR_LOG = 4
				} LogLevel;
	///记录日志
	void pfmcLog(const LogLevel level, const char* buf, ...);
	///记录消息到跟踪日志
	void pfmcTraceLog(std::string& traceContent);
	
};

using namespace PfmcLogSpace;

#ifdef __cplusplus
   }
#endif

#endif

