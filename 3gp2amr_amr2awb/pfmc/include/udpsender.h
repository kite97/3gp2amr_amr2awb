/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: udpsender.h,v $
 *  Last Revision        : $Revision: 1.4 $
 *  Last Revision Date   : $Date: 2012/09/05 07:48:48 $
 *  Author               :
 *  Description          :
 **********************************************************/
#ifndef _UDPSENDER_H
#define _UDPSENDER_H

#include <vector>
#include <string>
#include <sstream>
#include <pthread.h>
#include "pfmccommon.h"

#ifdef __cplusplus
extern "C" {
#endif

namespace PfmcApi
{
	class UDPSender
	{
	public:
		/**
		 * 构造函数
		 */
		UDPSender();
		/**
		 * 析构函数
		 */
		~UDPSender();
		/**
		 * 加载目的IP和端口
		 * @param ip 目的IP地址
		 * @param port 目的端口
		 * @return 成功返回true,异常返回false
		 */
		bool addIp(const std::string& ip, const int port);
		/**
		 * 删除原有的目的IP地址列表
		 * @return 成功返回true,异常返回false
		 */
		bool clearIPList();
		/**
		 * 将一个Message消息以UDP消息方式往一个目的地址发送
		 * @param message 一个Message消息内容
		 * @param address 一个目的地址
		 * @return 
		 *  0  SUCCESS
		 *  -1 FAIL
		 */
		int send(const std::string& message, UDPAddress& address);
		/**
		 * 将一个Message消息以UDP消息方式往目的地址列表发送
		 * @param message 一个Message消息内容
		 * @return 
		 *  0  SUCCESS
		 *  -1 FAIL
		 */
		int sendUDPMessage(const std::string& message);
		
	private:
		/**
		 * 检查ip地址是IPv4还是IPv6
		 * @param ip 目的IP地址
		 * @return IPV4=0, IPV6=1
		 */
		IPVersion checkIpVersion(const std::string& ip);
		/**
		 * 检查ip地址是否是IPv4
		 *@return 是ipv4返回true,否则返回false
		 */
		bool isIPV4(const std::string& ip) const;
	#ifdef PFMCAPI_IPv6
		/**
		 * 检查ip地址是否是IPv6
		 *@return 是ipv6返回true,否则返回false
		 */
		bool isIPV6(const std::string& ip) const;
	#endif
	private:
		///目的IP地址和端口列表
		std::vector< UDPAddress > m_IPAndPortList;
	};
}

#ifdef __cplusplus
   }
#endif

#endif


