/***********************************************************
 *  Copyright(C) 2010 EASTCOM-BUPT Inc.
 *
 *  Filename             : $RCSfile: udpsender.C,v $
 *  Last Revision        : $Revision: 1.7 $
 *  Last Revision Date   : $Date: 2012/12/13 02:52:56 $
 *  Author               :
 *  Description          :
 **********************************************************/
static const char rcs_id[] = "$Id: udpsender.C,v 1.7 2012/12/13 02:52:56 cvs_wg Exp $";

#include <unistd.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <arpa/inet.h>
#include <errno.h>
#include <string.h>
#include <vector>
#include "udpsender.h"
#include "pfmclog.h"

using namespace PfmcApi;

extern char g_logPathAndName[];

UDPSender::UDPSender()
{

}

UDPSender::~UDPSender()
{

}

bool UDPSender::addIp(const std::string& ip, const int port)
{
	IPVersion version = checkIpVersion(ip);
	if (version == ERRORIPV)
	{
		pfmcLog(ERROR_LOG, "UDPSender::addIp() checkIpVersion: ERRORIPVERSION ");
		return false;
	}
	struct UDPAddress address(ip, port, version);
	m_IPAndPortList.push_back(address);

	pfmcLog(INFO_LOG, "UDPSender::addIp() addIP succ %s:%d version:%d ",  ip.c_str(), port, version);
	return true;
}

bool UDPSender::clearIPList()
{
	m_IPAndPortList.clear();
	pfmcLog(INFO_LOG, "UDPSender::clearIPList() clearIPList end ");
	return true;
}

int UDPSender::send(const std::string& message, UDPAddress& address)
{
	int sendResult;
	int sockfdClient;
	int len = message.length();
	int sysBufSize, sysBufLen;
	sysBufSize = 64*1024;
	sysBufLen = sizeof(sysBufSize);

	if (address.ipVersion == IPV4)
	{
		pfmcLog(INFO_LOG, "UDPSender::send() IPV4 send UDP to %s, %d ", address.ipAddress.c_str(), address.udpPort);
		sockfdClient = socket(AF_INET, SOCK_DGRAM, 0);
		if (sockfdClient < 0)
		{
			pfmcLog(ERROR_LOG, "UDPSender::send() [Socket connection fail] create socket error ");
			return -1;
		}
		if (setsockopt(sockfdClient, SOL_SOCKET, SO_SNDBUF, (char*)&sysBufSize, sysBufLen) < 0)
		{
			pfmcLog(ERROR_LOG, "UDPSender::send() [Socket connection fail] Set send buffer size to %d failed:%s ",
			 sysBufSize, strerror(errno));
			return -1;
		}
		///IPV4
		struct sockaddr_in destAddr;
		memset(&destAddr, 0, sizeof(destAddr));
		destAddr.sin_family = AF_INET;
		destAddr.sin_addr.s_addr = inet_addr(address.ipAddress.c_str());
		destAddr.sin_port = htons(address.udpPort);
		sendResult = sendto(sockfdClient, message.c_str(), len, 0,
					(struct sockaddr*) &destAddr, sizeof(destAddr));
		close(sockfdClient);
	}
	else
	{
		///IPV6
#ifdef PFMCAPI_IPv6

		pfmcLog(INFO_LOG, "UDPSender::send() IPV6 send UDP to %s, %d ", address.ipAddress.c_str(), address.udpPort);
		sockfdClient = socket(AF_INET6, SOCK_DGRAM, 0);
		if (sockfdClient < 0)
		{
			pfmcLog(ERROR_LOG, "UDPSender::send() [Socket connection fail] create socket error ");
			return -1;
		}
		if (setsockopt(sockfdClient, SOL_SOCKET, SO_SNDBUF, (char*)&sysBufSize, sysBufLen) < 0)
		{
			pfmcLog(ERROR_LOG, "UDPSender::send() [Socket connection fail] Set send buffer size to %d failed:%s ",
			 sysBufSize, strerror(errno));
			return -1;
		}
		struct sockaddr_in6 destAddr;
		memset(&destAddr, 0, sizeof(destAddr));
		unsigned int scope = 0;

		destAddr.sin6_family = AF_INET6;
		destAddr.sin6_port = htons(address.udpPort);
		destAddr.sin6_scope_id = scope;
		if (inet_pton(AF_INET6, address.ipAddress.c_str(), &destAddr.sin6_addr) < 0)
		{
			pfmcLog(ERROR_LOG, "UDPSender::send() error:%s", strerror(errno));
			return -1;
		}

		sendResult = sendto(sockfdClient, message.c_str(), len, 0,
					(struct sockaddr*) &destAddr, sizeof(destAddr));
		close(sockfdClient);
#else
		pfmcLog(ERROR_LOG, "UDPSender::send() Enable IPv6 and recompile pfmc_api");
		return -1;
#endif
	}

	if (sendResult < 0)
	{
		pfmcLog(ERROR_LOG, "UDPSender::send() error sending packet: %s", strerror(errno));
		return -1;
	}
	return 0;
}

int UDPSender::sendUDPMessage(const std::string& message)
{
	int rt = 0;
	if (m_IPAndPortList.size() == 0)
	{
		pfmcLog(ERROR_LOG, "UDPSender::sendUDPMessage() ip list empty");
		return -1;
	}
	for (std::vector<UDPAddress>::iterator it = m_IPAndPortList.begin(); it != m_IPAndPortList.end(); it++)
	{
		if (send(message, *it) < 0)
		{
			rt = -1;
		}
	}
	return rt;
}

bool UDPSender::isIPV4(const std::string& ip) const
{
	if (ip.empty())
	{
		return false;
	}

	unsigned int p1, p2, p3, p4;
	int count = 0;
	int result = sscanf(ip.c_str(), "%u.%u.%u.%u%n", &p1, &p2, &p3, &p4, &count);
	if ((result == 4)
		&& (p1 <= 255)
		&& (p2 <= 255)
		&& (p3 <= 255)
		&& (p4 <= 255)
		&& (count == ip.size()))
	{
		return true;
	}

	return false;
}

#ifdef PFMCAPI_IPv6

bool UDPSender::isIPV6(const std::string& ip) const
{
	if (ip.empty())
	{
		return false;
	}
	int colon_count = 0;
	int dot_count = 0;
	int len = ip.size();
	int position = 0;
	while(position < len)
	{
		position = ip.find(".", position);
		position++;

		if (position == 0)
		{
			break;
		}
		else
		{
		dot_count++;
		}
	}
	position = 0;
	while(position < len)
	{
		position = ip.find(":", position);
		position++;

		if (position == 0)
		{
			break;
		}
		else
		{
		colon_count++;
		}
	}
	/// must have between two and seven colons
	if ((colon_count > 7) || (colon_count < 2))
	{
		return false;
	}
	/// if there was a dot there must be three of them
	if ((dot_count > 0) && (dot_count != 3))
	{
		return false;
	}
	pfmcLog(DEBUG_LOG, "UDPSender::isIPV6() colon_count:%d,dot_count:%d", colon_count, dot_count);
	return true;
}
#endif

IPVersion UDPSender::checkIpVersion(const std::string& ip)
{
	if (isIPV4(ip))
	{
		pfmcLog(INFO_LOG, "UDPSender::checkIpVersion() ipv4");
		return IPV4;
	}
#ifdef PFMCAPI_IPv6
	if (isIPV6(ip))
	{
		pfmcLog(INFO_LOG, "UDPSender::checkIpVersion() ipv6");
		return IPV6;
	}
#endif
	pfmcLog(ERROR_LOG, "UDPSender::checkIpVersion() error ip %s", ip.c_str());
	return ERRORIPV;
}


