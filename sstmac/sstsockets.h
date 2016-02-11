#ifndef SOCKETS_H
#define SOCKETS_H

#include <sstmac/util.h>
#include <sstmac/libraries/sockets/socket_api.h>
#define inet_addr(...) _SSTMAC_inet_addr(__VA_ARGS__)
#define socket(...) _SSTMAC_socket(__VA_ARGS__)
#define recvfrom(...) _SSTMAC_recvfrom(__VA_ARGS__)
#define sendto(...) _SSTMAC_sendto(__VA_ARGS__)
#define bind(...) _SSTMAC_bind(__VA_ARGS__)
#define close(...) _SSTMAC_close(__VA_ARGS__)

#endif // SOCKETS_H

