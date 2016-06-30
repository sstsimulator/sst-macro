#ifndef sstmac_software_libraries_SOCKETAPI_H
#define sstmac_software_libraries_SOCKETAPI_H

#include <unistd.h>


#define O_NONBLOCK (1 << 0)

#define MSG_PEEK (1 << 0)
#define MSG_WAITALL (1 << 2)
#define MSG_OOB (1 << 3)
#define MSG_DONTWAIT (1 << 4)

#ifdef __cplusplus
#include <cstring>
extern "C" {
#endif

typedef int _sstmac_socklen_t;
#define socklen_t _sstmac_socklen_t

#undef htons
#define htons(x) x

typedef enum {
  /** File system pathnames */
  AF_UNIX,
  /** Internet address */
  AF_INET
} socket_domain_t;
#define PF_INET AF_INET
#define PF_UNIX AF_UNIX

typedef enum {
  IPPROTO_TCP,
  IPPROTO_UDP
} socket_protocol_t;

static const unsigned long INADDR_ANY = -38;

typedef enum {
  /** Provides sequenced, reliable, bidirectional, connection-mode byte streams, and may provide a transmission mechanism for out-of-band data. */
  SOCK_STREAM,
  /** Provides datagrams, which are connectionless-mode, unreliable messages of fixed maximum length. */
  SOCK_DGRAM,
  /** Provides sequenced, reliable, bidirectional, connection-mode transmission path for records.
      A record can be sent using one or more output operations and received using one or more input operations,
      but a single operation never transfers part of more than one record.
      Record boundaries are visible to the receiver via the MSG_EOR flag.
  */
  SOCK_SEQPACKET
} socket_type_t;

struct in_addr {
  unsigned long s_addr;
};

struct sockaddr {
  short sin_family;
  unsigned short sin_port;
  struct in_addr sin_addr;
};
#define sockaddr_in sockaddr
#define sockaddr_in6 sockaddr

unsigned long _SSTMAC_inet_addr(const char* name);

int _SSTMAC_socket(socket_domain_t domain, socket_type_t,
                   socket_protocol_t protocol);

ssize_t _SSTMAC_recvfrom(int socket, void *buffer, size_t length,
                         int flags, struct sockaddr *address, socklen_t *address_len);

ssize_t _SSTMAC_sendto(int socket, const void *message, size_t length,
                       int flags,
                       const struct sockaddr *dest_addr, socklen_t dest_len);

int _SSTMAC_bind(int fd, const struct sockaddr* addr, socklen_t addr_len);

int _SSTMAC_close(int fd);



#ifdef __cplusplus
}
#endif

#endif // SOCKETAPI_H

