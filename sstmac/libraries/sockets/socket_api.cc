#include <sstmac/libraries/sockets/socket_api.h>
#include <sstmac/libraries/sockets/lib_sockets.h>
#include <sstmac/software/process/app.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/app_manager.h>
#include <sstmac/software/launch/launcher.h>
#include <algorithm>

using namespace sstmac;
using namespace sstmac::sw;

static std::string socket_libname = lib_compute_memmove::static_name("libsocket");

#define NULL_NODEID node_id(-1)

static socketapi*
get_lib_socket()
{
  thread* t = operating_system::current_thread();
  return t->get_api<socketapi>();
}

extern "C" int
_SSTMAC_socket(socket_domain_t domain, socket_type_t type,
               socket_protocol_t protocol)
{
  int fd = get_lib_socket()->allocate_socket(domain, type, protocol);
  return fd;
}

extern "C" int
_SSTMAC_close(int fd)
{
  socketapi* socklib = get_lib_socket();
  socklib->close(fd);
  return 0;
}

extern "C" ssize_t
_SSTMAC_sendto(int fd, const void *message, size_t length, int flags,
               const sockaddr *dest_addr, socklen_t dest_len)
{
  socketapi* socklib = get_lib_socket();
  node_id addr(dest_addr->sin_addr.s_addr);
  socklib->connect(fd, dest_addr->sin_port, addr);

  bool nonblocking = flags & MSG_DONTWAIT;
  bool need_ack = !nonblocking;
  socklib->send(fd, length, need_ack);
  socklib->disconnect(fd);

  //for now just always assume that every byte gets sent
  return length;
}

extern "C" ssize_t
_SSTMAC_recvfrom(int fd, void *buffer, size_t length, int flags,
                 sockaddr *address, socklen_t *address_len)
{
  socketapi* socklib = get_lib_socket();
  bool blocking_recv = flags & MSG_WAITALL;
  bool blocking_accept = !(flags & MSG_DONTWAIT);
  bool peek_only = flags & MSG_PEEK;
  bool oob = flags & MSG_OOB;
  if (oob) {
    spkt_throw_printf(sprockit::value_error,
                     "simulated socket API in SST/macro does not support MSG_OOB flag");
  }

  node_id remote_addr = node_id(address->sin_addr.s_addr);
  node_id conn_addr = socklib->accept(fd, remote_addr, blocking_accept);
  if (conn_addr == NULL_NODEID) {
    if (blocking_accept) {
      spkt_throw_printf(sprockit::illformed_error,
                       "blocking accept on fd %d did not return valid connection");
    }
    else {
      return EWOULDBLOCK;
    }
  }

  if (peek_only) {
    if (blocking_recv || blocking_accept) {
      spkt_throw_printf(sprockit::illformed_error,
                       "cannot specify booth MSG_PEEK and MSG_WAITALL in recvfrom on socket");
    }

    sstmac::sw::socket* sock = socklib->get_socket(fd);
    return std::min(sock->bytes_available, (long) length);
  }

  ssize_t bytes_recvd = socklib->recv(fd, length, blocking_recv);
  address->sin_addr.s_addr = conn_addr;

  socklib->unaccept(fd);
  return bytes_recvd;
}

extern "C" unsigned long
_SSTMAC_inet_addr(const char *name)
{
  bool localhost = !(::strcmp(name, "localhost"))
                   || !(::strcmp(name, "127.0.0.1"));
  if (localhost) {
    return operating_system::current_node_id();
  }
  else {
    return operating_system::current_env()->node_address(name);
  }
}

extern "C" int
_SSTMAC_bind(int fd, const sockaddr *addr, socklen_t addr_len)
{
  socketapi* socklib = get_lib_socket();
  unsigned short number = addr->sin_port;
  node_id node_addr = operating_system::current_node_id();
  socklib->bind(fd, number, node_addr);
  return 0;
}

