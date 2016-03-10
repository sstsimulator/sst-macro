#ifndef sstmac_software_libraries_socketapiS_H
#define sstmac_software_libraries_socketapiS_H


#include <sstmac/libraries/sockets/socket_api.h>
#include <sstmac/software/libraries/service.h>
#include <sstmac/software/process/software_id.h>
#include <sstmac/software/api/api.h>
#include <sstmac/hardware/network/network_message.h>
#include <sstmac/common/messages/library_message.h>

#include <sstmac/software/libraries/compute/lib_compute_memmove_fwd.h>
#include <sstmac/software/process/operating_system_fwd.h>
#include <sstmac/software/libraries/service_fwd.h>

#include <sstmac/common/sstmac_config.h>

#include <vector>

namespace sstmac {
namespace sw {


class socket_message :
  public hw::network_message,
  public library_interface
{
 public:
  typedef enum { client, server } type_t;

 public:
  socket_message() {}

  socket_message(
    int number, long bytes,
    type_t ty,
    const std::string& libname,
    node_id toaddr,
    node_id fromaddr);

  int
  number() const;

  type_t
  type() const {
    return type_;
  }

  type_t
  reverse(type_t ty) const;

  void
  offset(long nbytes);

  hw::network_message*
  clone_injection_ack() const;

  virtual void
  serialize_order(sprockit::serializer& ser);

 protected:
  void
  clone_into(socket_message* cln);

 private:
  int number_;

  static task_id nic_task;

  type_t type_;


};

class socket  {
  // ------- constructor / boost stuff -------------//
 public:
  typedef enum { BLOCK_ON_ACCEPT, BLOCK_ON_RECV, BLOCK_ON_SEND, UNBLOCKED } state_t;


 public:
  socket(int fd, node_id my_addr,
         socket_domain_t domain, socket_type_t typ,
         socket_protocol_t protocol,
         operating_system* os);

  std::string
  to_string() const;

  void connect(int number, node_id addr);

  void disconnect();

  void bind(int number, node_id addr);

  void shutdown();

  long recv(long max_bytes);

  node_id send_addr() const;

  node_id recv_addr() const;

  node_id my_addr() const;

  void block(state_t state);

  void unblock();

  state_t state() const;

  void rebind();

  long bytes_available;

  long bytes_needed_to_unblock;

  static const long unblocked = -1;

  static const int unbound = -1;

  void accept(node_id remote_addr);

  void unaccept();

  int fd() const;

  int number() const;

  void add_pending(socket_message* msg);

  static const char* tostr(state_t state);

 private:
  int number_;

  int fd_;

  socket_domain_t domain_;

  socket_protocol_t protocol_;

  socket_type_t type_;

  node_id my_addr_;

  node_id remote_addr_;

  std::list<socket_message*> pending_recv_;

  key* key_;

  operating_system* os_;

  state_t state_;


};

class socket_server;
class socketapi :
  public api
{
 protected:
  std::list<socket_message*> pending_recv_;

  std::map<int,socket*> all_sockets_;

  std::map<int,socket*> bound_sockets_;

  std::map<int,socket*> connected_sockets_;

  std::vector<int> free_sockets_;

  int next_free_socket_;

  bool in_blocking_send_;

  lib_compute_memmove* lib_mem_;

  software_id id_;

  std::string server_libname_;

  socket_server* server_;

 public:
  socketapi();

  virtual ~socketapi();

 public:
  void connect(int fd, int number, node_id addr);

  void disconnect(int fd);

  void unaccept(int fd);

  void bind(int fd, int number, node_id addr);

  long send(int fd, long bytes, bool wait_on_ack = true);

  long recv(int fd, long max_bytes, bool blocking);

  node_id accept(int fd, node_id addr, bool blocking);

  void close(int fd);

  virtual bool supported() const;

  void init_param1(const software_id &sid);

  void init_os(operating_system* os);

  int allocate_socket(socket_domain_t domain, socket_type_t typ,
                      socket_protocol_t prot);

  void incoming_message(socket_message* msg);

  socket*
  get_socket(int fd) const;

  socket*
  get_bound_socket(int number) const;

  socket*
  get_connected_socket(int number) const;

};  

class socket_server :
  public sstmac::sw::service
{
 public:
  void incoming_message(sst_message* msg);

  void open_socket(int number, socketapi* proc);
  void close_socket(int number);

  void bind_socket(int number, socketapi* proc);
  void unbind_socket(int number);

  socket_server(const std::string& libname);

 protected:
  socketapi*
  get_server_socket(int number) const;

  socketapi*
  get_client_socket(int number) const;

 private:
  spkt_unordered_map<int, socketapi*> client_sockets_;
  spkt_unordered_map<int, socketapi*> server_sockets_;

};

}
} //end of namespace sstmac

#endif // socketapiS_H

