#include <sstmac/libraries/sockets/lib_sockets.h>
#include <sstmac/software/process/operating_system.h>
#include <sstmac/software/process/api.h>
#include <sstmac/software/libraries/compute/lib_compute_memmove.h>
#include <sprockit/util.h>

#define enumcase(x) case x: return #x;

ImplementAPI(sstmac::sw, socketapi, "socket");

#define NULL_NODEID node_id(-1)

#define socket_debug(...)

namespace sstmac {
namespace sw {

SpktRegister("socket", api, socketapi, "Create bindings for sockets");

task_id socket_message::nic_task(-1);

socket_message::socket_message(
  int number, long bytes,
  type_t ty,
  const std::string &libname,
  node_id toaddr,
  node_id fromaddr)
  : network_message(toaddr, fromaddr, task_id(), task_id(), bytes), //use blank task ids
    library_interface(libname),
    number_(number),
    type_(ty)
{
  sst_message::msgtype_ = network_message::NETWORK;
  network_message::type_ = network_message::payload;
}

void
socket_message::serialize_order(sprockit::serializer& ser)
{
 spkt_throw(sprockit::unimplemented_error,
    "socket_message::serialize_order");
}

int
socket_message::number() const
{
  return number_;
}

socket_message::type_t
socket_message::reverse(type_t ty) const
{
  switch(ty) {
    case client:
      return server;
    case server:
      return client;
  }
}

hw::network_message::ptr
socket_message::clone_injection_ack() const
{
  ptr cln = new socket_message;
  cln->number_ = number_;
  cln->type_ = reverse(type_);
  network_message::clone_into(cln);
  library_interface::clone_into(cln.get());
  cln->convert_to_ack();
  return cln;
}

socket::socket(int fd, node_id my_addr,
               socket_domain_t domain,
               socket_type_t typ,
               socket_protocol_t protocol,
               operating_system* os)
  :  number_(unbound), fd_(fd),
     domain_(domain), protocol_(protocol),
     type_(typ), my_addr_(my_addr),
     key_(key::construct()), os_(os),
     state_(UNBLOCKED), bytes_available(0)
{
}

int
socket::number() const
{
  return number_;
}

void
socket::bind(int number, node_id addr)
{
  if (number_ != unbound) {
    spkt_throw_printf(sprockit::value_error, "socket %d is already bound.  cannot bind again",
                     number_);
  }
  number_ = number;
  remote_addr_ = addr;
}

void
socket::disconnect()
{
  remote_addr_ = NULL_NODEID;
  number_ = unbound;
}

void
socket::connect(int number, node_id addr)
{
  if (remote_addr_ != NULL_NODEID) {
    spkt_throw_printf(sprockit::value_error, "socket %d is already connected", number_);
  }
  remote_addr_ = addr;
  number_ = number;
}

void
socket::unaccept()
{
  bytes_available = 0;
  //remote_addr_ = 0;
  pending_recv_.clear();
}

void
socket::shutdown()
{
  number_ = unbound;
  bytes_available = 0;
  remote_addr_ = NULL_NODEID;
}

std::string
socket::to_string() const
{
  return sprockit::printf("socket fd=%d on node %ld",
                   number_, long(my_addr_));
}

void
socket::add_pending(const socket_message::ptr& msg)
{
  bytes_available += msg->byte_length();
  pending_recv_.push_back(msg);
}

void
socket::accept(node_id addr)
{
  remote_addr_ = addr;
  if (remote_addr_ == NULL_NODEID) {
    spkt_throw_printf(sprockit::illformed_error, "socket::accept: null address");
  }
}

node_id
socket::send_addr() const
{
  return remote_addr_;
}

node_id
socket::my_addr() const
{
  return my_addr_;
}

long
socket::recv(long max_bytes)
{
  long bytes_recved = 0;
  std::list<socket_message::ptr>::iterator it = pending_recv_.begin(),
                                           end = pending_recv_.end();
  while (it != end) {
    std::list<socket_message::ptr>::iterator tmp = it++;
    socket_message::ptr msg = *tmp;
    long next_bytes = bytes_recved + msg->byte_length();
    if (next_bytes > max_bytes) { //can't go over!
      return bytes_recved;
    }
    bytes_recved += msg->byte_length();
    pending_recv_.erase(tmp);
    bytes_available -= msg->byte_length();
  }
  return bytes_recved;
}

socket::state_t
socket::state() const
{
  return state_;
}

int
socket::fd() const
{
  return fd_;
}

void
socket::rebind()
{
  //remote_addr_ = 0;
}

node_id
socket::recv_addr() const
{
  return remote_addr_;
}

void
socket::block(state_t state)
{
  state_ = state;
  os_->block(key_);
}

void
socket::unblock()
{
  state_ = UNBLOCKED;
  os_->unblock(key_);
}

const char*
socket::tostr(state_t state)
{
  switch (state) {
      enumcase(UNBLOCKED);
      enumcase(BLOCK_ON_ACCEPT);
      enumcase(BLOCK_ON_SEND);
      enumcase(BLOCK_ON_RECV);
  }
  return "";
}

socketapi::socketapi()
  : api(key::general),
    next_free_socket_(0),
    in_blocking_send_(false)
{
}

socketapi::~socketapi()
{
}

socket*
socketapi::get_socket(int fd) const
{
  std::map<int, socket*>::const_iterator it = all_sockets_.find(fd);
  if (it == all_sockets_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "socket %d does not exist yet in socketapi::recv");
  }
  return it->second;
}

socket*
socketapi::get_connected_socket(int number) const
{
  std::map<int, socket*>::const_iterator it = connected_sockets_.find(number);
  if (it == connected_sockets_.end()) {
    return 0;
  }
  return it->second;
}

socket*
socketapi::get_bound_socket(int number) const
{
  std::map<int, socket*>::const_iterator it = bound_sockets_.find(number);
  if (it == bound_sockets_.end()) {
    return 0;
  }
  return it->second;
}

long
socketapi::recv(int fd, long max_bytes, bool blocking)
{
  socket_debug("Starting %s receive of %ld bytes on socket %d",
            blocking ? "blocking" : "non-blocking", max_bytes, fd);
  socket* sock = get_socket(fd);
  if (blocking && sock->bytes_available == 0) { //gotta recv something!
    socket_debug("Blocking on socket %d until %ld bytes are available.  Only have %ld",
                 fd, max_bytes, sock->bytes_available);
    sock->block(socket::BLOCK_ON_RECV);
    socket_debug("Unblocking on socket %d now that %ld bytes are available",
                 fd, sock->bytes_available);
  }
  long bytes = std::min(sock->bytes_available, max_bytes);

  long dstid = os_->current_threadid();
  sock->rebind();
  lib_mem_->read(bytes);
  return bytes;
}

long
socketapi::send(int fd, long bytes, bool wait_on_ack)
{
  socket_debug("Starting send of %ld bytes on socket %d", bytes, fd);
  socket* sock = get_socket(fd);

  bool from_server = bound_sockets_.find(sock->number()) != bound_sockets_.end();
  socket_message::type_t going_to = from_server ?
                                    socket_message::client : socket_message::server;

  socket_message::ptr msg = new socket_message(
                              sock->number(), bytes,
                              going_to,
                              server_libname_,
                              sock->send_addr(),
                              sock->my_addr());

  msg->set_needs_ack(wait_on_ack);
  long srcid = os_->current_threadid();
  lib_mem_->write(bytes);
  os_->execute_kernel(ami::COMM_SEND, msg);
  if (wait_on_ack) {
    socket_debug("Waiting for ack on socket %d", fd);
    sock->block(socket::BLOCK_ON_SEND);
    socket_debug("Got ack on socket %d", fd);
  }
  return bytes;
}

void
socketapi::init_os(operating_system* os)
{
  library::init_os(os);
  os_->register_lib(this, lib_mem_);

  library* server_lib = os_->lib(server_libname_);
  // only do one server per app per node
  if (server_lib == 0) {
    server_ = new socket_server(server_libname_);
  } else {
    server_ = safe_cast(socket_server, server_lib);
  }

  register_lib(server_);

  if (server_lib == 0)
    server_->start();

}

void
socketapi::close(int fd)
{
  std::map<int, socket*>::iterator it = all_sockets_.find(fd);
  if (it == all_sockets_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "cannot close socket %d. no such file descriptor could be found", fd);
  }
  all_sockets_.erase(it);
  free_sockets_.push_back(fd);
}

int
socketapi::allocate_socket(socket_domain_t domain, socket_type_t typ,
                           socket_protocol_t prot)
{
  int fd;
  if (free_sockets_.empty()) {
    fd = next_free_socket_++;

  }
  else {
    fd = free_sockets_.back();
    free_sockets_.pop_back();
  }
  node_id addr = operating_system::current_node_id();
  socket* newsock = new socket(fd, addr, domain, typ, prot, os_);
  all_sockets_[fd] = newsock;
  return fd;
}

void
socketapi::incoming_message(const socket_message::ptr& msg)
{
  int number = msg->number();

  socket_debug(
     "Incoming message of type=%s size=%ld from addr=%ld on socket number=%d\n"
     "# Bound = %d, # Connected = %d\n",
     msg->sst_message::type().tostr(),
     msg->byte_length(),
     long(msg->fromaddr()),
     number,
     bound_sockets_.size(),
     connected_sockets_.size());

  if (msg->is_nic_ack()) {
    socket* sock = get_connected_socket(number);
    if (!sock) {
      spkt_throw_printf(sprockit::illformed_error,
                       "received ack for unconnected send on socket number=%d", number);
    }
    else if (sock->state() != socket::BLOCK_ON_SEND) {
      spkt_throw_printf(sprockit::illformed_error,
                       "received ack for non-blocking send on socket number=%d", number);
    }
    else {
      sock->unblock();
    }
  }
  else {
    socket* sock = get_bound_socket(number);
    if (!sock) {
      socket_debug("No bound socket found for number=%d\n", number);
      pending_recv_.push_back(msg);
      return;
    }
    else if (sock->recv_addr() == msg->fromaddr()) {
      sock->add_pending(msg);
      socket_debug("Socket number=%d now has %ld bytes available",
                   sock->number(), sock->bytes_available);
    }
    else {
      pending_recv_.push_back(msg);
    }

    socket_debug("Checking socket on fd=%d number=%d in state=%s",
                 sock->fd(), sock->number(), socket::tostr(sock->state()));


    if (sock->state() == socket::BLOCK_ON_ACCEPT) {
      if (sock->recv_addr() == msg->fromaddr()) {
        socket_debug("Unblocking accept on fd=%d number=%d on node=%ld",
                     sock->fd(), sock->number(), long(msg->fromaddr()));
        sock->accept(msg->fromaddr());
        sock->unblock();
      }
      else {
        socket_debug("Failing to unblock on fd=%d number=%d for needed addr=%ld on msg addr=%ld",
                   sock->fd(), sock->number(),
                   long(sock->recv_addr()),
                   long(msg->fromaddr()));
      }
    }
    else if (sock->state() == socket::BLOCK_ON_RECV) {
        socket_debug("Socket fd=%d number=%d has %ld bytes available and needs %ld to unblock",
                     sock->fd(), sock->number(),
                     sock->bytes_available,
                     sock->bytes_needed_to_unblock);
      sock->unblock();
    }

  }
}

node_id
socketapi::accept(int fd, node_id addr, bool blocking)
{
  socket* sock = get_socket(fd);

  socket_debug("%s accept on socket fd=%d number=%d for addr %ld",
                blocking ? "Blocking" : "Non-blocking",
                fd, sock->number(), long(addr));

  {
    std::list<socket_message::ptr>::iterator it = pending_recv_.begin(),
                                             end = pending_recv_.end();
    for ( ; it != end; ++it) {
      socket_message::ptr msg = *it;
      if (msg->number() == sock->number() && addr == msg->fromaddr()) {
        sock->accept(msg->fromaddr());
        break;
      }
    }
  }

  if (!sock->recv_addr()) {
    if (blocking) {
      sock->accept(addr);
      sock->block(socket::BLOCK_ON_ACCEPT);
    }
    else {
      return NULL_NODEID;
    }
  }

  {
    std::list<socket_message::ptr>::iterator it = pending_recv_.begin(),
                                             end = pending_recv_.end();
    for ( ; it != end; ++it) {
      //socket_message::ptr msg = *it;
    }
  }

  socket_debug("Accept on socket fd=%d number=%d found addr %ld",
               fd, sock->number(), long(sock->recv_addr()));

  return sock->recv_addr();
}

void
socketapi::unaccept(int fd)
{
  socket* sock = get_socket(fd);
  socket_debug("Unaccept socket fd=%d number=%d", fd, sock->number());
  sock->unaccept();
}

void
socketapi::connect(int fd, int number, node_id addr)
{
  socket_debug("Connected socket fd=%d number=%d", fd, number);
  socket* sock = get_socket(fd);
  sock->connect(number,addr);
  connected_sockets_[number] = sock;
  server_->open_socket(number, this);
}

void
socketapi::disconnect(int fd)
{
  socket* sock = get_socket(fd);
  socket_debug("Disconnected socket fd=%d number=%d",
    sock->fd(), sock->number());
  server_->close_socket(sock->number());
  sock->disconnect();
  connected_sockets_.erase(sock->number());
}

void
socketapi::bind(int fd, int number, node_id addr)
{
  socket_debug("Binding socket on fd %d number %d", fd, number);
  socket* sock = get_socket(fd);
  sock->bind(number, addr);
  bound_sockets_[number] = sock;
  server_->bind_socket(number, this);
}

void
socketapi::init_param1(const software_id &sid)
{
  api::init_param1(sid);
  id_ = sid;
  libname_ = "libsocket-" + sid.to_string();

  std::string mem_lib_name = sprockit::printf("%s-libmem", libname_.c_str());
  lib_mem_ = new lib_compute_memmove(mem_lib_name);

  server_libname_ = sprockit::printf("socket_server_daemon%d", int(sid.app_));
}

bool
socketapi::supported() const
{
  return os_->kernel_supported(ami::COMM_SEND);
}

socket_server::socket_server(const std::string& libname)
{
  libname_ = libname;
}

void
socket_server::incoming_message(const sst_message::ptr& msg)
{
  socket_message::ptr sockmsg = ptr_safe_cast(socket_message, msg);
  if (sockmsg->type() == socket_message::client) {
    get_client_socket(sockmsg->number())->incoming_message(sockmsg);
  }
  else {
    get_server_socket(sockmsg->number())->incoming_message(sockmsg);
  }
}

socketapi*
socket_server::get_client_socket(int number) const
{
  spkt_unordered_map<int, socketapi*>::const_iterator
  it = client_sockets_.find(number);
  if (it == client_sockets_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "socket_server::get_client_socket: invalid socket %d",
                     number);
  }
  return it->second;
}

socketapi*
socket_server::get_server_socket(int number) const
{
  spkt_unordered_map<int, socketapi*>::const_iterator
  it = server_sockets_.find(number);
  if (it == server_sockets_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "socket_server::get_server_socket: invalid socket %d",
                     number);
  }
  return it->second;
}

void
socket_server::bind_socket(int number, socketapi*proc)
{
  socketapi*& existing = server_sockets_[number];
  if (existing) {
    spkt_throw_printf(sprockit::illformed_error,
                     "socket_server::bind_socket: %d is already open",
                     number);
  }
  existing = proc;
}

void
socket_server::unbind_socket(int number)
{
  spkt_unordered_map<int,socketapi*>::iterator
  it = server_sockets_.find(number);
  if (it == server_sockets_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "socket_server::unbind_socket: no socket on fd %d",
                     number);
  }
  server_sockets_.erase(it);
}

void
socket_server::open_socket(int number, socketapi*proc)
{
  socketapi*& existing = client_sockets_[number];
  if (existing) {
    spkt_throw_printf(sprockit::illformed_error,
                     "socket_server::open_socket: %d is already open",
                     number);
  }
  existing = proc;
}

void
socket_server::close_socket(int number)
{
  spkt_unordered_map<int,socketapi*>::iterator
  it = client_sockets_.find(number);
  if (it == client_sockets_.end()) {
    spkt_throw_printf(sprockit::value_error,
                     "socket_server::close_socket: no socket on fd %d",
                     number);
  }
  client_sockets_.erase(it);
}


}
}

