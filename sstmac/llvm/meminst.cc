#include <cstdint>
#include <iostream>
#include <mutex>
#include <numeric>
#include <vector>

namespace {

int TracingDepth = 0;
uint64_t MaxAddrs = 1000000;

struct AddrData {
  void *addr;
  int64_t size;
  int64_t thread_id;
};

std::mutex Mutex;
std::vector<AddrData> LoadAddrs;
std::vector<AddrData> StoreAddrs;

// For now do not call outside of a locked region
void sstmac_flush_addresses() {}
} // namespace

extern "C" {

void sstmac_address_load(void *addr, int64_t size, int32_t thread_id) {
  std::lock_guard<std::mutex> lock(Mutex);
  if (TracingDepth > 0) {
    LoadAddrs.push_back({addr, size, thread_id});
  }
  if (LoadAddrs.size() > MaxAddrs) {
    sstmac_flush_addresses();
  }
}

void sstmac_address_store(void *addr, int64_t size, int32_t thread_id) {
  std::lock_guard<std::mutex> lock(Mutex);
  if (TracingDepth > 0) {
    StoreAddrs.push_back({addr, size, thread_id});
  }
  if (StoreAddrs.size() > MaxAddrs) {
    sstmac_flush_addresses();
  }
}

void sstmac_start_trace() {
  std::lock_guard<std::mutex> lock(Mutex);
  ++TracingDepth;
}

void sstmac_end_trace() {
  std::lock_guard<std::mutex> lock(Mutex);
  --TracingDepth;
  if (TracingDepth == 0) {
    sstmac_flush_addresses();
  }
}

void sstmac_print_address_info() {
  auto stored =
      std::accumulate(StoreAddrs.begin(), StoreAddrs.end(), 0,
                      [](int &val, AddrData const &a) { return val + a.size; });

  auto loaded =
      std::accumulate(LoadAddrs.begin(), LoadAddrs.end(), 0,
                      [](int &val, AddrData const &a) { return val + a.size; });

  std::cout << "Number of stored bytes: " << stored << "\n";
  std::cout << "Number of loaded bytes: " << loaded << "\n\n";
#if 0
  std::stable_sort(LoadAddrs.begin(), LoadAddrs.end(),
                   [](AddrData const &a, AddrData const &b) {
                     return a.thread_id < b.thread_id;
                   });

  auto max_thread = LoadAddrs.back().thread_id;
  std::vector<std::pair<AddrData *, AddrData *>> thread_info(max_thread + 1);

  auto find_partitions = [&](int64_t thread_id) {
    auto Func = [=](AddrData const &a) { return a.thread_id == thread_id; };
    auto Func2 = [=](AddrData const &a) { return a.thread_id == thread_id + 1; };
    auto FirstPtr = &*std::find_if(LoadAddrs.begin(), LoadAddrs.end(), Func);
    auto LastPtr = &*std::find_if(LoadAddrs.begin(), LoadAddrs.end(), Func2);

    return std::make_pair(FirstPtr, LastPtr);
  };

  for (auto i = 0; i < max_thread + 1; ++i) {
    thread_info[i] = find_partitions(i);
  }

  auto test_condition = [&](decltype(thread_info) const &ti) {
    for (auto const &pair : ti) {
      if (pair.first != pair.second) {
        return true;
      }
    }
    return false;
  };

  std::cout << "  | ";
  for (auto i = 0ul; i < thread_info.size(); ++i) {
    std::cout << "Thread           " << i << "   ";
  }
  std::cout << "\n----";
  for (auto i = 0ul; i < thread_info.size(); ++i) {
    std::cout << "---------------------";
  }
  std::cout << "\n";
  auto line = 0;
  while (test_condition(thread_info)) {
    if (line >= 10) {
      std::cout << line++ << "| ";
    } else {
      std::cout << " " << line++ << "| ";
    }
    for (auto &pair : thread_info) {
      if (pair.first != pair.second) {
        std::cout << pair.first->addr << " (" << pair.first->size << ")   ";
        pair.first += 1;
      } else {
        std::cout << "Null                 ";
      }
    }
    std::cout << "\n";
  }
#endif
}
}
