#include <dlfcn.h>
#include <vector>
#include <string>
#include <cstring>
#include <sys/stat.h>
#include <sprockit/errors.h>

namespace sstmac {

static std::vector<std::string> split_path(const std::string& searchPath)
{
  std::vector<std::string> paths;
  char * pathCopy = new char [searchPath.length() + 1];
  std::strcpy(pathCopy, searchPath.c_str());
  char *brkb = NULL;
  char *p = NULL;
  for ( p = strtok_r(pathCopy, ":", &brkb); p ; p = strtok_r(NULL, ":", &brkb) ) {
    paths.push_back(p);
  }

  delete [] pathCopy;
  return paths;
}

std::string load_extern_path_str(){
  const char* libpath_str = getenv("SST_LIB_PATH");
  if (libpath_str){
    return libpath_str;
  } else {
    return "";
  }
}

void* load_extern_library(const std::string& libname, const std::string& searchPath)
{
  std::vector<std::string> paths = split_path(searchPath);
  //always include current directory
  paths.push_back(".");
  std::string fullpath;
  void *handle;

  int ret = 1;
  for (auto&& path : paths) {
    struct stat sbuf;

    fullpath = path + "/" + libname;
    ret = stat(fullpath.c_str(), &sbuf);
    if (ret == 0) break;
  }

  if (ret != 0){
    //didn't find it
    spkt_abort_printf("%s not found in current directory or in $SST_LIB_PATH", libname.c_str());
  }

  std::cerr << "Loading external library " << fullpath << std::endl;

  // This is a little weird, but always try the last path - if we
  // didn't succeed in the stat, we'll get a file not found error
  // from dlopen, which is a useful error message for the user.
  handle = dlopen(fullpath.c_str(), RTLD_NOW|RTLD_LOCAL);
  if (NULL == handle) {
    spkt_abort_printf("Opening library %s failed\n:%s", libname.c_str(), dlerror());
  }
  return handle;
}

void* load_extern_library(const std::string& libname)
{
  return load_extern_library(libname, load_extern_path_str());
}

void unload_extern_library(void* handle)
{
  dlclose(handle);
}

}
