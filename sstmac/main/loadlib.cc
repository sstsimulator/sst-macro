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

std::string loadExternPathStr(){
  const char* libpath_str = getenv("SST_LIB_PATH");
  if (libpath_str){
    return libpath_str;
  } else {
    return "";
  }
}

void* loadExternLibrary(const std::string& libname, const std::string& searchPath)
{
  struct stat sbuf;
  int ret = stat(libname.c_str(), &sbuf);
  std::string fullpath;
  if (ret != 0){
    std::vector<std::string> paths = split_path(searchPath);
    //always include current directory
    paths.push_back(".");

    for (auto&& path : paths) {
      fullpath = path + "/" + libname;
      ret = stat(fullpath.c_str(), &sbuf);
      if (ret == 0) break;
    }
  } else {
    fullpath = libname;
  }

  if (ret != 0){
    //didn't find it
    spkt_abort_printf("%s not found in current directory or in path=%s",
                      libname.c_str(), searchPath.c_str());
  }

  //std::cerr << "Loading external library " << fullpath << std::endl;

  // This is a little weird, but always try the last path - if we
  // didn't succeed in the stat, we'll get a file not found error
  // from dlopen, which is a useful error message for the user.
  void* handle = dlopen(fullpath.c_str(), RTLD_NOW|RTLD_LOCAL);
  if (NULL == handle) {
    spkt_abort_printf("Opening library %s failed\n:%s", libname.c_str(), dlerror());
  }
  return handle;
}

void* loadExternLibrary(const std::string& libname)
{
  return loadExternLibrary(libname, loadExternPathStr());
}

void unloadExternLibrary(void* handle)
{
  dlclose(handle);
}

}
