import os
import re
import sys

from standardize import clean_comments

class HeaderContextError:
    
    def __init__(self, msg = "parse header error"):
        self.msg = msg

    def __repr__(self):
        return self.msg

class HeaderData:
    using_cls_arrs = {}
    include_arrs = {}
    parsed_files = {}

def is_parsed(fname):
    return fname in HeaderData.parsed_files

def parse_header(fullpath, do_gui=False):
    from pysst.path import get_include_path

    if "boost" in fullpath:
        return
    elif fullpath in HeaderData.parsed_files:
        return


    folder, name = os.path.split(fullpath)
    text = open(fullpath).read()
    text = clean_comments(text)

    my_include_arr = []
    includes = re.compile("include\s*(.*)").findall(text)
    for path in includes:
        path = path.strip().strip('"').strip("<").strip(">")
        incpath = get_include_path(path, folder)
        if incpath:
            my_include_arr.append(incpath)

    HeaderData.parsed_files[fullpath] = True
    HeaderData.include_arrs[fullpath] = my_include_arr

    #get all the includes
    for incpath in my_include_arr:
        parse_header(incpath, do_gui)

    from pysst.parse import do_parse_text
    do_parse_text(fullpath, text, do_gui)

def set_using_namespace(fullpath, using_arr):
    HeaderData.using_cls_arrs[fullpath] = using_arr

def get_header_context(path, using_cls_arr, already_included):
    if path in already_included:
        return

    already_included[path] = True

    if path in HeaderData.using_cls_arrs:
        new_using_arr = HeaderData.using_cls_arrs[path]
        for entry in new_using_arr:
            if not entry in using_cls_arr:
                using_cls_arr.append(entry)

    try:
        subincludes = HeaderData.include_arrs[path]
    except KeyError:
        raise HeaderContextError("Failed parsing %s\n" % path)
        
    for inc in subincludes:
        try:
            get_header_context(inc, using_cls_arr, already_included)
        except HeaderContextError as e:
            sys.stderr.write("In parsing %s, somehow failed in parsing %s\n" % (path, inc))
            raise e

