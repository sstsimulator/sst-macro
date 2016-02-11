import re
import sys
from util import traceback

class AppendData:
    factory_regexp = re.compile("\s([_a-zA-Z:]+)factory\s*[:][:]get(.*?)[(](.*?)[;]",re.DOTALL)

    keyword_types = [
        ("int", "integer"),
        ("long", "long"),
        ("bool", "boolean"),
        ("double", "double"),
        ("time", "timestamp"),
        ("freq", "frequency"),
        ("vector", "vector"),
        ("bandwidth", "bandwidth"),
        ("factory", "factory"),
        ("byte_length", "length"),
    ]

def do_append_keywords_prefix(prefix, body, scope, using_ns_arr, using_cls_arr):
    from pysst.keyword import Keyword
    from pysst.sstglobals import GlobalData

    #check for string parameters
    matches = re.compile("(?<!:)%s_param\s*[(][\s]*[\"](.*?)[\"]" % prefix, re.DOTALL).findall(body)
    for match in matches:
        keyword = Keyword(match, "string")
        GlobalData.keywords[scope] = keyword

    matches = re.compile("(?<!:)%s_optional_param\s*[(][\s\n]*[\"](.*?)[\"]" % prefix, re.DOTALL).findall(body)
    for match in matches:
        keyword = Keyword(match, "string")
        GlobalData.keywords[scope] = keyword

    for flag, keytype in AppendData.keyword_types:
        #check for mandatory keywords
        regexp = "%s_%s_param\s*[(][\n\s]*[\"](.*?)[\"]" % (prefix, flag)
        matches = re.compile(regexp, re.DOTALL).findall(body)
        for name in matches:
            keyword = Keyword(name, keytype)
            GlobalData.keywords[scope] = keyword
            if keytype == "factory":
                keyword.factory_type = True

        regexp = "%s_optional_%s_param\s*[(][\n\s]*[\"](.*?)[\"]\s*[,]\s*(.*?)[\)][.; \n]" % (prefix, flag)
        matches = re.compile(regexp, re.DOTALL).findall(body)
        for name, default_val in matches:
            default_val = default_val.strip().strip('"')
            keyword = Keyword(name, keytype, default_val)
            GlobalData.keywords[scope] = keyword

def do_append_factories(body, scope, using_ns_arr, using_cls_arr, resolve_factory_name):
    from pysst.scope import get_full_scope_name, GetScopeError
    from pysst.keyword import Keyword
    from pysst.sstglobals import GlobalData
    # now check for factories
    scope_arr = scope.split("::") 
    matches = AppendData.factory_regexp.findall(body)
    for factory_type, read_type, params in matches:
        factory_type = factory_type.strip().strip("_")
        full_factory_name = factory_type
        if resolve_factory_name:
            try:
                full_factory_name = get_full_scope_name(factory_type, scope_arr, using_cls_arr, using_ns_arr)
            except GetScopeError as e:
                sys.exit("Failed to resolve scope of factory %s" % factory_type)
                
        if read_type.endswith("_param"): 
            param_name = param_obj = ""
            optional_val = None
            extra = False
            if read_type == "_optional_param":
                param_name, optional_val, param_obj = params.split(",")[:3]
                optional_val = optional_val.replace('"', "").strip()
            elif read_type == "_extra_param":
                param_name, param_obj = params.split(",")[:2]
                extra = True
            elif read_type == "_param":
                param_name, param_obj = params.split(",")[:2]

            param_name = param_name.replace('"', "").strip()
            keyword = Keyword(param_name, full_factory_name, optional_val, extra)
            reg_cls = GlobalData.keywords.get_registered_class(full_factory_name)
            keyword.allowed = reg_cls.allowed
            keyword.factory_type = True
            GlobalData.keywords[scope] = keyword
        else:
            pass

def do_append_keywords(body, scope, using_ns_arr, using_cls_arr, resolve_factory_name = True, include_reread=False):
    do_append_keywords_prefix("get", body, scope, using_ns_arr, using_cls_arr)
    do_append_factories(body, scope, using_ns_arr, using_cls_arr, resolve_factory_name)
    if include_reread:
        do_append_keywords_prefix("reread", body, scope, using_ns_arr, using_cls_arr)

def append_keywords(fname,scope,include_reread=False):
    body = open(fname).read()
    do_append_keywords(body, scope, [], [], False, include_reread)

"""
    Add documentation for an SpktRegisterX line in the code
"""
def add_registered_class(text, scoped_ns_arr, using_ns_arr, using_cls_arr):
    from pysst.scope import get_full_scope_name, GetScopeError
    from pysst.keyword import Keyword
    from pysst.sstglobals import GlobalData
    regexp = "SpktRegister(.*?)\s*[(](.*?)[)][;]"
    match = re.compile(regexp,re.DOTALL).search(text)
    if not match:
        return
    clsname, specs = match.groups()
    # The regexp accidentally picks up function declarations rather
    # than actual instantiations. This ignores the declaration function
    if '?"' in clsname or "appstr" in specs or "servstr" in specs:
        return

    # We have in this order: 
    #   allowed values "|" separated
    #   the type name of the class
    #   optionally, a doc string for the keyword 
    match = None
    keyvals = typenames = docstring = ""
    match = re.compile('["](.*?)["]\s*[,](.*?)["](.*?)["]').search(specs)
    if not match:
        match = re.compile('["](.*?)["]\s*[,](.*)').search(specs)
        if not match:
            sys.exit("Could not parse SpktRegister %s" % specs)
        keyvals, typenames = match.groups()
        typenames = typenames.strip().strip(")").strip()
    else:
        keyvals, typenames, docstring = match.groups()

    typenames = typenames.strip(",").strip().strip(",").strip()

    keyvals = keyvals.replace('"','')
    keyvals = keyvals.strip()

    #docstring = docstring.replace('"','')
    docstring = docstring.strip()
    docstring = docstring.replace("\n"," ")
    length = len(docstring)
    newlength = length + 1
    while length != newlength:
        length = len(docstring)
        docstring = docstring.replace("  "," ")
        newlength = len(docstring)

    parent = child = None
    if clsname == "App":
        parent = "sstmac::sw::app"
        child = typenames
    else:
        parent, child = typenames.split(",")
        parent = parent.replace('"','')
        parent = parent.strip()
        try:
            parent = get_full_scope_name(parent, scoped_ns_arr, using_cls_arr, using_ns_arr)
        except GetScopeError as e:
            child = child.replace('"','')
            child = child.strip()
            sys.exit("Could not resolve parent name %s for subclass %s" % (parent, child))

    child = child.replace('"','')
    child = child.strip()
    child = get_full_scope_name(child, scoped_ns_arr, using_ns_arr, using_cls_arr)

    # parent = X in SpktRegisterX(...)
    # keyvals = "|" separated valid keywords
    # child = actual name of the class in C++ code
    # docstring = optional docstring
    GlobalData.keywords.add_registered_class(parent, keyvals, child, docstring)
