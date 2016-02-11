import sys

class ScopeData:
    synonyms = {}

class GetScopeError:
    pass

skip_subclasses = [
    "tbb",
    "connectable_factory"
]

def skip_subclass_of(parent):
    for cls in skip_subclasses:
        if cls in parent:
            return True
    return False

def try_get_full_scope_name(cls, scope_arr):
    from sstglobals import GlobalData

    cls = cls.strip(":")
    for i in range(len(scope_arr)+1):
        cls_arr = scope_arr[:i]
        cls_arr.append(cls)
        scoped_name = "::".join(cls_arr)
        if scoped_name in GlobalData.keywords:
            return scoped_name

def try_get_synonym_name(cls, scope_arr):
    from pysst.sstglobals import GlobalData
    for i in range(len(scope_arr)+1):
        cls_arr = scope_arr[:i]
        subscope = "::".join(cls_arr)
        if not subscope in ScopeData.synonyms:
            continue

        for syn in ScopeData.synonyms[subscope]:
            scoped_name = syn + "::" + cls
            if scoped_name in GlobalData.keywords:
                return scoped_name
    return None

def add_synonym(child, parent):
    ScopeData.synonyms[child].extend(ScopeData.synonyms[parent])
    ScopeData.synonyms[child].append(parent)

def register_scope(name):
    ScopeData.synonyms[name] = []

def get_full_scope_name(cls, scope_arr, using_cls_arr, using_ns_arr):
    if "std" in cls:
        return cls

    from pysst.sstglobals import GlobalData

    scoped_name = try_get_full_scope_name(cls, scope_arr)
    if scoped_name: return scoped_name
    
    for ns in using_ns_arr:
        ns_scope_arr = ns.split("::")
        scoped_name = try_get_full_scope_name(cls, ns_scope_arr)
        if scoped_name: return scoped_name

    for full_cls in using_cls_arr:
        if cls in full_cls and full_cls in GlobalData.keywords:
            return full_cls

    scoped_name = try_get_synonym_name(cls, scope_arr)
    if scoped_name: return scoped_name

    from pysst.sstglobals import GlobalData
    #sys.stderr.write("%s\n" % "\n".join(GlobalData.keywords.keynames()))
    sys.stderr.write("Could not find scoped name for %s with namespaces %s and using %s %s\n" % (cls, scope_arr, using_cls_arr, using_ns_arr))
    raise GetScopeError
