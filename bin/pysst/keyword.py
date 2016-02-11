import re

class KeywordData:
    docstring_regexp = re.compile("\s*ENDL\s*")

    keyword_owners = {
        "network_train_payload_fraction" : [
            "sstmac::hw::traininterconnect"
        ],
    }
    ignored_keywords = {
        "sstmac::hw::topology" : [
            "router"
        ],
        "sstmac::hw::simplenode" : [
            "node_preemption",
            "node_frequency_stdev",
            "disk_type",
            "node_nppfs_diskfile",
            "node_nppfs_server",
        ]
    }

    ignored_class_values = {
        "sstmac::hw::interconnect" : [
            "dist_train",
            "multi"
        ],
        "sstmac::sw::launcher" : [
            "real",
        ],
        "sstmac::hw::networkinterface" : [
            "multinic",
        ],
        "sstmac::hw::node" : [
            "detailed",
            "degraded",
            "eiger",
        ],
    }

    ignored_values = {
        "launcher" : [
            "stearley"
        ],
    }

def make_valid_link(word):
    return word.replace(" ","_").replace("::","_")

def make_valid_syntax(word):
    return word.replace(" ","_")

class AllowedKeyword:
    
    def __init__(self, name, docstring, typename = ""):
        self.name = name
        self.docstring = docstring
        self.typename = typename

    def __str__(self):
        return "%s->%s:\n%s" % (self.name, self.typename, self.docstring)


class Constant:
    def __init__(self, name, keyword_type, value):
        self.name = name
        self.keytype = keyword_type
        self.value = value

    def __str__(self):
        _str = "<b>C%30s</b>: <i>%15s constant</i> %s" % (self.name, self.keytype, self.value)
        return _str


class Keyword:
    
    def __init__(self, name, keyword_type, default = "", extra = False, yesno = False):
        self.name = name
        self.keytype = keyword_type
        self.extra = extra
        self.default = default
        self.gui_default = default #for now
        self.allowed = []
        self.yesno = yesno
        self.docstring = ""
        self.factory_type = False
        self.fake_type = False
        self.alias = ""
        self.tab = ""

    def add_allowed(self,allowed_val):
        if self.name in KeywordData.ignored_values:
            if allowed_val in KeywordData.ignored_values[self.name]:
                return
        self.allowed.append(allowed_val)

    def __lt__(self, other):
        return self.name < other.name

    def __str__(self):
        keytype = self.keytype
        if self.factory_type:
            keytype = '\\ref registered_%s' % make_valid_link(self.keytype)
        _str = "<b>%30s</b>: <i>%15s</i>" % (self.name, keytype)
        if self.default:
            _str += " [default = %s]" % self.default
        else:
            _str += " [no default]"

        if self.extra:
            _str += " EXTRA";

        if self.docstring:
            _str += "  %s" % KeywordData.docstring_regexp.sub(" ", self.docstring)

        if self.allowed:
            str_arr = ["\n\t<ul>"]
            for allowed in self.allowed:
                str_arr.append("\t\t<li> <tt>%s:</tt>" % allowed.name)
                #str_arr.append("\t\t<pre>")
                str_arr.append("\t\t%s" % KeywordData.docstring_regexp.sub(" ", allowed.docstring))
                #str_arr.append("\t\t</pre>")
            str_arr.append("\t</ul>")
            _str += "\n".join(str_arr)

        return _str


class RegisteredClass:
    
    def __init__(self, register_name):
        self.allowed = []
        name_arr = register_name.split("::")
        self.namespace = "::".join(name_arr[:-1])
        self.register_name = name_arr[-1]

    def __iter__(self):
        return iter(self.allowed)

    def add(self, vals, typename, docstring):
        vals = map(lambda x: x.strip(), vals.split("|"))
        valname = vals[0]

        fullname = self.namespace + "::" + self.register_name
        if fullname in KeywordData.ignored_class_values:
            if valname in KeywordData.ignored_class_values[fullname]:
                return

        for entry in self.allowed: #avoid repeats
            if entry.name == valname:
                return

        val = AllowedKeyword(valname, docstring, typename)
        self.allowed.append(val)

    def __getitem__(self, key):
        return self.allowedvals[key]

class KeywordScope:
    
    def __init__(self):
        self.keywords = []
        self.parents = []

    def __len__(self):
        return len(self.keywords)

    """
        @param parent Parent KeywordScope
    """
    def add_parent(self,parent):
        self.parents.append(parent)

    def get_parents(self):
        return self.parents

    def __getitem__(self,name):
        for entry in self.keywords:
            if entry.name == name:
                return entry
        raise KeyError(name)

    def sort(self):
        self.keywords.sort()

    def append(self, item):
        self.keywords.append(item)

    def __iter__(self):
        return iter(self.keywords)


class KeywordSet:

    
    def __init__(self):
        self.keywords = {}
        self.registered_classes = {}
        self.length = 0


    def keynames(self):
        return self.keywords.keys()

    def __setitem__(self, scope, keyobj):
        from pysst.sstglobals import GlobalData
        if GlobalData.allow_ignore:
            if keyobj.name in KeywordData.keyword_owners:
                owners = KeywordData.keyword_owners[keyobj.name]
                if not scope in owners:
                    return
            elif scope in KeywordData.ignored_keywords:
                ignored = KeywordData.ignored_keywords[scope]
                if keyobj.name in ignored:
                    return
                
                
        

        if not self.keywords.has_key(scope):
            self.keywords[scope] = KeywordScope()
        self.keywords[scope].append(keyobj)
        self.length += 1

    def get_parents(self, name):
        return self.keywords[name].get_parents()

    def register_scope(self, name):
        self.keywords[name] = KeywordScope()
        from pysst.scope import register_scope
        register_scope(name)

    def __iter__(self):
        return iter(self.keywords)

    def __getitem__(self, scope):
        return self.keywords[scope]

    def __len__(self):
        return self.length

    def add_parent(self, parent, child):
        from pysst.scope import add_synonym
        if not "std" in parent:
            add_synonym(child, parent)

        if not self.keywords.has_key(child):
            self.keywords[child] = KeywordScope()
        if not self.keywords.has_key(parent):
            self.keywords[parent] = KeywordScope()

        self.keywords[child].add_parent(parent)

    def get_parent_set(self, parent, child):
        if self.keywords.has_key(parent):
            return self.keywords[parent]

        keynames = keywords.keynames()
        keynames.sort()
        sys.stdout("\n".join(keynames))
        sys.stdout("\n")
        sys.exit("Unable to resolve parent %s to child %s\n" % (parent,child))

    """
        Add documentation for a class that is instantiated via the SpktRegisterX statements
        @param cls X in SpktRegisterX(...)
        @param values "|" separated valid keywords
        @param typename actual name of the class in C++ code
        @param docstring optional docstring
    """
    def add_registered_class(self, cls, value, typename, docstring):
        if not self.registered_classes.has_key(cls):
            self.registered_classes[cls] = RegisteredClass(cls)
        self.registered_classes[cls].add(value, typename, docstring)

    def get_registered_classes(self):
        return self.registered_classes.values()

    def get_registered_class(self, cls):
        if not self.registered_classes.has_key(cls):
            self.registered_classes[cls] = RegisteredClass(cls)
        return self.registered_classes[cls]

    def __str__(self):
        str_arr = ['/** \page keywords Keyword Listing for SST/macro']

        clses = self.registered_classes.keys()
        clses.sort()
        str_arr.append("\nRegistered class types")
        str_arr.append("<ul>")
        for cls in clses:
            str_arr.append('  <li> \\ref registered_%s' % make_valid_link(cls))
        str_arr.append("</ul>")

        str_arr.append("\nKeyword scopes")
        scopes = self.keywords.keys()
        scopes.sort()
        str_arr.append("<ul>")
        for scope in scopes:
            keyscope = self.keywords[scope]
            if not keyscope:
                continue

            scope_name = scope
            if scope_name == "":
                scope_name = "global"
            str_arr.append('  <li> \\ref scope_%s' % make_valid_link(scope_name))
        str_arr.append("   <li> \\ref complete_index")
        str_arr.append("</ul>")

        
        for cls in clses:
            str_arr.append('\section registered_%s Allowed values for %s' % (make_valid_link(cls), cls))
            entry = self.registered_classes[cls]
            str_arr.append("<ul>")
            for allowed in entry:
                string_name = allowed.name
                docstring = KeywordData.docstring_regexp.sub(" ",allowed.docstring)
                str_arr.append("  <li> <b>\"%s\"</b>: %s" % (string_name, docstring))
            str_arr.append("</ul>\n")
            
        index_map = {}
        for scope in scopes:
            scope_name = scope
            if scope_name == "":
                scope_name = "global"

            keyscope = self.keywords[scope]
            if not keyscope:
                continue

            def get_valid_scopes(parent_scope):
                parent_keyscope = self.keywords[parent_scope]
                if parent_keyscope:
                    return [parent_scope]

                next_scopes = self.keywords[parent_scope].get_parents()
                my_scopes = []
                for next_scope in next_scopes:
                    my_scopes.extend(get_valid_scopes(next_scope))
                return my_scopes

            scope_parents = self.keywords[scope_name].get_parents()
            inherit_arr = []
            if scope_parents:
                for parent_scope in scope_parents:
                    #find the first real keyscope
                    valid_parent_scopes = get_valid_scopes(parent_scope)
                    for valid_scope in valid_parent_scopes:
                        inherit_arr.append('\\ref scope_%s' % make_valid_link(valid_scope))
            inherit_str = ""
            if inherit_arr:
                inherit_str = "Inherits from: %s" % (",".join(inherit_arr))

            str_arr.append('\section scope_%s %s' % (make_valid_link(scope_name), scope_name))
            if inherit_str:
                str_arr.append(inherit_str)
            keyscope = self.keywords[scope]
            keyscope.sort()
            str_arr.append("\n<ul>")
            for keyword in keyscope:
                str_arr.append("  <li> \\anchor subsect_%s %s" % (make_valid_syntax(keyword.name), keyword))
                if not index_map.has_key(keyword.name):
                    index_map[keyword.name] = [scope_name]
                else:
                    index_map[keyword.name].append(scope_name)
            str_arr.append("</ul>\n")

        index_arr = []
        index_keys = index_map.keys()
        index_keys.sort()
        for key in index_keys:
            scopes = index_map[key]
            scope_text = ", ".join(scopes)
            scope_word = None
            if len(scopes) > 1: scope_word = "scopes"
            else: scope_word = "scope"
            index_arr.append('  <li> \\ref subsect_%s "%s: Read by %s %s"'% (make_valid_syntax(key), key, scope_word, scope_text))

        str_arr.append("\section complete_index Complete index")
        str_arr.append("<ul>")
        str_arr.extend(index_arr)
        str_arr.append("</ul>")

        str_arr.append("*/")

        return "\n".join(str_arr)
