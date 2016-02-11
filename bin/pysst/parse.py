import re
import sys
import os

from util import traceback

class FrameArray:
    
    def __init__(self):
        self.lines = []

    def append(self,line):
        self.lines.append(line)

    def __iter__(self):
        return iter(self.lines)

def do_parse_text(fullpath, text, do_gui=False):
    from pysst.path import skip_file, get_include_path
    from pysst.header import get_header_context, HeaderContextError
    from pysst.standardize import clean_file_text
    from pysst.sstglobals import GlobalData
    from pysst.scope import get_full_scope_name, GetScopeError, skip_subclass_of
    from pysst.append import do_append_keywords, append_keywords, add_registered_class
    from pysst.keyword import AllowedKeyword, Keyword

    fxn_re = re.compile("([a-zA-Z\d_::]+)[(]")

    if skip_file(fullpath):
        return

    print "Parsing", fullpath

    using_cls_arr = []
    already_included = {}
    includes = re.compile("include\s*(.*)").findall(text)
    folder, name = os.path.split(fullpath)
    for path in includes:
        path = path.strip()
        incpath = ""
        if '"' in path:
            path = path.strip('"')
            incpath = get_include_path(path, folder, local_first=True)
        else:
            path = path.strip("<").strip(">")
            incpath = get_include_path(path, folder, local_first=False)
        if incpath:
            try:
                get_header_context(incpath, using_cls_arr, already_included)
            except HeaderContextError as e:
                print traceback(e)
                sys.exit("In parsing %s, somehow failed in parsing %s" % (fullpath, incpath))

    
    text = clean_file_text(text)

    nopen = text.count("{")
    nclose = text.count("}")
    if nopen != nclose:
        sys.stderr.write("Skipping file %s with unbalanced brackets\n" % fullpath)
        return

    lines = text.split()



    frame_arr = FrameArray()
    sst_mode = False
    sstvariable_arr = []
    sstvariable_depth = 0
    sstkeyword_arr = [] #for manual sst keywords
    sstkeyword_scopes_arr = []
    sstkeyword_depth = 0
    sstobject_arr = [] #for manual sst keywords
    sstobject_scopes_arr = []
    sstobject_depth = 0
    scoped_ns_arr = []
    my_using_cls_arr = []
    using_ns_arr = []

    scope_cls_arr = []
    fxn_cls_arr = []

    idx = 0
    def debug_print(pos):
        return
        print_str = "%d %s %d %d %d %d %d %d %d %s" % (idx, line, namespace_depth, class_depth, fxn_depth, struct_depth, declare_depth, template_depth, conditional_increment, pos)
        sys.stdout("%s\n" % print_str)

    Namespace = 0
    namespace_depth = 0
    Class = 1
    class_depth = 0
    Struct = 2
    struct_depth = 0
    Enum = 3
    enum_depth = 0
    Union = 4
    union_depth = 0
    Extern = 5
    extern_depth = 0
    Fxn = 6
    fxn_depth = 0
    Declare = 7
    declare_depth = 0
    AnonClass = 8

    template_depth = 0 
    template_mode = False

    parentheses_depth = 0
        
    nextincrement = []
    nextdecrement = []
    conditional_increment = False

    typedef_line = False

    template_inheritance_depth = 0
    parent_class_name = ""
    inheritance_line = False
    constructor_line = False

    paren_depth = 0
    current_fxn = ""

    def tostr(val):
        if val == Namespace:
            return "Namespace"
        elif val == Class:
            return "Class"
        elif val == Fxn:
            return "Fxn"
        elif val == Declare:
            return "Declare"

    do_print = False #"programming" in fullpath and "main.cc" in fullpath
    while idx < len(lines):
        line = lines[idx]
        if do_print: 
            print line
        if False: #"programming" in fullpath and "main.cc" in fullpath:
            print "%s ns=%d cl=%d st=%d fxn=%d dec=%d tmpl=%d" % (line, namespace_depth, class_depth, struct_depth, fxn_depth, declare_depth, template_depth), map(tostr, nextincrement), map(tostr, nextdecrement), scope_cls_arr
        debug_print("top level")
        if fxn_depth == 0 and re.match(r'template\b', line):
            template_mode = True

            template_depth += line.count("<")
            template_depth -= line.count(">")
        elif sstvariable_depth == 1:
            if line == "}":
                sstvariable_depth = 0
            sstvariable_arr.append(line)
        elif sstobject_depth == 1:
            if line == "}":
                sstobject_depth = 0
            sstobject_arr.append(line)
        elif sstkeyword_depth == 1:
            if line == "}":
                sstkeyword_depth = 0
                #also append any keyword context after it
                #to tell us what the keyword name is
                context_arr = []
                context_idx = idx
                context_line = line
                initial_idx = context_idx
                try:
                    while not "_param" in context_line:
                        context_arr.append(context_line)
                        context_idx += 1
                        context_line = lines[context_idx]
                    while not ";" in context_line:
                        context_arr.append(context_line)
                        context_idx += 1
                        context_line = lines[context_idx]
                except IndexError:
                    sys.stderr.write("\n".join(sstkeyword_arr))
                    sys.stderr.write("\n")
                    sys.exit("Somehow ran off the end trying to parse keyword description and the lines above. Are you sure there's a keyword there in file %s?" % fullpath)

                context = "\n".join(context_arr)
                sstkeyword_arr.append(context)
            sstkeyword_arr.append(line)
        elif template_mode:
            template_depth += line.count("<")
            template_depth -= line.count(">")
            template_mode = template_depth != 0
        elif parentheses_depth:
            parentheses_depth += line.count("(")
            parentheses_depth -= line.count(")")
        elif "SpktRegister" in line:
            reg_cls_arr = []
            while not ";" in line:
                reg_cls_arr.append(line)
                idx += 1
                line = lines[idx]
            reg_cls_arr.append(");")
            reg_cls_specs = " ".join(reg_cls_arr)
            add_registered_class(reg_cls_specs, scoped_ns_arr, using_ns_arr, using_cls_arr)
        elif line == "using":
            idx += 1 #skip namespace
            line = lines[idx]
            if line == "namespace":
                idx += 1 #skip ahead
                line = lines[idx]
                scopes = line.strip().strip(";")
                using_ns_arr.append(scopes)
            else:
                cls = line.strip().strip(";");
                my_using_cls_arr.append(cls)
                using_cls_arr.append(cls)
            if "{" in line or "}" in line:
                sys.exit("using namespace: " + line)
        elif line == "namespace":
            nextincrement.append(Namespace)
            idx += 1
            line = lines[idx]
            debug_print("found namespace")
            if do_print: print "NS", line
            scoped_ns_arr.append(line)
            if do_print: print scoped_ns_arr
            if line == "{":
                scoped_ns_arr.append("anon")
                nextincrement.pop()
                namespace_depth += 1
                nextdecrement.append(Namespace)
            elif "{" in line or "}" in line:
                sys.exit("namespace: " + line)
        elif line == "={":
            declare_depth += 1
            nextdecrement.append(Declare)
        elif line == "{":
            inheritance_line = False
            conditional_increment = False
            constructor_line = False
            if nextincrement:
                incrementer = nextincrement.pop()
                if incrementer == Namespace:
                    namespace_depth += 1
                elif incrementer == Class or incrementer == AnonClass:
                    class_depth += 1
                elif incrementer == Fxn:
                    fxn_depth += 1
                elif incrementer == Declare:
                    declare_depth += 1
                elif incrementer == Union:
                    union_depth += 1
                elif incrementer == Enum:
                    enum_depth += 1
                elif incrementer == Struct:
                    struct_depth += 1
                elif incrementer == Extern:
                    extern_depth += 1
                nextdecrement.append(incrementer)
            else: #nothing pending, a fxn!
                fxn_depth += 1
                nextdecrement.append(Fxn)
        elif inheritance_line:
            if "public" in line:
                pass
            elif "virtual" in line:
                pass
            else:
                parent_class_name = line
                if "<" == line:
                    template_inheritance_depth += 1
                    idx += 1
                    continue
                elif ">" == line:
                    template_inheritance_depth -= 1
                    idx += 1
                    continue

                if template_inheritance_depth:
                    idx += 1
                    continue
                
                scope_arr = scoped_ns_arr[:]

                for entry in scope_cls_arr:
                    scope_arr.extend(entry.split("::"))

                full_childname = "::".join(scope_arr)
                for parent in line.strip().split(","):
                    parent = parent.strip()
                    if parent and not skip_subclass_of(parent):
                        try:
                            parent_plus_scope = get_full_scope_name(parent, scope_arr, using_cls_arr, using_ns_arr)
                        except GetScopeError:
                            sys.exit("Failed to find parent %s for child class %s in parsing %s\n" % (parent, full_childname, fullpath))
                        if not parent_plus_scope:
                            sys.exit("no parent scope")
                        GlobalData.keywords.add_parent(parent_plus_scope, full_childname)
        elif line == "}" or line == "};":
            decrementer = nextdecrement.pop()
            if decrementer == Namespace:
                namespace_depth -= 1
                if do_print: print "NS Drop"
                scoped_ns_arr.pop()
            elif decrementer == AnonClass:
                class_depth -= 1
            elif decrementer == Class:
                class_depth -= 1
                scope_cls_arr.pop()
            elif decrementer == Fxn:
                fxn_depth -= 1
                if fxn_depth == 0: #close out the scope
                    class_arr = scope_cls_arr[:] ; class_arr.extend(fxn_cls_arr)
                    classname = "::".join(class_arr)
                    scope = "global" 
                    if classname:
                        try:
                            scope = get_full_scope_name(classname, scoped_ns_arr, using_cls_arr, using_ns_arr)
                        except GetScopeError:
                            sys.exit("Failure resolving namespaces in parsing function %s of class %s" % (current_fxn, classname))
                    elif scoped_ns_arr:
                        scope = "::".join(scoped_ns_arr)
                    frame_arr.append(" ") #padding to make parsing work
                    body = " ".join(frame_arr)
                    if do_print: print "Scope=",scope
                    do_append_keywords(body, scope, using_ns_arr, using_cls_arr)
                    frame_arr = FrameArray()
                    fxn_cls_arr = []
            elif decrementer == Declare:
                declare_depth -= 1
            elif decrementer == Union:
                union_depth -= 1
            elif decrementer == Enum:
                enum_depth -= 1
            elif decrementer == Struct:
                struct_depth -= 1
            elif decrementer == Extern:
                extern_depth -= 1
        elif "{" in line:
            sys.exit(line)
        elif "}" in line:
            sys.exit(line)
        elif line == "typedef":
            #peak ahead a bit
            line_p_1 = lines[idx+1]
            if line_p_1 == "struct":
                line_p_3 = lines[idx+3]
                if ";" in line_p_3:
                    #straigh up typedef - skip it all
                    idx += 3
        elif line == "class" or line == "struct":
            next_line = lines[idx+1]
            if "{" in next_line:
                conditional_increment = True
                nextincrement.append(AnonClass)
            else:
                idx += 1
                prev_line = line
                line = lines[idx]
                if ";" in line: #forward declaration
                    conditional_increment = False
                elif "{" in line or "}" in line:
                    sys.exit("class: " + line)
                else:
                    conditional_increment = True
                    nextincrement.append(Class)
                    clsname = line.strip(":")
                    scope_cls_arr.append(clsname)
                    all_scopes = scoped_ns_arr[:]
                    all_scopes.extend(scope_cls_arr)
                    scoped_name = "::".join(all_scopes)
                    GlobalData.keywords.register_scope(scoped_name)
                    print "\t\t", scoped_name
        elif line == "enum" and paren_depth == 0:
            nextincrement.append(Enum)
            conditional_increment = True
        elif line == "extern":
            next_line = lines[idx+1]
            if next_line == '"C"':
                idx += 1
            else:
                nextincrement.append(Extern)
                conditional_increment = True
        elif ";" in line: #throw away the increment - just a forward declare
            if conditional_increment:
                incrementer = nextincrement.pop()
                if incrementer == Class:
                    scope_cls_arr.pop() #throw out forward declared class
                conditional_increment = False
            elif fxn_depth != 0:
                frame_arr.append(";")
        elif nextincrement and nextincrement[-1] == Class and fxn_depth == 0:
            if "public" in line:
                pass
            elif "virtual" in line:
                pass
            elif line == ":":
                inheritance_line = True
            else:
                pass #some craziness
        elif fxn_depth == 0 and line.count(":") == 1:
            constructor_line = True
        elif not constructor_line and not paren_depth and fxn_depth == 0 and "::" in line and fxn_re.search(line):
            fxn_spec = fxn_re.search(line).groups()[0]
            fxn_spec = fxn_spec.split("::")
            fxn_cls_arr = fxn_spec[:-1]
            current_fxn = fxn_spec[-1]
        elif "sstobject" in line:
            idx += 1
            class_arr = scope_cls_arr[:] ; class_arr.extend(fxn_cls_arr)
            classname = "::".join(class_arr)
            scope = "global" 
            if classname:
                scope = get_full_scope_name(classname, scoped_ns_arr, using_cls_arr, using_ns_arr)
            sstobject_scopes_arr.append(scope)
            sstobject_arr.append("sstobject {")
            sstobject_depth = 1
        elif "sstvariable" in line:
            idx += 1
            sstvariable_arr.append("sstvariable {")
            sstvariable_depth = 1
        elif "sstkeyword" in line:
            idx += 1
            class_arr = scope_cls_arr[:] ; class_arr.extend(fxn_cls_arr)
            classname = "::".join(class_arr)
            scope = "global" 
            if classname:
                scope = get_full_scope_name(classname, scoped_ns_arr, using_cls_arr, using_ns_arr)
            sstkeyword_scopes_arr.append(scope)
            sstkeyword_arr.append("sstkeyword {")
            sstkeyword_depth = 1
        else:
            if fxn_depth > 0:
                frame_arr.append(line)

        paren_depth += line.count("(")
        paren_depth -= line.count(")")
        idx += 1
    
    from pysst.header import set_using_namespace
    set_using_namespace(fullpath, my_using_cls_arr)

    if namespace_depth != 0 or class_depth != 0 or fxn_depth != 0 or struct_depth != 0 or enum_depth != 0 or template_depth != 0 or parentheses_depth != 0 or extern_depth != 0:
        sys.exit("depths wrong on parsed file")

    #now process the sst variables
    sstvariable_text = "\n".join(sstvariable_arr)
    matches = re.compile('sstvariable\s*[{](.*?[}])',re.DOTALL).findall(sstvariable_text)
    for body in matches:
        attr_matches = re.compile("([_a-zA-Z]+)\s*[=](.*?)[;]",re.DOTALL).findall(body)
        for attr_name, attr_val in attr_matches:
            attr_val = re.compile("([\n\s]+)").sub(" ",attr_val).strip()
            GlobalData.sstvariables[attr_name] = attr_val
            
        

    #now process the sst keywords
    sstkeyword_text = "\n".join(sstkeyword_arr)
    matches = re.compile('sstkeyword\s*[{](.*?[}]).*?param\s*[(]\s*["](.*?)["].',re.DOTALL).findall(sstkeyword_text)

    # You can declare variables for the documentation system.
    # This is useful if you have a lot of parameters with basically the same documentation
    # and you just want to reuse the same documentation for each one
    def sstkeyword_var_repl(match):
        varname = match.groups()[0]
        return GlobalData.sstvariables[varname]

    for i in range(len(matches)):
        body, name = matches[i]

        body = re.compile("[$]([\da-zA-Z_]+)").sub(sstkeyword_var_repl, body)
            
        scope = sstkeyword_scopes_arr[i]
        attr_matches = re.compile("([_a-zA-Z]+)\s*[=](.*?)[;]",re.DOTALL).findall(body)
        docstring = ""
        defval = ""
        allowed = []
        extra = None
        alias = ""
        tab = ""
        for attr_name, attr_val in attr_matches:
            #attr_val = attr_val.strip().replace("\n"," ")
            attr_val = re.compile("([\n\s]+)").sub(" ",attr_val).strip()
            if attr_name == "docstring":
                docstring = attr_val
            elif attr_name == "gui":
                defval = attr_val
            elif attr_name == "default":
                defval = attr_val
            elif attr_name == "extra":
                if attr_val == "true":
                    extra = True
                elif attr_val == "false":
                    extra = False
                else:
                    sys.exit("Invalid value %s for extra specification" % attr_val)
            elif attr_name == "tab":
                tab = attr_val
            elif attr_name == "alias":
                alias = attr_val
            else:
                allowed.append((attr_name,attr_val))
        keyword = None
        try:
            keyword = GlobalData.keywords[scope][name]
        except:
            sys.stderr.write("Bad sstkeyword:\nscope=%s\nname=%s\n%s\n" % (scope,name,body))
            sys.exit("Probably you forgot a semicolon")

        docstring_split = docstring.split()
        docstring_arr = []
        docstring_line_arr = []
        maxwidth = 50
        docstring_line_length = 0
        for entry in docstring_split:
            docstring_line_arr.append(entry)
            docstring_line_length += len(entry)
            if "ENDL" in entry:
                docstring_line_length = 0
            elif docstring_line_length > maxwidth:
                docstring_arr.append(" ".join(docstring_line_arr))
                docstring_line_arr = []
                docstring_line_length = 0
        if docstring_line_arr:
            docstring_arr.append(" ".join(docstring_line_arr))
        docstring = "ENDL".join(docstring_arr)
            
        
        if defval:
            keyword.gui_default = defval
        keyword.docstring = docstring
        keyword.tab = tab
        keyword.alias = alias
        if extra != None:
            keyword.extra = extra
        for val, valdocstring in allowed:
            allowed_val = AllowedKeyword(val, valdocstring)
            keyword.add_allowed(allowed_val)

    #sst objects are a gui only thing
    if not do_gui:
        return

    #now process the sst objects
    sstobject_text = "\n".join(sstobject_arr)
    matches = re.compile('sstobject\s*[{](.*?)[}]',re.DOTALL).findall(sstobject_text)
    for i in range(len(matches)):
        body = matches[i]
        scope = sstobject_scopes_arr[i]
        attr_matches = re.compile("([_a-zA-Z]+)\s*[=](.*?)[;]",re.DOTALL).findall(body)
        docstring = ""
        name = ""
        defval = ""
        clstype = ""
        tab = ""
        alias = ""
        yesno = False
        extra = False
        for attr_name, attr_val in attr_matches:
            attr_val = attr_val.strip().replace("\n"," ")
            if attr_name == "docstring":
                docstring = attr_val
            elif attr_name == "gui":
                defval = attr_val
            elif attr_name == "name":
                name = attr_val
            elif attr_name == "tab":
                tab = attr_val
            elif attr_name == "alias":
                alias = attr_val
            elif attr_name == "yesno":
                yesno = True;
                if attr_val != "true":
                    sys.exit("Invalid yesno specification %s.  Should only be true." % attr_val)
            elif attr_name == "type":
                clstype = attr_val
            else:
                sys.exit("invalid sstobject attribute %s" % attr_name)        
        if not name or not clstype or not defval:
            sys.exit("Invalid sstobject. Check that you have all atributes (docstring,gui,name,type) and used semicolons:\n%s" % body)
        keyword = Keyword(name, clstype, defval, extra, yesno)
        keyword.gui_default = defval
        keyword.fake_type = True
        keyword.tab = tab
        keyword.alias = alias
        keyword.docstring = docstring
        keyword.yesno = yesno
        if yesno:
            keyword.fake_type = False
            allowed_val = AllowedKeyword("yes", "Do use", clstype)
            keyword.add_allowed(allowed_val)
            allowed_val = AllowedKeyword("no", "Do NOT use", clstype)
            keyword.add_allowed(allowed_val)
        else:
            allowed_val = AllowedKeyword(defval, "", clstype)
            keyword.add_allowed(allowed_val)
        GlobalData.keywords[scope] = keyword
        
def parse_text(fname, do_gui=False):
    from pysst.header import is_parsed, parse_header
    if is_parsed(fname):
        return

    if fname.endswith(".h") or fname.endswith(".hpp"):
        parse_header(fname, do_gui)
    else:
        text = open(fname).read()
        do_parse_text(fname, text, do_gui)
        #parsed_files[fname] = True
