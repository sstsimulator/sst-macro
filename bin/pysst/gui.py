import os
import re

first_delim = "|";
second_delim = ";";
third_delim = "/";
topdir = os.getcwd()

def gui_default(keytype,value):
    if keytype == "integer" or keytype == "long":
        try:
            defval = int(value)
        except:
            return "0"
    elif keytype == "boolean":
        defval = value.lower()
        if defval == "true":
           return defval
        elif defval == "false":
           return defval
        else:
           return "true"
    elif keytype == "length":
        match = re.compile("(\d+[.]?\d*)(.*)").search(value)
        if not match:
            return "0.0"
        length, units = match.groups()
        length = eval(length)
        units = units.replace(" ","")
        if units == "GB":
            bw *= 1e9
        elif units == "MB":
            bw *= 1e6
        elif units == "KB":
            bw *= 1e3
        elif units == "B":
            bw /= 8.0
        elif units == "":
            pass
        else:
            sys.stderr.write("invalid units %s in byte length %s\n" % (units, value))
            return "0.0" #invalid units

        return "%24.10f" % length 
    elif keytype == "bandwidth":
        match = re.compile("(\d+[.]?\d*)(.*)").search(value)
        if not match:
            return "0.0"
        bw, units = match.groups()
        bw = eval(bw)
        units = units.replace(" ","")
        if units == "GB/s":
            bw *= 1e9
        elif units == "MB/s":
            bw *= 1e6
        elif units == "KB/s":
            bw *= 1e3
        elif units == "b/s":
            bw /= 8.0
        elif units == "":
            pass
        else:
            sys.stderr.write("invalid units %s in bandwidth %s\n" % (units, value))
            return "0.0" #invalid units

        return "%24.10f" % bw

    elif keytype == "frequency":
        match = re.compile("(\d+[.]?\d*[e]?[-]?\d*)(.*)").search(value)
        if not match:
            return "0"

        freq, units = match.groups()
        freq = eval(freq)
        units = units.lower().replace(" ","")
        if units == "ghz":
            freq *= 1e9
        elif units == "mhz":
            freq *= 1e6
        elif units == "khz":
            freq *= 1e3
        elif units == "":
            pass
        else:
            sys.stderr.write("invalid units %s in frequency %s\n" % (units, freq))
            return "0" #invalid units

        return "%24.10f" % freq

    elif keytype == "timestamp":
        defval = value.replace("timestamp","")
        defval = defval.replace("(","")
        defval = defval.replace(")","")
        match = re.compile("(\d+[.]?\d*[e]?[-]?\d*)(.*)").search(defval)
        if not match:
            return "0"

        ts, units = match.groups()
        ts = eval(ts)
        units = units.lower().replace(" ","")
        if units == "ps":
            ts *= 1e-12
        elif units == "ns":
            ts *= 1e-9
        elif units == "us":
            ts *= 1e-6
        elif units == "ms":
            ts *= 1e-3
        elif units == "":
            pass
        else:
            sys.stderr.write("invalid units %s in timestamp %s\n" % (units, ts))
            return "0" #invalid units

        return "%24.10f" % ts
    elif keytype == "double":
        try:
            defval = float(value)
        except:
            return "0.0"

    if value == "nodefault":
        return ""
    else:
        return value

def gui_keyword_line(keyval):
    line_arr = []
    line_arr.append(keyval.name)
    if keyval.keytype == "long":
        line_arr.append("integer")
    else:
        line_arr.append(keyval.keytype)

    if keyval.keytype == "boolean":
        line_arr.append("boolean")
    elif keyval.fake_type:
        line_arr.append("fake")
    elif keyval.yesno:
        line_arr.append("yesno")
    elif keyval.factory_type:
        line_arr.append("factory")
    elif keyval.allowed:
        line_arr.append("combo")
    else:
        line_arr.append("open")

    if keyval.gui_default:
        line_arr.append(gui_default(keyval.keytype,keyval.gui_default))
    else:
        line_arr.append(gui_default(keyval.keytype,"nodefault"))

    if keyval.allowed:
        allowed_arr = []
        for value in keyval.allowed:
            value_arr = [value.name, value.typename, value.docstring]
            allowed_arr.append(third_delim.join(value_arr))
        line_arr.append(second_delim.join(allowed_arr))
    else:
        line_arr.append("")

    line_arr.append(keyval.docstring)

    line_arr.append(keyval.tab)
    line_arr.append(keyval.alias)

    if keyval.extra:
        line_arr.append("true")
    else:
        line_arr.append("false")

    # to help string split parsing in Qt
    line_arr.append("")
        
    return first_delim.join(line_arr)

def append_gui_parent(str_arr,scope,parent):
    from pysst.sstglobals import GlobalData

    parent_set = GlobalData.keywords.get_parent_set(parent, scope)
    for entry in parent_set:
        str_arr.append(gui_keyword_line(entry))

    for grandparent in parent_set.parents:
        append_gui_parent(str_arr, parent, grandparent)

def gui_file(folder, scope):
    from pysst.sstglobals import GlobalData
    if not scope in GlobalData.keywords:
        return

    keyset = GlobalData.keywords[scope]

    if not scope:
        scope = "global"

    fname = scope + ".gui"
    str_arr = []
    for entry in keyset:
        str_arr.append(gui_keyword_line(entry))

    for parent in keyset.parents:
        append_gui_parent(str_arr, scope, parent)

    if not str_arr:
        return

    fname = os.path.join(folder, fname)
    open(fname,"w").write("\n".join(str_arr))
    return fname
