import re
import sys

def normalize(fulltext):
    fulltext = fulltext.replace("namespace","\nnamespace\n")
    fulltext = fulltext.replace(";",";\n")
    fulltext = fulltext.replace("{","\n{\n")
    fulltext = fulltext.replace("}","\n}\n")
    fulltext = fulltext.replace("\n\n","\n")
    fulltext = fulltext.replace("\n\n","\n")
    fulltext = fulltext.replace("\n\n","\n")
    return fulltext

def clearAllAttributes(text):
    import re
    regexp = re.compile("__attribute__\s*[(].*?[)] ")
    text = regexp.sub("", text)
    return text

def clearAllPounds(fulltext):
    import re, sys
    str_arr = []
    splitlines = fulltext.splitlines()
    for entry in splitlines:
        stripped_entry = entry.strip()
        if stripped_entry and entry[0] == "#":
            pass
        else:
            str_arr.append(entry)
    text = "\n".join(str_arr)
    return text

def getFunctionBodies(fulltext):
    depth = 0
    structDepth = 0
    depthN = []
    for idx in range(len(fulltext)):
        char = fulltext[idx]
        if depth > 0:
            depthN.append(char)

        if char == "{":
            depth += 1
        elif char == "}":
            depth -= 1

    depthN = "".join(depthN)
    return depthN


def appendChar(fulltext, idx, charArr):
    charArr.append(fulltext[idx])
    return 0

def doNothing(fulltext, idx, charArr):
    return 0

def parseTextAction(inputText, depth0action, depthNaction):
    NoMatch = -1
    Function = 0
    Struct = 1
    Namespace = 2
    Union = 3
    Class = 4
    Template = 5
    Quotes = 6
    depths = [0,0,0,0,0,0,0]
    totalDepth = 0

    #fricking c++11
    fulltext = inputText.replace("<= sizeof", "BAD_CPP11_LEQ")
    fulltext = fulltext.replace("< sizeof", "BAD_CPP11_LT")
    charArr = []
    
    def checkFrame(idx, fulltext):
      checker = fulltext[idx:idx+9]
      if checker == "namespace":
        #sys.stderr.write("Namspace:\n%s\n" % fulltext[idx:idx+20])
        return Namespace

      if checker[:8] == "template" and not checker[0] == ":":
        return Template

      checker = checker[:6]
      if checker == "struct":
        #sys.stderr.write("Struct d=%d:\n%s\n" % (depths[Function], fulltext[idx:idx+40]))
        return Struct

      checker = checker[:5]
      if checker == "union":
        return Union
      elif checker == "class":
        return Class

      if checker[0] == '"':
        return Quotes

      return NoMatch

    def followUpFrame(frame):
      if frame in (Union, Template):
        return frame
      else:
        return Function

    skipped = []
    structDepth = 0
    currentFrames = [-1]
    pendingFrame = Function
    depth0 = []
    struct = False
    lastFrame = currentFrames[-1]
    for idx in range(len(fulltext)):
        if depths[Template] > 0:
          charArr.append(fulltext[idx]) #no special processing
        elif depths[Function] == 0:
          idx += depth0action(fulltext, idx, charArr)
        else:
          idx += depthNaction(fulltext, idx, charArr)

        char = fulltext[idx]
        if (pendingFrame < Template and char == "{") or (pendingFrame == Template and char == "<"):
          #print fulltext[idx-60:idx+1]
          #print "%60s" % "", pendingFrame, "%2d" % lastFrame, depths, "++", currentFrames
          depths[pendingFrame] += 1
          totalDepth += 1
          currentFrames.append(pendingFrame)
          lastFrame = pendingFrame
          pendingFrame = followUpFrame(pendingFrame)
        elif (lastFrame < Template and char == "}") or (lastFrame == Template and char == ">") or (lastFrame == Quotes and char == '"'):
          #print fulltext[idx-60:idx+1]
          #print "%60s" % "", pendingFrame, "%2d" % lastFrame, depths, "--", currentFrames
          frame = currentFrames[-1]
          depths[frame] -= 1
          totalDepth -= 1
          currentFrames.pop();
          if frame == Function and depths[Function] == 0:
            depth0action(fulltext, idx, charArr)
          lastFrame = currentFrames[-1]
          pendingFrame = followUpFrame(lastFrame)
        else:
          #if we are inside a template - neglect everything else
          if pendingFrame < Template:
            frame = checkFrame(idx, fulltext)
            if frame == Quotes:
              totalDepth += 1
              depths[Quotes] += 1
              currentFrames.append(frame)
              lastFrame = frame
              pendingFrame = Quotes
            elif frame > 0:
              pendingFrame = frame

    text = "".join(charArr)
    text = text.replace("BAD_CPP11_LEQ", "<= sizeof")
    text = text.replace("BAD_CPP11_LT", "< sizeof")
    return text


def testReplaceAppend(fulltext, idx, charArr, varDict):
    chars = '[]=,;()\- &.<>!~+\n'
    leadIn = fulltext[idx-1]
    if not leadIn in chars:
      charArr.append(fulltext[idx])
      return 0 #do not advance

    stopIdx = idx
    length = len(fulltext)
    while stopIdx < length and not fulltext[stopIdx] in chars:
      stopIdx += 1

    varName = fulltext[idx:stopIdx]
    try:
      #we have found a valid variable
      varobj = varDict[varName]
      repl = "((%s)%s)" % (varobj.castType(), varobj.name())
      print "Replacing %s with %s" % (varName, repl)
      charArr.append(repl)
      return len(varName) #skip ahead
    except: #not a variable
      charArr.append(fulltext[idx])
      return 0
      

def getRefactoredText(fulltext, variables):
    depthNfxn = lambda x,y,z: testReplaceAppend(x,y,z,variables)
    return parseTextAction(fulltext, appendChar, depthNfxn)

def getGlobalText(fulltext):
    return parseTextAction(fulltext, appendChar, doNothing)

def getFxnBodies(text, fxn):
    retext = r"""[};]               #previous fxn, declaration closes
                [\s\na-zA-Z*_\d]+ #return type
                (%s[\s\n]*         #function name and any extra whitespace
                [(].*?[)]         #function args
                [\s\n]*)[{]         #the opening of the function 
                """ % fxn
    regexp = re.compile(retext)
    match = regexp.search(text)
    if not match:
        return None

    strToFind = match.groups()[0]
    indexStart = text.find(strToFind)
    if indexStart == -1:
        return None

def findall(substring, text):
    indices = []
    idx = text.find(substring, 0)
    while not idx == -1:
        indices.append(idx)
        idx = text.find(substring, idx+1)
    return indices

def findBackwardFxnIndex(startIdx, text):
    idx = startIdx - 1
    #make sure we didn't accidentally pick up a substring
    if text[idx] == "_":
        if not "__attribute__" in text:
            return -1
    elif not text[idx] in (" ",";","}","*","&"): 
        return -1

    while not text[idx] in (";", "}"):
        idx -= 1

    return idx+1

def findArgFxnIndices(startIdx, text):
    depth = 0
    idx = startIdx
    #go until open parentheses
    while not text[idx] == "(":
        if not text[idx] == " ": #should only be spaces
            #again, picked up substring
            return -1, -1
        idx += 1
    openIdx = idx
    depth = 1
    idx += 1
    while depth > 0:
        char = text[idx]
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        idx += 1
    closeIdx = idx
    return openIdx, closeIdx

def findFxnTailIndex(startIdx, text):
    idx = startIdx
    while not text[idx] in (';', '{'):
        idx += 1
    return idx

def findReturnType(preamble):
    searchStr = "__attribute__"
    idx = preamble.find(searchStr)
    if idx == -1:
        return preamble.strip()

    startIdx = idx
    idx = idx + len(searchStr)
    while not preamble[idx] == "(":
        idx += 1

    depth = 1
    idx += 1
    while depth > 0:
        char = preamble[idx]
        if char == "(":
            depth += 1
        elif char == ")":
            depth -= 1
        idx += 1

    attrPart = preamble[startIdx:idx+1]
    cleanPreamble = preamble.replace(attrPart, "")
    return cleanPreamble.strip()

def getFxnCalls(text, fxn):
    indices = findall(fxn, text)
    matches = []
    regexp = re.compile('[a-zA-Z\d_]')
    for idx in indices:
        prevIdx = idx-1
        if regexp.search(text[prevIdx]):
            continue #accidentally matched substring
        stopIdx = idx+len(fxn)
        if regexp.search(text[stopIdx]):
            continue #accidentally matched substring
        elif text[stopIdx] in (',', ')'): #used as function pointer
            continue
        while not text[stopIdx] == "(": #go until parentheses
            stopIdx += 1
            
        name = text[prevIdx:stopIdx]
        if '"' in name: #oops, in quotes
            continue

        depth = 1
        argStopIdx = stopIdx+1
        while depth > 0:
            char = text[argStopIdx]
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
            argStopIdx += 1
        args = text[stopIdx:argStopIdx]
        
        #declStopIdx = argStopIdx+1
        #while not text[declStopIdx] in (";", "{"):
        #    declStopIdx += 1
        #lastToken = text[declStopIdx]
        #if lastToken == ";": #a function call
        matches.append((name, args))
    return matches


def getFxnDefStart(text, fxn):
    indices = findall(fxn, text)
    for idx in indices:
        backidx = findBackwardFxnIndex(idx, text)
        if backidx == -1:
            #oops, this wasn't a valid match, substring or something
            continue
        argStartIdx, argStopIdx = findArgFxnIndices(idx+len(fxn), text)
        if argStartIdx == -1:
            #oops, this wasn't a valid match, substring or something
            continue
        tailIdx = findFxnTailIndex(argStopIdx, text)
        if text[tailIdx] == "{":
            return text[backidx:tailIdx+1]

def getFxnDecls(text, fxn):
    indices = findall(fxn, text)
    decls = []
    for idx in indices:
        backidx = findBackwardFxnIndex(idx, text)
        if backidx == -1:
            #oops, this wasn't a valid match, substring or something
            continue
        argStartIdx, argStopIdx = findArgFxnIndices(idx+len(fxn), text)
        if argStartIdx == -1:
            #oops, this wasn't a valid match, substring or something
            continue
        tailIdx = findFxnTailIndex(argStopIdx, text)
        tailToken = text[tailIdx]

        preamble = text[backidx:idx].lstrip()
        name = text[idx:argStartIdx]
        args = text[argStartIdx:argStopIdx]
        retType = findReturnType(preamble)
        decls.append((retType, preamble, name, args))
    return decls
    

    retext = r"""[};]               #previous fxn, declaration closes
                ([\s\na-zA-Z*_\d]+) #return type 
                (%s[\s\n]*)         #function name and any extra whitespace
                [(](.*?)[)]         #function args
                [\s\n]*[;{]         #the opening of the function 
                """ % fxn

    regexp = re.compile(retext,re.DOTALL | re.VERBOSE)
    matches = regexp.findall(text)
    return matches

def getFxnDeclsAndCallsNoReturn(text, fxn):
    matches = []
    for ret, fxn, args in getFxnDeclsAndCalls(text,fxn):
        matches.append((fxn,args))
    return matches

class Variable:

    def newType(self):
      return self.newTypeString(self.getDatatype())

    def newTypeString(self, strng):
      return "sstmac::sw::global_variable<%s> " % strng

    def name(self):
      return self.varname

    def __repr__(self):
      return self.name()

    def readOnly(self):
      return self.isReadOnly

    def hasPostText(self):
      return False

class StructVariable(Variable):
    def __init__(self, structDef, varDecl, tailtoken, readOnly):
        self.varDecl = varDecl
        self.tailtoken = tailtoken
        self.isReadOnly = readOnly
        self.varname = varDecl.split("=")[0].strip()

        structIdx = structDef.find("struct")
        bracketIdx = structDef.find("{")
        self.qualifiers = structDef[:structIdx]
        self.typeName = structDef[structIdx+6:bracketIdx]
        self.structDef = structDef[bracketIdx:]
        self.static = "static" in self.qualifiers
        self.extern = "extern" in self.qualifiers

    def rename(self, name):
        self.varDecl = name

    def __str__(self):
        return "%s %s" % (self.structDef, self.varDecl)

    def isExtern(self):
        return "extern" in self.structDef

    def stripIniter(self):
        pass

    def clearStaticQualifier(self):
        self.structDef = self.structDef.replace("static ","")

    def cleanDatatype(self):
        pass

    def castType(self):
      return "%s&" % self.getDatatype()

    def getDatatype(self):
        name = self.typeName.strip()
        if not name:
          name = self.varname + "_type"
        return "struct %s" % name

    def initMatch(self):
        if self.readOnly():
            return "DO NOT MATCH AND DELETE THIS"
        else:
            return "%sstruct%s%s%s%s" % (self.qualifiers, self.typeName, self.structDef, self.varDecl, self.tailtoken)

    def initReplacement(self):
        str_arr  = ["%s%s" % (self.qualifiers, self.getDatatype())]
        str_arr.append("%s%s" % (self.structDef, self.tailtoken))
        str_arr.append("%s ___%s___;" % (self.newType(), self.varname))
        return "\n".join(str_arr)

class BasicVariable(Variable):
    def __init__(self, varname, leadtoken, datatype, tailtoken, readOnly):
        self.leadtoken = leadtoken
        self.datatype = datatype
        self.qualifiers = ""
        self.extern = False
        qualifiers = []
        self.varname = varname


        if "extern" in self.datatype:
            self.datatype = self.datatype.replace("extern ","")
            qualifiers.append("extern ")
            self.extern = True
        self.qualifiers = "".join(qualifiers)
        self.tailtoken = tailtoken
        self.isReadOnly = readOnly

    def __str__(self):
        return "%s %s" % (self.datatype, self.varname)

    def isExtern(self):
        return self.extern

    def rename(self, name):
        self.varname = name

    def stripIniter(self):
        pass

    def clearStaticQualifier(self):
        self.datatype = self.datatype.replace("static ","")

    def castType(self):
        return "%s&" % self.datatype

    def cleanDatatype(self):
        self.datatype = self.datatype.strip().replace(" *", "*")

    def getDatatype(self):
        return self.datatype

    def initMatch(self):
        if self.readOnly():
            return "DO NOT MATCH AND DELETE THIS"
        else:
            return "%s%s%s%s%s" % (self.leadtoken, self.qualifiers, self.datatype, self.varname, self.tailtoken)

    def initReplacement(self):
      return "%s%s%s___%s___%s" % (self.leadtoken, self.qualifiers, self.newType(), self.varname, self.tailtoken)

    #get the main typedef working for both cases
    #need a flag for has initer 


class ArrayVariable(BasicVariable):
    def __init__(self, varname, leadtoken, datatype, tailtoken, readOnly, arrayPart):
        BasicVariable.__init__(self, varname, leadtoken, datatype, tailtoken, readOnly)

        self.arrayPart = arrayPart

    def getDatatype(self):
        return "%s_type" % self.varname

    def _parseArrayParts(cls, arrayPart):
      startIdx = 0
      while not arrayPart[startIdx] == "[":
        startIdx += 1
      endIdx = startIdx + 1
      while not arrayPart[endIdx] == "]":
        endIdx += 1
      endIdx += 1
      firstArray = arrayPart[startIdx:endIdx].replace(" ","")
      otherArrays = arrayPart[endIdx:]
      return firstArray, otherArrays
    parseArrayParts = classmethod(_parseArrayParts)

    def subTypeLine(self, otherArrays):
      return "typedef %s %s_subtype%s;" % (self.datatype, self.varname, otherArrays)

    def initReplacement(self):
      firstArray, otherArrays = self.parseArrayParts(self.arrayPart)
      str_arr = [self.leadtoken]
      str_arr.append(self.subTypeLine(otherArrays))
      str_arr.append("typedef %s %s_type%s;" % (self.datatype, self.varname, self.arrayPart))
      str_arr.append("%s ___%s___%s" % (self.newType(), self.varname, self.tailtoken))
      return "\n".join(str_arr)

    def castType(self):
      return "%s_subtype*" % self.varname

    def initMatch(self):
        if self.readOnly():
            return "DO NOT MATCH AND DELETE THIS"
        else:
            return "%s%s%s%s%s%s" % (self.leadtoken, self.qualifiers, self.datatype, self.varname, self.arrayPart, self.tailtoken)

class InitializedArray(ArrayVariable):

    def __init__(self, varname, leadtoken, datatype, tailtoken, readOnly, arrayPart):
        ArrayVariable.__init__(self, varname, leadtoken, datatype, tailtoken, readOnly, arrayPart)

    def initReplacement(self):
      firstArray, otherArrays = self.parseArrayParts(self.arrayPart)
      str_arr = [self.leadtoken]
      str_arr.append(self.subTypeLine(otherArrays))
      str_arr.append("typedef %s %s_type%s;" % (self.datatype, self.varname, self.arrayPart))
      str_arr.append(initerLine())
      #replace the original declaration with the initer
      str_arr.append(newVarLine())
      return "\n".join(str_arr)

    def initMatch(self):
      match = ArrayVariable.initMatch(self) + self.initer
      return match

    def findIniter(self, fulltext):
      matcher = ArrayVariable.initMatch(self)
      startIdx = fulltext.find(matcher)
      initIdx = startIdx + len(matcher)
      stopIdx = initIdx + 1
      while not fulltext[stopIdx] == ";":
        stopIdx += 1
      self.initer = fulltext[initIdx:stopIdx]

    def initerLine(self):
      return "%s _%s_initer_%s%s%s;" % (self.datatype, self.varname, self.arrayPart, self.tailtoken, self.initer)

    def newVarLine(self):
      return "%s ___%s___(_%s_initer_)" % (self.newType(), self.varname, self.varname)

class VariableSizeArray(InitializedArray):
    def __init__(self, varname, leadtoken, datatype, tailtoken, readOnly, arrayPart):
        InitializedArray.__init__(self, varname, leadtoken, datatype, tailtoken, readOnly, arrayPart)

    def newType(self):
      return self.newTypeString("%s_type" % self.varname)

    def initReplacement(self):
      firstArray, otherArrays = self.parseArrayParts(self.arrayPart)
      str_arr = [self.leadtoken]
      #replace the original declaration with the initer
      str_arr.append(self.initerLine())
      str_arr.append(self.subTypeLine(otherArrays))
      firstArraySize = "sizeof(_%s_initer_)/sizeof(%s_subtype)" % (self.varname, self.varname)
      str_arr.append("typedef %s %s_type[%s]%s;" % (self.datatype, self.varname, firstArraySize, otherArrays))
      str_arr.append(self.newVarLine())
      return "\n".join(str_arr)

def getVarDefinitions(varname, fxnargs):
    #make sure we dont accidentally pick up substrings
    readOnly = False #for now

    depth0 = fxnargs.depth0

    matches = []
    retext = "%s([\[\]\d+ ]*)\s*([=;(])" % varname
    regexp = re.compile(retext)
    for match in regexp.finditer(fxnargs.depth0):
        #figure out the datatype
        arrayPart, tailtoken = match.groups()
        startIdx = match.start(0)
        idx = startIdx - 1
        zero = match.start(0)
        one = match.start(1)
        two = match.start(2)
        #print zero, one, two
        #print zero, text[zero:zero+10]
        #print one, text[one:one+10]
        #print two, text[two:two+10]
        #print startIdx, varname, retext
        #back up until the next break to find the datatype
        if idx > 0 and not depth0[idx] in (' ','\n', ';', '}'):
            #I have accidentally matched a substring
            continue

        while idx > 0 and not depth0[idx] in (";","}"):
            idx -= 1
        datatype = depth0[idx+1:startIdx]
        stripped = datatype.lstrip()
        leadtoken = depth0[idx]
        if leadtoken == "}" and not stripped : 
            #hmm, guess it's a struct or union
            structStopIdx = idx + 1
            depth = 1
            while depth > 0:
                idx -= 1
                char = depth0[idx]
                if char == "}": depth += 1
                elif char == "{": depth -= 1
            #okay, finished struct def - no go until semicolon
            idx -= 1
            while not depth0[idx] in (";","}"):
                idx -= 1
            structDef = depth0[idx:structStopIdx]
            varnameStopIdx = match.start(2)
            varDecl = depth0[structStopIdx:varnameStopIdx]
            tailtoken = depth0[varnameStopIdx]
            matches.append(StructVariable(structDef,varDecl,tailtoken,readOnly))
            continue

        diff = len(datatype) - len(stripped)
        if not diff == 0:
            leadtoken = datatype[diff-1]
            datatype = stripped

        isArray = bool(arrayPart.strip())
        var = None
        if isArray:
          if ";" in tailtoken: #no initializer
            #print "Adding array variable", varname
            var = ArrayVariable(varname, leadtoken, datatype, tailtoken, readOnly, arrayPart)
          else:
            firstArray, otherArrays = ArrayVariable.parseArrayParts(arrayPart)
            if firstArray == "[]":
              #print "Adding variable sized array", varname
              var = VariableSizeArray(varname, leadtoken, datatype, tailtoken, readOnly, arrayPart)
            else:
              #print "Adding initialized array", varname
              var = InitializedArray(varname, leadtoken, datatype, tailtoken, readOnly, arrayPart)
            var.findIniter(fxnargs.fulltext)
        else:
          tailtoken = arrayPart + tailtoken
          #print "Adding basic variable", varname
          var = BasicVariable(varname, leadtoken, datatype, tailtoken, readOnly)
        matches.append(var)
    return matches

