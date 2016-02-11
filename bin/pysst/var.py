
import re
import sys

def replVarDecls(fulltext, fxnargs, varobj): 
    from fakeparse import getVarDefinitions
    matches = getVarDefinitions(varobj.name(), fxnargs)
    if varobj.readOnly():
        return fulltext #no replacements required

    #fix all the sizeofs
    fulltext = fulltext.replace("sizeof(%s)" % varobj.name(), "sizeof(%s)" % varobj.getDatatype());

    for varmatch in matches:
        #print varmatch.initMatch()
        #print "->"
        #print varmatch.initReplacement()
        fulltext = fulltext.replace(varmatch.initMatch(), varmatch.initReplacement())
        if varobj.hasPostText():
          fulltext = fulltext.replace("POST TEXT", varobj.postText())
    return fulltext

    repl = "((%s)%s)" % (varobj.castType(), varobj.name())

    def repl_fxn(repl, match):
        char1, var, char2 = match.groups()
        #sys.stderr.write("replacing %s with %s\n" % (var, repl))
        return "%s%s%s" % (char1, repl, char2)

    chars = "[\[\]=,;()\- &.<>!~+]"
    regexp = re.compile('(%s)(%s)(%s)' % (chars, varobj.name(), chars)) #make sure we only pick up valid vars
    return regexp.sub(lambda x: repl_fxn(repl,x), fulltext)

def replVars(fulltext, fxnargs, outvars):
    for var in outvars:
        fulltext = replVarDecls(fulltext, fxnargs, var)

    varDict = {}
    for var in outvars:
      varDict[var.name()] = var

    from fakeparse import getRefactoredText
    fulltext = getRefactoredText(fulltext, varDict)

    for var in outvars:
        #clean up initializers
        fulltext = fulltext.replace("___%s___" % var.name(), var.name())
    return fulltext

def collectVars(fxnargs, allvars, outvars, scope):
    import sys
    from fakeparse import getVarDefinitions
    for varname in allvars:
        matches = getVarDefinitions(varname, fxnargs)
        outvars.extend(matches)
        #for varmatch in matches:
        #    varobj = GlobalVar(varname, varmatch, scope)
        #    outvars.append(varobj)


