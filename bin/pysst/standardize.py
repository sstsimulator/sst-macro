import re

slash_comment_regexp = re.compile('//.*?\n')
asterisk_comment_regexp = re.compile('/[*].*?[*]/', re.DOTALL)


whitespace_regexp = re.compile("[\n\s]*")
class_regexp = re.compile("class[\s\n]+([a-zA-Z\d_]+)\s*([<:{])", re.DOTALL)
class_declare_regexp = re.compile("class[\s\n]+(.*?[;])")
def class_repl(x):
    classname, trailer = x.groups()
    repl = "class@%s %s" % (classname, trailer)
    return repl
operator_regexp = re.compile("(operator.*?\s*)[(]")
def operator_repl(x):
    match = x.groups()[0]
    repl = match.strip()
    return repl
openbracket_regexp = re.compile('([_a-zA-Z\)]){')
def openbracket_repl(x):
    match = x.groups()[0]
    return "%s {" % match

closebracket_regexp = re.compile('}([_a-zA-Z\(])')
def closebracket_repl(x):
    match = x.groups()[0]
    return "} %s" % match


precursor_closebracket_regexp = re.compile('([_a-zA-Z\d\)\]])}')
def precursor_closebracket_repl(x):
    match = x.groups()[0]
    return "%s }" % match

precursor_openbracket_regexp = re.compile('{([_a-zA-Z\d\(\[])')
def precursor_openbracket_repl(x):
    match = x.groups()[0]
    return "{ %s" % match

openparentheses_regexp = re.compile('[(]\s+')
closeparentheses_regexp = re.compile('\s+[)]')

arraydeclare_regexp = re.compile('([a-zA-Z\d_\]])=\s*[{]')
def arraydeclare_repl(x):
    match = x.groups()[0]
    return "%s ={" % match

preprocessor_regexp = re.compile("[#].*?\n")

sstkeyword_comment_regexp = re.compile('/[*][*]?\s+(sstkeyword.*?)[*]/', re.DOTALL)
sstvariable_comment_regexp = re.compile('/[*][*]?\s+(sstvariable.*?)[*]/', re.DOTALL)
sstobject_comment_regexp = re.compile('/[*][*]?\s+(sstobject.*?)[*]/', re.DOTALL)
def sstkeyword_repl(match):
    return match.groups()[0]

colon_fixer = re.compile("(::\s+)")
parentheses_fixer = re.compile("(\s+[(])")

def clean_comments(text):
    text = sstvariable_comment_regexp.sub(sstkeyword_repl, text)
    text = slash_comment_regexp.sub("", text)
    text = asterisk_comment_regexp.sub("", text)
    return text


def clean_file_text(inp_text):
    text = sstkeyword_comment_regexp.sub(sstkeyword_repl, inp_text)
    # nuke bit shifters
    text = text.replace("<<","")
    text = text.replace(">>","")
    text = sstobject_comment_regexp.sub(sstkeyword_repl, text)
    text = sstvariable_comment_regexp.sub(sstkeyword_repl, text)
    text = slash_comment_regexp.sub("", text)
    text = asterisk_comment_regexp.sub("", text)
    text = text.replace('\\\n', " ") #kill continuations
    text = preprocessor_regexp.sub("", text)
    text = openbracket_regexp.sub(openbracket_repl, text)
    text = closebracket_regexp.sub(closebracket_repl, text)
    text = precursor_closebracket_regexp.sub(precursor_closebracket_repl, text)
    text = precursor_openbracket_regexp.sub(precursor_openbracket_repl, text)
    text = closeparentheses_regexp.sub(")", text)
    text = openparentheses_regexp.sub("(", text)
    text = text.replace("template<", "template <")
    text = text.replace("{}", "{ }")
    text = text.replace("{", " { ")
    text = text.replace("}", " } ")
    text = arraydeclare_regexp.sub(arraydeclare_repl, text)
    text = text.replace("(", " ( ")
    text = text.replace(")", " ) ")
    text = text.replace("<", " < ")
    text = text.replace(">", " > ")
    text = colon_fixer.sub("::", text)
    text = parentheses_fixer.sub("(",text)
    text = text.replace("{(","{ (")
    text = text.replace("}(","} (")
    text = re.sub("\s+", " ", text)
    return text
