
def traceback(error=None):
    import sys
    import traceback
    if error:
        tb_list = traceback.format_tb(sys.exc_info()[2])
        return "%s\n%s" % (error, "\n".join(tb_list))
    else:
        import StringIO
        output = StringIO.StringIO()
        traceback.print_stack(file=output)
        contents = output.getvalue()
        return contents
