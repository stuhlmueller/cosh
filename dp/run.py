#!/usr/bin/python
from subprocess import Popen
from external import optfunc

def read(fn):
    f = open(fn)
    s = f.read()
    f.close()
    return s

def write(fn, s):
    print "writing", fn
    f = open(fn, "w")
    f.write(s)
    f.close()

def cmd(c):
    print c
    p = Popen(c, shell=True)
    p.communicate()

def scheme_filter(filter_fn, filtered_fn, expr, debug):
    # write temporary file that will print converted code
    printer = read(filter_fn)
    generator = printer % { "expr" : expr }
    write(filtered_fn + "gen", generator)
    # convert code
    if debug:
        cmd("ikarus --debug --r6rs-script %sgen > %s" % (filtered_fn, filtered_fn))
    else:
        cmd("ikarus --r6rs-script %sgen > %s" % (filtered_fn, filtered_fn))
    return read(filtered_fn)

@optfunc.main
def main(fn, debug=False, dryrun=False):
    """Usage: %prog <file>"""

    # add preamble to code
    prog = read(fn)
    preamble = read("preamble.church")
    preambled_expr = "(begin \n" + preamble + "\n\n" + prog + "\n)"
    # print preambled_expr

    desugared_code = scheme_filter("desugar-printer.ss", "%s.desugared" % fn, preambled_expr, debug)
    # print desugared_code
    
    cccode = scheme_filter("cc-printer.ss", "%s.cc" % fn, desugared_code, debug)
    # print cccode
    
    # add wrapper with erp definitions
    wrapper = read("wrapper.ss")
    generated = wrapper % { "prog" : cccode }
    write("%s.out" % fn, generated)
    
    # run generated file
    if not dryrun:
        if debug:
            cmd("ikarus --debug --r6rs-script %s.out" % fn)
        else:
            cmd("ikarus --r6rs-script %s.out" % fn)
