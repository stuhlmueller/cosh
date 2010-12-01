#!/usr/bin/python
from subprocess import Popen
from external import optfunc

# enumerate smartly & print out context graph

@optfunc.main
def main(fn, debug=False):
    """Usage: %prog <file>"""
    # read fn
    expr = open(fn).read()
    # write temporary file that will pretty-print cc-converted file
    cc_generator = open("cc-wrapper.ss").read() % { "expr" : expr }
    f = open("cc.gen", "w")
    f.write(cc_generator)
    f.close()
    # run temporary file, get output
    p = Popen("ikarus --r6rs-script ./cc.gen > ./cc.tmp", shell=True)
    p.communicate()
    cc_code = open("cc.tmp").read()
    # read wrapper template & fill in cc-converted code & write to file    
    graph_generator = open("graph-wrapper.ss").read() % { "cc_code" : cc_code }
    f = open("cc.out", "w")
    f.write(graph_generator)
    f.close()
    # run
    if debug:
        p = Popen("ikarus --debug --r6rs-script ./cc.out", shell=True)
    else:
        p = Popen("ikarus --r6rs-script ./cc.out", shell=True)
    p.communicate()
