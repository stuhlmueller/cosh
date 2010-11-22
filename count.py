#!/usr/bin/python
from subprocess import Popen
from external import optfunc

# enumerate dumbly & print out state counts

@optfunc.main
def main(fn):
    """Usage: %prog <file> [options]"""
    # read fn
    expr = open(fn).read()
    # write temporary file that will pretty-print cc-converted file
    count_generator = open("count-wrapper.ss").read() % { "expr" : expr }
    f = open("count.gen", "w")
    f.write(count_generator)
    f.close()
    # run temporary file, get output    
    p = Popen("ikarus --r6rs-script ./count.gen > ./count.tmp", shell=True)
    p.communicate()
    code = open("count.tmp").read()
    # read wrapper template & fill in cc-converted code & write to file        
    wrapper = open("wrapper.ss").read()
    out = open("count.out", "w")
    out.write(wrapper % { "code" : code })
    out.close()
    # run
    p = Popen("ikarus --r6rs-script ./count.out", shell=True)
    p.communicate()
