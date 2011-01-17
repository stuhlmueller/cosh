#!/usr/bin/python
import os, sys
from subprocess import Popen
from utils import optfunc
from os.path import abspath, dirname
from datetime import datetime

def vprint(s, verbose):
    if verbose:
        print
        print s

def call(cmd, verbose=False, allow_fail=False):
    if verbose:
        print cmd
    p = Popen(cmd, shell=True)
    p.communicate()
    status = p.returncode
    if status != 0 and not allow_fail:
        print "command failed:\n%s" % cmd
        exit()
    else:
        return status

TEMPLATE = """#!r6rs

(import (rnrs)
        (scheme-tools)
        (cosh)
        (cosh visualize)
        (cosh preamble)
        (cosh header)
        (cosh-test pragmatics))

(define (marg expr)
  (map pretty-print
       (marginalize-expr header
                         (with-preamble expr))))

(define expr
  '(
%(code)s

))

(marg expr)
"""

@optfunc.main
@optfunc.arghelp('verbose', 'display all executed commands')
@optfunc.arghelp('debug', 'run all ikarus commands in debug mode')
@optfunc.arghelp('time', 'record the time it takes for the compiled file to run')
@optfunc.arghelp('keep', 'do not delete compiled file')
def main(file, verbose=False, debug=False, time=False, keep=False):
    """Usage: %prog <file> [options]"""
    in_path = abspath(file)
    settings = {
        "debug" : debug and "--debug" or "",
        "out_path" : in_path + ".tmp",
        "cosh_path" : abspath(dirname(sys.argv[0]))
    }
    
    vprint("generating scheme code", verbose)
    code = open(in_path).read()
    generated = TEMPLATE % { "code" : code }
    f = open(settings["out_path"], "w")
    f.write(generated)
    f.close()

    vprint("running generated scheme with ikarus", verbose)
    pre = datetime.now()
    call("export IKARUS_LIBRARY_PATH=%(cosh_path)s:$IKARUS_LIBRARY_PATH && ikarus %(debug)s --r6rs-script '%(out_path)s'" % settings, verbose)
    post = datetime.now()    

    if time:
        delta = post-pre
        seconds = delta.seconds + delta.microseconds/1000000.0
        print("runtime: %fs" % seconds)

    if not keep:
        vprint("removing compiled file", verbose)
        call("rm -f '%(out_path)s'" % settings, verbose)        
    
