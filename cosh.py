#!/usr/bin/python
from subprocess import Popen

p = Popen("ikarus --r6rs-script ./cosh.ss > ./cosh.tmp", shell=True)
p.communicate()

code = open("cosh.tmp").read()
wrapper = open("wrapper.ss").read()
out = open("cosh.out", "w")
out.write(wrapper % { "code" : code })
out.close()

p = Popen("ikarus --r6rs-script ./cosh.out", shell=True)
p.communicate()
