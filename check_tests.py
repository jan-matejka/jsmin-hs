#! /usr/bin/env python
# -*- coding: utf-8 -*-
import os
from os.path import join as j
import subprocess as s

for root,dirs,files in os.walk("tests"):
    for f in files:
        path = j(".",root,f)

        fp = {}
        fp["in_hs"] = open(path,"r")
        fp["hs"] = open(j(".","hs"),"w")
        p_hs = s.Popen([j(".","jsmin")],stdin=fp["in_hs"],stdout=fp["hs"])

        fp["in_c"] = open(path,"r")
        fp["c"] = open(j(".","c"),"w")
        p_c = s.Popen([j(".","jsminc")],stdin=fp["in_c"],stdout=fp['c'])

        p_hs.wait()
        p_c.wait()

        s_tmp = j(".","tmp")
        fp["s"] = open(s_tmp,"w")
        diff = s.Popen(["diff","c","hs","-u"],stdout=fp["s"])
        diff.wait()

        for k in fp:
            fp[k].close()

        out = "".join(open(s_tmp,"r").readlines())

        if(len(out) > 0):
            print f
            print out
