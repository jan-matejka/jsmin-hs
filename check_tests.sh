#! /usr/bin/env bash

for i in tests/*; do
	echo "checking $i"
	echo "c"
	cat $i | ./jsminc >c
	echo "hs"
	cat $i | ./jsmin >hs
	echo "diffing"
	diff -u c hs
done
