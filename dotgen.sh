#!/bin/sh

if [ $# -lt 1 ]
then
	echo "usage:" `basename $0` "word [ infiles ]"
	exit 1
fi

if [ $# -gt 1 ]
then
	./grep.sh $* | grep $1 | ./cat.sh
else
	./grep.sh | grep $1 | ./cat.sh
fi
