#!/bin/sh

rel=-1
last=0
sed 's/u.*$//' | \
	while read i
do
	if [ $rel = -1 ]
	then
		rel=$i
	fi
	echo -n "$i-$last: "
	echo "$i-$last" | bc -l
	last=$i
done
