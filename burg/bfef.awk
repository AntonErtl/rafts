#!/bin/nawk -f
BEGIN {
  FS = "#"; 
  rule = 1;
}

/^%%$/ {
  part += 1;
  print $0;
  if (part!=2)
    next;
  for (i=rule-1; i>0; i--)
    print ":noname", action[i], ";";
  printf "%d array_noallot [burm_reduce]\n", rule;
  print "  0 ,"
  for (i=1; i<rule; i++) {
    printf "  ,";
    if (((i-1)%16) == 15)
      print "";
  }
  print "";
  next;
}

part == 0

part == 1 && NF>0 {
  printf "%s = %d", $1, rule;
  if ($2 != "")
    printf " (%s)", $2;
  print ";";
  action[rule] = $3;
  rule++;
  next;
}

part == 2
