#!/bin/nawk -f
BEGIN {
  FS = "#"; 
  rule = 1;
}

/^%%$/ {
  part += 1;
  print $0; 
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
END {
  if (part == 1)
    print "%%"
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
  print ": burm_reduce ( goal node-addr -- )";
  print "  \\\\ .\\\" printReduce1\\\" hex.s cr";
  print "  tuck burm_STATE_LABEL@ swap burm_rule";
  print "  dup 0= if";
  print "    nip nip";
  print "    burm_assert\\\" no cover\\\" cr else";
  print "    2dup 2>r";
  print "    dup burm_nts@ >r				\\\\ (R: goal node_addr nts-addr)";
  print "    depth >r burm_kids depth r> - 2 +";
  print "    r>";
  print "    swap case";
  print "      0 of";
  print "        endof";
  print "      1 of";
  print "        swap >r endof";
  print "      2 of";
  print "        rot >r swap >r endof endcase";
  print "    >r";
  print "    begin";
  print "      r> dup @ dup 0<> while";
  print "      r> rot cell+ >r";
  print "      recurse";
  print "      repeat";
  print "    2drop";
  print "    2r>";
  print "    [burm_reduce] @ execute";
  print "    endif";
  print "  \\\\ .\\\" printReduce2\\\" hex.s cr";
  print "  ;";
}
