##! ./tcsh

time gforth -m 16M c.fs -e "bye"
time gforth -m 16M c.fs -e "include c.fs bye"
time gforth -m 16M c.fs -e "include c.fs include c.fs bye"
time gforth -m 16M c.fs -e "include c.fs include c.fs include c.fs bye"
time gforth -m 16M c.fs -e "include c.fs include c.fs include c.fs include c.fs bye"
time gforth -m 16M c.fs -e "include c.fs include c.fs include c.fs include c.fs include c.fs bye"
time gforth -m 16M c.fs -e "include c.fs include c.fs include c.fs include c.fs include c.fs include c.fs bye"
time gforth -m 16M c.fs -e "include c.fs include c.fs include c.fs include c.fs include c.fs include c.fs include c.fs bye"
time gforth -m 16M c.fs -e "include c.fs include c.fs include c.fs include c.fs include c.fs include c.fs include c.fs include c.fs bye"
