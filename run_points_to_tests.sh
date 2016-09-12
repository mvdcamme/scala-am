#!/bin/bash

trap "exit" INT

for testFile in blur.scm count.scm fib.scm eta.scm fact.scm gcipd.scm inc.scm kcfa2.scm kcfa3.scm mj09.scm mut-rec.scm optimization_test.scm rotate.scm sq.scm ack.scm collatz.scm widen.scm loop2.scm rsa.scm sat.scm dderiv.scm
do
	echo "run -m Hybrid -c -l Concrete -f test/$testFile --tracing f -o all --initial f --switch f" | sbt
done
