#!/bin/bash

trap "exit" INT

for testFile in optimization_test.scm blur.scm count.scm fib.scm fact.scm gcipd.scm mut-rec.scm rotate.scm ack.scm collatz.scm widen.scm
do for i in {1..20}
	do
		echo "run -m Hybrid -c -l Concrete -f test/benchmarks/$testFile --tracing t -o all --threshold 5 --switch t" | sbt
	done
done
