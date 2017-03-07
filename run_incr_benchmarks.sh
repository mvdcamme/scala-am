#!/bin/bash

trap "exit" INT

for i in {1..5}
	do
		echo "run --result fact_incr_analysis.txt -f test/fact.scm -m Hybrid -l Concrete -c --tracing f --rt_analysis_interval r" | sbt
	done


for i in {1..5}
	do
		echo "run --result fact_incr_analysis.txt -f test/fact.scm -m Hybrid -l Concrete -c --tracing f --rt_analysis_interval 1 -q" | sbt
	done


for i in {1..5}
	do
		echo "run --result fact_incr_analysis.txt -f test/fact.scm -m Hybrid -l Concrete -c --tracing f --rt_analysis_interval 1" | sbt
	done
