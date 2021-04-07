#!/usr/bin/env bash
#pipes the output from one process into another

tail -1 state.dat | awk '{for(i=2; i<=50; i++) printf $i"  ";}' | ./te_070421 -l