#!/bin/bash

sudo nice -n -2 perf stat -r 5 -a -B -e cycles:u,instructions:u ./Realloc
