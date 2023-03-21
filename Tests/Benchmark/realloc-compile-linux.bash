#!/bin/bash

fpc -B -MObjFPC -Tlinux -Px86_64 -O4 -CpCOREAVX2 Realloc.dpr

