#!/bin/bash
2>&1
NAME=simulation
gcc -c src/${NAME}.adb -o debug/${NAME}.o
printf "compiling...\n"
printf "debug: "
ls -1 debug/
gnatbind debug/${NAME}.ali
printf "binding...\n"
printf "debug: "
ls -1 debug/
gnatlink debug/${NAME}.ali -o build/${NAME}
printf "linking...\n"
printf "build: "
ls -1 build/
