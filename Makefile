all: libzip main qcr

libzip: conanfile.txt
	conan install conanfile.txt

main: main.rkt libzip
	raco exe -o main main.rkt

qcr: main
	raco distribute qcr main

clear: 
	rm -rf libzip qcr 