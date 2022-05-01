all: libzip main qcr

libzip: conanfile.txt
	conan install conanfile.txt --build=missing

main: main.rkt libzip
	raco exe -o main main.rkt

qcr: main
	raco distribute qcr main

clean: 
	rm -rf libzip qcr* main 
