.PHONY : clean

libzip_win32 : conanfile.txt
	conan install conanfile.txt --build=missing --settings os="Windows" --settings compiler="Visual Studio"

libzip_linux : conanfile.txt
	conan install conanfile.txt --build=missing --settings os="Linux" --settings compiler="gcc"

main : main.rkt
	raco exe main.rkt

win32 : main libzip_win32
	raco distribute qcr-x86_64-win32 main.exe

linux : main libzip_linux
	raco distribute qcr-x86_64-linux main

clean : 
	-rm -rf libzip qcr* main main.exe