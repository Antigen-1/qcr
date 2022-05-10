.PHONY : clean

main : main.rkt
	raco exe main.rkt

win32 : main
	raco distribute qcr-x86_64-win32 main.exe

linux : main
	raco distribute qcr-x86_64-linux main

clean : 
	-rm -rf qcr* main main.exe