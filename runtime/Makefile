all: 
	nasm -felf64 compiled_code.s -o compiled_code.o
	ar r libcompiled_code.a compiled_code.o 
	rustc stub.rs -L . -o stub.exe

clean: 
	rm compiled_code.o stub.exe libcompiled_code.a
