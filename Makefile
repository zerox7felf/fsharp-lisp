# Makefile

all: FsLisp.exe

FsLisp.exe: main.fs types.fs parser.fs eval.fs
	fsharpc --out:FsLisp.exe types.fs parser.fs eval.fs main.fs

run: FsLisp.exe
	mono FsLisp.exe
