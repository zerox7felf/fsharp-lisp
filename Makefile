# Makefile

all: Main.exe run

Main.exe: main.fs types.fs parser.fs eval.fs
	fsharpc --out:Main.exe types.fs parser.fs eval.fs main.fs

run: Main.exe
	mono Main.exe