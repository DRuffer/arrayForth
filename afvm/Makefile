#Makefile for afvm example

#Copyright (C) 2001,2003,2007,2008 Free Software Foundation, Inc.

#This file is part of Gforth.

#Gforth is free software; you can redistribute it and/or
#modify it under the terms of the GNU General Public License
#as published by the Free Software Foundation, either version 3
#of the License, or (at your option) any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.#See the
#GNU General Public License for more details.

#You should have received a copy of the GNU General Public License
#along with this program; if not, see http://www.gnu.org/licenses/.

LEX=flex -l
YACC=bison -y
#YACC=yacc
VMGEN=vmgen
#GCC=gcc -g -Wall
GCC=gcc -O3 -fomit-frame-pointer -Wall
CC=$(GCC)
M4=m4 -s #recommended if supported
#M4=m4
OBJECTS_AFVM=afvm.tab.o support.o peephole.o profile.o disasm.o engine.o engine-debug.o

afvm: $(OBJECTS_AFVM)
	$(CC) $(OBJECTS_AFVM) -o $@

lex.yy.c: afvm.l
	$(LEX) afvm.l

afvm.tab.c: afvm.y lex.yy.c
	$(YACC) afvm.y && mv y.tab.c $@

afvm-vm.i afvm-disasm.i afvm-gen.i afvm-labels.i afvm-profile.i afvm-peephole.i: afvm.vmg
	$(VMGEN) afvm.vmg

afvm.vmg: afvm-inst.vmg afvm-super.vmg
	$(M4) afvm-inst.vmg >$@

afvm.tab.o: afvm.tab.c afvm-gen.i lex.yy.c afvm.h

support.o: support.c afvm.h

peephole.o: peephole.c afvm-peephole.i afvm.h

profile.o: profile.c afvm-profile.i afvm.h

disasm.o: disasm.c afvm-disasm.i afvm.h

engine.o: engine.c afvm-vm.i afvm-labels.i afvm.h

engine-debug.o: engine.c afvm-vm.i afvm-labels.i afvm.h
	$(CC) -DVM_DEBUG -DVM_PROFILING -Dengine=engine_debug -c -o $@ engine.c

clean:
	rm -f *.*~ *.o afvm afvm.exe afvm-*.i lex.yy.c afvm.tab.c afvm.vmg
	rm -f afvm.aux afvm.log afvm.out afvm.toc comment.cut texput.log

check:  afvm
	./afvm test.afvm | tr -d '\015' | diff - test.out

checkall:
	for i in 1 3 5 8 9 10; do make clean; echo $$i; make check VMGEN=vmgen CC="gcc -O3 -fomit-frame-pointer -Wall -DTHREADING_SCHEME=$$i"; echo $$i >>xxx-bench; \time afvm fib.afvm 2>>xxx-bench; done; cat xxx-bench; rm xxx-bench

#make profiles
%.prof: %.afvm afvm
	./afvm -p $< 2>$@
