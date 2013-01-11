LABEL(add) /* add ( i1 i2 -- i ) */
/*  */
NAME("add")
{
DEF_CA
Cell i1;
Cell i2;
Cell i;
NEXT_P0;
vm_Cell2i(sp[1],i1);
vm_Cell2i(spTOS,i2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
fputs(" i2=", vm_out); printarg_i(i2);
}
#endif
sp += 1;
{
#line 40 "mini.vmg"
i = i1+i2;
#line 23 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(add)
NEXT_P2;
}

LABEL(sub) /* sub ( i1 i2 -- i ) */
/*  */
NAME("sub")
{
DEF_CA
Cell i1;
Cell i2;
Cell i;
NEXT_P0;
vm_Cell2i(sp[1],i1);
vm_Cell2i(spTOS,i2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
fputs(" i2=", vm_out); printarg_i(i2);
}
#endif
sp += 1;
{
#line 43 "mini.vmg"
i = i1-i2;
#line 59 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(sub)
NEXT_P2;
}

LABEL(mul) /* mul ( i1 i2 -- i ) */
/*  */
NAME("mul")
{
DEF_CA
Cell i1;
Cell i2;
Cell i;
NEXT_P0;
vm_Cell2i(sp[1],i1);
vm_Cell2i(spTOS,i2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
fputs(" i2=", vm_out); printarg_i(i2);
}
#endif
sp += 1;
{
#line 46 "mini.vmg"
i = i1*i2;
#line 95 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(mul)
NEXT_P2;
}

LABEL(and) /* and ( i1 i2 -- i ) */
/*  */
NAME("and")
{
DEF_CA
Cell i1;
Cell i2;
Cell i;
NEXT_P0;
vm_Cell2i(sp[1],i1);
vm_Cell2i(spTOS,i2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
fputs(" i2=", vm_out); printarg_i(i2);
}
#endif
sp += 1;
{
#line 49 "mini.vmg"
i = i1 & i2;
#line 131 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(and)
NEXT_P2;
}

LABEL(or) /* or ( i1 i2 -- i ) */
/*  */
NAME("or")
{
DEF_CA
Cell i1;
Cell i2;
Cell i;
NEXT_P0;
vm_Cell2i(sp[1],i1);
vm_Cell2i(spTOS,i2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
fputs(" i2=", vm_out); printarg_i(i2);
}
#endif
sp += 1;
{
#line 52 "mini.vmg"
i = i1 | i2;
#line 167 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(or)
NEXT_P2;
}

LABEL(lessthan) /* lessthan ( i1 i2 -- i ) */
/*  */
NAME("lessthan")
{
DEF_CA
Cell i1;
Cell i2;
Cell i;
NEXT_P0;
vm_Cell2i(sp[1],i1);
vm_Cell2i(spTOS,i2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
fputs(" i2=", vm_out); printarg_i(i2);
}
#endif
sp += 1;
{
#line 55 "mini.vmg"
i = i1<i2;
#line 203 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(lessthan)
NEXT_P2;
}

LABEL(equals) /* equals ( i1 i2 -- i ) */
/*  */
NAME("equals")
{
DEF_CA
Cell i1;
Cell i2;
Cell i;
NEXT_P0;
vm_Cell2i(sp[1],i1);
vm_Cell2i(spTOS,i2);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
fputs(" i2=", vm_out); printarg_i(i2);
}
#endif
sp += 1;
{
#line 58 "mini.vmg"
i = i1==i2;
#line 239 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(equals)
NEXT_P2;
}

LABEL(not) /* not ( i1 -- i2 ) */
/*  */
NAME("not")
{
DEF_CA
Cell i1;
Cell i2;
NEXT_P0;
vm_Cell2i(spTOS,i1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
}
#endif
{
#line 61 "mini.vmg"
i2 = !i1;
#line 271 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i2=", vm_out); printarg_i(i2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i2,spTOS);
LABEL2(not)
NEXT_P2;
}

LABEL(negate) /* negate ( i1 -- i2 ) */
/*  */
NAME("negate")
{
DEF_CA
Cell i1;
Cell i2;
NEXT_P0;
vm_Cell2i(spTOS,i1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i1=", vm_out); printarg_i(i1);
}
#endif
{
#line 64 "mini.vmg"
i2 = -i1;
#line 303 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i2=", vm_out); printarg_i(i2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i2,spTOS);
LABEL2(negate)
NEXT_P2;
}

LABEL(lit) /* lit ( #i -- i ) */
/*  */
NAME("lit")
{
DEF_CA
Cell i;
NEXT_P0;
IF_spTOS(sp[0] = spTOS);
vm_Cell2i(IMM_ARG(IPTOS,305397760 ),i);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i=", vm_out); printarg_i(i);
}
#endif
INC_IP(1);
sp += -1;
{
#line 67 "mini.vmg"
#line 336 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(lit)
NEXT_P2;
}

LABEL(drop) /* drop ( i -- ) */
/*  */
NAME("drop")
{
DEF_CA
Cell i;
NEXT_P0;
vm_Cell2i(spTOS,i);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i=", vm_out); printarg_i(i);
}
#endif
sp += 1;
{
#line 69 "mini.vmg"
#line 366 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
IF_spTOS(spTOS = sp[0]);
LABEL2(drop)
NEXT_P2;
}

LABEL(print) /* print ( i -- ) */
/*  */
NAME("print")
{
DEF_CA
Cell i;
NEXT_P0;
vm_Cell2i(spTOS,i);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i=", vm_out); printarg_i(i);
}
#endif
sp += 1;
{
#line 71 "mini.vmg"
printf("%ld\n", i);
#line 397 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
IF_spTOS(spTOS = sp[0]);
LABEL2(print)
NEXT_P2;
}

LABEL(branch) /* branch ( #target -- ) */
/*  */
NAME("branch")
{
DEF_CA
Inst * target;
NEXT_P0;
vm_Cell2target(IMM_ARG(IPTOS,305397761 ),target);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" target=", vm_out); printarg_target(target);
}
#endif
INC_IP(1);
{
#line 74 "mini.vmg"
SET_IP(target);
#line 428 "mini-vm.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
LABEL2(branch)
NEXT_P2;
}

LABEL(zbranch) /* zbranch ( #target i -- ) */
/*  */
NAME("zbranch")
{
DEF_CA
Inst * target;
Cell i;
NEXT_P0;
vm_Cell2target(IMM_ARG(IPTOS,305397762 ),target);
vm_Cell2i(spTOS,i);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" target=", vm_out); printarg_target(target);
fputs(" i=", vm_out); printarg_i(i);
}
#endif
INC_IP(1);
sp += 1;
{
#line 77 "mini.vmg"
if (i==0) {
  SET_IP(target);
  SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
IF_spTOS(spTOS = sp[0]);
NEXT_P2;

}
#line 476 "mini-vm.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
IF_spTOS(spTOS = sp[0]);
LABEL2(zbranch)
NEXT_P2;
}

LABEL(call) /* call ( #target #iadjust -- targetret aoldfp ) */
/*  */
NAME("call")
{
DEF_CA
Inst * target;
Cell iadjust;
Inst * targetret;
char * aoldfp;
NEXT_P0;
IF_spTOS(sp[0] = spTOS);
vm_Cell2target(IMM_ARG(IPTOS,305397763 ),target);
vm_Cell2i(IMM_ARG(IP[1],305397764 ),iadjust);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" target=", vm_out); printarg_target(target);
fputs(" iadjust=", vm_out); printarg_i(iadjust);
}
#endif
INC_IP(2);
sp += -2;
{
#line 103 "mini.vmg"
/* IF_spTOS(sp[2] = spTOS);*/ /* unnecessary; vmgen inserts a flush anyway */
targetret = IP;
SET_IP(target);
aoldfp = fp;
sp = (Cell *)(((char *)sp)+iadjust);
fp = (char *)sp;
/* IF_spTOS(spTOS = sp[0]); */ /* dead, thus unnecessary; vmgen copies aoldfp there */
#line 521 "mini-vm.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" targetret=", vm_out); printarg_target(targetret);
fputs(" aoldfp=", vm_out); printarg_a(aoldfp);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_target2Cell(targetret,sp[1]);
vm_a2Cell(aoldfp,spTOS);
LABEL2(call)
NEXT_P2;
}

LABEL(return) /* return ( #iadjust target afp i1 -- i2 ) */
/*  */
NAME("return")
{
DEF_CA
Cell iadjust;
Inst * target;
char * afp;
Cell i1;
Cell i2;
NEXT_P0;
vm_Cell2i(IMM_ARG(IPTOS,305397765 ),iadjust);
vm_Cell2target(sp[2],target);
vm_Cell2a(sp[1],afp);
vm_Cell2i(spTOS,i1);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" iadjust=", vm_out); printarg_i(iadjust);
fputs(" target=", vm_out); printarg_target(target);
fputs(" afp=", vm_out); printarg_a(afp);
fputs(" i1=", vm_out); printarg_i(i1);
}
#endif
INC_IP(1);
sp += 2;
{
#line 112 "mini.vmg"
/* IF_spTOS(sp[-2] = spTOS); */ /* unnecessary; that stack item is dead */
SET_IP(target);
sp = (Cell *)(((char *)sp)+iadjust);
fp = afp;
i2=i1;
/* IF_spTOS(spTOS = sp[0]); */ /* dead, thus unnecessary; vmgen copies i2 there */
#line 572 "mini-vm.i"
}
SUPER_END;

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i2=", vm_out); printarg_i(i2);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i2,spTOS);
LABEL2(return)
NEXT_P2;
}

LABEL(loadlocal) /* loadlocal ( #ioffset -- i ) */
/*  */
NAME("loadlocal")
{
DEF_CA
Cell ioffset;
Cell i;
NEXT_P0;
IF_spTOS(sp[0] = spTOS);
vm_Cell2i(IMM_ARG(IPTOS,305397766 ),ioffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ioffset=", vm_out); printarg_i(ioffset);
}
#endif
INC_IP(1);
sp += -1;
{
#line 123 "mini.vmg"
i = *(Cell *)(fp+ioffset);
#line 608 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
NEXT_P1;
vm_i2Cell(i,spTOS);
LABEL2(loadlocal)
NEXT_P2;
}

LABEL(storelocal) /* storelocal ( #ioffset i -- ) */
/*  */
NAME("storelocal")
{
DEF_CA
Cell ioffset;
Cell i;
NEXT_P0;
vm_Cell2i(IMM_ARG(IPTOS,305397767 ),ioffset);
vm_Cell2i(spTOS,i);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ioffset=", vm_out); printarg_i(ioffset);
fputs(" i=", vm_out); printarg_i(i);
}
#endif
INC_IP(1);
sp += 1;
{
#line 126 "mini.vmg"
*(Cell *)(fp+ioffset) = i;
#line 644 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
IF_spTOS(spTOS = sp[0]);
LABEL2(storelocal)
NEXT_P2;
}

LABEL(end) /* end ( i -- ) */
/*  */
NAME("end")
{
DEF_CA
Cell i;
NEXT_P0;
vm_Cell2i(spTOS,i);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i=", vm_out); printarg_i(i);
}
#endif
sp += 1;
{
#line 129 "mini.vmg"
/* SUPER_END would increment the next BB count (because IP points there);
   this would be a problem if there is no following BB.
   Instead, we do the following to add an end point for the current BB: */
#ifdef VM_PROFILING
block_insert(IP); /* we also do this at compile time, so this is unnecessary */
#endif
return i;
#line 681 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
NEXT_P1;
IF_spTOS(spTOS = sp[0]);
LABEL2(end)
NEXT_P2;
}

LABEL(ll)
{
DEF_CA
Cell MAYBE_UNUSED _IP0;
Cell MAYBE_UNUSED _IP1;
Cell MAYBE_UNUSED _sp0;
Cell MAYBE_UNUSED _sp1;
NEXT_P0;
IF_spTOS(sp[0] = spTOS);
/* loadlocal ( #ioffset -- i ) */
NAME("loadlocal")
{
Cell ioffset;
Cell i;
vm_Cell2i(IMM_ARG(IPTOS,305397768 ),ioffset);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" ioffset=", vm_out); printarg_i(ioffset);
}
#endif
sp += -2;
{
#line 129 "mini.vmg"
i = *(Cell *)(fp+ioffset);
#line 719 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputs(" i=", vm_out); printarg_i(i);
fputc('\n', vm_out);
}
#endif
vm_i2Cell(i,sp[1]);
}
/* lit ( #i -- i ) */
NAME("lit")
{
Cell i;
vm_Cell2i(IMM_ARG(IP[1],305397769 ),i);
#ifdef VM_DEBUG
if (vm_debug) {
fputs(" i=", vm_out); printarg_i(i);
}
#endif
INC_IP(2);
{
#line 129 "mini.vmg"
#line 743 "mini-vm.i"
}

#ifdef VM_DEBUG
if (vm_debug) {
fputs(" -- ", vm_out); fputc('\n', vm_out);
}
#endif
vm_i2Cell(i,spTOS);
}
NEXT_P1;
LABEL2(ll)
NEXT_P2;
}

