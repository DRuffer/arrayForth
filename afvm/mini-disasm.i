if (VM_IS_INST(*ip, 0)) {
  fputs("add", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 1)) {
  fputs("sub", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 2)) {
  fputs("mul", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 3)) {
  fputs("and", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 4)) {
  fputs("or", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 5)) {
  fputs("lessthan", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 6)) {
  fputs("equals", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 7)) {
  fputs("not", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 8)) {
  fputs("negate", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 9)) {
  fputs("lit", vm_out);
{
Cell i;
vm_Cell2i(IMM_ARG(IPTOS,305397760 ),i);
fputs(" i=", vm_out); printarg_i(i);
}
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 10)) {
  fputs("drop", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 11)) {
  fputs("print", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 12)) {
  fputs("branch", vm_out);
{
Inst * target;
vm_Cell2target(IMM_ARG(IPTOS,305397761 ),target);
fputs(" target=", vm_out); printarg_target(target);
}
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 13)) {
  fputs("zbranch", vm_out);
{
Inst * target;
vm_Cell2target(IMM_ARG(IPTOS,305397762 ),target);
fputs(" target=", vm_out); printarg_target(target);
}
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 14)) {
  fputs("call", vm_out);
{
Inst * target;
vm_Cell2target(IMM_ARG(IPTOS,305397763 ),target);
fputs(" target=", vm_out); printarg_target(target);
}
{
Cell iadjust;
vm_Cell2i(IMM_ARG(IP[1],305397764 ),iadjust);
fputs(" iadjust=", vm_out); printarg_i(iadjust);
}
  ip += 3;
  goto _endif_;
}
if (VM_IS_INST(*ip, 15)) {
  fputs("return", vm_out);
{
Cell iadjust;
vm_Cell2i(IMM_ARG(IPTOS,305397765 ),iadjust);
fputs(" iadjust=", vm_out); printarg_i(iadjust);
}
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 16)) {
  fputs("loadlocal", vm_out);
{
Cell ioffset;
vm_Cell2i(IMM_ARG(IPTOS,305397766 ),ioffset);
fputs(" ioffset=", vm_out); printarg_i(ioffset);
}
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 17)) {
  fputs("storelocal", vm_out);
{
Cell ioffset;
vm_Cell2i(IMM_ARG(IPTOS,305397767 ),ioffset);
fputs(" ioffset=", vm_out); printarg_i(ioffset);
}
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 18)) {
  fputs("end", vm_out);
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 19)) {
  fputs("ll", vm_out);
{
Cell _IP0;
vm_Cell2Cell(IMM_ARG(IPTOS,305397768 ),_IP0);
fputs(" _IP0=", vm_out); printarg_Cell(_IP0);
}
{
Cell _IP1;
vm_Cell2Cell(IMM_ARG(IP[1],305397769 ),_IP1);
fputs(" _IP1=", vm_out); printarg_Cell(_IP1);
}
  ip += 3;
  goto _endif_;
}
