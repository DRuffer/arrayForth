void gen_add(Inst **ctp)
{
  gen_inst(ctp, vm_prim[0]);
}
void gen_sub(Inst **ctp)
{
  gen_inst(ctp, vm_prim[1]);
}
void gen_mul(Inst **ctp)
{
  gen_inst(ctp, vm_prim[2]);
}
void gen_and(Inst **ctp)
{
  gen_inst(ctp, vm_prim[3]);
}
void gen_or(Inst **ctp)
{
  gen_inst(ctp, vm_prim[4]);
}
void gen_lessthan(Inst **ctp)
{
  gen_inst(ctp, vm_prim[5]);
}
void gen_equals(Inst **ctp)
{
  gen_inst(ctp, vm_prim[6]);
}
void gen_not(Inst **ctp)
{
  gen_inst(ctp, vm_prim[7]);
}
void gen_negate(Inst **ctp)
{
  gen_inst(ctp, vm_prim[8]);
}
void gen_lit(Inst **ctp, Cell i)
{
  gen_inst(ctp, vm_prim[9]);
  genarg_i(ctp, i);
}
void gen_drop(Inst **ctp)
{
  gen_inst(ctp, vm_prim[10]);
}
void gen_print(Inst **ctp)
{
  gen_inst(ctp, vm_prim[11]);
}
void gen_branch(Inst **ctp, Inst * target)
{
  gen_inst(ctp, vm_prim[12]);
  genarg_target(ctp, target);
}
void gen_zbranch(Inst **ctp, Inst * target)
{
  gen_inst(ctp, vm_prim[13]);
  genarg_target(ctp, target);
}
void gen_call(Inst **ctp, Inst * target, Cell iadjust)
{
  gen_inst(ctp, vm_prim[14]);
  genarg_target(ctp, target);
  genarg_i(ctp, iadjust);
}
void gen_return(Inst **ctp, Cell iadjust)
{
  gen_inst(ctp, vm_prim[15]);
  genarg_i(ctp, iadjust);
}
void gen_loadlocal(Inst **ctp, Cell ioffset)
{
  gen_inst(ctp, vm_prim[16]);
  genarg_i(ctp, ioffset);
}
void gen_storelocal(Inst **ctp, Cell ioffset)
{
  gen_inst(ctp, vm_prim[17]);
  genarg_i(ctp, ioffset);
}
void gen_end(Inst **ctp)
{
  gen_inst(ctp, vm_prim[18]);
}
