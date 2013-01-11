if (VM_IS_INST(*ip, 0)) {
  add_inst(b, "add");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 1)) {
  add_inst(b, "sub");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 2)) {
  add_inst(b, "mul");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 3)) {
  add_inst(b, "and");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 4)) {
  add_inst(b, "or");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 5)) {
  add_inst(b, "lessthan");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 6)) {
  add_inst(b, "equals");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 7)) {
  add_inst(b, "not");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 8)) {
  add_inst(b, "negate");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 9)) {
  add_inst(b, "lit");
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 10)) {
  add_inst(b, "drop");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 11)) {
  add_inst(b, "print");
  ip += 1;
  goto _endif_;
}
if (VM_IS_INST(*ip, 12)) {
  add_inst(b, "branch");
  ip += 2;
  return;
}
if (VM_IS_INST(*ip, 13)) {
  add_inst(b, "zbranch");
  ip += 2;
  return;
}
if (VM_IS_INST(*ip, 14)) {
  add_inst(b, "call");
  ip += 3;
  return;
}
if (VM_IS_INST(*ip, 15)) {
  add_inst(b, "return");
  ip += 2;
  return;
}
if (VM_IS_INST(*ip, 16)) {
  add_inst(b, "loadlocal");
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 17)) {
  add_inst(b, "storelocal");
  ip += 2;
  goto _endif_;
}
if (VM_IS_INST(*ip, 18)) {
  add_inst(b, "end");
  ip += 1;
  return;
}
if (VM_IS_INST(*ip, 19)) {
  add_inst(b, "ll");
  ip += 3;
  goto _endif_;
}
