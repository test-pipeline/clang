// RUN: echo "#include <stddef.h>" > %t.h
// RUN: %clang_cc1 -S -debug-info-kind=limited -include %t.h %s -emit-llvm -o - | FileCheck %s

// CHECK: !"0x34\00outer\00outer\00\00[[@LINE+1]]\000\001"
int outer = 42;

