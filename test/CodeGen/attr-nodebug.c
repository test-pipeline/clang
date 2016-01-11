// RUN: %clang_cc1 -debug-info-kind=limited -emit-llvm -o - %s | FileCheck %s

void t1() __attribute__((nodebug));

void t1()
{
  int a = 10;
  
  a++;
}

