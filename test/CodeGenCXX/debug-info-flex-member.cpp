// RUN: %clang_cc1 -emit-llvm -debug-info-kind=limited -triple x86_64-apple-darwin %s -o - | FileCheck %s

// CHECK:  !"0x21\000\00-1"}        ; [ DW_TAG_subrange_type ]

struct StructName {
  int member[];
};

struct StructName SN;
