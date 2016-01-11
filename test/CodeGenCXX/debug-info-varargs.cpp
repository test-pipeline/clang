// RUN: %clang_cc1 -triple %itanium_abi_triple -emit-llvm -debug-info-kind=limited %s -o - | FileCheck %s

struct A
{
  // CHECK-DAG:  !"0x2e\00a\00a\00_ZN1A1aEiz\00[[@LINE+1]]\00{{[^,]*}}"{{, [^,]+, [^,]+}}, ![[ATY:[0-9]+]]{{.*}}[ DW_TAG_subprogram ]{{.*}}[a]
  void a(int c, ...) {}
  // CHECK: ![[ATY]] ={{.*}} ![[AARGS:[0-9]+]], null, null, null} ; [ DW_TAG_subroutine_type ]
  // We no longer use an explicit unspecified parameter. Instead we use a trailing null to mean the function is variadic.
  // CHECK: ![[AARGS]] = !{null, !{{[0-9]+}}, !{{[0-9]+}}, null}
};

  // CHECK:  !"0x2e\00b\00b\00_Z1biz\00[[@LINE+1]]\00{{[^,]*}}"{{, [^,]+, [^,]+}}, ![[BTY:[0-9]+]]{{.*}}[ DW_TAG_subprogram ]{{.*}}[b]
void b(int c, ...) {
  // CHECK: ![[BTY]] ={{.*}} ![[BARGS:[0-9]+]], null, null, null} ; [ DW_TAG_subroutine_type ]
  // CHECK: ![[BARGS]] = !{null, !{{[0-9]+}}, null}

  A a;

  // CHECK: !DILocalVariable(name: "fptr"
  // CHECK-SAME:             line: [[@LINE+2]]
  // CHECK-SAME:             type: ![[PST:[0-9]+]]
  void (*fptr)(int, ...) = b;
  // CHECK: ![[PST]] ={{.*}} ![[BTY]]} ; [ DW_TAG_pointer_type ]
}
