// RUN: %clang_cc1 -std=c++11 -emit-llvm -debug-info-kind=limited -triple x86_64-apple-darwin %s -o - | FileCheck %s
// Test (r)value and CVR qualifiers on C++11 non-static member functions.
class A {
public:
  // CHECK: !"0x2e\00l\00{{.*}}\00[[@LINE+2]]"{{, [^,]+, [^,]+}}, ![[PLSR:[0-9]+]], {{.*}}[ DW_TAG_subprogram ] [line [[@LINE+2]]] [public] [reference] [l]
  // CHECK: ![[PLSR]] ={{.*}}[ DW_TAG_subroutine_type ]{{.*}}[reference]
  void l() const &;
  // CHECK: ![[ARGS:[0-9]+]] = !{null, ![[THIS:[0-9]+]]}
  // CHECK: ![[THIS]] = {{.*}} ![[CONST_A:.*]]} ; [ DW_TAG_pointer_type ]
  // CHECK: ![[CONST_A]] = {{.*}} [ DW_TAG_const_type ]
  // CHECK: !"0x2e\00r\00{{.*}}\00[[@LINE+2]]"{{, [^,]+, [^,]+}}, ![[PRSR:[0-9]+]], {{.*}}[ DW_TAG_subprogram ] [line [[@LINE+2]]] [public] [rvalue reference] [r]
  // CHECK: ![[PRSR]] ={{.*}}![[ARGS]], null, null, null}{{.*}}[ DW_TAG_subroutine_type ]{{.*}}[rvalue reference]
  void r() const &&;
};

void g() {
  A a;
  // The type of pl is "void (A::*)() const &".
  // CHECK: !DILocalVariable(name: "pl",
  // CHECK-SAME:             line: [[@LINE+3]]
  // CHECK-SAME:             type: ![[PL:[0-9]+]]
  // CHECK: !DIDerivedType(tag: DW_TAG_ptr_to_member_type, baseType: ![[PLSR]]
  auto pl = &A::l;

  // CHECK: !DILocalVariable(name: "pr",
  // CHECK-SAME:             line: [[@LINE+3]]
  // CHECK-SAME:             type: ![[PR:[0-9]+]]
  // CHECK: !DIDerivedType(tag: DW_TAG_ptr_to_member_type, baseType: ![[PRSR]]
  auto pr = &A::r;
}
