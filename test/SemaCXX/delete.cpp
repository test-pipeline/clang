// RUN: %clang_cc1 -fsyntax-only -verify %s
// RUN: cp %s %t
// RUN: %clang_cc1 -fixit -x c++ %t
// RUN: %clang_cc1 -E -o - %t | FileCheck %s

void f(int a[10][20]) {
  // CHECK: delete[] a;
  delete a; // expected-warning {{'delete' applied to a pointer-to-array type}}
}

namespace MissingInitializer {
template<typename T>
struct Base {
  struct S {
    const T *p1 = nullptr;
    const T *p2 = new T[3];
  };
};

void null_init(Base<double>::S s) {
  delete s.p1;
  delete s.p2;
}
}

#ifndef WITH_PCH
pch_test::X::X()
  : a(new int[1])  // expected-note{{allocated with 'new[]' here}}
{ }
pch_test::X::X(int i)
  : a(new int[i])  // expected-note{{allocated with 'new[]' here}}
{ }
#endif
