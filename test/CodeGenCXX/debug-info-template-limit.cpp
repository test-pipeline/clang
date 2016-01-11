// RUN: %clang_cc1 -emit-llvm -debug-info-kind=limited -triple %itanium_abi_triple %s -o - | FileCheck %s

// Check that this pointer type is TC<int>
// CHECK: ![[LINE:[0-9]+]] = !{!"0x2\00TC<int>\00{{.*}}", {{.*}} !"_ZTS2TCIiE"} ; [ DW_TAG_class_type ]
// CHECK: !"_ZTS2TCIiE"} ; [ DW_TAG_pointer_type ]{{.*}}[from _ZTS2TCIiE]

template<typename T>
class TC {
public:
  TC(const TC &) {}
  TC() {}
};

TC<int> tci;
