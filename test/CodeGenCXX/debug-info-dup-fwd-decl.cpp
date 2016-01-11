// RUN: %clang_cc1 -emit-llvm -debug-info-kind=standalone -triple x86_64-apple-darwin %s -o - | FileCheck %s

class Test
{
public:
    Test () : reserved (new data()) {}

    unsigned
    getID() const
    {
        return reserved->objectID;
    }
protected:
    struct data {
        unsigned objectID;
    };
    data* reserved;
};

Test t;

// CHECK: ; [ DW_TAG_pointer_type ]
// CHECK: ; [ DW_TAG_structure_type ] [data]
// CHECK-NOT: ; [ DW_TAG_structure_type ] [data]
