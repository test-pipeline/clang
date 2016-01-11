// RUN: %clang_cc1 -triple x86_64-apple-darwin10 -emit-llvm -debug-info-kind=limited %s -o - | FileCheck %s
__attribute((objc_root_class)) @interface NSObject {
	id isa;
}
+ (id)alloc;
- (id)init;
- (id)retain;
@end

void NSLog(id, ...);

@protocol MyProtocol

-(const char *)hello;

@end

@interface MyClass : NSObject {
}

@property (nonatomic, assign) id <MyProtocol> bad_carrier;
@property (nonatomic, assign) id good_carrier;

@end

@implementation MyClass
@end

int main()
{
    @autoreleasepool
    {
        MyClass *my_class = [MyClass alloc];
        NSLog(@"%p\n", my_class.bad_carrier);
        NSLog(@"%p\n", my_class.good_carrier);
    }
}
// Verify that the debug type for both variables is 'id'.
// CHECK:  !"0x101\00bad_carrier\00{{[0-9]+}}\000", !{{[0-9]+}}, null, ![[IDTYPE:[0-9]+]]} ; [ DW_TAG_arg_variable ] [bad_carrier] [line 0]
//
// CHECK:  !DILocalVariable(name: "bad_carrier", arg:
// CHECK-NOT:               line:
// CHECK-SAME:              type: ![[IDTYPE]]
//
// CHECK:  !DILocalVariable(name: "good_carrier", arg:
// CHECK-NOT:               line:
// CHECK-SAME:              type: ![[IDTYPE]]
