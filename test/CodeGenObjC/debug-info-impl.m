// RUN: %clang_cc1 -triple x86_64-apple-darwin10 -debug-info-kind=limited -S -emit-llvm %s -o - | FileCheck %s
@interface NSObject {
  struct objc_object *isa;
}
@end

@interface Shape : NSObject

@end
@interface Circle : Shape

@end
@implementation Circle

@end
