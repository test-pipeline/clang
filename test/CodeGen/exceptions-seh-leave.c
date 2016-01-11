// RUN: %clang_cc1 %s -triple x86_64-pc-win32 -fms-extensions -fnew-ms-eh -emit-llvm -o - | opt -instnamer -S | FileCheck %s

// FIXME: Rewrite CHECKs for unnamed BBs and Insts.
// REQUIRES: asserts

void g();

//////////////////////////////////////////////////////////////////////////////
// __leave with __except

// Nothing in the __try block can trap, so __try.cont isn't created.
int __leave_with___except_simple() {
  int myres = 0;
  __try {
    myres = 15;
    __leave;
    myres = 23;
  } __except (1) {
    return 0;
  }
  return 1;
}
// CHECK-LABEL: define i32 @__leave_with___except_simple()
// CHECK: store i32 15, i32* %myres
// CHECK-NEXT: br label %__try.__leave
// CHECK-NOT: store i32 23
// CHECK: __try.__leave:
// CHECK-NEXT: ret i32 1


// The "normal" case.
int __leave_with___except() {
  int myres = 0;
  __try {
    g();
    __leave;
    myres = 23;
  } __except (1) {
    return 0;
  }
  return 1;
}
// CHECK-LABEL: define i32 @__leave_with___except()
// CHECK: invoke void bitcast (void (...)* @g to void ()*)()
// CHECK-NEXT:       to label %[[cont:.*]] unwind label %{{.*}}
// For __excepts, instead of an explicit __try.__leave label, we could use
// use invoke.cont as __leave jump target instead.  However, not doing this
// keeps the CodeGen code simpler, __leave is very rare, and SimplifyCFG will
// simplify this anyways.
// CHECK: [[cont]]
// CHECK-NEXT: br label %__try.__leave
// CHECK-NOT: store i32 23
// CHECK: __try.__leave:
// CHECK-NEXT: br label %__try.cont


//////////////////////////////////////////////////////////////////////////////
// __leave with __finally

void abort(void) __attribute__((noreturn));

// Nothing in the __try block can trap, so __finally.cont and friends aren't
// created.
int __leave_with___finally_simple() {
  int myres = 0;
  __try {
    myres = 15;
    __leave;
    myres = 23;
  } __finally {
    return 0;
  }
  return 1;
}
// CHECK-LABEL: define i32 @__leave_with___finally_simple()
// CHECK: store i32 15, i32* %myres
// CHECK-NEXT: br label %__try.__leave
// CHECK-NOT: store i32 23
// CHECK: __try.__leave:
// CHECK-NEXT: store i8 0, i8* %abnormal.termination.slot
// CHECK-NEXT: br label %__finally

// __finally block doesn't return, __finally.cont doesn't exist.
int __leave_with___finally_noreturn() {
  int myres = 0;
  __try {
    myres = 15;
    __leave;
    myres = 23;
  } __finally {
    abort();
  }
  return 1;
}
// CHECK-LABEL: define i32 @__leave_with___finally_noreturn()
// CHECK: store i32 15, i32* %myres
// CHECK-NEXT: br label %__try.__leave
// CHECK-NOT: store i32 23
// CHECK: __try.__leave:
// CHECK-NEXT: store i8 0, i8* %abnormal.termination.slot
// CHECK-NEXT: br label %__finally

// The "normal" case.
int __leave_with___finally() {
  int myres = 0;
  __try {
    g();
    __leave;
    myres = 23;
  } __finally {
    return 0;
  }
  return 1;
}
// CHECK-LABEL: define i32 @__leave_with___finally()
// CHECK: invoke void bitcast (void (...)* @g to void ()*)()
// CHECK-NEXT:       to label %[[cont:.*]] unwind label %{{.*}}
// For __finally, there needs to be an explicit __try.__leave, because
// abnormal.termination.slot needs to be set there.
// CHECK: [[cont]]
// CHECK-NEXT: br label %__try.__leave
// CHECK-NOT: store i32 23
// CHECK: __try.__leave:
// CHECK-NEXT: store i8 0, i8* %abnormal.termination.slot
// CHECK-NEXT: br label %__finally


//////////////////////////////////////////////////////////////////////////////
// Mixed, nested cases.

// FIXME: Test with outer __finally once PR22553 is fixed.

int nested___except___finally() {
  int myres = 0;
  __try {
    __try {
      g();
    } __finally {
      g();
      __leave;  // Refers to the outer __try, not the __finally!
      myres = 23;
      return 0;
    }

    myres = 51;
  } __except (1) {
  }
  return 1;
}
// The order of basic blocks in the below doesn't matter.
// CHECK-LABEL: define i32 @nested___except___finally()

// CHECK-LABEL: invoke void bitcast (void (...)* @g to void ()*)()
// CHECK-NEXT:       to label %[[g1_cont:.*]] unwind label %[[g1_lpad:.*]]

// CHECK: [[g1_cont]]:
// CHECK-NEXT: store i8 0, i8* %abnormal.termination.slot
// CHECK-NEXT: br label %__finally

// CHECK-LABEL: __finally:
// CHECK-NEXT: invoke void bitcast (void (...)* @g to void ()*)() #3
// CHECK-NEXT:       to label %[[g2_cont:.*]] unwind label %[[g2_lpad:.*]]

// CHECK: [[g1_lpad]]
// CHECK-NEXT: cleanuppad
// CHECK-NEXT: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK-NEXT: invoke void @"\01?fin$0@0@nested___except___finally@@"(i8 1, i8* %[[fp]])
// CHECK-NEXT:       to label %[[g1_resume:.*]] unwind label %[[g2_lpad]]
// CHECK: cleanupret {{.*}} unwind label %[[g2_lpad]]

// CHECK: [[g2_lpad]]
// CHECK: catchpad {{.*}} [i8* null]
// CHECK: catchret
// CHECK: br label %[[trycont]]

// CHECK: [[g2_lpad]]:
// CHECK-NOT: %abnormal.termination.slot
// CHECK: br label %__except

// CHECK-LABEL: __except:
// CHECK-NEXT: br label %__try.cont

// CHECK-LABEL: __try.__leave:
// CHECK-NEXT: br label %__try.cont

int nested___except___except() {
  int myres = 0;
  __try {
    __try {
      g();
      myres = 16;
    } __except (1) {
      g();
      __leave;  // Refers to the outer __try, not the __except we're in!
      myres = 23;
      return 0;
    }

    myres = 51;
  } __except (1) {
  }
  return 1;
}
// The order of basic blocks in the below doesn't matter.
// CHECK-LABEL: define i32 @nested___except___except()

// CHECK-LABEL: invoke void bitcast (void (...)* @g to void ()*)()
// CHECK-NEXT:       to label %[[g1_cont:.*]] unwind label %[[g1_lpad:.*]]

// CHECK: [[g1_lpad]]
// CHECK: catchpad {{.*}} [i8* null]
// CHECK: catchret {{.*}} to label %[[except:[^ ]*]]
// CHECK: [[except]]
// CHECK: invoke void @g()
// CHECK-NEXT:       to label %[[g2_cont:.*]] unwind label %[[g2_lpad:.*]]

// CHECK: [[g2_lpad]]
// CHECK: catchpad {{.*}} [i8* null]
// CHECK: catchret
// CHECK: br label %[[trycont4:[^ ]*]]

// CHECK-LABEL: __except3:
// CHECK-NEXT: br label %__try.cont4

// CHECK: [[g2_cont]]
// CHECK-NEXT: br label %[[tryleave:[^ ]*]]
// CHECK-NOT: store i32 23

// CHECK: [[g1_cont]]
// CHECK: store i32 16, i32* %myres
// CHECK-NEXT: br label %[[trycont:[^ ]*]]

// CHECK: [[trycont]]
// CHECK-NEXT: store i32 51, i32* %myres
// CHECK-NEXT: br label %[[tryleave]]

// CHECK: [[tryleave]]
// CHECK-NEXT: br label %[[trycont4]]

int nested___finally___except() {
  int myres = 0;
  __try {
    __try {
      g();
    } __except (1) {
      g();
      __leave;  // Refers to the outer __try, not the __except!
      myres = 23;
      return 0;
    }

    myres = 51;
  } __finally {
  }
  return 1;
}
// The order of basic blocks in the below doesn't matter.
// CHECK-LABEL: define i32 @nested___finally___except()

// CHECK-LABEL: invoke void @g()
// CHECK-NEXT:       to label %[[g1_cont:.*]] unwind label %[[g1_lpad:.*]]

// CHECK: [[g1_lpad]]
// CHECK: catchpad
// CHECK: catchret
// CHECK: invoke void @g()
// CHECK-NEXT:       to label %[[g2_cont:.*]] unwind label %[[g2_lpad:.*]]

// CHECK: [[g2_cont]]
// CHECK: br label %[[tryleave:[^ ]*]]
// CHECK-NOT: 23

// CHECK: [[g1_cont]]
// CHECK-NEXT: br label %[[trycont:[^ ]*]]

// CHECK: [[trycont]]
// CHECK: store i32 51, i32* %
// CHECK-NEXT: br label %[[tryleave]]

// CHECK: [[tryleave]]
// CHECK: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK-NEXT: call void @"\01?fin$0@0@nested___finally___except@@"(i8 0, i8* %[[fp]])
// CHECK-NEXT: ret i32 1

// CHECK: [[g2_lpad]]
// CHECK: cleanuppad
// CHECK: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK-NEXT: call void @"\01?fin$0@0@nested___finally___except@@"(i8 1, i8* %[[fp]])
// CHECK: cleanupret {{.*}} unwind to caller

// CHECK-LABEL: define internal void @"\01?fin$0@0@nested___finally___except@@"(i8 %abnormal_termination, i8* %frame_pointer)
// CHECK: ret void

int nested___finally___finally() {
  int myres = 0;
  __try {
    __try {
      g();
      myres = 16;
    } __finally {
      g();
      __leave;  // Refers to the outer __try, not the __finally we're in!
      myres = 23;
      return 0;
    }

    myres = 51;
  } __finally {
  }
  return 1;
}
// The order of basic blocks in the below doesn't matter.
// CHECK-LABEL: define i32 @nested___finally___finally()

// CHECK: invoke void @g()
// CHECK-NEXT:       to label %[[g1_cont:.*]] unwind label %[[g1_lpad:.*]]

// CHECK: [[g1_cont]]
// CHECK: store i32 16, i32* %[[myres:[^ ]*]],
// CHECK: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK-NEXT: invoke void @"\01?fin$1@0@nested___finally___finally@@"(i8 0, i8* %[[fp]])
// CHECK-NEXT:       to label %[[finally_cont:.*]] unwind label %[[g2_lpad:.*]]

// CHECK: [[finally_cont]]
// CHECK: store i32 51, i32* %[[myres]]
// CHECK: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK-NEXT: call void @"\01?fin$0@0@nested___finally___finally@@"(i8 0, i8* %[[fp]])
// CHECK-NEXT: ret i32 1

// CHECK: [[g1_lpad]]
// CHECK-NEXT: %[[padtoken:[^ ]*]] = cleanuppad within none []
// CHECK-NEXT: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK-NEXT: invoke void @"\01?fin$1@0@nested___finally___finally@@"(i8 1, i8* %[[fp]])
// CHECK-NEXT:       to label %[[finally_cont2:.*]] unwind label %[[g2_lpad]]
// CHECK: [[finally_cont2]]
// CHECK: cleanupret from %[[padtoken]] unwind label %[[g2_lpad]]

// CHECK: [[g2_lpad]]
// CHECK-NEXT: %[[padtoken:[^ ]*]] = cleanuppad within none []
// CHECK-NEXT: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK-NEXT: call void @"\01?fin$0@0@nested___finally___finally@@"(i8 1, i8* %[[fp]])
// CHECK: cleanupret from %[[padtoken]] unwind to caller

// CHECK-LABEL: define internal void @"\01?fin$0@0@nested___finally___finally@@"(i8 %abnormal_termination, i8* %frame_pointer)
// CHECK: ret void

// CHECK-LABEL: __try.__leave:
// CHECK-NEXT: br label %__try.cont4
