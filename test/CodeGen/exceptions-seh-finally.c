// RUN: %clang_cc1 %s -triple x86_64-pc-win32 -fms-extensions -emit-llvm -o - | FileCheck %s

void abort(void) __attribute__((noreturn));
void might_crash(void);
void cleanup(void);
int check_condition(void);
void basic_finally(void) {
  __try {
    might_crash();
  } __finally {
    cleanup();
  }
}

// CHECK-LABEL: define void @basic_finally()
// CHECK: invoke void @might_crash()
// CHECK:     to label %[[invoke_cont:[^ ]*]] unwind label %[[lpad:[^ ]*]]
//
// CHECK: [[invoke_cont]]
// CHECK: store i8 0, i8* %[[abnormal:[^ ]*]]
// CHECK: br label %[[finally:[^ ]*]]
//
// CHECK: [[finally]]
// CHECK: call void @cleanup()
// CHECK: load i8* %[[abnormal]]
// CHECK: icmp eq
// CHECK: br i1 %{{.*}}, label %[[finallycont:[^ ]*]], label %[[resumecont:[^ ]*]]
//
// CHECK: [[finallycont]]
// CHECK-NEXT: ret void
//
// CHECK: [[lpad]]
// CHECK-NEXT: %[[pad:[^ ]*]] = cleanuppad
// CHECK: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK: call void @"\01?fin$0@0@basic_finally@@"({{i8( zeroext)?}} 1, i8* %[[fp]])
// CHECK-NEXT: cleanupret from %[[pad]] unwind to caller

// Mostly check that we don't double emit 'r' which would crash.
void decl_in_finally(void) {
  __try {
    might_crash();
  } __finally {
    int r;
  }
}

// Ditto, don't crash double emitting 'l'.
void label_in_finally(void) {
  __try {
    might_crash();
  } __finally {
l:
    cleanup();
    if (check_condition())
      goto l;
  }
}

// CHECK-LABEL: define void @label_in_finally()
// CHECK: invoke void @might_crash()
// CHECK:     to label %[[invoke_cont:[^ ]*]] unwind label %[[lpad:[^ ]*]]
//
// CHECK: [[invoke_cont]]
// CHECK: store i8 0, i8* %[[abnormal:[^ ]*]]
// CHECK: br label %[[finally:[^ ]*]]
//
// CHECK: [[finally]]
// CHECK: br label %[[l:[^ ]*]]
//
// CHECK: [[l]]
// CHECK: call void @cleanup()
// CHECK: call i32 @check_condition()
// CHECK: br i1 {{.*}}, label
// CHECK: br label %[[l]]

int crashed;
void use_abnormal_termination(void) {
  __try {
    might_crash();
  } __finally {
    crashed = __abnormal_termination();
  }
}

// CHECK-LABEL: define void @use_abnormal_termination()
// CHECK: invoke void @might_crash()
// CHECK:     to label %[[invoke_cont:[^ ]*]] unwind label %[[lpad:[^ ]*]]
//
// CHECK: [[invoke_cont]]
// CHECK: store i8 0, i8* %[[abnormal:[^ ]*]]
// CHECK: br label %[[finally:[^ ]*]]
//
// CHECK: [[finally]]
// CHECK: load i8* %[[abnormal]]
// CHECK: zext i8 %{{.*}} to i32
// CHECK: store i32 %{{.*}}, i32* @crashed
// CHECK: load i8* %[[abnormal]]
// CHECK: icmp eq
// CHECK: br i1 %{{.*}}, label %[[finallycont:[^ ]*]], label %[[resumecont:[^ ]*]]
//
// CHECK: [[finallycont]]
// CHECK-NEXT: ret void
//
// CHECK: [[lpad]]
// CHECK-NEXT: %[[pad:[^ ]*]] = cleanuppad
// CHECK: %[[fp:[^ ]*]] = call i8* @llvm.localaddress()
// CHECK: call void @"\01?fin$0@0@use_abnormal_termination@@"({{i8( zeroext)?}} 1, i8* %[[fp]])
// CHECK-NEXT: cleanupret from %[[pad]] unwind to caller

void noreturn_noop_finally() {
  __try {
    __noop();
  } __finally {
    abort();
  }
}

// CHECK-LABEL: define void @noreturn_noop_finally()
// CHECK: store i8 0, i8* %
// CHECK: br label %[[finally:[^ ]*]]
// CHECK: [[finally]]
// CHECK: call void @abort()
// CHECK-NEXT: unreachable
// CHECK-NOT: load

void noreturn_finally() {
  __try {
    might_crash();
  } __finally {
    abort();
  }
}

// CHECK-LABEL: define void @noreturn_finally()
// CHECK: invoke void @might_crash()
// CHECK:     to label %[[cont:[^ ]*]] unwind label %[[lpad:[^ ]*]]
//
// CHECK: [[cont]]
// CHECK: store i8 0, i8* %
// CHECK: br label %[[finally:[^ ]*]]
//
// CHECK: [[lpad]]
// CHECK-NEXT: %[[pad:[^ ]*]] = cleanuppad
// CHECK: call void @"\01?fin$0@0@noreturn_finally@@"({{.*}})
// CHECK-NEXT: cleanupret from %[[pad]] unwind to caller

// CHECK: define internal void @"\01?fin$0@0@noreturn_finally@@"({{.*}})
// CHECK: call void @abort()
// CHECK-NEXT: unreachable
//
// CHECK: [[lpad]]
// CHECK-NEXT: %[[pad:[^ ]*]] = cleanuppad
// CHECK: call void @"\01?fin$0@0@nested___finally___finally@@"({{.*}})
// CHECK-NEXT: cleanupret from %[[pad]] unwind to caller

// CHECK-LABEL: define internal void @"\01?fin$0@0@nested___finally___finally@@"({{.*}})
// CHECK: ret void

// CHECK-LABEL: define internal void @"\01?fin$1@0@nested___finally___finally@@"({{.*}})
// CHECK: unreachable

// FIXME: Our behavior seems suspiciously different.

int nested___finally___finally_with_eh_edge() {
  __try {
    __try {
      might_crash();
    } __finally {
      return 899;
    }
  } __finally {
    // Intentionally no return here.
  }
  return 912;
}
// CHECK-LABEL: define i32 @nested___finally___finally_with_eh_edge
// CHECK: invoke void @might_crash()
// CHECK-NEXT: to label %[[invokecont:[^ ]*]] unwind label %[[lpad1:[^ ]*]]
//
// [[invokecont]]
// CHECK: invoke void @"\01?fin$1@0@nested___finally___finally_with_eh_edge@@"({{.*}})
// CHECK-NEXT:       to label %[[outercont:[^ ]*]] unwind label %[[lpad2:[^ ]*]]
//
// CHECK: [[outercont]]
// CHECK: call void @"\01?fin$0@0@nested___finally___finally_with_eh_edge@@"({{.*}})
// CHECK-NEXT: ret i32 912
//
// CHECK: [[lpad1]]
// CHECK-NEXT: %[[innerpad:[^ ]*]] = cleanuppad
// CHECK: invoke void @"\01?fin$1@0@nested___finally___finally_with_eh_edge@@"({{.*}})
// CHECK-NEXT:    label %[[innercleanupretbb:[^ ]*]] unwind label %[[lpad2:[^ ]*]]
//
// CHECK: [[innercleanupretbb]]
// CHECK-NEXT: cleanupret from %[[innerpad]] unwind label %[[lpad2]]
//
// CHECK: [[lpad2]]
// CHECK-NEXT: %[[outerpad:[^ ]*]] = cleanuppad
// CHECK: call void @"\01?fin$0@0@nested___finally___finally_with_eh_edge@@"({{.*}})
// CHECK-NEXT: cleanupret from %[[outerpad]] unwind to caller

// CHECK-LABEL: define internal void @"\01?fin$0@0@nested___finally___finally_with_eh_edge@@"({{.*}})
// CHECK: ret void

// CHECK-LABEL: define internal void @"\01?fin$1@0@nested___finally___finally_with_eh_edge@@"({{.*}})
// CHECK: unreachable
