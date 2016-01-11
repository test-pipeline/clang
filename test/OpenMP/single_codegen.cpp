// RUN: %clang_cc1 -verify -fopenmp -fnoopenmp-use-tls -x c++ -triple x86_64-unknown-unknown -emit-llvm %s -fexceptions -fcxx-exceptions -o - | FileCheck %s
// RUN: %clang_cc1 -fopenmp -fnoopenmp-use-tls -x c++ -std=c++11 -triple x86_64-unknown-unknown -fexceptions -fcxx-exceptions -emit-pch -o %t %s
// RUN: %clang_cc1 -fopenmp -fnoopenmp-use-tls -x c++ -triple x86_64-unknown-unknown -fexceptions -fcxx-exceptions -std=c++11 -include-pch %t -verify %s -emit-llvm -o - | FileCheck %s
// RUN: %clang_cc1 -verify -triple x86_64-apple-darwin10 -fopenmp -fnoopenmp-use-tls -fexceptions -fcxx-exceptions -debug-info-kind=line-tables-only -x c++ -emit-llvm %s -o - | FileCheck %s --check-prefix=TERM_DEBUG
// RUN: %clang_cc1 -verify -fopenmp -fnoopenmp-use-tls -x c++ -std=c++11 -DARRAY -triple x86_64-apple-darwin10 -emit-llvm %s -o - | FileCheck -check-prefix=ARRAY %s
// expected-no-diagnostics

#ifndef HEADER
#define HEADER

// CHECK:       [[IDENT_T_TY:%.+]] = type { i32, i32, i32, i32, i8* }

// CHECK:       define void [[FOO:@.+]]()

void foo() {}

// CHECK-LABEL: @main
int main() {
  // CHECK:       [[A_ADDR:%.+]] = alloca i8
  char a;
  char a2[2];
  TestClass &c = tc;

// CHECK:       [[GTID:%.+]] = call i32 @__kmpc_global_thread_num([[IDENT_T_TY]]* [[DEFAULT_LOC:@.+]])
// CHECK:       [[RES:%.+]] = call i32 @__kmpc_single([[IDENT_T_TY]]* [[DEFAULT_LOC]], i32 [[GTID]])
// CHECK-NEXT:  [[IS_SINGLE:%.+]] = icmp ne i32 [[RES]], 0
// CHECK-NEXT:  br i1 [[IS_SINGLE]], label {{%?}}[[THEN:.+]], label {{%?}}[[EXIT:.+]]
// CHECK:       [[THEN]]
// CHECK-NEXT:  store i8 2, i8* [[A_ADDR]]
// CHECK-NEXT:  call void @__kmpc_end_single([[IDENT_T_TY]]* [[DEFAULT_LOC]], i32 [[GTID]])
// CHECK-NEXT:  br label {{%?}}[[EXIT]]
// CHECK:       [[EXIT]]
#pragma omp single
  a = 2;
// CHECK:       [[RES:%.+]] = call i32 @__kmpc_single([[IDENT_T_TY]]* [[DEFAULT_LOC]], i32 [[GTID]])
// CHECK-NEXT:  [[IS_SINGLE:%.+]] = icmp ne i32 [[RES]], 0
// CHECK-NEXT:  br i1 [[IS_SINGLE]], label {{%?}}[[THEN:.+]], label {{%?}}[[EXIT:.+]]
// CHECK:       [[THEN]]
// CHECK-NEXT:  call void [[FOO]]()
// CHECK-NEXT:  call void @__kmpc_end_single([[IDENT_T_TY]]* [[DEFAULT_LOC]], i32 [[GTID]])
// CHECK-NEXT:  br label {{%?}}[[EXIT]]
// CHECK:       [[EXIT]]
#pragma omp single
  a = 2;
// CHECK:       store i32 0, i32* [[DID_IT]]
// CHECK:       [[RES:%.+]] = call i32 @__kmpc_single([[IDENT_T_TY]]* [[DEFAULT_LOC]], i32 [[GTID]])
// CHECK-NEXT:  [[IS_SINGLE:%.+]] = icmp ne i32 [[RES]], 0
// CHECK-NEXT:  br i1 [[IS_SINGLE]], label {{%?}}[[THEN:.+]], label {{%?}}[[EXIT:.+]]
// CHECK:       [[THEN]]
// CHECK-NEXT:  invoke void [[FOO]]()
// CHECK:       to label {{%?}}[[CONT:.+]] unwind
// CHECK:       [[CONT]]
// CHECK:       store i32 1, i32* [[DID_IT]]
// CHECK:       call void @__kmpc_end_single([[IDENT_T_TY]]* [[DEFAULT_LOC]], i32 [[GTID]])
// CHECK-NEXT:  br label {{%?}}[[EXIT]]
// CHECK:       [[EXIT]]
// CHECK:       [[A_PTR_REF:%.+]] = getelementptr inbounds [5 x i8*], [5 x i8*]* [[COPY_LIST]], i{{[0-9]+}} 0, i{{[0-9]+}} 0
// CHECK:       store i8* [[A_ADDR]], i8** [[A_PTR_REF]],
// CHECK:       [[C_PTR_REF:%.+]] = getelementptr inbounds [5 x i8*], [5 x i8*]* [[COPY_LIST]], i{{[0-9]+}} 0, i{{[0-9]+}} 1
// CHECK:       store i8* {{.+}}, i8** [[C_PTR_REF]],
// CHECK:       [[TC_PTR_REF:%.+]] = getelementptr inbounds [5 x i8*], [5 x i8*]* [[COPY_LIST]], i{{[0-9]+}} 0, i{{[0-9]+}} 2
// CHECK:       [[TC_THREADPRIVATE_ADDR_VOID_PTR:%.+]] = call{{.*}} i8* @__kmpc_threadprivate_cached
// CHECK:       [[TC_THREADPRIVATE_ADDR:%.+]] = bitcast i8* [[TC_THREADPRIVATE_ADDR_VOID_PTR]] to [[TEST_CLASS_TY]]*
// CHECK:       [[TC_PTR_REF_VOID_PTR:%.+]] = bitcast [[TEST_CLASS_TY]]* [[TC_THREADPRIVATE_ADDR]] to i8*
// CHECK:       store i8* [[TC_PTR_REF_VOID_PTR]], i8** [[TC_PTR_REF]],
// CHECK:       [[A2_PTR_REF:%.+]] = getelementptr inbounds [5 x i8*], [5 x i8*]* [[COPY_LIST]], i{{[0-9]+}} 0, i{{[0-9]+}} 3
// CHECK:       [[BITCAST:%.+]] = bitcast [2 x i8]* [[A2_ADDR]] to i8*
// CHECK:       store i8* [[BITCAST]], i8** [[A2_PTR_REF]],
// CHECK:       [[TC2_PTR_REF:%.+]] = getelementptr inbounds [5 x i8*], [5 x i8*]* [[COPY_LIST]], i{{[0-9]+}} 0, i{{[0-9]+}} 4
// CHECK:       [[TC2_THREADPRIVATE_ADDR_VOID_PTR:%.+]] = call{{.*}} i8* @__kmpc_threadprivate_cached
// CHECK:       [[TC2_THREADPRIVATE_ADDR:%.+]] = bitcast i8* [[TC2_THREADPRIVATE_ADDR_VOID_PTR]] to [2 x [[TEST_CLASS_TY]]]*
// CHECK:       [[TC2_PTR_REF_VOID_PTR:%.+]] = bitcast [2 x [[TEST_CLASS_TY]]]* [[TC2_THREADPRIVATE_ADDR]] to i8*
// CHECK:       store i8* [[TC2_PTR_REF_VOID_PTR]], i8** [[TC2_PTR_REF]],
// CHECK:       [[COPY_LIST_VOID_PTR:%.+]] = bitcast [5 x i8*]* [[COPY_LIST]] to i8*
// CHECK:       [[DID_IT_VAL:%.+]] = load i32, i32* [[DID_IT]],
// CHECK:       call void @__kmpc_copyprivate([[IDENT_T_TY]]* [[DEFAULT_LOC]], i32 [[GTID]], i64 40, i8* [[COPY_LIST_VOID_PTR]], void (i8*, i8*)* [[COPY_FUNC:@.+]], i32 [[DID_IT_VAL]])
// CHECK-NOT:   call {{.+}} @__kmpc_cancel_barrier
#pragma omp single copyprivate(a, c, tc, a2, tc2)
  foo();
// CHECK-NOT:   call i32 @__kmpc_single
// CHECK-NOT:   call void @__kmpc_end_single
  return a;
}

#endif
