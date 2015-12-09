//== StubSMTConstraintManager.cpp -------------------------------*- C++ -*--==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines implements a stub for CreateSMTConstraintManager() that
//  simply errors if it is invoked without the plug-in enabled.
//
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/ConstraintManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExprEngine.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"

#include "llvm/Support/DynamicLibrary.h"
#include "llvm/Support/ErrorHandling.h"


using llvm::sys::DynamicLibrary;


namespace clang {

namespace ento {

std::unique_ptr<ConstraintManager>
StubCreateSMTConstraintManager(ProgramStateManager &StMgr, SubEngine *Eng) {
  static ConstraintManagerCreator CMCreator = NULL;
  
  // Attempt to look up the real CreateSMTConstraintManager implementation;
  // cache the result for the future. 
  if (!CMCreator) {
    CMCreator = (ConstraintManagerCreator)(intptr_t)
      DynamicLibrary::SearchForAddressOfSymbol("CreateSMTConstraintManager");
  }
  
  if (CMCreator)
    return CMCreator(StMgr, Eng);
  
  llvm::report_fatal_error("The SMT constraint manager plug-in is not loaded.");
  return NULL;
}

} // end of namespace ento

} // end of namespace clang
