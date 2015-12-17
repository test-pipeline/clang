//== FunctionSummary.h - Stores summaries of functions. ------------*- C++ -*-//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines a summary of a function gathered/used by static analysis.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_FUNCTIONSUMMARY_H
#define LLVM_CLANG_STATICANALYZER_CORE_PATHSENSITIVE_FUNCTIONSUMMARY_H

#include "clang/Basic/LLVM.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallBitVector.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/TaintTag.h"

#include <deque>

namespace clang {
class Decl;

namespace ento {
typedef std::deque<Decl*> SetOfDecls;
typedef llvm::DenseSet<const Decl*> SetOfConstDecls;

class FunctionSummariesTy {
  class FunctionSummary {
  public:
    /// Marks the IDs of the basic blocks visited during the analyzes.
    llvm::SmallBitVector VisitedBasicBlocks;

    /// Total number of blocks in the function.
    unsigned TotalBasicBlocks : 30;

    /// True if this function has been checked against the rules for which
    /// functions may be inlined.
    unsigned InlineChecked : 1;

    /// True if this function may be inlined.
    unsigned MayInline : 1;

    /// The number of times the function has been inlined.
    unsigned TimesInlined : 32;

    /* We need a multi-map to support multiple types of taint on
     * a single Decl. For the moment, we only support one taint
     * (one of possibly many types) per Decl.
     */
    typedef llvm::DenseMap<const Decl *, TaintTagType> TLDTaintMapTy;
    typedef std::pair<const Decl *, TaintTagType> DTPair;

    TLDTaintMapTy TLDTaintMap;

    void addTaint(const Decl *D, TaintTagType Kind = TaintTagIPA) {

      // Is Decl already in Taint map?
      TLDTaintMapTy::iterator I = TLDTaintMap.find(D);
      if (I != TLDTaintMap.end())
	return;

      // Taint
      I = TLDTaintMap.insert(DTPair(D, Kind)).first;
      assert(I != TLDTaintMap.end());
    }

    bool isTainted(const Decl *D, TaintTagType Kind = TaintTagIPA) {
      if(!D)
	return false;

      bool Tainted = false;
      TLDTaintMapTy::iterator I = TLDTaintMap.find(D);
      if (I != TLDTaintMap.end() && I->second == Kind)
	Tainted = true;

      return Tainted;
    }

    FunctionSummary() :
      TotalBasicBlocks(0),
      InlineChecked(0),
      TimesInlined(0) {}
  };

public:
  typedef llvm::DenseMap<const Decl *, FunctionSummary> MapTy;
private:
  MapTy Map;

public:
  MapTy::iterator findOrInsertSummary(const Decl *D) {
    MapTy::iterator I = Map.find(D);
    if (I != Map.end())
      return I;

    typedef std::pair<const Decl *, FunctionSummary> KVPair;
    I = Map.insert(KVPair(D, FunctionSummary())).first;
    assert(I != Map.end());
    return I;
  }

  const MapTy &getMap(){
    const MapTy &MapConstRef = Map;
    return MapConstRef;
  }

  void addTaint(const Decl *FDecl, const Decl *TaintDecl) {
    MapTy::iterator I = findOrInsertSummary(FDecl);
    I->second.addTaint(TaintDecl);
  }

  bool isTainted(const Decl *FDecl, const Decl *isTaintedDecl) {
    MapTy::iterator I = Map.find(FDecl);
    if (I == Map.end())
      return false;

    return I->second.isTainted(isTaintedDecl);
  }

  void markMayInline(const Decl *D) {
    MapTy::iterator I = findOrInsertSummary(D);
    I->second.InlineChecked = 1;
    I->second.MayInline = 1;
  }

  void markShouldNotInline(const Decl *D) {
    MapTy::iterator I = findOrInsertSummary(D);
    I->second.InlineChecked = 1;
    I->second.MayInline = 0;
  }

  void markReachedMaxBlockCount(const Decl *D) {
    markShouldNotInline(D);
  }

  Optional<bool> mayInline(const Decl *D) {
    MapTy::const_iterator I = Map.find(D);
    if (I != Map.end() && I->second.InlineChecked)
      return I->second.MayInline;
    return None;
  }

  void markVisitedBasicBlock(unsigned ID, const Decl* D, unsigned TotalIDs) {
    MapTy::iterator I = findOrInsertSummary(D);
    llvm::SmallBitVector &Blocks = I->second.VisitedBasicBlocks;
    assert(ID < TotalIDs);
    if (TotalIDs > Blocks.size()) {
      Blocks.resize(TotalIDs);
      I->second.TotalBasicBlocks = TotalIDs;
    }
    Blocks.set(ID);
  }

  unsigned getNumVisitedBasicBlocks(const Decl* D) {
    MapTy::const_iterator I = Map.find(D);
    if (I != Map.end())
      return I->second.VisitedBasicBlocks.count();
    return 0;
  }

  unsigned getNumTimesInlined(const Decl* D) {
    MapTy::const_iterator I = Map.find(D);
    if (I != Map.end())
      return I->second.TimesInlined;
    return 0;
  }

  void bumpNumTimesInlined(const Decl* D) {
    MapTy::iterator I = findOrInsertSummary(D);
    I->second.TimesInlined++;
  }

  /// Get the percentage of the reachable blocks.
  unsigned getPercentBlocksReachable(const Decl *D) {
    MapTy::const_iterator I = Map.find(D);
      if (I != Map.end())
        return ((I->second.VisitedBasicBlocks.count() * 100) /
                 I->second.TotalBasicBlocks);
    return 0;
  }

  unsigned getTotalNumBasicBlocks();
  unsigned getTotalNumVisitedBasicBlocks();

};

}} // end clang ento namespaces

#endif
