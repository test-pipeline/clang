//== SMTConstraintManager.cpp -----------------------------------*- C++ -*--==//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//  This file defines SMTConstraintManager, a constraint manager that uses the
//  STP theorem prover to reason about conditions.
//
//===----------------------------------------------------------------------===//

#include "clang/StaticAnalyzer/Core/PathSensitive/APSIntType.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ConstraintManager.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ExprEngine.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramState.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/ProgramStateTrait.h"

#include "llvm/ADT/ImmutableSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/ErrorHandling.h"

#include <algorithm>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

#define Type STPType
#define Expr STPExpr
#define VC STPValidityChecker
#include "stp/c_interface.h"
#undef VC
#undef Expr
#undef Type


namespace {
  void STPErrorHandler(const char *message) {
    llvm::report_fatal_error("STP returned an error.");
  }
}


namespace clang {

namespace ento {


static BinaryOperator::Opcode OpcodeForSymExpr(const SymExpr *SE) {
  switch (SE->getKind()) {
    default:
      llvm_unreachable("SymExpr has no opcode");
    case SymExpr::SymIntKind:
      return cast<SymIntExpr>(SE)->getOpcode();
    case SymExpr::IntSymKind:
      return cast<IntSymExpr>(SE)->getOpcode();
    case SymExpr::SymSymKind:
      return cast<SymSymExpr>(SE)->getOpcode();
  }
}


static
BinaryOperator::Opcode OpcodeForNegatedComparison(BinaryOperator::Opcode Op) {
  switch (Op) {
    case BO_EQ: return BO_NE;
    case BO_NE: return BO_EQ;
    case BO_LT: return BO_GE;
    case BO_GT: return BO_LE;
    case BO_LE: return BO_GT;
    case BO_GE: return BO_LT;
    default:
      llvm_unreachable("Non-negatable comparison operator.");
  }
}


/*
This class encapsulates an STP query. It can also lazily construct
counterexamples.
*/
class STPBuilder;

class STPQuery {
  friend class STPBuilder;
  
public:
  bool areAxiomsConsistent() {
    /*
    Returns true if the axioms are consistent and satisfiable. 
    */
    runSolver();
    return !isInconsistent;
  }
  
  // TODO: We need a method of getting a counterexample from a query, so that
  // we can implement STPConstraintManager::getSymVal().

protected:
  STPBuilder &Builder;
  std::set<STPExpr> Asserts;
  
  bool isInconsistent;
  bool hasRunSolver;

  STPQuery(STPBuilder &B, std::set<STPExpr> &Axioms)
    : Builder(B), Asserts(Axioms), hasRunSolver(false)
    { }
  
  void runSolver();
};


/*
The STPBuilder is responsible for performing conversions between SymExprs and
STPExprs.
*/
class STPBuilder {
  friend class STPQuery;
  
public:
  STPBuilder(ASTContext &_AC, BasicValueFactory &_BVF)
    : AC(_AC), BVF(_BVF) {
      
    VC = vc_createValidityChecker();
    
    // Register our error handler
    vc_registerErrorHandler(::STPErrorHandler);
      
    /*
    KLEE writes,
      
    In newer versions of STP, a memory management mechanism has been introduced
    that automatically invalidates certain C interface pointers at vc_Destroy 
    time.  ... By setting EXPRDELETE to 0 we restore the old behaviour.
    */
    vc_setInterfaceFlags(VC, EXPRDELETE, 0);
  }
  
  ~STPBuilder() {
    // 
    
    /*
    FIXME: I'm not confident about memory management of STPExprs.
    
    An STPExpr is a pointer to a BEEV::ASTNode, whose destructor decrements the
    refcount of a BEEV::ASTInternal object, which is garbage collected.
    */
    
#if 0
    for (std::map<const SymExpr *, STPExpr>::iterator I = ExprMap.begin(),
         E = ExprMap.end(); I != E; ++I)
      vc_DeleteExpr(I->second);
    
    vc_Destroy(VC);
#endif
  }
  
  void addAxiom(const SymExpr *SE) {
    convertExpression(SE);
    Axioms.insert(SE);
  }
  
  template <typename iterator>
  void addImportantAxioms(iterator CandidatesBegin, iterator CandidatesEnd) {
    /*
    We consider an axiom "important" if it depends on some terminal symbol that
    another axiom in the set depends on.
    */
      
    // Construct the initial set of important terminals; this will expand as we
    // identify new important terminals.
    std::set<const SymExpr *> ImportantTerminals;
    for (std::set<const SymExpr *>::const_iterator I = Axioms.begin(),
         E = Axioms.end(); I != E; ++I) {
      
      std::set<const SymExpr *> Terminals = getTerminalSymbols(*I);
      ImportantTerminals.insert(Terminals.begin(), Terminals.end());
    }
    
    // Construct a mapping from candidate axioms to their terminal symbols.
    std::map<const SymExpr *, std::set<const SymExpr *> > TerminalsMap;
    
    for (iterator I = CandidatesBegin, E = CandidatesEnd; I != E; ++I)
      TerminalsMap.insert(std::make_pair(*I, getTerminalSymbols(*I)));
    
    // See if any candidate axiom's terminals intersect the important set.
    // If so, merge the sets.
    bool updated;
    do {
      updated = false;
      
      for (std::map<const SymExpr *, std::set<const SymExpr *> >::iterator
           I = TerminalsMap.begin(), E = TerminalsMap.end(); I != E; ++I) {
        
        // Go go gadget STL.
        std::set<const SymExpr *> Intersection;
        std::set_intersection(I->second.begin(), I->second.end(),
          ImportantTerminals.begin(), ImportantTerminals.end(),
          std::insert_iterator<std::set<const SymExpr *> >(Intersection,
            Intersection.end()));
        
        if (!Intersection.empty()) {
          addAxiom(I->first);
          ImportantTerminals.insert(I->second.begin(), I->second.end());
          TerminalsMap.erase(I);
          
          // Since we've updated the ImportantTerminals set, we need to start
          // the outer loop again, in case a new intersection arises.
          updated = true;
          break;
        }
      }
      
    } while (updated);
  }
  
  STPQuery evaluate() {
    // Build up an STPExpr set from our Axiom set of SymExprs.
    std::set<STPExpr> STPAxioms;
    
#if 0
    llvm::outs() << getAsCVC() << "\n";
#endif
    
    for (std::set<const SymExpr *>::iterator I = Axioms.begin(),
         E = Axioms.end(); I != E; ++I)
      STPAxioms.insert(ExprMap[*I]);
    
    return STPQuery(*this, STPAxioms);
  }
  
  std::string getAsCVC() {
    char *buffer = NULL;
    unsigned long len = 0;
    
    vc_push(VC);
    
    for (std::set<const SymExpr *>::const_iterator I = Axioms.begin(),
         E = Axioms.end(); I != E; ++I)
      vc_assertFormula(VC, ExprMap[*I]);
    
    vc_printQueryStateToBuffer(VC, vc_falseExpr(VC), &buffer, &len, false);
    assert(buffer && "STP failed to allocate buffer.");
    assert(len > 0 && "STP failed to write to buffer.");
    
    std::string resultStr = std::string(buffer, len);
    free(buffer);
    
    vc_pop(VC);
    
    return resultStr;
  }
  
  static std::set<const SymExpr *> getTerminalSymbols(const SymExpr *Root) {
    /*
    This visits each node in the SymExpr DAG and collects all of the SymbolData
    nodes.
    */
      
    std::set<const SymExpr *> Terminals;
    
    for (SymExpr::symbol_iterator I = Root->symbol_begin(),
         E = Root->symbol_end(); I != E; ++I) {
      if (isa<SymbolData>(*I))
        Terminals.insert(*I);
    }
    
    return Terminals;
  }
  
protected:
  ASTContext &AC;
  BasicValueFactory &BVF;
  STPValidityChecker VC;
  
  std::set<const SymExpr *> Axioms;
  std::map<const SymExpr *, STPExpr> ExprMap;
  
  STPExpr convertExpression(const SymExpr *SE) {
    /*
    Start here. If we've already performed this conversion, then we can just
    return the stored result. Otherwise, convert it and store the result.
    */
    
    std::map<const SymExpr *, STPExpr>::iterator I = ExprMap.find(SE);
    if (I != ExprMap.end())
      return I->second;
    
    STPExpr E;
    switch (SE->getKind()) {
      case SymExpr::ConjuredKind:
      case SymExpr::DerivedKind:
      case SymExpr::ExtentKind:
      case SymExpr::MetadataKind:
      case SymExpr::RegionValueKind:
        E = convertExpressionSpecialized(cast<SymbolData>(SE));
        break;
      case SymExpr::SymSymKind:
        E = convertExpressionSpecialized(cast<SymSymExpr>(SE));
        break;
      case SymExpr::SymIntKind:
        E = convertExpressionSpecialized(cast<SymIntExpr>(SE));
        break;
      case SymExpr::IntSymKind:
        E = convertExpressionSpecialized(cast<IntSymExpr>(SE));
        break;
      case SymExpr::CastSymbolKind:
        E = convertExpressionSpecialized(cast<SymbolCast>(SE));
        break;
    }
    
    ExprMap.insert(std::make_pair(SE, E));    
    return E;
  }
  
  STPExpr convertExpressionSpecialized(const SymbolData *SD) {
    return buildVariable(SD);
  }
  
  STPExpr convertExpressionSpecialized(const SymSymExpr *SE) {
    STPExpr LHS = convertExpression(SE->getLHS());
    STPExpr RHS = convertExpression(SE->getRHS());
    return buildOperator(SE, SE->getOpcode(), LHS, RHS);
  }
  
  STPExpr convertExpressionSpecialized(const SymIntExpr *SE) {
    STPExpr LHS = convertExpression(SE->getLHS());
    STPExpr RHS = buildInteger(SE->getRHS());
    return buildOperator(SE, SE->getOpcode(), LHS, RHS);
  }
  
  STPExpr convertExpressionSpecialized(const IntSymExpr *SE) {
    STPExpr LHS = buildInteger(SE->getLHS());
    STPExpr RHS = convertExpression(SE->getRHS());
    return buildOperator(SE, SE->getOpcode(), LHS, RHS);
  }
  
  STPExpr convertExpressionSpecialized(const SymbolCast *SC) {
    STPExpr Operand = convertExpression(SC->getOperand());
    return buildCast(SC->getType(), SC->getOperand()->getType(), Operand);
  }
  
  STPExpr
  buildCast(const QualType &From, const QualType &To, STPExpr Operand) {
    uint64_t ResultTypeSize = AC.getTypeSize(To);
    uint64_t InputTypeSize = AC.getTypeSize(From);
    
    bool isToSignedType = To->isSignedIntegerOrEnumerationType();
    
    // Nop if the cast doesn't change the width of the type.
    if (ResultTypeSize == InputTypeSize)
      return Operand;
    
    // Sign extend if we're casting to a signed data type.
    if (isToSignedType)
      return vc_bvSignExtend(VC, Operand, ResultTypeSize);
    
    // Zero extend if we're casting to a larger unsigned data type.
    if (ResultTypeSize > InputTypeSize && !isToSignedType)
      return vc_bvConcatExpr(VC, vc_bvConstExprFromInt(VC, ResultTypeSize -
        InputTypeSize, 0), Operand);
    
    // Truncate if we're casting to a smaller unsigned data type.
    return vc_bvExtract(VC, Operand, ResultTypeSize - 1, 0);
  }
  
  STPExpr buildInteger(const llvm::APSInt &X) {
    // FIXME: use APSInt::toString()?
    llvm::SmallString<64> S;
    
    for (unsigned i = X.getBitWidth(); i >= 1; -- i)
      S.append(1, X[i - 1] ? '1' : '0');
    
    return vc_bvConstExprFromStr(VC, S.c_str());
  }
  
  STPExpr buildVariable(const SymExpr *SE) {
    assert(isa<SymbolData>(SE));
    
    std::string varName = "Var_" + cast<SymbolData>(SE)->getSymbolID();
    llvm::Twine name = llvm::Twine(varName);
    
    STPType T = vc_bvType(VC, AC.getTypeSize(SE->getType()));
    STPExpr E = vc_varExpr(VC, name.str().c_str(), T);
    vc_DeleteExpr(T);
    
    return E;
  }
  
  void
  hackishlyFixOperands(const SymExpr *SE, BinaryOperator::Opcode Op,
    STPExpr &LHS, STPExpr &RHS) {
    
    /*
    !! SUPER FIXME !!
    
    Ideally all SymExprs would have operands of the same bit width, but it seems
    this isn't the case.
    
    It's possible that the signedness checks here are irrelevant. Honestly I
    just kept tweaking it until STP stopped choking.
    */
    
    QualType ResultType = SE->getType();
    
    if (isa<SymSymExpr>(SE)) {
      QualType LHSType = cast<SymSymExpr>(SE)->getLHS()->getType();
      QualType RHSType = cast<SymSymExpr>(SE)->getRHS()->getType();
      
      bool isLHSSameSize = (AC.getTypeSize(LHSType) ==
        AC.getTypeSize(ResultType));
      bool isRHSSameSize = (AC.getTypeSize(RHSType) ==
        AC.getTypeSize(ResultType));
      
      if (!isLHSSameSize)
        LHS = buildCast(LHSType, ResultType, LHS);
      if (!isRHSSameSize)
        RHS = buildCast(RHSType, ResultType, RHS);
    } else if (isa<SymIntExpr>(SE)) {
      QualType LHSType = cast<SymIntExpr>(SE)->getLHS()->getType();
      const llvm::APSInt &RHSInt = cast<SymIntExpr>(SE)->getRHS();
      
      bool isLHSSameSize = (AC.getTypeSize(LHSType) ==
        AC.getTypeSize(ResultType));
      bool isRHSSameSize = (RHSInt.getBitWidth() ==
        AC.getTypeSize(ResultType));
      
      if (!isLHSSameSize)
        LHS = buildCast(LHSType, ResultType, LHS);
      if (!isRHSSameSize) {
        vc_DeleteExpr(RHS);
        RHS = buildInteger(RHSInt.extOrTrunc(AC.getTypeSize(ResultType)));
      }
    } else if (isa<IntSymExpr>(SE)) {
      const llvm::APSInt &LHSInt = cast<IntSymExpr>(SE)->getLHS();
      QualType RHSType = cast<IntSymExpr>(SE)->getRHS()->getType();
      
      bool isLHSSameSize = (LHSInt.getBitWidth() ==
        AC.getTypeSize(ResultType));
      bool isRHSSameSize = (AC.getTypeSize(RHSType) ==
        AC.getTypeSize(ResultType));
      
      if (!isLHSSameSize) {
        vc_DeleteExpr(LHS);
        LHS = buildInteger(LHSInt.extOrTrunc(AC.getTypeSize(ResultType)));
      }
      if (!isRHSSameSize)
        RHS = buildCast(RHSType, ResultType, RHS);
    }
  }
  
  STPExpr
  buildOperator(const SymExpr *SE, BinaryOperator::Opcode Op, STPExpr LHS,
    STPExpr RHS) {
      
    // See comment within this method, above. FIXME FIXME FIXME
    hackishlyFixOperands(SE, Op, LHS, RHS);
    
    // Many of STP's functions require knowing the exact width of the result.
    uint64_t outWidth = AC.getTypeSize(SE->getType());
    
    // On with the actual operator construction.
    STPExpr E = NULL;
    switch (Op) {
      default:
        llvm_unreachable("Unimplemented opcode.");
      
      /* Arithmetic operators */
      
      case BO_Mul:
        E = vc_bvMultExpr(VC, outWidth, LHS, RHS);
        break;
      
      case BO_Div:
        if (SE->getType()->isSignedIntegerOrEnumerationType())
          E = vc_sbvDivExpr(VC, outWidth, LHS, RHS);
        else
          E = vc_bvDivExpr(VC, outWidth, LHS, RHS);
        break;
      
      case BO_Rem:
        if (SE->getType()->isSignedIntegerOrEnumerationType())
          E = vc_sbvModExpr(VC, outWidth, LHS, RHS);
        else
          E = vc_bvModExpr(VC, outWidth, LHS, RHS);
        break;
      
      case BO_Add:
        E = vc_bvPlusExpr(VC, outWidth, LHS, RHS);
        break;
        
      case BO_Sub:
        E = vc_bvMinusExpr(VC, outWidth, LHS, RHS);
        break;
      
      /* Shift operators */
      
      // FIXME: c_interface.h claims these can handle shifts with symbolic
      // operands, but I don't think this is true. These could be reimplemented
      // using sign/zero extension and concatenation.
      
      case BO_Shl:
        E = vc_bvLeftShiftExprExpr(VC, outWidth, LHS, RHS);
        break;
        
      case BO_Shr:
        if (SE->getType()->isSignedIntegerOrEnumerationType())
          E = vc_bvSignedRightShiftExprExpr(VC, outWidth, LHS, RHS);
        else
          E = vc_bvRightShiftExprExpr(VC, outWidth, LHS, RHS);
        break;
      
      /* Boolean comparison operators */
      
      case BO_LT:
        E = vc_bvLtExpr(VC, LHS, RHS);
        break;
      
      case BO_GT:
        E = vc_bvGtExpr(VC, LHS, RHS);
        break;
        
      case BO_LE:
        E = vc_bvLeExpr(VC, LHS, RHS);
        break;
        
      case BO_GE:
        E = vc_bvGeExpr(VC, LHS, RHS);
        break;
        
      case BO_EQ:
        E = vc_eqExpr(VC, LHS, RHS);
        break;
        
      case BO_NE:
        E = vc_notExpr(VC, vc_eqExpr(VC, LHS, RHS));
        break;
      
      /* Bitwise operators */
      
      case BO_And:
        E = vc_bvAndExpr(VC, LHS, RHS);
        break;
        
      case BO_Or:
        E = vc_bvOrExpr(VC, LHS, RHS);
        break;
        
      case BO_Xor:
        E = vc_bvXorExpr(VC, LHS, RHS);
        break;
        
      // TODO: Bitwise not (~), unary minus (-)
    }
    
    /*
    For division and modulo with a symbolic divisor, the divisor must be
    nonzero. This should be done automatically by the DivZeroChecker, but if
    there was an implicit truncation, then the assertion that the full-width
    value is nonzero does not imply that the truncated value is nonzero.
    */
#if 0
    if ((Op == BO_Rem || Op == BO_Div) &&
      (isa<IntSymExpr>(SE) || isa<SymSymExpr>(SE))) {
      
      // TODO: We need to pass this upwards so that we can ultimately combine
      // the top-level STPExpr with the assertion that these are nonzero.
      
      
    }
#endif
    
    assert(E && "Failed to build operator.");
    return E;
  }
  
};


void STPQuery::runSolver() {
  if (hasRunSolver)
    return;
  
  vc_push(Builder.VC);
    
  for (std::set<STPExpr>::iterator I = Asserts.begin(), E = Asserts.end();
       I != E; ++I)
    vc_assertFormula(Builder.VC, *I);
    
  int solverResult = vc_query(Builder.VC, vc_falseExpr(Builder.VC));
  assert(solverResult == 0 || solverResult == 1);
    
  vc_pop(Builder.VC);
  
  isInconsistent = (solverResult == 1);
  hasRunSolver = true;
}


/*
VerySimpleConstraintManager implements assume() for conditions other than
nonloc::SymbolVal; these are canonicalized and then passed to the subclass to
handle.

This could be eliminated with refactoring of the ConstraintManager base class
and/or SimpleConstraintManager.
*/
class VerySimpleConstraintManager : public ConstraintManager {
  SubEngine *SubEng;
  SValBuilder &Builder;
  
public:
  VerySimpleConstraintManager(SubEngine *subengine, SValBuilder &svb)
    : SubEng(subengine), Builder(svb) { }
  
  ProgramStateRef assume(ProgramStateRef state, DefinedSVal Cond,
                         bool Assumption) override {
    if (Cond.getAs<NonLoc>())
      state = assume(state, *Cond.getAs<NonLoc>(), Assumption);
    else
      state = assume(state, *Cond.getAs<Loc>(), Assumption);
    
    if (SubEng && NotifyAssumeClients)
      state = SubEng->processAssume(state, Cond, Assumption);
    
    return state;
  }
  
  ProgramStateRef assume(ProgramStateRef state, Loc Cond, bool Assumption) {
    switch (Cond.getSubKind())
    {
      default:
        llvm_unreachable("'Assume' not implemented for this Loc");
      
      case loc::GotoLabelKind:
        return Assumption ? state : NULL;
      
      case loc::MemRegionKind:
        return Assumption ? state : NULL;
      
      case loc::ConcreteIntKind: {
        bool b = Cond.getAs<loc::ConcreteInt>()->getValue() != 0;
        bool isFeasible = b ? Assumption : !Assumption;
        return isFeasible ? state : NULL;
      }
    }
  }
  
  ProgramStateRef assume(ProgramStateRef state, NonLoc Cond, bool Assumption) {
    switch (Cond.getSubKind())
    {
      default:
        llvm_unreachable("'Assume' not implemented for this NonLoc");
        break;
      
      case nonloc::ConcreteIntKind: {
        bool b = Cond.getAs<nonloc::ConcreteInt>()->getValue() != 0;
        bool isFeasible = b ? Assumption : !Assumption;
        return isFeasible ? state : NULL;
      }
      
      case nonloc::LocAsIntegerKind: {
        Optional<nonloc::LocAsInteger> LAI = Cond.getAs<nonloc::LocAsInteger>();
        return assume(state, LAI->getLoc(), Assumption);
      }
      
      case nonloc::SymbolValKind: {
        Optional<nonloc::SymbolVal> SV = Cond.getAs<nonloc::SymbolVal>();
        return assumeSymExpr(state, SV->getSymbol(), Assumption);
      }
      
      case nonloc::CompoundValKind:
      case nonloc::LazyCompoundValKind:
        llvm_unreachable("Unhandled NonLoc encountered.");
    }
  }
  
  ProgramStateRef assumeSymExpr(ProgramStateRef state, const SymExpr *SE,
                                bool Assumption) {
                           
    /*
    This canonicalizes the given SymExpr by constructing an explicit boolean
    expression as follows:
    
      Given a boolean expression and assumption of 'true', the input SymExpr is
      unmodified.
    
      Given a boolean expression and assumption of 'false', a new SymExpr is
      constructed with the inverse boolean operator (== -> !=, etc.).
    
      For other SymExprs, we turn a 'true' assumption into SE != 0 and a 'false'
      assumption into SE == 0.
    
    This canonicalization simplifies our downstream constraint managers, but
    does have memory impact.
    */
    
    BinaryOperator::Opcode Op;
    bool NeedsComparisonToZero;
    
    // First, determine whether this is already an explicit comparison.
    if (isa<IntSymExpr>(SE) || isa<SymIntExpr>(SE) || isa<SymSymExpr>(SE)) {
      Op = OpcodeForSymExpr(SE);
      NeedsComparisonToZero = !BinaryOperator::isComparisonOp(Op);
    } else {
      NeedsComparisonToZero = true;
    }
    
    // If we're lacking an explicit comparison, we will add a comparison to
    // zero.
    if (NeedsComparisonToZero) {
      QualType T = SE->getType();
      
      // We can't reason about anything other than integral types and pointers.
      // Anything else (in particular floats and doubles) we will just pretend
      // the state is feasible and move on. (FIXME)
      if (!(T->isIntegralOrEnumerationType() || Loc::isLocType(T)))
        return state;
      
      const llvm::APSInt &RHS = Builder.getBasicValueFactory().getValue(0, T);
      SE = Builder.getSymbolManager().getSymIntExpr(SE,
        (Assumption ? BO_NE : BO_EQ), RHS, T);
    }
    
    // If we have an explicit comparison, but we're assuming the condition is
    // false, then we negate the comparison operator.
    if (!NeedsComparisonToZero && !Assumption)
      SE = negateComparison(SE);
    
    return assume(state, SE);
  }
  
protected:
  const SymExpr *negateComparison(const SymExpr *SE) {
    assert(isa<IntSymExpr>(SE) || isa<SymIntExpr>(SE) || isa<SymSymExpr>(SE));
    
    BinaryOperator::Opcode Op = OpcodeForSymExpr(SE);
    BinaryOperator::Opcode NotOp = OpcodeForNegatedComparison(Op);
      
    switch (SE->getKind()) {
      default:
        llvm_unreachable("Unexpected SymExpr::Kind");
        break;
      case SymExpr::SymIntKind: {
        const SymIntExpr *SE2 = cast<SymIntExpr>(SE);
        SE = Builder.getSymbolManager().getSymIntExpr(SE2->getLHS(),
          NotOp, SE2->getRHS(), SE2->getType());
        break;
      }
      case SymExpr::IntSymKind: {
        const IntSymExpr *SE2 = cast<IntSymExpr>(SE);
        SE = Builder.getSymbolManager().getIntSymExpr(SE2->getLHS(),
          NotOp, SE2->getRHS(), SE2->getType());
        break;
      }
      case SymExpr::SymSymKind: {
        const SymSymExpr *SE2 = cast<SymSymExpr>(SE);
        SE = Builder.getSymbolManager().getSymSymExpr(SE2->getLHS(),
          NotOp, SE2->getRHS(), SE2->getType());
        break;
      }
    }
    
    return SE;
  }
  
public:
  /* Subclasses *must* implement these! */
  
  // In the future I think this might be not be pure virtual; VerySimple-
  // ConstraintManager can definitely answer affirmatively about the SVals it
  // reasons about, and then the subclass can handle the rest.
  virtual bool canReasonAbout(SVal X) const = 0;
  
  virtual
  ProgramStateRef assume(ProgramStateRef state, const SymExpr *SE) = 0;
  
  
  /* Subclasses *should* implement these; otherwise they're nops. */
  
  ProgramStateRef
  removeDeadBindings(ProgramStateRef state, SymbolReaper &SymReaper) {
    return state;
  }
  
  void
  print(ProgramStateRef state, raw_ostream &Out, const char *nl,
    const char *sep) { }
  
  const llvm::APSInt* getSymVal(ProgramStateRef state, SymbolRef sym) const {
    return NULL;
  }

};



class STPConstraintManager;
typedef llvm::ImmutableSet<const SymExpr *> AxiomSet;

template<>
struct ProgramStateTrait<STPConstraintManager>
  : public ProgramStatePartialTrait<AxiomSet> {
  static void *GDMIndex() { static int index = 0; return &index; }
};


/*
STPConstraintManager uses the STP solver to determine whether a given condition
is consistent with previous assumptions.
*/
class STPConstraintManager : public VerySimpleConstraintManager {
  ProgramStateManager &SM;
  
public:
  STPConstraintManager(SubEngine *subengine, ProgramStateManager &StMgr)
    : VerySimpleConstraintManager(subengine, StMgr.getSValBuilder()), SM(StMgr)
    { }

  bool canReasonAbout(SVal X) const {
    /*
    What can STP reason about? Good question!
    
    TODO: STP cannot reason about:
      - Binary shifts where the right-hand operand is symbolic
      - Floats and doubles
    */
    return true;
  }
  
  ProgramStateRef assume(ProgramStateRef state, const SymExpr *SE) override {
    // Add the constraint to the state's log
    state = state->add<STPConstraintManager>(SE);
    
    // Now generate the STP input for this
    STPBuilder Builder(SM.getContext(), SM.getBasicVals());
    Builder.addAxiom(SE);
    
    // Add additional constraints as axioms, if they are relevant to the primary
    // axiom. The STPBuilder takes care of eliminating superfluous axioms.
    const AxiomSet &Constraints = state->get<STPConstraintManager>();
    Builder.addImportantAxioms(Constraints.begin(), Constraints.end());
    
    // Now ask STP to reason about it...
    STPQuery result = Builder.evaluate(); 
    if (!result.areAxiomsConsistent())
      return NULL;
    
    // TODO: Control this with a heuristic and/or command line switch
    state = minimizeAxiomSet(state);
    
    return state;
  }
  
  ProgramStateRef minimizeAxiomSet(ProgramStateRef state) {
    /*
    Attempt to remove redundant axioms, namely those that are implied by others.
    This is an expensive computation, as it requires invoking STP once per
    axiom.
    
    Note that some minimization is already done in ConstraintManager::
    assumeDual() to prevent accumulation of redundant axioms as in:
    
      if (x == 3)
        if (x > 0)
        
    However it's more difficult to remove axioms that became redundant later:
    
      if (x > 0)
        if (x == 3)
    */
    
    const AxiomSet &Axioms = state->get<STPConstraintManager>();
    std::vector<const SymExpr *> Constraints(Axioms.begin(), Axioms.end());
    
    while (!Constraints.empty()) {
      const SymExpr *Current = Constraints.back();
      Constraints.pop_back();
      
      STPBuilder Builder(SM.getContext(), SM.getBasicVals());
      
      /*
      Idea for a possible optimization:
      If this constraint's terminal set does not intersect any other's, then
      running STP is unnecessary.
      */
      
      // Assert all of the other axioms.
      for (std::vector<const SymExpr *>::iterator I = Constraints.begin(),
           E = Constraints.end(); I != E; ++I)
        Builder.addAxiom(*I);
      
      // Now construct a SymExpr representing the negation of the current
      // expression, and assert that.
      const SymExpr *Negated = negateComparison(Current);
      Builder.addAxiom(Negated);
      
      // Ask STP if the constraints are satisfiable; if they are not, then
      // the removed axiom was implied by the others.
      STPQuery result = Builder.evaluate();
      if (!result.areAxiomsConsistent())
        state = state->remove<STPConstraintManager>(Current);
    }
    
    return state;
  }
  
  const llvm::APSInt* getSymVal(ProgramStateRef state, SymbolRef sym) const {
    /* 
    If sym is known to have a concrete integer value, we can return it here.
    
    Outline of the process:
      1. Declare a bitvector X for the given symbol.
      2. Assert the axioms on X.
      3. Declare a bitvector X' of the same width as X.
      4. Assert the same axioms on X'.
      5. Assert X != X'.
      6. If these axioms are consistent, then return NULL because the symbol
         does not have a concrete value.
      7. Otherwise, ask STP for a counterexample. This should be the value of
         the symbol.
    
    Note that at (7) the inconsistency of the axioms implies that there are 0
    or 1 values that X can hold. However, we would not have reached this state
    if X were not already satisfiable, so this guarantees that there is a
    solution.
    
    This doesn't integrate nicely with the STPBuilder, though.
    */
    
    return NULL;
  }
  
  ProgramStateRef
  removeDeadBindings(ProgramStateRef state, SymbolReaper &SymReaper) {
    /*
    At some point, this should remove constraints from the GDM for symbols that
    are considered dead.
    */
    
    AxiomSet Axioms = state->get<STPConstraintManager>();
    AxiomSet::Factory &Factory = state->get_context<STPConstraintManager>();
    
    for (AxiomSet::iterator I = Axioms.begin(), E = Axioms.end();
         I != E; ++I) {
      
      // Get the terminal symbols referenced by this assertion.
      std::set<const SymExpr *> Terminals = STPBuilder::getTerminalSymbols(*I);
      
      // If any of these is considered 'dead', then we should remove the axiom.
      for (std::set<const SymExpr *>::const_iterator II = Terminals.begin(),
           EE = Terminals.end(); II != EE; ++II) {
        
        if (SymReaper.maybeDead(*II))
          Axioms = Factory.remove(Axioms, *I);
        
      }
    }
    
    /*
    There's no need to minimize the axiom set here: removing an indepedent
    axiom does not affect the independence of remaining axioms.
    */
    
    return state->set<STPConstraintManager>(Axioms);
  }
  
  void
  print(ProgramStateRef state, raw_ostream &Out, const char *nl,
        const char *sep) {
    
    STPBuilder Builder(SM.getContext(), SM.getBasicVals());
        
    // Assert all of the other axioms.
    const AxiomSet &Axioms = state->get<STPConstraintManager>();
    for (AxiomSet::iterator I = Axioms.begin(), E = Axioms.end(); I != E; ++I)
      Builder.addAxiom(*I);
    
    // Write the CVC program to a StringStream, then read it back out by lines.
    std::stringstream SS;
    SS << Builder.getAsCVC();
    
    Out << nl << sep;
    
    std::string Line;
    while (SS.good()) {
      std::getline(SS, Line);
      Out << nl << Line;
    }
    
    Out << nl;
  }
  
};

} // end of namespace ento

} // end of namespace clang

extern "C" {
  std::unique_ptr<clang::ento::ConstraintManager>
  CreateSMTConstraintManager(clang::ento::ProgramStateManager &StMgr,
                             clang::ento::SubEngine *Eng) {
    return llvm::make_unique<clang::ento::STPConstraintManager>(Eng, StMgr);
  }
}
