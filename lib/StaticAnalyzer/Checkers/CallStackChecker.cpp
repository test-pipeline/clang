#include "ClangSACheckers.h"
#include "clang/StaticAnalyzer/Core/Checker.h"
#include "clang/StaticAnalyzer/Core/BugReporter/BugType.h"
#include "clang/StaticAnalyzer/Core/CheckerRegistry.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CheckerContext.h"
#include "clang/StaticAnalyzer/Core/PathSensitive/CallEvent.h"
#include "clang/Analysis/Analyses/Dominators.h"

#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::ento;

namespace {

    typedef llvm::SmallVector<std::string, 4> LitVecTy;

    class CallStackChecker : public Checker<check::PreCall,
                                            check::PreStmt<IntegerLiteral>,
                                            check::ASTCodeBody> {
    public:
        void checkPreCall(const CallEvent &Call, CheckerContext &C) const;
        void checkPreStmt(const IntegerLiteral *IL, CheckerContext &C) const;
        void checkASTCodeBody(const Decl *D, AnalysisManager &mgr, BugReporter &BR) const;
        mutable StringRef matchThis;
        static std::string formatIntegerLiteral(const IntegerLiteral *IL);
        static void printToken(std::string inString, CheckerContext &C);
        static void printTokens(LitVecTy litVector, const Decl *D);
        static void iterateOver(Stmt *S, const Decl *D, LitVecTy &litVector);
        static LitVecTy obtainBlockLits(CFGBlock *block, const Decl *D);
    };
} // end anonymous namespace

void CallStackChecker::checkPreCall(const CallEvent &Call, CheckerContext &C) const {

//    const NamedDecl *ND = dyn_cast<NamedDecl>(Call.getDecl());
//    if (!ND)
//        return;
//
//    if (!matchThis.equals(ND->getQualifiedNameAsString()))
//        return;
//
//    Call.dump();
//    const NamedDecl *FD = dyn_cast<NamedDecl>(C.getCurrentAnalysisDeclContext()->getDecl());
//    if (!FD)
//        return;
//
//    llvm::errs() << "\n" << FD->getQualifiedNameAsString() << "\n";
//    return;
}

void CallStackChecker::checkPreStmt(const IntegerLiteral *IL, CheckerContext &C) const {
//    std::string inString = formatIntegerLiteral(IL);
//    if (inString.empty())
//        return;
//
//    printToken(inString, C);
}

std::string CallStackChecker::formatIntegerLiteral(const IntegerLiteral *IL) {
    std::string inString = IL->getValue().toString(16, false);
    if (!inString.empty())
        inString.insert(0, "0x");
    return inString;
}

void CallStackChecker::printToken(std::string inString, CheckerContext &C) {
    const NamedDecl *FD = dyn_cast<NamedDecl>(C.getCurrentAnalysisDeclContext()->getDecl());

    llvm::errs() << "\n" << inString << " in " << (FD ? FD->getQualifiedNameAsString() :
                                                   "null") << "\n";
}

void CallStackChecker::printTokens(LitVecTy litVector, const Decl *D) {
    const NamedDecl *FD = dyn_cast<NamedDecl>(D);
    std::string functionName;
    if (FD)
        functionName = FD->getQualifiedNameAsString();
    else
        functionName = "null";

    for (std::string inString : litVector) {
        llvm::errs() << "\n" << inString << " in " << functionName << "\n";
    }

}

void CallStackChecker::iterateOver(Stmt *S, const Decl *D, LitVecTy &litVector) {
    for (Stmt *SubStmt : S->children()) {
        if (SubStmt) {
            if (IntegerLiteral *IL = dyn_cast<IntegerLiteral>(SubStmt->IgnoreImplicit())) {
                std::string inString = formatIntegerLiteral(IL);
                if (inString.empty())
                    continue;
                litVector.push_back(inString);
            }
            iterateOver(SubStmt->IgnoreImplicit(), D, litVector);
        }
    }
}

LitVecTy CallStackChecker::obtainBlockLits(CFGBlock *block, const Decl *D) {
    LitVecTy litVector;
    // Check if block has a terminator condition containing a integer/string literal
    if (!block->getTerminatorCondition())
        return litVector;

    Stmt *termCond = block->getTerminatorCondition()->IgnoreImplicit();
    // Obtain Integer/string literals (if any) in terminator condition
    iterateOver(termCond, D, litVector);
    return litVector;
}

void CallStackChecker::checkASTCodeBody(const Decl *D, AnalysisManager &mgr, BugReporter &BR) const {

    CFG *cfg = mgr.getCFG(D);
    if (!cfg)
        return;

    // Domtree data structure (required for obtaining immediate dom)
    DominatorTree dom;

    // Find immediate dom and obtain its Integer/string literals (if any) in term condition
    bool isADCNonNull = false;
    AnalysisDeclContext *AC = mgr.getAnalysisDeclContext(D);
    if (AC) {
        isADCNonNull = true;
        dom.buildDominatorTree(*AC);
    }

    for (CFG::iterator it = cfg->begin(), ei = cfg->end(); it != ei; ++it) {
        // Present CFG block
        CFGBlock *block = *it;
        // Present CFG block's immediate dominator
        CFGBlock *DomBlock = nullptr;

        // List of literals in present CFG block/its dominator
        LitVecTy blockLiterals, domBlockLiterals;

        blockLiterals = obtainBlockLits(block, D);
        printTokens(blockLiterals, D);

        if (!isADCNonNull)
            continue;

        DomBlock = dom.getIDom(block);
        if (!DomBlock)
            continue;

        domBlockLiterals = obtainBlockLits(DomBlock, D);
        printTokens(blockLiterals, D);
    }
}

// Register Checker
void ento::registerCallStackChecker(CheckerManager &mgr) {
    mgr.registerChecker<CallStackChecker>();
}

// Register plugin for out-of-tree builds!
//extern "C"
//void clang_registerCheckers(CheckerRegistry &registry) {
//  registry.addChecker<Line2ConstraintChecker>("custom.bs.l2c", "line2constraint checker");
//}
//
//extern "C"
//const char clang_analyzerAPIVersionString[] = CLANG_ANALYZER_API_VERSION_STRING;