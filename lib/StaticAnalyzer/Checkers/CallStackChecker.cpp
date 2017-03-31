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

    class CallStackChecker : public Checker<check::ASTCodeBody> {
    public:
        void checkASTCodeBody(const Decl *D, AnalysisManager &mgr, BugReporter &BR) const;
        static std::string formatIntegerLiteral(const IntegerLiteral *IL);
        static void printToken(std::string inString, CheckerContext &C);
        static void printTokens(LitVecTy litVector, const Decl *D);
        static void printSuccTokens(LitVecTy litVecOne, LitVecTy litVecTwo);
        static void iterateOver(Stmt *S, const Decl *D, LitVecTy &ilVec, LitVecTy &slVec);
        static void obtainBlockLits(CFGBlock *block, const Decl *D, LitVecTy &ilVec, LitVecTy &slVec);
        static void writeToFile(StringRef Filename, LitVecTy tokens);
        static void writeSuccToFile(StringRef Filename, LitVecTy tokens, LitVecTy succTokens);
    };
} // end anonymous namespace

std::string CallStackChecker::formatIntegerLiteral(const IntegerLiteral *IL) {
    std::string inString = IL->getValue().toString(16, false);
    if (!inString.empty())
#if 0
        inString.insert(0, "0x");
#else
    {
        size_t inStrLen = inString.size();
        if (inStrLen % 2) {
            inString.insert(0, "0");
            inStrLen++;
        }
        for (size_t i = 0; i < (2 * inStrLen); i += 4) {
            inString.insert(i, "\\x");
        }
    }
#endif
    return inString;
}

void CallStackChecker::printToken(std::string inString, CheckerContext &C) {
    const NamedDecl *FD = dyn_cast<NamedDecl>(C.getCurrentAnalysisDeclContext()->getDecl());

    llvm::errs() << "\n" << inString << " in " << (FD ? FD->getQualifiedNameAsString() :
                                                   "null") << "\n";
}

void CallStackChecker::writeToFile(StringRef Filename, LitVecTy tokens) {
    std::string content;
    std::error_code EC;
    llvm::raw_fd_ostream fileStream(Filename, EC, llvm::sys::fs::F_Append);

    for (auto token: tokens)
#if 0
        content.append(token + "\n");
#else
        content.append("\"" + token + "\"" + "\n");
#endif

    if (EC)
        llvm::errs() << EC.message() << "\n";
    fileStream << content;
    fileStream.close();
    if (fileStream.has_error())
        llvm::errs() << "Error writing to file\n";
}

void CallStackChecker::writeSuccToFile(StringRef Filename, LitVecTy tokens, LitVecTy succTokens) {
    std::string content;
    std::error_code EC;
    llvm::raw_fd_ostream fileStream(Filename, EC, llvm::sys::fs::F_Append);

    for (auto token: tokens)
        for (auto succToken : succTokens)
            content.append(token + "\t" + succToken + "\n");

    if (EC)
        llvm::errs() << EC.message() << "\n";
    fileStream << content;
    fileStream.close();
    if (fileStream.has_error())
        llvm::errs() << "Error writing to file\n";
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

void CallStackChecker::printSuccTokens(LitVecTy litVecOne, LitVecTy litVecTwo) {
    for (auto inStrOne : litVecOne)
        for (auto inStrTwo : litVecTwo)
            llvm::errs() << "\n" << inStrOne << "," << inStrTwo << "\n";
}

void CallStackChecker::iterateOver(Stmt *S, const Decl *D, LitVecTy &intLitVector, LitVecTy &strLitVector) {
    for (Stmt *SubStmt : S->children()) {
        if (SubStmt) {
            SubStmt = SubStmt->IgnoreImplicit();
            if (IntegerLiteral *IL = dyn_cast<IntegerLiteral>(SubStmt)) {
                std::string inString = formatIntegerLiteral(IL);
                if (inString.empty())
                    continue;
                intLitVector.push_back(inString);
            }
            else if (StringLiteral *SL = dyn_cast<StringLiteral>(SubStmt)) {
                std::string inString = SL->getString();
                if (inString.empty())
                    continue;
                strLitVector.push_back(inString);
            }
            iterateOver(SubStmt, D, intLitVector, strLitVector);
        }
    }
}

void CallStackChecker::obtainBlockLits(CFGBlock *block, const Decl *D, LitVecTy &intLitVector,
                                        LitVecTy &strLitVector) {
    // Check if block has a terminator condition containing a integer/string literal
    if (!block->getTerminator() || !block->getTerminatorCondition())
        return;

    Stmt *termCond = block->getTerminatorCondition()->IgnoreImplicit();
    // Obtain Integer/string literals (if any) in terminator condition
    iterateOver(termCond, D, intLitVector, strLitVector);
    return;
}

void CallStackChecker::checkASTCodeBody(const Decl *D, AnalysisManager &mgr, BugReporter &BR) const {

    CFG *cfg = mgr.getCFG(D);
    if (!cfg)
        return;

    for (CFG::iterator it = cfg->begin(), ei = cfg->end(); it != ei; ++it) {
        // Present CFG block
        CFGBlock *block = *it;

        // List of literals in present CFG block/its dominator
        LitVecTy blockIntLiterals, blockStrLiterals;
        LitVecTy succIntLiterals, succStrLiterals;

        obtainBlockLits(block, D, blockIntLiterals, blockStrLiterals);
        writeToFile("./Int.csv", blockIntLiterals);
        writeToFile("./Str.csv", blockStrLiterals);

        // Print all tokens in successors of the present CFG block
        for (CFGBlock *succ : block->succs()) {
            if (succ) {
                obtainBlockLits(succ, D, succIntLiterals, succStrLiterals);
                writeSuccToFile("./Int.csv", blockIntLiterals, succIntLiterals);
                writeSuccToFile("./Str.csv", blockStrLiterals, succStrLiterals);
            }
        }
    }
}

// Register Checker
void ento::registerCallStackChecker(CheckerManager &mgr) {
    mgr.registerChecker<CallStackChecker>();
}