#pragma once
#ifndef __SYSU_OPTIMIZER_PLUGIN_HH_
#define __SYSU_OPTIMIZER_PLUGIN_HH_

#include <llvm/ADT/MapVector.h>
#include <llvm/IR/AbstractCallSite.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Pass.h>
#include <llvm/Passes/PassPlugin.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Analysis/BasicAliasAnalysis.h>
#include <llvm/Analysis/LoopAnalysisManager.h>
#include <llvm/IR/Dominators.h>

namespace sysu {

class StaticCallCounter : public llvm::AnalysisInfoMixin<StaticCallCounter> {
public:
  using Result = llvm::MapVector<const llvm::Function *, unsigned>;
  Result run(llvm::Module &M, llvm::ModuleAnalysisManager &);

private:
  // A special type used by analysis passes to provide an address that
  // identifies that particular analysis pass type.
  static llvm::AnalysisKey Key;
  friend struct llvm::AnalysisInfoMixin<StaticCallCounter>;
};

class StaticCallCounterPrinter
    : public llvm::PassInfoMixin<StaticCallCounterPrinter> {
public:
  explicit StaticCallCounterPrinter(llvm::raw_ostream &OutS) : OS(OutS) {}
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);

private:
  llvm::raw_ostream &OS;
};


class DCE : public llvm::PassInfoMixin<DCE> {
public:
  using Result = llvm::PreservedAnalyses;
  Result run(llvm::Function &M, llvm::FunctionAnalysisManager &FAM);
private:
  bool isProcessable(llvm::Instruction &I);
  bool isDead(llvm::Instruction &I);
  void removeAlloca(llvm::Function &F);
  void removeCommon(llvm::Function &F);
  void removeRecur(llvm::Instruction &I, llvm::DenseSet<llvm::Instruction*> &toRemove);
};


// class CSE : public llvm::PassInfoMixin<CSE> {
// public:
//   using Result = llvm::PreservedAnalyses;
//   Result run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM);

//   // explicit CSE() { my_DT = new llvm::DominatorTreeBase<llvm::BasicBlock, false>(); };
//   // ~CSE() { delete my_DT; };
// private:
//   // llvm::DominatorTreeBase<llvm::BasicBlock, false> *my_DT;

//   // llvm::BasicBlock* firstDomChild(llvm::BasicBlock &BB);
//   // llvm::BasicBlock* nextDomChild(llvm::BasicBlock &BB, llvm::BasicBlock &Child);

//   bool isCommonSubExp(llvm::Instruction &I, llvm::Instruction &J);
//   bool isProcessable(llvm::Instruction &I);
//   bool isDead(llvm::Instruction &I);
//   llvm::Instruction* storeCSE(llvm::Instruction &I, llvm::BasicAA::Result &AA);
//   void loadCSE(llvm::Instruction &I, llvm::BasicAA::Result &AA);
//   void commonCSE(llvm::BasicBlock &BB, llvm::Instruction &I, llvm::Instruction *next);

//   void runOnFunc(llvm::Function &func, llvm::BasicAA::Result &AA);
//   void runOnModule(llvm::Module &M, llvm::FunctionAnalysisManager &MAM);
// };

class CSE : public llvm::PassInfoMixin<CSE> {
public:
  using Result = llvm::PreservedAnalyses;
  // Result run(llvm::Module &F, llvm::ModuleAnalysisManager &MAM);
  Result run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);

  // explicit CSE() { my_DT = new llvm::DominatorTreeBase<llvm::BasicBlock, false>(); };
  // ~CSE() { delete my_DT; };
private:
  llvm::DominatorTreeBase<llvm::BasicBlock, false> *my_DT;
  llvm::DenseSet<llvm::Value*> CCodeDiv;

  llvm::BasicBlock* firstDomChild(llvm::BasicBlock &BB);
  llvm::BasicBlock* nextDomChild(llvm::BasicBlock &BB, llvm::BasicBlock &Child);

  bool isCommonSubExp(llvm::Instruction &I, llvm::Instruction &J);
  bool isProcessable(llvm::Instruction &I);
  bool isDead(llvm::Instruction &I);
  llvm::Instruction* storeCSE(llvm::Instruction &I, llvm::BasicAA::Result &AA);
  void loadCSE(llvm::Instruction &I, llvm::BasicAA::Result &AA);
  void commonCSE(llvm::BasicBlock &BB, llvm::Instruction &I, llvm::Instruction *next);

  void associativeBinOp(llvm::Instruction &I);

  void runOnFunc(llvm::Function &func, llvm::BasicAA::Result &AA);
  // void runOnModule(llvm::Module &M, llvm::FunctionAnalysisManager &MAM);
};



class InstSimplify : public llvm::PassInfoMixin<InstSimplify> {
public:
  using Result = llvm::PreservedAnalyses;
  Result run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
private:
  void MulSimply(llvm::Instruction &I, llvm::DenseSet<llvm::Instruction*> &worklist);
  void DivSimply(llvm::Instruction &I, llvm::DenseSet<llvm::Instruction*> &worklist);
};


class InstComb : public llvm::PassInfoMixin<InstComb> {
public:
  using Result = llvm::PreservedAnalyses;
  Result run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
private:
  void combine(llvm::Instruction *I);
};


// 写好了，但是不会用
class LICM : public llvm::PassInfoMixin<LICM> {
public:
  using Result = llvm::PreservedAnalyses;
  Result run(llvm::Loop &L, llvm::LoopAnalysisManager &LAM, llvm::LoopStandardAnalysisResults &LSAR);
private:
  void runOnLoop(llvm::Loop &L, llvm::LoopInfo &LI, llvm::DominatorTree &DT);
  void preOrder(llvm::DomTreeNode *N, llvm::SmallVector<llvm::BasicBlock*, 256> &worklist, llvm::Loop &L, llvm::DominatorTree &DT);
  bool isLoopInvariant(llvm::Loop &L, llvm::Instruction &I);
  bool canHoist(llvm::Loop &L, llvm::Instruction &I, llvm::DominatorTree &DT);
};


} // namespace sysu

extern "C" {
llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK llvmGetPassPluginInfo();
}

#endif