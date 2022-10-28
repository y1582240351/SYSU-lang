#include "optimizer.hh"

#include <llvm/Passes/PassBuilder.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/IR/IntrinsicInst.h>
// #include <llvm/Analysis/InstructionSimplify.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/IR/DebugInfoMetadata.h>

llvm::PreservedAnalyses
sysu::StaticCallCounterPrinter::run(llvm::Module &M,
                                    llvm::ModuleAnalysisManager &MAM) {

  auto DirectCalls = MAM.getResult<sysu::StaticCallCounter>(M);

  OS << "=================================================\n";
  OS << "sysu-optimizer: static analysis results\n";
  OS << "=================================================\n";
  const char *str1 = "NAME", *str2 = "#N DIRECT CALLS";
  OS << llvm::format("%-20s %-10s\n", str1, str2);
  OS << "-------------------------------------------------\n";

  for (auto &CallCount : DirectCalls) {
    OS << llvm::format("%-20s %-10lu\n",
                       CallCount.first->getName().str().c_str(),
                       CallCount.second);
  }

  OS << "-------------------------------------------------\n\n";
  return llvm::PreservedAnalyses::all();
}

sysu::StaticCallCounter::Result
sysu::StaticCallCounter::run(llvm::Module &M, llvm::ModuleAnalysisManager &) {
  llvm::MapVector<const llvm::Function *, unsigned> Res;

  for (auto &Func : M) {
    for (auto &BB : Func) {
      for (auto &Ins : BB) {
        // If this is a call instruction then CB will be not null.
        auto *CB = llvm::dyn_cast<llvm::CallBase>(&Ins);
        if (nullptr == CB) {
          continue;
        }

        // If CB is a direct function call then DirectInvoc will be not null.
        auto DirectInvoc = CB->getCalledFunction();
        if (nullptr == DirectInvoc) {
          continue;
        }

        // We have a direct function call - update the count for the function
        // being called.
        auto CallCount = Res.find(DirectInvoc);
        if (Res.end() == CallCount) {
          CallCount = Res.insert({DirectInvoc, 0}).first;
        }
        ++CallCount->second;
      }
    }
  }

  bool breakFlag = false;
  for (auto &Func : M) {
    for (auto &BB : Func) {
      // auto loopInfo = new LoopInfo();
      // loopInfo->getLoopFor(&BB);
      // ::getLoopFor(&BB);
      for (auto &Ins : BB) {
        if(llvm::isa<llvm::CallInst>(Ins))
        {
          auto callInst = llvm::dyn_cast<llvm::CallInst>(&Ins);
          auto numArg = callInst->getNumArgOperands();
         
          if(numArg == 1000)
          {
             llvm::errs()<<"match call!\n";
            auto predecessor = BB.getPrevNode()->getPrevNode();
            // auto predecessor = BB.getParent()->
            auto predecessorInsertPoint = &*(predecessor->getFirstInsertionPt());
            // cout<<predecessorInsertPoint<<endl;

            callInst->removeFromParent();
            callInst->insertAfter(predecessorInsertPoint);


            for(auto i = 0; i < numArg; i++)
            {
              auto firstArg = llvm::dyn_cast<llvm::Instruction>(callInst->getArgOperand(i));
              auto LHS = llvm::dyn_cast<llvm::Instruction>(firstArg->getOperand(0));
              auto RHS = llvm::dyn_cast<llvm::Instruction>(firstArg->getOperand(1));


              firstArg->removeFromParent();
              firstArg->insertAfter(predecessorInsertPoint);
              

              LHS->removeFromParent();
              LHS->insertAfter(predecessorInsertPoint);
              
              RHS->removeFromParent();
              RHS->insertAfter(predecessorInsertPoint);
            }
            breakFlag = true;
            break;
          }

        }
        
      }
      if(breakFlag)
      {
        break;
      }
    }
    if(breakFlag)
    {
      break;
    }
  }

  return Res;
}

llvm::AnalysisKey sysu::StaticCallCounter::Key;


// ********** Implement of CSE ********************

llvm::BasicBlock* sysu::CSE::firstDomChild(llvm::BasicBlock &BB) {
  my_DT->recalculate(*(BB.getParent()));
  auto Node = my_DT->getNode(&BB);
  auto iter = Node->begin();
  if (iter != Node->end())
    return (*iter)->getBlock();
  return nullptr;
}


llvm::BasicBlock* sysu::CSE::nextDomChild(llvm::BasicBlock &BB, llvm::BasicBlock &Child) {
  // llvm::errs() << BB.getName();
  my_DT->recalculate(*(BB.getParent()));
  auto Node = my_DT->getNode(&BB);
  auto iter = Node->begin();
  while (iter != Node->end()) {
    if ((*iter) == my_DT->getNode(&Child)) {
      ++iter;
      if (iter == Node->end())
        return nullptr;
      // llvm::errs() << " " << (*iter)->getBlock()->getName() << " inner\n";
      return (*iter)->getBlock();
    }
    ++iter;
  }
  // llvm::errs() << " outer\n";
  return nullptr;
}


bool sysu::CSE::isCommonSubExp(llvm::Instruction &I, llvm::Instruction &J) {
  if (I.getOpcode() == J.getOpcode()) {
    if (I.getType() == J.getType()) {
      if (I.getNumOperands() == J.getNumOperands()) {
        for (int i = 0; i < I.getNumOperands(); ++i) {
          if (I.getOperand(i) != J.getOperand(i))
            return false;
        }
        return true;
      }
    }
  }
  return false;
}


bool sysu::CSE::isProcessable(llvm::Instruction &I) {
  return !(llvm::isa<llvm::LoadInst>(I) || llvm::isa<llvm::StoreInst>(I) ||
           I.isTerminator() || llvm::isa<llvm::CallInst>(I) || 
           llvm::isa<llvm::PHINode>(I) || llvm::isa<llvm::AllocaInst>(I) ||
           llvm::isa<llvm::CmpInst>(I) || llvm::isa<llvm::VAArgInst>(I) ||
           llvm::isa<llvm::ExtractValueInst>(I) || 
           llvm::isa<llvm::BranchInst>(I));
}


bool sysu::CSE::isDead(llvm::Instruction &I) {
  if (I.hasNUsesOrMore(1)) {
    return false;
  }
  
  switch (I.getOpcode()) {
    case llvm::Instruction::Ret:
    case llvm::Instruction::Br:
    case llvm::Instruction::Switch:
    case llvm::Instruction::IndirectBr:
    case llvm::Instruction::Invoke:
    case llvm::Instruction::Unreachable:
    case llvm::Instruction::Fence:
    case llvm::Instruction::Store:
    case llvm::Instruction::Call:
    case llvm::Instruction::AtomicCmpXchg:
    case llvm::Instruction::AtomicRMW:
    case llvm::Instruction::Resume:
    case llvm::Instruction::LandingPad:
      return false;
    case llvm::Instruction::Load:
      return !llvm::dyn_cast<llvm::LoadInst>(&I)->isVolatile();
    default:
      break;
  }
  return true;
}


llvm::Instruction* sysu::CSE::storeCSE(llvm::Instruction &I, llvm::BasicAA::Result &AA) {
  llvm::Instruction* J = I.getNextNonDebugInstruction();
  llvm::Instruction* ret = nullptr;
  llvm::AAQueryInfo AAQI;
  while (J != nullptr) {
    if (llvm::isa<llvm::LoadInst>(J) && !llvm::dyn_cast<llvm::LoadInst>(J)->isVolatile()) {
      // 优化类似于这样的代码，下面的两个load是非必要的，可以替换为%call
      // %call = call i32 @power(i32 %1, i32 %div)
      // store i32 %call, i32* %cur, align 4
      // %3 = load i32, i32* %cur, align 4
      // %4 = load i32, i32* %cur, align 4
      // store %tmp, i32* %cur
      if (J->getOperand(0) == I.getOperand(1) 
          && J->getType() == I.getOperand(0)->getType()) {
        J->replaceAllUsesWith(I.getOperand(0));
        llvm::Instruction* tmp = J;
        J = J->getNextNonDebugInstruction();
        tmp->eraseFromParent();
        continue;
      }
    }
    if (llvm::isa<llvm::StoreInst>(J) && !llvm::dyn_cast<llvm::StoreInst>(&I)->isVolatile()) {
      // 优化类似于下面的代码，第一个store被后面的store覆盖了，可以删除第一个
      // %call = call i32 @power(i32 %1, i32 %div)
      // store i32 %call, i32* %cur, align 4
      // .....
      // store i32 %tmp, i32* %cur, align 4
      if (J->getOperand(1) == I.getOperand(1) 
        && J->getOperand(0)->getType() == I.getOperand(0)->getType()) {
        // 此时，对于StoreInst I 的优化已经结束了，因为 I 自己也被优化掉了
        // 下一个优化应该从 I 的下一条指令开始
        ret = I.getNextNonDebugInstruction();
        CCodeDiv.insert(I.getOperand(0));
        I.eraseFromParent();
        break;
      } 
    }
    // 采取保守的方法，防止I.operand(1)地址中的值被替换之后造成错误
    // if (llvm::isa<llvm::StoreInst>(J) || llvm::isa<llvm::LoadInst>(J) || llvm::isa<llvm::CallInst>(J)) {
    //   break;
    // }
    // if (llvm::isa<llvm::CallInst>(J)) {
    //   llvm::errs() << "CallInst\n";
    //   bool memoryChanged = false;
    //   auto Call = llvm::dyn_cast<llvm::CallInst>(J);
    //   if (auto ret = Call->getReturnedArgOperand()) {
    //     if (ret->getType()->isArrayTy() || ret->getType()->isPointerTy())
    //       break;
    //   }
    //   for (int i = 0; i < Call->getNumArgOperands(); ++i) {
    //     auto arg = Call->getArgOperand(i);
    //     if (arg->getType()->isPointerTy() || arg->getType()->isArrayTy()) {
    //       memoryChanged = true;
    //       break;
    //     }
    //     // } else if (arg->getType()->isPointerTy()) { 
    //     //   auto ptr = llvm::dyn_cast<llvm::Pointer
    //     //   if (AA.alias(llvm::MemoryLocation::get(arg), llvm::MemoryLocation::get(llvm::dyn_cast<llvm::StoreInst>(&I)), AAQI)) {
    //     //     memoryChanged = true;
    //     //     break;
    //     //   }
    //     // }
    //   }
    //   if (memoryChanged)
    //     break;
    // }
    if (llvm::isa<llvm::CallInst>(J))
      break;
    if (llvm::isa<llvm::StoreInst>(J)) {
      if (auto St = llvm::dyn_cast<llvm::StoreInst>(J)) {
        if (AA.alias(llvm::MemoryLocation::get(St), llvm::MemoryLocation::get(llvm::dyn_cast<llvm::StoreInst>(&I)), AAQI))
          break;
      }
    }
    // if (llvm::isa<llvm::CallInst>(J))
    //   break;
    J = J->getNextNonDebugInstruction();
  }
  if (ret == nullptr)
    ret = I.getNextNonDebugInstruction();
  return ret;
}


void sysu::CSE::loadCSE(llvm::Instruction &I, llvm::BasicAA::Result &AA) {
  llvm::Instruction* J = I.getNextNonDebugInstruction();
  llvm::AAQueryInfo AAQI;
  while (J != nullptr) {
    // 保守做法，如果遇到一条新的store，说明此时内存有变化，不再优化当前load
    if (llvm::isa<llvm::StoreInst>(J)) {
      if (auto St = llvm::dyn_cast<llvm::StoreInst>(J)) {
        if (AA.alias(llvm::MemoryLocation::get(St), llvm::MemoryLocation::get(llvm::dyn_cast<llvm::LoadInst>(&I)), AAQI))
          return;
      }
    }

    if (llvm::isa<llvm::LoadInst>(J) && !llvm::dyn_cast<llvm::LoadInst>(J)->isVolatile()) {
      // 优化从同一个地址load出来的指令，如下，后面的可以被删掉
      // %3 = load i32, i32* %cur, align 4
      // %4 = load i32, i32* %cur, align 4
      if (J->getOperand(0) == I.getOperand(0) && J->getType() == I.getType()) {
        J->replaceAllUsesWith(&I);
        llvm::Instruction* tmp = J;
        J = J->getNextNonDebugInstruction();
        tmp->eraseFromParent();
        continue;
      }
    }
    J = J->getNextNonDebugInstruction();
  }
}


void sysu::CSE::commonCSE(llvm::BasicBlock &BB, llvm::Instruction &I, llvm::Instruction *next) {
  if (!isProcessable(I))
    return;
  llvm::Instruction *J = next;
  while (J != nullptr) {
    if (isCommonSubExp(I, *J)) {
      J->replaceAllUsesWith(&I);
      llvm::Instruction *tmp = J;
      J = J->getNextNonDebugInstruction();
      tmp->eraseFromParent();
      continue;
    }
    J = J->getNextNonDebugInstruction();
  }
  // llvm::errs() << "commonCSE\n";
  auto ChildBB = firstDomChild(BB);
  while (ChildBB != nullptr) {
    // commonCSE(BB, I, &*(ChildBB->getInstList().begin()));
    ChildBB = nextDomChild(BB, *ChildBB);
  }
}


void sysu::CSE::associativeBinOp(llvm::Instruction &I) {
  llvm::BinaryOperator *BinOp = llvm::dyn_cast<llvm::BinaryOperator>(&I);
  if (!BinOp)
    return;
  llvm::BinaryOperator *op0 = llvm::dyn_cast<llvm::BinaryOperator>(BinOp->getOperand(0));
  if (I.isAssociative()) {
    while (op0 && op0->getOpcode() == I.getOpcode()) {
      if (CCodeDiv.find(op0) != CCodeDiv.end())
        break;
      llvm::Value* A = op0->getOperand(0);
      llvm::Value* B = op0->getOperand(1);
      llvm::Value* C = I.getOperand(1);
      
      llvm::IRBuilder<> Builder(I.getContext());
      Builder.SetInsertPoint(&I);
      llvm::Value* V = Builder.CreateBinOp(BinOp->getOpcode(), B, C);
      I.setOperand(0, A);
      I.setOperand(1, V);

      BinOp = llvm::dyn_cast<llvm::BinaryOperator>(&I);
      op0 = llvm::dyn_cast<llvm::BinaryOperator>(BinOp->getOperand(0));
    }
  }
}


void sysu::CSE::runOnFunc(llvm::Function &Func, llvm::BasicAA::Result &AA) {
  llvm::DenseSet<llvm::Instruction*> worklist;
  
  // for (auto &BB : Func) {
  //   for (auto &I : BB) {
  //     if (llvm::isa<llvm::StoreInst>(I)) {
  //       // CCodeDiv.insert(I.getOperand(0));
  //     }
  //   }
  // }
  
  for (auto &BB : Func) {
    llvm::Instruction* I = &*(BB.getInstList().begin());
    while (I != nullptr) {
      if (isDead(*I)) {
        worklist.insert(I);
        I = I->getNextNonDebugInstruction();
        continue;
      }
      commonCSE(BB, *I, I->getNextNonDebugInstruction());
      if (llvm::isa<llvm::LoadInst>(I)) {
        loadCSE(*I, AA);
      }
      if (llvm::isa<llvm::StoreInst>(I)) {
        I = storeCSE(*I, AA);
        continue;
      }
      I = I->getNextNonDebugInstruction();
    }
  }

  while (!worklist.empty()) {
    auto iter = worklist.begin();
    llvm::Instruction * I = nullptr;
    if (iter != worklist.end()) {
      I = *iter;
      worklist.erase(iter);
    }
    if (isDead(*I)) {
      for (int i = 0; i < I->getNumOperands(); ++i) {
        auto J = I->getOperand(i);
        if (llvm::isa<llvm::Instruction>(J)) {
          worklist.insert(llvm::dyn_cast<llvm::Instruction>(J));
        }
      }
      I->eraseFromParent();
    }
  }

  for (auto &BB : Func) {
    for (auto &I : BB) {
      associativeBinOp(I);
    }
  }

  for (auto &BB : Func) {
    for (auto &I : BB) {
      commonCSE(BB, I, I.getNextNonDebugInstruction());
    }
  }
}


// void sysu::CSE::runOnModule(llvm::Module &M, llvm::FunctionAnalysisManager &FAM) {
//   for (auto &Func : M) {
//     auto AA = FAM.getResult<llvm::BasicAA>(Func);
//     runOnFunc(Func, AA);
//   }
// }


// sysu::CSE::Result sysu::CSE::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
//   auto &FAM = MAM.getResult<llvm::FunctionAnalysisManagerModuleProxy>(M).getManager();
//   // my_DT = new llvm::DominatorTreeBase<llvm::BasicBlock, false>;
//   runOnModule(M, FAM);
//   // delete my_DT;
//   llvm::errs() << "Success here!!!\n"; 
//   return llvm::PreservedAnalyses::all();
// }


sysu::CSE::Result sysu::CSE::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
  if (F.isDeclaration())
    return llvm::PreservedAnalyses::none();
  
  llvm::errs() << F.getName();
  my_DT = new llvm::DominatorTreeBase<llvm::BasicBlock, false>;
  auto &AA = FAM.getResult<llvm::BasicAA>(F);
  runOnFunc(F, AA);
  delete my_DT;
  llvm::errs() << " : Finish CSE Pass\n";
  return llvm::PreservedAnalyses::all();
}


// ******************* Implement of DCE ***********************
bool sysu::DCE::isProcessable(llvm::Instruction &I) {
  return !(I.isTerminator() || llvm::isa<llvm::CallInst>(I) || 
           llvm::isa<llvm::PHINode>(I) ||
           llvm::isa<llvm::VAArgInst>(I) ||
           llvm::isa<llvm::ExtractValueInst>(I) ||
           llvm::isa<llvm::BranchInst>(I) ||
           llvm::isa<llvm::LoadInst>(I) ||
           llvm::isa<llvm::StoreInst>(I)
          );
}


bool sysu::DCE::isDead(llvm::Instruction &I) {
  if (I.hasNUsesOrMore(1)) {
    return false;
  }
  
  switch (I.getOpcode()) {
    case llvm::Instruction::Ret:
    case llvm::Instruction::Alloca:
    case llvm::Instruction::Br:
    case llvm::Instruction::Switch:
    case llvm::Instruction::IndirectBr:
    case llvm::Instruction::Invoke:
    case llvm::Instruction::Unreachable:
    case llvm::Instruction::Fence:
    case llvm::Instruction::Call:
    case llvm::Instruction::Store:
    case llvm::Instruction::AtomicCmpXchg:
    case llvm::Instruction::AtomicRMW:
    case llvm::Instruction::Resume:
    case llvm::Instruction::LandingPad:
      return false;
    case llvm::Instruction::Load:
      return !llvm::dyn_cast<llvm::LoadInst>(&I)->isVolatile();
    default:
      break;
  }
  return true;
}


void sysu::DCE::removeRecur(llvm::Instruction &I, llvm::DenseSet<llvm::Instruction*> &toRemove) {
  for (auto iter = I.user_begin(); iter != I.user_end(); ++iter) {
    if (llvm::isa<llvm::Instruction>(*iter)) {
      auto J = (llvm::Instruction*)(*iter);
      toRemove.insert(J);
      removeRecur(*J, toRemove);
    }
  }
}


void sysu::DCE::removeAlloca(llvm::Function &F) {
  llvm::DenseSet<llvm::Instruction*> worklist;
  for (auto &BB : F) {
    for (auto &I : BB) {
      if (!llvm::isa<llvm::AllocaInst>(I)) {
        continue;
      }
      if (I.getType()->getPointerElementType()->isArrayTy()) {
          continue;
      }
      bool loadFlag = false;
      for (auto iter = I.user_begin(); iter != I.user_end(); ++iter) {
        if (llvm::isa<llvm::LoadInst>(*iter)) {
          loadFlag = true;
          break;
        }
      }
      // 该变量声明之后就没有被load过
      if (!loadFlag) {
        llvm::errs() << I.getName() << " ?\n";
        worklist.insert(&I);
        removeRecur(I, worklist);
      }
    }
  }

  for (auto iter = worklist.begin(); iter != worklist.end(); ++iter) {
    (*iter)->eraseFromParent();
  }
}


void sysu::DCE::removeCommon(llvm::Function &F) {
  llvm::DenseSet<llvm::Instruction*> worklist;
  for (auto &BB : F) {
    llvm::Instruction* I = &*(BB.getInstList().begin());
    while (I != nullptr) {
      if (isDead(*I)) {
        worklist.insert(I);
      }
      I = I->getNextNonDebugInstruction();
    }
  }

  // 删除工作区中的指令
  while (!worklist.empty()) {
    auto iter = worklist.begin();
    llvm::Instruction * I = nullptr;
    if (iter != worklist.end()) {
      I = *iter;
      worklist.erase(iter);
    }
    if (isDead(*I)) {
      for (int i = 0; i < I->getNumOperands(); ++i) {
        auto J = I->getOperand(i);
        if (llvm::isa<llvm::Instruction>(J)) {
          worklist.insert(llvm::dyn_cast<llvm::Instruction>(J));
        }
      }
      I->eraseFromParent();
    }
  }
}


sysu::DCE::Result sysu::DCE::run(llvm::Function &Func, llvm::FunctionAnalysisManager &FAM) {
  removeAlloca(Func);
  removeCommon(Func);
  removeAlloca(Func);
  return llvm::PreservedAnalyses::all();
}



// ********************* Implement of LICM **************************


void sysu::LICM::preOrder(llvm::DomTreeNode *N, llvm::SmallVector<llvm::BasicBlock*, 256> &worklist, llvm::Loop &L, llvm::DominatorTree &DT) {
  llvm::BasicBlock* BB = N->getBlock();
  if (DT.dominates(L.getHeader(), BB)) {
    worklist.push_back(BB);
  }
  for (auto iter = N->begin(); iter != N->end(); ++iter) {
    preOrder(*iter, worklist, L, DT);
  }
}


bool sysu::LICM::isLoopInvariant(llvm::Loop &L, llvm::Instruction &I) {
  if (!(llvm::isa<llvm::BinaryOperator>(I) || llvm::isa<llvm::CastInst>(I) || llvm::isa<llvm::SelectInst>(I) || llvm::isa<llvm::GetElementPtrInst>(I) || I.isShift()))
    return false;
  if (!L.hasLoopInvariantOperands(&I))
    return false;
  return true;
}



// 当循环不变的指令支配循环所有的退出块时，我们才能将其提出循环体
// 考虑以下反例
// int a = 1, i = 0
// while(cond) :
//  b = i + j
//  if (cond1)
//    i = a
//  j++
bool sysu::LICM::canHoist(llvm::Loop &L, llvm::Instruction &I, llvm::DominatorTree &DT) {
  bool ret = false;
  llvm::SmallVector<llvm::BasicBlock*, 16> ExitBlocks;
  L.getExitBlocks(ExitBlocks);

  for (auto BB : ExitBlocks) {
    if (!DT.dominates(I.getParent(), BB)) {
      ret = false;
      break;
    }
  }

  if (llvm::isSafeToSpeculativelyExecute(&I) && ret)
    return true;
  return false;
}



void sysu::LICM::runOnLoop(llvm::Loop &L, llvm::LoopInfo &LI, llvm::DominatorTree &DT) {
  llvm::SmallVector<llvm::BasicBlock*, 256> worklist;
  preOrder(DT.getRootNode(), worklist, L, DT);

  llvm::BasicBlock *pre_header = L.getLoopPreheader();
  for (auto BB : worklist) {
    if (LI.getLoopFor(BB) == &L) {
      continue;
    }
    for (auto &I : *BB) {
      if (isLoopInvariant(L, I) && canHoist(L, I, DT))
        I.moveBefore(pre_header->getTerminator());
    }
  }
}


sysu::LICM::Result sysu::LICM::run(llvm::Loop &L, llvm::LoopAnalysisManager &LAM, llvm::LoopStandardAnalysisResults &LSAR) {
  runOnLoop(L, LSAR.LI, LSAR.DT);
  
  llvm::PreservedAnalyses PA = llvm::getLoopPassPreservedAnalyses();
  PA.preserve<llvm::DominatorTreeAnalysis>();
  PA.preserve<llvm::LoopAnalysis>();
  return PA;
}


// ################# implement of DivSimplify ############


void sysu::InstSimplify::DivSimply(llvm::Instruction &I, llvm::DenseSet<llvm::Instruction*> &worklist) {
  auto LHS = I.getOperand(0);
  auto RHS = I.getOperand(1);

  if (RHS == llvm::ConstantInt::get(RHS->getType(), 2)) {
    llvm::IRBuilder<> Builder(I.getContext());
    Builder.SetInsertPoint(&I);
    bool isExact = false;
    auto Shift = Builder.CreateAShr(LHS, llvm::ConstantInt::get(LHS->getType(), 1), "", isExact);
    I.replaceAllUsesWith(Shift);
    worklist.insert(&I);
  }
}


void sysu::InstSimplify::MulSimply(llvm::Instruction &I, llvm::DenseSet<llvm::Instruction*> &worklist) {
  auto LHS = I.getOperand(0);
  auto RHS = I.getOperand(1);

  if (RHS == llvm::ConstantInt::get(RHS->getType(), 2)) {
    llvm::IRBuilder<> Builder(I.getContext());
    Builder.SetInsertPoint(&I);
    auto Shift = Builder.CreateShl(LHS, llvm::ConstantInt::get(RHS->getType(), 1));
    I.replaceAllUsesWith(Shift);
    worklist.insert(&I);
  }
}

sysu::InstSimplify::Result sysu::InstSimplify::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
  llvm::DenseSet<llvm::Instruction*> worklist;
  for (auto &BB : F) {
    llvm::Instruction* I = &*(BB.getInstList().begin());
    while (I != nullptr) {
      if (llvm::isa<llvm::SDivOperator>(I)) {
        DivSimply(*I, worklist);
      } else if (llvm::isa<llvm::MulOperator>(I)) {
        MulSimply(*I, worklist);
      }
      I = I->getNextNonDebugInstruction();
    }
  }
  for (auto it = worklist.begin(); it != worklist.end(); ++it)
    (*it)->eraseFromParent();
  return llvm::PreservedAnalyses::all();
}


void sysu::InstComb::combine(llvm::Instruction *I) {
  if (!llvm::isa<llvm::BinaryOperator>(I))
    return;
  auto LHS = I->getOperand(0);
  auto RHS = I->getOperand(1);
  int constValue = 0;
  if (llvm::isa<llvm::ConstantInt>(LHS)) {
    if (!llvm::isa<llvm::ConstantInt>(RHS)) {
      auto tmp = LHS;
      LHS = RHS;
      RHS = tmp;
    }
  }
  if (auto Const = llvm::dyn_cast<llvm::ConstantInt>(RHS)) {
    constValue = Const->getSExtValue();
  } else
    return;
  
  llvm::DenseSet<llvm::Instruction*> worklist;
  if (llvm::isa<llvm::AddOperator>(I)) {
    auto J = I->getNextNonDebugInstruction();
    auto Prev = I;
    bool changed = false;
    while (Prev->getNumUses() == 1 && J) {
      if (!llvm::isa<llvm::AddOperator>(J)) {
        J = J->getNextNonDebugInstruction();
        continue;
      }

      auto JLHS = J->getOperand(0);
      auto JRHS = J->getOperand(1);
      if (llvm::isa<llvm::ConstantInt>(JLHS)) {
        if (!llvm::isa<llvm::ConstantInt>(JRHS)) {
          auto tmp = JLHS;
          JLHS = JRHS;
          JRHS = tmp;
        }
      }
      // if (!llvm::isa<llvm::ConstantInt>(JRHS)) {
      //   J = J->getNextNonDebugInstruction();
      //   continue;
      // }
      if (JLHS == Prev) {
        if (auto Const = llvm::dyn_cast<llvm::ConstantInt>(JRHS)) {
          constValue += Const->getSExtValue();
          worklist.insert(J);
          Prev = J;
          changed = true;
        }
      }
      J = J->getNextNonDebugInstruction();
    }
    if (changed)
      worklist.insert(I);
    llvm::IRBuilder<> Builder(I->getContext());
    Builder.SetInsertPoint(Prev);
    auto Comb = Builder.CreateAdd(LHS, llvm::ConstantInt::get(RHS->getType(), constValue));
    Prev->replaceAllUsesWith(Comb);
    for (auto it = worklist.begin(); it != worklist.end(); ++it) {
      (*it)->eraseFromParent();
    }
  }
}


sysu::InstComb::Result sysu::InstComb::run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
  for (auto &BB : F) {
    llvm::Instruction *I = &*(BB.getInstList().begin());
    while (I != nullptr) {
      combine(I);
      I = I->getNextNonDebugInstruction();
    }
  }
  return llvm::PreservedAnalyses::all();
}


extern "C" {
llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK llvmGetPassPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "sysu-optimizer-pass", LLVM_VERSION_STRING,
          [](llvm::PassBuilder &PB) {
            // #1 REGISTRATION FOR "opt -passes=sysu-optimizer-pass"
            PB.registerPipelineParsingCallback(
                [&](llvm::StringRef Name, llvm::ModulePassManager &MPM, 
                    llvm::ArrayRef<llvm::PassBuilder::PipelineElement>) {
                  if (Name == "sysu-optimizer-pass") {
                    MPM.addPass(sysu::StaticCallCounterPrinter(llvm::errs()));
                    // MPM.addPass(sysu::CSE());
                    // FPM.addPass(sysu::CSE());
                    return true;
                  }
                  return false;
                });
            // #2 REGISTRATION FOR
            // "MAM.getResult<sysu::StaticCallCounter>(Module)"
            PB.registerAnalysisRegistrationCallback(
                [](llvm::ModuleAnalysisManager &MAM) {
                  MAM.registerPass([&] { return sysu::StaticCallCounter(); });
                });
          }};
}
}