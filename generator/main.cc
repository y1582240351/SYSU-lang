#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/JSON.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/ValueSymbolTable.h>

namespace {
llvm::LLVMContext TheContext;
llvm::Module TheModule("-", TheContext);
llvm::IRBuilder<> Builder(TheContext);
std::map<llvm::StringRef, llvm::AllocaInst *> syms;
std::map<std::string, llvm::Function*> overloadFuncs;
struct whileBeginEnd {
  llvm::BasicBlock *whileBegin;
  llvm::BasicBlock *whileEnd;
  whileBeginEnd(llvm::BasicBlock *begin, llvm::BasicBlock *end) : whileBegin(begin), whileEnd(end){}
  llvm::BasicBlock* begin() {return this->whileBegin;}
  llvm::BasicBlock* end() {return this->whileEnd;}
};
llvm::SmallVector<whileBeginEnd, 50> whileBeginEndStack; // 为break与continue提供当前所处的while

struct SCThenEnd {
  llvm::BasicBlock *SCThen;
  llvm::BasicBlock *SCEnd;
  SCThenEnd(llvm::BasicBlock *then, llvm::BasicBlock *end) : SCThen(then), SCEnd(end) {}
  llvm::BasicBlock* then() {return SCThen;}
  llvm::BasicBlock* end() {return SCEnd;}
};
llvm::SmallVector<SCThenEnd, 16> SCThenEndStack; // 为短路(Short-Circuit)控制提供当前所处的if块

llvm::Value* buildImplicitCastExpr(const llvm::json::Object *O);
llvm::Type* buildtype(const llvm::json::Object *O, bool &isConstant);
llvm::Value* buildreferencedDecl(const llvm::json::Object *O);
llvm::Constant * buildIntegerLiteral(const llvm::json::Object *O);
llvm::Value* buildArraySubscriptExpr(const llvm::json::Object *O);
void buildCompoundStmt(const llvm::json::Object *O);
void buildContinueStmt(const llvm::json::Object *O);
void buildBreakStmt(const llvm::json::Object *O);
llvm::Value* buildUnaryOperator(const llvm::json::Object *O);
llvm::Value* buildParenExpr(const llvm::json::Object *O);
void buildWhileStmt(const llvm::json::Object *O);
llvm::Value* buildBinaryOperator(const llvm::json::Object *O);
void buildDoStmt(const llvm::json::Object *O);


llvm::Type* buildtype(const llvm::json::Object *O, bool &isConstant) {
  auto qualType = *(O->getString("qualType"));
  // 判断是否是一个函数指针类型 "Type (*)(...)"
  auto point = qualType.find("(*)");
  if (point != std::string::npos) { 
    point = qualType.substr(point+3).find('(');
    if (point != std::string::npos)
      return llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), llvm::Type::getVoidTy(TheContext));
  }

  auto type = qualType.split(' ');
  llvm::Type* ret = llvm::Type::getInt32Ty(TheContext);
  if (type.first == "const") {
    type = type.second.rsplit(' ');
    isConstant = true;
  } else {
    type = qualType.rsplit(' ');
    isConstant = false;
  }

  int l = type.second.rfind('['), r = type.second.rfind(']');
  // not a array
  if (l == std::string::npos) {
    if (type.first == "int") 
      ret = llvm::Type::getInt32Ty(TheContext);
    else if (type.first == "char")
      ret =  llvm::Type::getInt8Ty(TheContext);
    else if (type.first == "long")
      ret = llvm::Type::getInt64Ty(TheContext);
    else if (type.first == "unsigned")
      ret = llvm::Type::getInt32Ty(TheContext);
  } else { // is array
    long size;
    type.second.substr(l+1, r-l-1).getAsInteger(0, size);
    llvm::ArrayType* array_type;
    if (type.first == "int") 
      array_type = llvm::ArrayType::get(llvm::Type::getInt32Ty(TheContext), size);
    else if (type.first == "char")
      array_type = llvm::ArrayType::get(llvm::Type::getInt8Ty(TheContext), size);

    auto tmp = type.second.substr(0, l);
    l = tmp.rfind('[', l);
    r = tmp.rfind(']', r);
    while(l != std::string::npos) {
      tmp.substr(l+1, r-l-1).getAsInteger(0, size);
      array_type = llvm::ArrayType::get(array_type, size);
      tmp = tmp.substr(0, l);
      l = tmp.rfind('[', l);
      r = tmp.rfind(']', r);
    }
    ret = array_type;
  }

  point = qualType.find('*');
  while (point != std::string::npos) {
    ret = llvm::PointerType::get(ret, 0);
    qualType = qualType.substr(point+1);
    point = qualType.find('*');
  }
  return ret;
}


llvm::FunctionType* buildFuncType(const llvm::json::Object *O, bool &isDefine) {
  bool isConstant = false; // 用来作为函数参数，实际没用
  isDefine = false;
  auto retType_str = O->getObject("type")->getString("qualType")->split(' ').first;
  llvm::Type *retType;
  if (retType_str == "int")
    retType = llvm::Type::getInt32Ty(TheContext);
  else if (retType_str == "long") {
    retType = llvm::Type::getInt64Ty(TheContext);
  } else if (retType_str == "void")
    retType = llvm::Type::getVoidTy(TheContext);
  
  std::vector<llvm::Type*> args;
  if (auto inner = O->getArray("inner"))
    for (const auto & it : *inner)
      if (auto P = it.getAsObject())
        if (*(P->getString("kind")) == "ParmVarDecl")
          args.push_back(buildtype(P->getObject("type"), isConstant));
        else if (*(P->getString("kind")) == "CompoundStmt")
          isDefine = true;
  return llvm::FunctionType::get(retType, args, 0);
}


llvm::Constant * buildIntegerLiteral(const llvm::json::Object *O) {
  auto value = O->getString("value");
  return llvm::ConstantInt::get(TheContext, /* i32 3(decimal) */ llvm::APInt(32, *value, 10));
}


llvm::Value* buildStringLiteral(const llvm::json::Object *O) {
  bool isConstant = false;;
  auto type = llvm::cast<llvm::ArrayType>(buildtype(O->getObject("type"), isConstant));
  auto id = O->getString("id"); // 我们将id作为变量名
  auto str = new llvm::GlobalVariable(TheModule, type, isConstant, 
                llvm::GlobalValue::PrivateLinkage, 0, *id);
  std::vector<llvm::Constant*> value;
  auto value_str = O->getString("value")->str();
  for (int i = 1; i < value_str.size()-1; ++i) {
    char val = value_str[i];
    if (value_str[i] == '\\') { 
      ++i;
      if (value_str[i] == 'n')
        val = '\n';
      else if (value_str[i] == '\\')
        val = '\\';
      else if (value_str[i] == '\"')
        val = '\"';
    }
    value.push_back(llvm::ConstantInt::get(TheContext, llvm::APInt(8, val, false)));
  }
  while (value.size() < type->getArrayNumElements())
    value.push_back(llvm::ConstantInt::get(TheContext, llvm::APInt(8, 0, false)));
  llvm::errs() << value.size() << " hahe\n";
  str->setInitializer(llvm::ConstantArray::get(type, value));
  return str;
}


llvm::Value* buildreferencedDecl(const llvm::json::Object *O) {
  auto id = O->getString("id");
  auto name = O->getString("name");

  // local variable
  if (syms.find(*id) != syms.end())
    return syms[*id];

  // global variable
  llvm::GlobalVariable* G = TheModule.getNamedGlobal(*id);
  if (G != nullptr)
    return G;

  // overloadFunc
  auto type = O->getObject("type")->getString("qualType");
  auto name_ol = ((*name) + (*type)).str();
  if (overloadFuncs.find(name_ol) != overloadFuncs.end())
    return overloadFuncs[name_ol];

  // function
  auto Func = TheModule.getFunction(*name);
  if (Func != nullptr)
    return Func;

  printf("ERROR in ReferencedDecl: Can't find variable: %s", name->str().c_str());
  return nullptr;
}


// 返回变量对应的指针，注意！！只是指针，要使用值还需要执行Builder.CreateLoad(ret)指令
llvm::Value* buildDeclRefExpr(const llvm::json::Object *O) {
  if (auto V = O->getObject("referencedDecl"))
    return buildreferencedDecl(V);
  return nullptr;
}


llvm::Value* buildCallExpr(const llvm::json::Object *O) {
  auto V = O->getObject("type");
  bool isConstant;
  llvm::Type *type = buildtype(V, isConstant);

  if (auto inner = O->getArray("inner")) {
    auto iter = inner->begin();
    llvm::Function *func;
    if (auto P = iter->getAsObject())
      if (auto kind = P->getString("kind"))
        if (*kind == "ImplicitCastExpr")
          func = llvm::cast<llvm::Function>(buildImplicitCastExpr(P));

    iter = std::next(iter);
    std::vector<llvm::Value*> func_args;
    auto arg_iter = func->arg_begin();
    while (iter != inner->end()) {
      llvm::Value *arg;
      if (auto P = iter->getAsObject())
        if (auto kind = P->getString("kind"))
          if (*kind == "ImplicitCastExpr")
            arg = buildImplicitCastExpr(P);
          else if (*kind == "IntegerLiteral")
            arg = buildIntegerLiteral(P);
          else if (*kind == "CallExpr")
            arg = buildCallExpr(P);
          else if (*kind == "UnaryOperator")
            arg = buildUnaryOperator(P);
          else if (*kind == "BinaryOperator")
            arg = buildBinaryOperator(P);
          else 
            llvm::errs() << "Error in CallExpr\n";
      while (arg->getType()->isPointerTy() && arg->getType() != arg_iter->getType() 
             && arg->getType()->getPointerElementType()->isArrayTy()) {
        auto ConstZero = llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
        arg = Builder.CreateInBoundsGEP(arg, {ConstZero, ConstZero});
      }
      while (arg->getType()->isPointerTy() && arg->getType() != arg_iter->getType() 
             && arg->getType()->getPointerElementType()->isPointerTy()) 
        arg = Builder.CreateLoad(arg);
      
      if (arg->getType() != arg_iter->getType())
        arg = Builder.CreateZExt(arg, arg_iter->getType());
      
      func_args.push_back(arg);
      iter = std::next(iter);
      arg_iter = std::next(arg_iter);
    }
    return Builder.CreateCall(func, func_args);
  }

  printf("Error in CallExpr!!!\n");
  return nullptr;
}


llvm::Value* buildBinaryOperator(const llvm::json::Object *O) {
  llvm::Value *LHS, *RHS;
  llvm::BasicBlock *RHS_BB;
  auto opcode = O->getString("opcode");
  if (auto inner = O->getArray("inner")) {
    // 如果是短路运算符，在计算左边时要更新SCThenEnd栈中的内容
    if (*opcode == "||") {
      RHS_BB = llvm::BasicBlock::Create(TheContext, "orR", Builder.GetInsertBlock()->getParent());
      SCThenEndStack.push_back({SCThenEndStack.back().then(), RHS_BB});
    } else if (*opcode == "&&") {
      RHS_BB = llvm::BasicBlock::Create(TheContext, "andR", Builder.GetInsertBlock()->getParent());
      SCThenEndStack.push_back({RHS_BB, SCThenEndStack.back().end()});
    }

    auto l = inner->front().getAsObject();
    if (*(l->getString("kind")) == "DeclRefExpr") {
      LHS = buildDeclRefExpr(l);
      if (*opcode != "=") // 如果是赋值语句，LHS应该是指针
        LHS = Builder.CreateLoad(LHS);
    } else if (*(l->getString("kind")) == "IntegerLiteral")
      LHS = buildIntegerLiteral(l);
    else if (*(l->getString("kind")) == "BinaryOperator")
      LHS = buildBinaryOperator(l);
    else if (*(l->getString("kind")) == "ImplicitCastExpr")
      LHS = buildImplicitCastExpr(l);
    else if (*(l->getString("kind")) == "CallExpr")
      LHS = buildCallExpr(l);
    else if (*(l->getString("kind")) == "ArraySubscriptExpr")
      LHS = buildArraySubscriptExpr(l);
    else if (*(l->getString("kind")) == "UnaryOperator")
      LHS = buildUnaryOperator(l);
    else if (*(l->getString("kind")) == "ParenExpr")
      LHS = buildParenExpr(l);
    else 
      llvm::errs() << "Error in BinaryOP LHS\n";
    
    // 计算左边的短路
    if (*opcode == "&&" || *opcode == "||") {
      if (LHS->getType() != llvm::Type::getInt1Ty(TheContext))
        LHS = Builder.CreateICmpNE(LHS, llvm::ConstantInt::get(TheContext, llvm::APInt(32, "0", 10)));
      Builder.CreateCondBr(LHS, SCThenEndStack.back().then(), SCThenEndStack.back().end());
      RHS_BB->moveAfter(Builder.GetInsertBlock());
      Builder.SetInsertPoint(RHS_BB);
      SCThenEndStack.pop_back();
    }

    auto r = inner->back().getAsObject();
    if (*(r->getString("kind")) == "IntegerLiteral")
      RHS = buildIntegerLiteral(r);
    else if (*(r->getString("kind")) == "BinaryOperator")
      RHS = buildBinaryOperator(r);
    else if (*(r->getString("kind")) == "ImplicitCastExpr")
      RHS = buildImplicitCastExpr(r);
    else if (*(r->getString("kind")) == "CallExpr")
      RHS = buildCallExpr(r);
    else if (*(r->getString("kind")) == "ArraySubscriptExpr")
      RHS = buildArraySubscriptExpr(r);
    else if (*(r->getString("kind")) == "UnaryOperator")
      RHS = buildUnaryOperator(r);
    else if (*(r->getString("kind")) == "ParenExpr")
      RHS = buildParenExpr(r);
    else 
      llvm::errs() << "Error in BinaryOP RHS\n";
  }

  if (*opcode == "==" || *opcode == "!=") {
    if (RHS->getType() != LHS->getType()) {
      if (LHS->getType() == llvm::Type::getInt1Ty(TheContext))
        RHS = Builder.CreateICmpNE(RHS, llvm::ConstantInt::get(RHS->getType(), 0));
      else if (RHS->getType() == llvm::Type::getInt1Ty(TheContext))
        LHS = Builder.CreateICmpNE(LHS, llvm::ConstantInt::get(LHS->getType(), 0));
    }
  }
  
  if (*opcode == "=")
    return Builder.CreateStore(RHS, LHS);
  else if (*opcode == "+") {
    return Builder.CreateAdd(LHS, RHS);
  } else if (*opcode == "-") {
    return Builder.CreateSub(LHS, RHS);
  } else if (*opcode == "*") {
    return Builder.CreateMul(LHS, RHS);
  } else if (*opcode == "/") {
    return Builder.CreateSDiv(LHS, RHS);
  } else if (*opcode == "%") {
    return Builder.CreateSRem(LHS, RHS);
  } else if (*opcode == ">")
    return Builder.CreateICmpSGT(LHS, RHS);
  else if (*opcode == "<")
    return Builder.CreateICmpSLT(LHS, RHS);
  else if (*opcode == ">=")
    return Builder.CreateICmpSGE(LHS, RHS);
  else if (*opcode == "<=")
    return Builder.CreateICmpSLE(LHS, RHS);
  else if (*opcode == "==")
    return Builder.CreateICmpEQ(LHS, RHS);
  else if (*opcode == "!=")
    return Builder.CreateICmpNE(LHS, RHS);
  else if (*opcode == "||" || *opcode == "&&") {
    if (RHS->getType() != llvm::Type::getInt1Ty(TheContext)) 
      return Builder.CreateICmpNE(RHS, llvm::ConstantInt::get(RHS->getType(), 0));
    else 
      return RHS;
  }
  printf("Error in BinaryOP!!!\n");
  return nullptr;
}


llvm::Value* buildParenExpr(const llvm::json::Object *O) {
  if (auto inner = O->getArray("inner")) {
    if (inner->size() != 1) {
      llvm::errs() << "Error in ParenExpr!!!\n";
      return nullptr;
    }

    auto val = inner->front().getAsObject();
    if (*(val->getString("kind")) == "BinaryOperator")
      return buildBinaryOperator(val);
    else if (*(val->getString("kind")) == "DeclRefExpr")
      return buildDeclRefExpr(val);
    else if (*(val->getString("kind")) == "CallExpr")
      return buildCallExpr(val);
    else if (*(val->getString("kind")) == "IntegerLiteral")
      return buildIntegerLiteral(val);
    else if (*(val->getString("kind")) == "ImplicitCastExpr")
      return buildImplicitCastExpr(val);
    else if (*(val->getString("kind")) == "ParenExpr")
      return buildParenExpr(val);
    else if (*(val->getString("kind")) == "UnaryOperator")
      return buildUnaryOperator(val);
    else if (*(val->getString("kind")) == "ArraySubscriptExpr")
      return buildArraySubscriptExpr(val);
  }
  llvm::errs() << "Error in ParenExpr: Not inner!!!\n";
  return nullptr;
}


llvm::Value* buildUnaryOperator(const llvm::json::Object *O) {
  auto opcode = O->getString("opcode");
  if (auto inner = O->getArray("inner")) {
    if (inner->size() != 1) {
      llvm::errs() << "Error in UnaryOpetator!!!\n";
      return nullptr;
    }

    auto op_json = inner->front().getAsObject();
    llvm::Value *op;
    if (*(op_json->getString("kind")) == "BinaryOperator")
      op = buildBinaryOperator(op_json);
    else if (*(op_json->getString("kind")) == "ParenExpr")
      op = buildParenExpr(op_json);
    else if (*(op_json->getString("kind")) == "IntegerLiteral")
      op = buildIntegerLiteral(op_json);
    else if (*(op_json->getString("kind")) == "ImplicitCastExpr")
      op = buildImplicitCastExpr(op_json);
    else if (*(op_json->getString("kind")) == "ArraySubscriptExpr")
      op = buildArraySubscriptExpr(op_json);
    else if (*(op_json->getString("kind")) == "UnaryOperator")
      op = buildUnaryOperator(op_json);
    else if (*(op_json->getString("kind")) == "CallExpr")
      op = buildCallExpr(op_json);
    else
      llvm::errs() << "Error in CallExpr\n";

    if (*opcode == "!") {
      if (op->getType() != llvm::Type::getInt1Ty(TheContext))
        op = Builder.CreateICmpNE(op, llvm::ConstantInt::get(op->getType(), 0));
      return Builder.CreateXor(op, llvm::ConstantInt::get(op->getType(), 1));
    } else if (*opcode == "-")
      return Builder.CreateSub(llvm::ConstantInt::get(op->getType(), 0), op);
    else if (*opcode == "+")
      return op;
    else 
      llvm::errs() << "Error in UnaryOpetator!!!\n";
  }
  return nullptr;
}


llvm::Value* buildArraySubscriptExpr(const llvm::json::Object *O) {
  if (auto inner = O->getArray("inner")) {
    auto address_json = inner->front().getAsObject();
    llvm::Value* address;
    if (auto kind = address_json->getString("kind")) 
      if (*kind == "ImplicitCastExpr")
        address = buildImplicitCastExpr(address_json);
      else if (*kind == "BinaryOperator")
        address = buildBinaryOperator(address_json);
      else 
        llvm::errs() << "Error in ArrSub Addr\n";
    
    auto index_json = inner->back().getAsObject();
    llvm::Value* index;
    if (auto kind = index_json->getString("kind"))
      if (*kind == "IntegerLiteral")
        index = buildIntegerLiteral(index_json);
      else if (*kind == "BinaryOperator")
        index = buildBinaryOperator(index_json);
      else if (*kind == "ImplicitCastExpr")
        index = buildImplicitCastExpr(index_json);
      else if (*kind == "CallExpr")
        index = buildCallExpr(index_json);
      else 
        llvm::errs() << "Error in ArrSub index\n";

    llvm::SmallVector<llvm::Value*, 3> indeices{llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0))};
    llvm::Type *type = address->getType()->getPointerElementType();
    if (type->isPointerTy()) {
        indeices.pop_back();
        type = type->getPointerElementType();
        address = Builder.CreateLoad(address);
    }
    indeices.push_back(index);
    llvm::Value* base = Builder.CreateInBoundsGEP(type, address, indeices);

    return base;
  }

  printf("Error in ArraySubscriptExpr: format not match!!!\n");
  return nullptr;
}


llvm::Value* buildImplicitCastExpr(const llvm::json::Object *O) {
  auto V1 = O->getObject("type");
  bool isConstant;
  auto type = buildtype(V1, isConstant);
  llvm::Value *ret;
  if (auto inner = O->getArray("inner")) {
    for (const auto & it : *inner) {
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind"))
          if (*kind == "ArraySubscriptExpr") {
            ret = buildArraySubscriptExpr(P);
          } else if (*kind == "DeclRefExpr") {
            ret = buildDeclRefExpr(P);
          } else if (*kind == "CallExpr") {
            ret = buildCallExpr(P);
          } else if (*kind == "UnaryOperator") {
            ret = buildUnaryOperator(P);
          } else if (*kind == "ParenExpr") {
            ret = buildParenExpr(P);
          } else if (*kind == "IntegerLiteral") {
            ret = buildIntegerLiteral(P);
            if (ret->getType() != type)
              ret = Builder.CreateZExt(ret, type);
          } else if (*kind == "BinaryOperator") {
            ret = buildBinaryOperator(P);
          } else if (*kind == "StringLiteral") {
            auto ConstZero = llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
            ret = Builder.CreateInBoundsGEP(buildStringLiteral(P), {ConstZero, ConstZero});
          } else if (*kind == "ImplicitCastExpr") {
            ret = buildImplicitCastExpr(P);
          } else 
            llvm::errs() << "Error in Implicit\n";
    }
    if (type->isFunctionTy())
      return ret;
    else if (type->isArrayTy() || type->isPointerTy())
      return ret;
    else if (*(O->getString("valueCategory")) == "rvalue" && ret->getType()->isPointerTy()) {
      return Builder.CreateLoad(ret);
    } else
      return ret;
  }
  return nullptr;
}


llvm::Value* buildInitListExpr(const llvm::json::Object *O, llvm::Value *base) {
  auto V = O->getObject("type");
  bool isConstant;
  llvm::Type* type = buildtype(V, isConstant);

  int curr = 0;
  auto inner = O->getArray("inner");
  if (inner == nullptr)
    inner = O->getArray("array_filler");
  if (inner != nullptr) {
    for (const auto & it : *inner)
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind")) {
          if (*kind == "ImplicitValueInitExpr")
            continue;
          llvm::Value* idx = llvm::ConstantInt::get(TheContext, llvm::APInt(32, curr++));
          llvm::Value* curr_base = Builder.CreateInBoundsGEP(
              type, 
              base,
              {llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0)) ,idx}
          );
          if (*kind == "InitListExpr")
            buildInitListExpr(P, curr_base);
          else if (*kind == "IntegerLiteral") {
            llvm::Value * val = buildIntegerLiteral(P);
            Builder.CreateStore(val, curr_base);
          } else if (*kind == "ImplicitCastExpr") {
            llvm::Value* val = buildImplicitCastExpr(P);
            Builder.CreateStore(val, curr_base);
          } else if (*kind == "CallExpr") {
            llvm::Value *val = buildCallExpr(P);
            Builder.CreateStore(val, curr_base);
          } else if (*kind == "BinaryOperator") {
            llvm::Value *val = buildBinaryOperator(P);
            Builder.CreateStore(val, curr_base);
          } else if (*kind != "ImplicitValueInitExpr")
            llvm::errs() << "Error in InitialExpr\n";
        }
  }
  return nullptr;
}


llvm::Constant* buildGlobalInitListExpr(const llvm::json::Object *O) {
  auto V = O->getObject("type");
  bool isConstant;
  llvm::ArrayType *type = llvm::cast<llvm::ArrayType>(buildtype(V, isConstant));

  std::vector<llvm::Constant*> const_array_elems;
  if (auto inner = O->getArray("inner")) {
    for (const auto & it : *inner)
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind")) {
          if (*kind == "InitListExpr")
            const_array_elems.push_back(llvm::cast<llvm::Constant>(buildGlobalInitListExpr(P)));
          else if (*kind == "IntegerLiteral")
            const_array_elems.push_back(llvm::cast<llvm::Constant>(buildIntegerLiteral(P)));
          else if (*kind == "UnaryOperator")
            const_array_elems.push_back(llvm::cast<llvm::Constant>(buildUnaryOperator(P)));
          else if (*kind == "BinaryOperator")
            const_array_elems.push_back(llvm::cast<llvm::Constant>(buildBinaryOperator(P)));
          else if (*kind == "ImplicitValueInitExpr")
            llvm::errs() << "Error in GobalInit1\n";
        }
  } else if (auto array_filler = O->getArray("array_filler")) {
     for (const auto & it : *array_filler)
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind"))
          if (*kind == "InitListExpr")
            const_array_elems.push_back(llvm::cast<llvm::Constant>(buildGlobalInitListExpr(P)));
          else if (*kind == "IntegerLiteral")
            const_array_elems.push_back(llvm::cast<llvm::Constant>(buildIntegerLiteral(P)));
          else if (*kind == "UnaryOperator")
            const_array_elems.push_back(llvm::cast<llvm::Constant>(buildUnaryOperator(P)));
          else if (*kind == "BinaryOperator")
            const_array_elems.push_back(llvm::cast<llvm::Constant>(buildBinaryOperator(P)));
          else if (*kind != "ImplicitValueInitExpr")
            llvm::errs() << "Error in GobalInit2\n";
  }
  while (const_array_elems.size() < type->getArrayNumElements())
    const_array_elems.push_back(llvm::Constant::getNullValue(type->getArrayElementType()));

  return llvm::ConstantArray::get(type, const_array_elems);
}


// 声明局部变量
llvm::Value* buildVarDecl(const llvm::json::Object *O) {
  auto V = O->getObject("type");
  auto id = O->getString("id");
  bool isConstant;
  llvm::Type * type = buildtype(V, isConstant);

  llvm::BasicBlock *currBlock = Builder.GetInsertBlock();
  if (currBlock->getParent()->getEntryBlock().getTerminator() != nullptr) // 当前块不是最初始的块
    Builder.SetInsertPoint(currBlock->getParent()->getEntryBlock().getTerminator());

  llvm::AllocaInst * Alloca = Builder.CreateAlloca(type, 0, *id);
  syms.insert({*id, Alloca});
  Builder.SetInsertPoint(currBlock); // 生成完alloca语句之后我们回到原来的位置

  auto tmp = type;
  uint size = 1;
  while (tmp->isArrayTy()) {
    size *= tmp->getArrayNumElements();
    tmp = tmp->getArrayElementType();
  }
  size *= tmp->getIntegerBitWidth() / 8;
  if (type->isArrayTy()) // 如果是数组类型，需要我们初始化
    Builder.CreateMemSet(Alloca, llvm::ConstantInt::get(TheContext, llvm::APInt(8, 0)),
      llvm::ConstantInt::get(TheContext, llvm::APInt(32, size)), llvm::MaybeAlign(8));
  llvm::Value * Initial;
  if (auto inner = O->getArray("inner"))
    for (const auto & it : *inner)
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind"))
          if (*kind == "InitListExpr")
            buildInitListExpr(P, Alloca);
          else if (*kind == "IntegerLiteral") {
            llvm::Constant* val = buildIntegerLiteral(P);
            Builder.CreateStore(val, Alloca);
          } else if (*kind == "BinaryOperator") {
            llvm::Value *val = buildBinaryOperator(P);
            Builder.CreateStore(val, Alloca);
          } else if (*kind == "ImplicitCastExpr") {
            llvm::Value *val = buildImplicitCastExpr(P);
            Builder.CreateStore(val, Alloca);
          } else if (*kind == "CallExpr") {
            llvm::Value *val = buildCallExpr(P);
            Builder.CreateStore(val, Alloca);
          } else if (*kind == "UnaryOperator") {
            llvm::Value *val = buildUnaryOperator(P);
            Builder.CreateStore(val, Alloca);
          } else if (*kind == "StringLiteral") {
            auto ConstZero = llvm::ConstantInt::get(TheContext, llvm::APInt(32, 0));
            auto val = Builder.CreateInBoundsGEP(buildStringLiteral(P), {ConstZero, ConstZero});
            Builder.CreateMemCpy(Alloca, llvm::MaybeAlign(8), val, llvm::MaybeAlign(8), size);
          } else {
            llvm::errs() << "Error in VarDecl\n";
          }
  
  return nullptr;
}


void buildDeclStmt(const llvm::json::Object *O) {
  if (auto inner = O->getArray("inner"))
    for (const auto &it : *inner)
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind"))
          if (*kind == "VarDecl")
            buildVarDecl(P);
}


void buildReturnStmt(const llvm::json::Object *O) {
  llvm::Value *RetVal;
  llvm::Type *retType = Builder.GetInsertBlock()->getParent()->getReturnType();
  if (auto inner = O->getArray("inner")) {
    for (const auto &it : *inner) {
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind"))
          if (*kind == "IntegerLiteral")
            RetVal = buildIntegerLiteral(P);
          else if (*kind == "ImplicitCastExpr")
            RetVal = buildImplicitCastExpr(P);
          else if (*kind == "ParenExpr")
            RetVal = buildParenExpr(P);
          else if (*kind == "CallExpr")
            RetVal = buildCallExpr(P);
          else if (*kind == "BinaryOperator")
            RetVal = buildBinaryOperator(P);
          else if (*kind == "UnaryOperator")
            RetVal = buildUnaryOperator(P);
          else if (*kind == "ArraySubscriptExpr")
            RetVal = buildArraySubscriptExpr(P);
    }
    if (RetVal->getType() != retType)
      RetVal = Builder.CreateSExtOrTrunc(RetVal, retType);
    Builder.CreateRet(RetVal);
  } else {
    Builder.CreateRetVoid();
  }
}


void buildIfStmt(const llvm::json::Object *O) {
  llvm::Function *func = Builder.GetInsertBlock()->getParent();
  if (auto inner = O->getArray("inner")) {
    if (inner->size() != 2 && inner->size() != 3) {
      printf("Error in IfStmt!!!\n");
    }

    llvm::BasicBlock *ifThen = llvm::BasicBlock::Create(TheContext, "ifThen", func);
    llvm::BasicBlock *ifElse = llvm::BasicBlock::Create(TheContext, "ifElse", func);
    llvm::BasicBlock *ifEnd = llvm::BasicBlock::Create(TheContext, "ifEnd", func);
    
    auto cond_json = (*inner)[0].getAsObject();
    auto ifThen_json = (*inner)[1].getAsObject();

    if (inner->size() == 3)
      SCThenEndStack.push_back({ifThen, ifElse});
    else if (inner->size() == 2) {
      SCThenEndStack.push_back({ifThen, ifEnd});
      ifElse->eraseFromParent();
    }

    // if condiction
    llvm::Value *cond_value;
    if (*(cond_json->getString("kind")) == "BinaryOperator")
      cond_value = buildBinaryOperator(cond_json);
    else if (*(cond_json->getString("kind")) == "ImplicitCastExpr")
      cond_value = buildImplicitCastExpr(cond_json);
    else if (*(cond_json->getString("kind")) == "UnaryOperator")
      cond_value = buildUnaryOperator(cond_json);
    else if (*(cond_json->getString("kind")) == "CallExpr")
      cond_value = buildCallExpr(cond_json);
    else 
      llvm::errs() << "Error in IfCond\n";

    if (cond_value->getType() != llvm::Type::getInt1Ty(TheContext)) 
        cond_value = Builder.CreateICmpNE(cond_value, llvm::ConstantInt::get(cond_value->getType(), 0));
    Builder.CreateCondBr(cond_value, SCThenEndStack.back().then(), SCThenEndStack.back().end());
    SCThenEndStack.pop_back();

    // if then
    ifThen->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(ifThen);
    if (*(ifThen_json->getString("kind")) == "CompoundStmt")
      buildCompoundStmt(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "BinaryOperator")
      buildBinaryOperator(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "ImplicitCastExpr")
      buildImplicitCastExpr(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "ReturnStmt")
      buildReturnStmt(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "ContinueStmt")
      buildContinueStmt(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "BreakStmt")
      buildBreakStmt(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "IfStmt")
      buildIfStmt(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "WhileStmt")
      buildWhileStmt(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "CallExpr")
      buildCallExpr(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "VarDecl")
      buildVarDecl(ifThen_json);
    else if (*(ifThen_json->getString("kind")) == "DoStmt")
      buildDoStmt(ifThen_json);
    else
      llvm::errs() << "Error in ifThen\n";
    
    if (Builder.GetInsertBlock()->getTerminator() == nullptr)
      Builder.CreateBr(ifEnd);

    // if {} else {} 结构
    if (inner->size() == 3) {
      auto ifElse_json = (*inner)[2].getAsObject();
      // if else
      ifElse->moveAfter(Builder.GetInsertBlock());
      Builder.SetInsertPoint(ifElse);
      if (*(ifElse_json->getString("kind")) == "CompoundStmt")
        buildCompoundStmt(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "BinaryOperator")
        buildBinaryOperator(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "ImplicitCastExpr")
        buildImplicitCastExpr(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "ReturnStmt")
        buildReturnStmt(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "ContinueStmt")
        buildContinueStmt(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "BreakStmt")
        buildBreakStmt(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "IfStmt")
        buildIfStmt(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "WhileStmt")
        buildWhileStmt(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "CallExpr")
        buildCallExpr(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "VarDecl")
        buildVarDecl(ifElse_json);
      else if (*(ifElse_json->getString("kind")) == "DoStmt")
        buildDoStmt(ifElse_json);
      else
        llvm::errs() << "Error in ifElse\n";

      if (Builder.GetInsertBlock()->getTerminator() == nullptr)
        Builder.CreateBr(ifEnd);
    }

    ifEnd->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(ifEnd);
  }
}


void buildBreakStmt(const llvm::json::Object *O) {
  if (*(O->getString("kind")) == "BreakStmt") {
    llvm::BasicBlock *whileEnd = whileBeginEndStack.back().end();
    Builder.CreateBr(whileEnd);
  }
}


void buildContinueStmt(const llvm::json::Object *O) {
  if (*(O->getString("kind")) == "ContinueStmt") {
    llvm::BasicBlock *whileBegin = whileBeginEndStack.back().begin();
    Builder.CreateBr(whileBegin);
  }
}


void buildWhileStmt(const llvm::json::Object *O) {
  llvm::Function *func = Builder.GetInsertBlock()->getParent();
  if (auto inner = O->getArray("inner")) {
    auto whileCond_json = inner->front().getAsObject();
    auto whileBody_json = inner->back().getAsObject();

    llvm::BasicBlock *whileCond = llvm::BasicBlock::Create(TheContext, "whileCond", func);
    llvm::BasicBlock *whileBody = llvm::BasicBlock::Create(TheContext, "whileBody", func);
    llvm::BasicBlock *whileEnd = llvm::BasicBlock::Create(TheContext, "whileEnd", func);

    Builder.CreateBr(whileCond);

    // while condiction block
    SCThenEndStack.push_back({whileBody, whileEnd});
    Builder.SetInsertPoint(whileCond);
    llvm::Value *cond_value;
    if (*(whileCond_json->getString("kind")) == "BinaryOperator") 
      cond_value = buildBinaryOperator(whileCond_json);
    else if (*(whileCond_json->getString("kind")) == "UnaryOperator")
      cond_value = buildUnaryOperator(whileCond_json);
    else if (*(whileCond_json->getString("kind")) == "ImplicitCastExpr")
      cond_value = buildImplicitCastExpr(whileCond_json);
    else if (*(whileCond_json->getString("kind")) == "CallExpr")
      cond_value = buildCallExpr(whileCond_json);
    else if (*(whileCond_json->getString("kind")) == "IntegerLiteral")
      cond_value = buildIntegerLiteral(whileCond_json);
    else 
      llvm::errs() << "Error in while cond\n";
    if (cond_value->getType() != llvm::Type::getInt1Ty(TheContext)) 
        cond_value = Builder.CreateICmpNE(cond_value, llvm::ConstantInt::get(cond_value->getType(), 0));
    
    Builder.CreateCondBr(cond_value, whileBody, whileEnd);

    // 在栈中保存当前while循环体的起始与结束block，为函数体内的continue与break服务
    whileBeginEndStack.push_back({whileCond, whileEnd}); 
    // while body block
    Builder.SetInsertPoint(whileBody);
    if (*(whileBody_json->getString("kind")) == "CompoundStmt")
      buildCompoundStmt(whileBody_json);
    else if (*(whileBody_json->getString("kind")) == "BinaryOperator")
      buildBinaryOperator(whileBody_json);
    else if (*(whileBody_json->getString("kind")) == "CallExpr")
      buildCallExpr(whileBody_json);
    else if (*(whileBody_json->getString("kind")) == "ImplicitCastExpr")
      buildImplicitCastExpr(whileBody_json);
    else if (*(whileBody_json->getString("kind")) == "WhileStmt")
      buildWhileStmt(whileBody_json);
    else if (*(whileBody_json->getString("kind")) == "IfStmt")
      buildIfStmt(whileBody_json);
    else if (*(whileBody_json->getString("kind")) == "ReturnStmt")
      buildReturnStmt(whileBody_json);
    else 
      llvm::errs() << "Error in whilBody\n";

    if (Builder.GetInsertBlock()->getTerminator() == nullptr)
      Builder.CreateBr(whileCond);

    whileBeginEndStack.pop_back(); // 当前循环体结束，弹出保存的信息
    SCThenEndStack.pop_back();
    whileEnd->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(whileEnd);
  }
}


void buildDoStmt(const llvm::json::Object *O) {
  llvm::Function *func = Builder.GetInsertBlock()->getParent();
  if (auto inner = O->getArray("inner")) {
    auto doBody_json = inner->front().getAsObject();
    auto doCond_json = inner->back().getAsObject();

    llvm::BasicBlock *doBody = llvm::BasicBlock::Create(TheContext, "doBody", func);
    llvm::BasicBlock *doCond = llvm::BasicBlock::Create(TheContext, "doCond", func);
    llvm::BasicBlock *doEnd = llvm::BasicBlock::Create(TheContext, "doEnd", func);

    Builder.CreateBr(doBody);

    // do_while body
    Builder.SetInsertPoint(doBody);
    if (*(doBody_json->getString("kind")) == "CompoundStmt")
      buildCompoundStmt(doBody_json);
    if (Builder.GetInsertBlock()->getTerminator() == nullptr)
      Builder.CreateBr(doCond);
    
    // do_while cond
    doCond->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(doCond);
    llvm::Value *cond_val;
    if (*(doCond_json->getString("kind")) == "")
      cond_val = buildBinaryOperator(doCond_json);
    else if (*(doCond_json->getString("kind")) == "ImplicitCastExpr")
      cond_val = buildImplicitCastExpr(doCond_json);
    else if (*(doCond_json->getString("kind")) == "BinaryOperator")
      cond_val = buildBinaryOperator(doCond_json);
    if (cond_val->getType() != llvm::Type::getInt1Ty(TheContext))
      cond_val = Builder.CreateICmpNE(cond_val, llvm::ConstantInt::get(cond_val->getType(), 0));
    Builder.CreateCondBr(cond_val, doBody, doEnd);

    // do_while end
    doEnd->moveAfter(Builder.GetInsertBlock());
    Builder.SetInsertPoint(doEnd);
  }
}


void buildCompoundStmt(const llvm::json::Object *O) {
  if (auto inner = O->getArray("inner")) {
    for (const auto &it : *inner) 
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind"))
          if (*kind == "ReturnStmt") 
            buildReturnStmt(P);
          else if (*kind == "DeclStmt")
            buildDeclStmt(P);
          else if (*kind == "BinaryOperator")
            buildBinaryOperator(P);
          else if (*kind == "IfStmt") 
            buildIfStmt(P);
          else if (*kind == "CallExpr")
            buildCallExpr(P);
          else if (*kind == "WhileStmt")
            buildWhileStmt(P);
          else if (*kind == "BreakStmt")
            buildBreakStmt(P);
          else if (*kind == "ContinueStmt")
            buildContinueStmt(P);
          else if (*kind == "UnaryOperator")
            buildUnaryOperator(P);
          else if (*kind == "CompoundStmt")
            buildCompoundStmt(P);
          else if (*kind == "DoStmt")
            buildDoStmt(P);
          else 
            llvm::errs() << "Error in CompoundStmt\n";
  }
}


void buildParmVarDecl(const llvm::json::Object *O, llvm::Value *arg) {
  auto V = O->getObject("type");
  auto id = O->getString("id");
  bool isConstant;
  llvm::Type *type = buildtype(V, isConstant);

  llvm::AllocaInst *Alloca = Builder.CreateAlloca(type, 0, *id);
  Builder.CreateStore(arg, Alloca);
  syms.insert({*id, Alloca});
}


llvm::Function *buildFunctionDecl(const llvm::json::Object *O) {
  auto TheName = O->get("name")->getAsString()->str();
  llvm::Function *TheFunction = TheModule.getFunction(TheName);

  bool isDefine = false;
  llvm::FunctionType *type = buildFuncType(O, isDefine);

  if (!TheFunction)
    TheFunction = llvm::Function::Create(type,
        llvm::Function::ExternalLinkage, TheName, &TheModule);
  else {
    std::string TheName_ol = TheName;
    if (type->getReturnType() == llvm::Type::getInt32Ty(TheContext)) {
      TheName_ol += std::string("_ri");
    } else if (type->getReturnType() == llvm::Type::getVoidTy(TheContext)) {
      TheName_ol += std::string("_rv");
    } else if (type->getReturnType() == llvm::Type::getInt64Ty(TheContext)){
      TheName_ol += std::string("_rl");
    } else 
      llvm::errs() << "Error in FuncDecl overload retType\n";
    
    for (int i = 0; i < type->getNumParams(); ++i) {
      if (type->getParamType(i) == llvm::Type::getInt32Ty(TheContext)) {
        TheName_ol += std::string("_i");
      } else if (type->getParamType(i) == llvm::Type::getInt64Ty(TheContext)) {
        TheName_ol += std::string("_l");
      } else if (type->getParamType(i) == llvm::Type::getInt32PtrTy(TheContext)) {
        TheName_ol += std::string("_ip");
      } else if (type->getParamType(i)->isArrayTy()) {
        TheName_ol += std::string("_a");
      } else if (type->getParamType(i)->isPointerTy()) {
        TheName_ol += std::string("_p");
      } else 
        llvm::errs() << "Error in FuncDecl overload paramType\n";
    }

    auto type_str = O->getObject("type")->getString("qualType")->str();
    TheFunction = llvm::Function::Create(type,
        llvm::Function::ExternalLinkage, TheName_ol, &TheModule);
    overloadFuncs.insert({TheName+type_str, TheFunction});
  }

  if (!TheFunction)
    return nullptr;

  if (auto inner = O->getArray("inner")) {
    if (!isDefine)
      return nullptr;
    auto BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    auto arg_iter = TheFunction->arg_begin();
    for (const auto & it : *inner)
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind")) 
          if (*kind == "CompoundStmt") {
            buildCompoundStmt(P);
          } else if (*kind == "ParmVarDecl") {
            llvm::Value* arg = &(*arg_iter);
            arg_iter = std::next(arg_iter);
            buildParmVarDecl(P, arg);
          }

    // 对于返回值是 void 的函数，我们需要另外为其定义返回语句
    if (Builder.GetInsertBlock()->getTerminator() == nullptr)
      if (TheFunction->getReturnType()->isVoidTy())
        Builder.CreateRetVoid();
      else
        Builder.CreateRet(llvm::Constant::getNullValue(TheFunction->getReturnType()));
    llvm::verifyFunction(*TheFunction);
    return TheFunction;
  }

  return nullptr;
}


void buildGlobalVarDecl(const llvm::json::Object *O) {
  llvm::Type * type;
  bool isConstant = false;
  if (auto V = O->getObject("type"))
    type = buildtype(V, isConstant);
  
  auto id = O->getString("id"); // 我们将id作为变量名
  auto g = new llvm::GlobalVariable(TheModule, type, isConstant, 
                llvm::GlobalValue::PrivateLinkage, 0, *id);

  if (auto inner = O->getArray("inner")){
    for (const auto &it : *inner)
      if (auto P = it.getAsObject())
        if (auto kind = P->getString("kind"))
          if (*kind == "IntegerLiteral") 
            g->setInitializer(buildIntegerLiteral(P));
          else if (*kind == "InitListExpr")
            g->setInitializer(buildGlobalInitListExpr(P));
          else if (*kind == "UnaryOperator")
            g->setInitializer(llvm::cast<llvm::Constant>(buildUnaryOperator(P)));
          else
            g->setInitializer(llvm::Constant::getNullValue(type));
  } else 
    g->setInitializer(llvm::Constant::getNullValue(type));
}


void buildTranslationUnitDecl(const llvm::json::Object *O) {
  if (O == nullptr)
    return;
  if (auto kind = O->get("kind")->getAsString()) {
    assert(*kind == "TranslationUnitDecl");
  } else {
    assert(0);
  }
  if (auto inner = O->getArray("inner"))
    for (const auto &it : *inner)
      if (auto P = it.getAsObject())
        if (auto kind = P->get("kind")->getAsString()) {
          if (*kind == "FunctionDecl")
            buildFunctionDecl(P);
          else if (*kind == "VarDecl")
            buildGlobalVarDecl(P);
        }
}
} // namespace

int main() {
  auto llvmin = llvm::MemoryBuffer::getFileOrSTDIN("-");
  auto json = llvm::json::parse(llvmin.get()->getBuffer());
  buildTranslationUnitDecl(json->getAsObject());
  TheModule.print(llvm::outs(), nullptr);
}