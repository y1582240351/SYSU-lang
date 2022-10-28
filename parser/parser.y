%{
#include "parser.hh"
#include <llvm/Support/JSON.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/raw_ostream.h>
#include <cstdio>
#include <string>
#include <sstream>
#define yyerror(x)                                                             \
  do {                                                                         \
    llvm::errs() << (x);                                                       \
  } while (0)
namespace {
auto llvmin = llvm::MemoryBuffer::getFileOrSTDIN("-");
auto input = llvmin.get() -> getBuffer();
auto end = input.end(), it = input.begin();
auto wk_getline(char endline = "\n"[0]) {
  auto beg = it;
  while (it != end && *it != endline)
    ++it;
  auto len = it - beg;
  if (it != end && *it == endline)
    ++it;
  return llvm::StringRef(beg, len);
}
llvm::json::Array stak;
llvm::json::Value tmp(llvm::json::Object{{}});  // 为if-else打的补丁
bool isElse = true;
int dec_counter = 0;
llvm::json::Object sym_table; // 简易版符号表，用来记录变量的类型

int getPriority(std::string &type) {
  if (type == "int")
    return 0;
  if (type == "unsigned int")
    return 1;
  if (type == "long")
    return 2;
  return 0;
}

} // namespace
auto yylex() {
  if (!isElse) {
    stak.push_back(tmp);
    isElse = true;
  }

  auto tk = wk_getline();
  auto b = tk.find("'") + 1, e = tk.rfind("'");
  auto s = tk.substr(b, e - b), t = tk.substr(0, tk.find(" "));
  if (t == "numeric_constant") {
    long num;
    s.getAsInteger(0, num);

    std::string type;
    if (num <= 0x7fFffffF) {
      type = "int";
    } else if (num <= 0xffffffff) {
      type = "unsigned int";
    } else 
      type = "long";
    stak.push_back(llvm::json::Object{{"kind", "IntegerLiteral"},
                                      {"value", std::to_string(num)},
                                      {"type", type}
                                    });
    return T_NUMERIC_CONSTANT;
  }
  if (t == "identifier") {
    stak.push_back(llvm::json::Object{{"kind", "ident"}, {"value", s}});
    return T_IDENTIFIER;
  }
  if (t == "int")
    return T_INT;
  if (t == "void")
    return T_VOID;
  if (t == "return")
    return T_RETURN;
  if (t == "semi")
    return T_SEMI;
  if (t == "l_paren")
    return T_L_PAREN;
  if (t == "r_paren")
    return T_R_PAREN;
  if (t == "l_brace")
    return T_L_BRACE;
  if (t == "r_brace")
    return T_R_BRACE;
  if (t == "l_square")
    return T_L_SQUARE;
  if (t == "r_square")
    return T_R_SQUARE;
  if (t == "comma")
    return T_COMMA;
  if (t == "equal")
    return T_EQUAL;
  if (t == "plus")
    return T_PLUS;
  if (t == "minus")
    return T_MINUS;
  if (t == "star")
    return T_STAR;
  if (t == "slash")
    return T_SLASH;
  if (t == "percent")
    return T_PERCENT;
  if (t == "const")
    return T_CONST;
  if (t == "if")
    return T_IF;
  if (t == "pipepipe")
    return T_PIPEPIPE;
  if (t == "ampamp")
    return T_AMPAMP;
  if (t == "exclaimequal")
    return T_EXCLAIMEQUAL;
  if (t == "equalequal")
    return T_EQUALEQUAL;
  if (t == "less")
    return T_LESS;
  if (t == "greater")
    return T_GREATER;
  if (t == "lessequal")
    return T_LESSEQUAL;
  if (t == "greaterequal")
    return T_GREATEREQUAL;
  if (t == "while")
    return T_WHILE;
  if (t == "break")
    return T_BREAK;
  if (t == "continue")
    return T_CONTINUE;
  if (t == "else")
    return T_ELSE;
  if (t == "exclaim")
    return T_EXCLAIM;
  if (t == "do")
    return T_DO;
  if (t == "char")
    return T_CHAR;
  if (t == "string_literal") {
    stak.push_back(llvm::json::Object{{"kind", "StringLiteral"}, {"value", s}});
    return T_STRING;
  }
  return YYEOF;
}
int main() {
  yyparse();
  llvm::outs() << stak.back() << "\n";
}
%}
%token T_NUMERIC_CONSTANT
%token T_IDENTIFIER
%token T_INT
%token T_VOID
%token T_RETURN
%token T_SEMI
%token T_L_PAREN
%token T_R_PAREN
%token T_L_BRACE
%token T_R_BRACE
%token T_L_SQUARE
%token T_R_SQUARE
%start CompUnit
%token T_COMMA
%token T_EQUAL
%token T_PLUS
%token T_MINUS
%token T_STAR
%token T_SLASH
%token T_PERCENT
%token T_CONST
%token T_IF
%token T_PIPEPIPE
%token T_AMPAMP
%token T_EXCLAIMEQUAL
%token T_EQUALEQUAL
%token T_LESS
%token T_GREATER
%token T_LESSEQUAL
%token T_GREATEREQUAL
%token T_WHILE
%token T_BREAK
%token T_CONTINUE
%token T_ELSE
%token T_EXCLAIM
%token T_DO
%token T_CHAR
%token T_STRING
%right IF_THEN T_ELSE
%right PrimLVal T_R_PAREN
%right PrimStrings T_STRING
%%

CompUnit
: FuncDef  {
  auto inner = stak.back();
  stak.pop_back();
  stak.push_back(llvm::json::Object{{"kind", "TranslationUnitDecl"},
                                    {"range", },
                                    {"inner", llvm::json::Array{inner}}});
}
| VarDecl  {
  llvm::json::Array VarDecl;
  auto inner = stak.back().getAsObject();
  while (inner->getString("kind")->equals("VarDecl")) {
    VarDecl.push_back(std::move(*inner));
    stak.pop_back();
    if (stak.empty())
      break;
    inner = stak.back().getAsObject();
  }
  
  stak.push_back(llvm::json::Object{{"kind", "TranslationUnitDecl"},
                                    {"range", },
                                    {"inner", std::move(VarDecl)}});
}
| CompUnit VarDecl {
  llvm::json::Array VarDecl;
  auto inner = stak.back().getAsObject();
  while (inner->getString("kind")->equals("VarDecl")) {
    VarDecl.push_back(std::move(*inner));
    stak.pop_back();
    if (stak.empty())
      break;
    inner = stak.back().getAsObject();
  }

  auto CompUnit = stak.back().getAsObject();
  assert(CompUnit->getString("kind")->equals("TranslationUnitDecl"));
  assert(CompUnit->getArray("inner") != nullptr);

  for (auto iter = VarDecl.begin(); iter != VarDecl.end(); ++iter) {
    CompUnit->getArray("inner")->push_back(*iter);
  }
}
| CompUnit FuncDef {
  auto FuncDef = stak.back();
  stak.pop_back();

  auto CompUnit = stak.back().getAsObject();
  assert(CompUnit->getString("kind")->equals("TranslationUnitDecl"));
  assert(CompUnit->getArray("inner") != nullptr);

  CompUnit->getArray("inner")->push_back(FuncDef);
}
;


FuncDef
: BType Ident T_L_PAREN T_R_PAREN Block {
  auto Block = stak.back();
  stak.pop_back();

  auto Ident = stak.back();
  // assert(Ident != nullptr);
  assert(Ident.getAsObject()->get("value") != nullptr);
  stak.pop_back();

  auto BType = stak.back();
  assert(BType.getAsObject()->get("kind") != nullptr);
  stak.pop_back();
  std::string type = BType.getAsObject()->getString("kind")->str() + " ()";

  // assert(Ident->get("value"));
  if (sym_table.get(*(Ident.getAsObject()->getString("value"))) == nullptr) {
    sym_table.insert({Ident.getAsObject()->getString("value")->str(), type});
  }

  stak.push_back(llvm::json::Object{{"loc", },
                                    {"range", },
                                    {"kind", "FunctionDecl"},
                                    {"name", *(Ident.getAsObject()->get("value"))},
                                    {"mangledName", *(Ident.getAsObject()->get("value"))},
                                    {"inner", llvm::json::Array{Block}},
                                    {"type", llvm::json::Object{{"qualType", type}}}
                                  });
}
| BType Ident T_L_PAREN FuncFParams T_R_PAREN Block {
  auto Block = stak.back();
  stak.pop_back();

  auto tmp = stak.back().getAsObject();
  assert(tmp->getArray("FuncFParams") != nullptr);
  assert(tmp->get("types") != nullptr);
  auto list = tmp->getArray("FuncFParams");
  llvm::json::Array FuncFParams;
  for (auto iter = list->begin(); iter != list->end(); ++iter) 
    FuncFParams.push_back(*iter); 
  FuncFParams.push_back(std::move(Block));
  auto types = tmp->getString("types")->str();
  stak.pop_back();

  auto Ident = stak.back();
  // assert(Ident != nullptr);
  assert(Ident.getAsObject()->get("value") != nullptr);
  stak.pop_back();

  auto BType = stak.back();
  assert(BType.getAsObject()->get("kind") != nullptr);
  stak.pop_back();
  std::string type = BType.getAsObject()->getString("kind")->str() + std::string(" (") + types + ")";

  if (sym_table.get(*(Ident.getAsObject()->getString("value"))) == nullptr) {
    sym_table.insert({Ident.getAsObject()->getString("value")->str(), type});
  }

  stak.push_back(llvm::json::Object{{"kind", "FunctionDecl"},
                                    {"name", *(Ident.getAsObject()->get("value"))},
                                    {"mangledName", *(Ident.getAsObject()->get("value"))},
                                    {"inner", std::move(FuncFParams)},
                                    {"type", llvm::json::Object{{"qualType", type}}}
                                  });
}
| BType Ident T_L_PAREN T_R_PAREN T_SEMI {
  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  assert(Ident != nullptr);
  assert(Ident->get("value") != nullptr);
  stak.pop_back();

  auto tmp_1 = stak.back();
  auto BType = tmp_1.getAsObject();
  assert(BType->get("kind") != nullptr);
  stak.pop_back();
  std::string type = BType->getString("kind")->str() + " ()";

  if (sym_table.get(*(Ident->getString("value"))) == nullptr) {
    sym_table.insert({Ident->getString("value")->str(), type});
  }

  stak.push_back(llvm::json::Object{{"loc", },
                                    {"range", },
                                    {"kind", "FunctionDecl"},
                                    {"name", *(Ident->get("value"))},
                                    {"mangledName", *(Ident->get("value"))},
                                    {"type", llvm::json::Object{{"qualType", type}}}
                                  });
}
| BType Ident T_L_PAREN FuncFParams T_R_PAREN T_SEMI {
  auto tmp = stak.back().getAsObject();
  assert(tmp->getArray("FuncFParams") != nullptr);
  assert(tmp->get("types") != nullptr);
  auto list = tmp->getArray("FuncFParams");
  llvm::json::Array FuncFParams;
  for (auto iter = list->begin(); iter != list->end(); ++iter) 
    FuncFParams.push_back(*iter); 
  auto types = tmp->getString("types")->str();
  stak.pop_back();

  auto tmp_1 = stak.back();
  auto Ident = tmp_1.getAsObject();
  assert(Ident != nullptr);
  assert(Ident->get("value") != nullptr);
  stak.pop_back();

  auto tmp_2 = stak.back();
  auto BType = tmp_2.getAsObject();
  assert(BType->get("kind") != nullptr);
  stak.pop_back();
  std::string type = BType->getString("kind")->str() + std::string(" (") + types + ")";

  if (sym_table.get(*(Ident->getString("value"))) == nullptr) {
    sym_table.insert({Ident->getString("value")->str(), type});
  }

  stak.push_back(llvm::json::Object{{"kind", "FunctionDecl"},
                                    {"name", *(Ident->get("value"))},
                                    {"mangledName", *(Ident->get("value"))},
                                    {"inner", std::move(FuncFParams)},
                                    {"type", llvm::json::Object{{"qualType", type}}}
                                  });
}
;


FuncFParams
: FuncFParam {
  auto FuncFParam = stak.back();
  stak.pop_back();

  std::string types(FuncFParam.getAsObject()->getObject("type")->getString("qualType")->str());
  stak.push_back(llvm::json::Object{{"FuncFParams", llvm::json::Array{FuncFParam}},
                                    {"types", types}
                                  });
}
| FuncFParams T_COMMA FuncFParam {
  auto FuncFParam = stak.back();
  stak.pop_back();

  auto FuncFParams = stak.back().getAsObject();
  assert(FuncFParams->get("FuncFParams") != nullptr);
  assert(FuncFParams->get("types") != nullptr);

  std::string types = FuncFParams->getString("types")->str() + ","
          + FuncFParam.getAsObject()->getObject("type")->getString("qualType")->str();
  (*FuncFParams)["types"] = types;
  FuncFParams->getArray("FuncFParams")->push_back(std::move(FuncFParam));
}
;


FuncFParam
: BType Ident {
  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  assert(Ident->get("value") != nullptr);
  stak.pop_back();

  auto tmp_1 = stak.back();
  auto BType = tmp_1.getAsObject();
  assert(BType->get("kind") != nullptr);
  stak.pop_back();

  if (sym_table.get(*(Ident->getString("value"))) == nullptr) {
    sym_table.insert({Ident->getString("value")->str(), BType->getString("kind")->str()});
  }

  stak.push_back(llvm::json::Object{{"kind", "ParmVarDecl"},
                                    {"name", *(Ident->get("value"))},
                                    {"type", llvm::json::Object{{"qualType", *(BType->get("kind"))}}}
                                  });
}
| BType Ident T_L_SQUARE T_R_SQUARE {
  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  assert(Ident->get("value") != nullptr);
  stak.pop_back();

  auto tmp_1 = stak.back();
  auto BType = tmp_1.getAsObject();
  assert(BType->get("kind") != nullptr);
  stak.pop_back();
  std::string type = BType->getString("kind")->str() + " []";

  if (sym_table.get(*(Ident->getString("value"))) == nullptr) {
    sym_table.insert({Ident->getString("value")->str(), BType->getString("kind")->str()});
  }

  stak.push_back(llvm::json::Object{{"kind", "ParmVarDecl"},
                                    {"name", *(Ident->get("value"))},
                                    {"type", llvm::json::Object{{"qualType", type},
                                                                {"desugaredQualType", type}
                                                              }},
                                  });
}
| BType Ident T_L_SQUARE T_R_SQUARE ArrExp {
  auto ArrExp = stak.back().getAsObject();
  assert(ArrExp->get("kind")->getAsString()->equals(llvm::StringRef("ArrExp")));
  assert(ArrExp->get("value")->getAsArray() != nullptr);
  auto value = ArrExp->get("value")->getAsArray();
  std::string arr("[]");
  for (auto iter = value->begin(); iter != value->end(); ++iter) {
    arr += std::string("[") + iter->getAsString()->str() + std::string("]");
  }
  stak.pop_back();

  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  assert(Ident->get("value") != nullptr);
  stak.pop_back();

  auto tmp_1 = stak.back();
  auto BType = tmp_1.getAsObject();
  assert(BType->get("kind") != nullptr);
  stak.pop_back();
  std::string type = BType->getString("kind")->str() + " (*)" + arr;

  if (sym_table.get(*(Ident->getString("value"))) == nullptr) {
    sym_table.insert({Ident->getString("value")->str(), BType->getString("kind")->str()});
  }

  stak.push_back(llvm::json::Object{{"kind", "ParmVarDecl"},
                                    {"name", *(Ident->get("value"))},
                                    {"type", llvm::json::Object{{"qualType", type},
                                                                {"desugaredQualType", type}
                                                              }}
                                  });
}
;


/* FuncType
: T_INT {
  stak.push_back(llvm::json::Object{{"kind", "int"}});
}
| T_VOID {
  stak.push_back(llvm::json::Object{{"kind", "int"}});
} */


BType
: T_INT {
  stak.push_back(llvm::json::Object{{"kind", "int"}});
}
| T_CONST T_INT {
  stak.push_back(llvm::json::Object{{"kind", "const int"}});
}
| T_VOID {
  stak.push_back(llvm::json::Object{{"kind", "int"}});
}
| T_CHAR {
  stak.push_back(llvm::json::Object{{"kind", "char"}});
}
| T_CONST T_CHAR {
  stak.push_back(llvm::json::Object{{"kind", "const char"}});
}
;


Ident
: T_IDENTIFIER {}


VarDecl
: BType VarDef T_SEMI {
  // VarDef: {"VarDef", [var, var, var]}
  auto VarDef = stak.back().getAsObject();
  assert(VarDef->get("VarDef")->getAsArray() != nullptr);
  llvm::json::Array list;  // 储存每一个var
  auto tmp = VarDef->getArray("VarDef");
  while (tmp->size()) {
    auto i = tmp->back();
    list.push_back(std::move(i));
    tmp->pop_back();
  }
  stak.pop_back();

  // 处理 BType
  auto BType = stak.back().getAsObject()->getString("kind")->str();
  stak.pop_back();

  for (auto iter = list.begin(); iter != list.end(); ++iter) {
    std::string type("");
    if (iter->getAsObject()->getString("type")->size() == 0) {
      type = BType;
    } else {
      type = BType + " ";
      auto strs = iter->getAsObject()->getString("type")->split(',');
      type += "[" + strs.first.str() + "]";
      while (strs.second.size() != 0) {
        strs = strs.second.split(',');
        type += "[" + strs.first.str() + "]";
      } 
    }

    if (sym_table.get(*(iter->getAsObject()->getString("name"))) == nullptr) {
      sym_table.insert({iter->getAsObject()->getString("name")->str(), type});
    }

		(*(iter->getAsObject()))["type"] = llvm::json::Object{{"qualType", type}};
    stak.push_back(std::move(*iter));
  }
}


VarDef
: Var {
  auto Var = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"VarDef", llvm::json::Array{std::move(Var)}}});
}
| VarDef T_COMMA Var {
  auto Var = stak.back();
  stak.pop_back();

  auto VarDef = stak.back().getAsObject();
  assert(VarDef->get("VarDef")->getAsArray() != nullptr);
  auto list = VarDef->getArray("VarDef");
  list->push_back(std::move(Var));
}
;


Var
: Ident {
  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  assert(Ident != nullptr);
  assert(Ident->get("value") != nullptr);
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "VarDecl"},
                                {"type", ""},
                                {"range", },
                                {"loc", },
                                {"name", *(Ident->get("value"))},
                                {"mangledName", *(Ident->get("value"))},
                                {"isUsed", }
                              });
}
| Ident ArrExp {
  auto ArrExp = stak.back().getAsObject();
  assert(ArrExp->get("kind")->getAsString()->equals(llvm::StringRef("ArrExp")));
  assert(ArrExp->get("value")->getAsArray() != nullptr);

  auto value = ArrExp->get("value")->getAsArray();
  std::string arr("");
  for (auto iter = value->begin(); iter != value->end(); ++iter) {
    arr += iter->getAsString()->str() + std::string(",");
  }

  stak.pop_back();

  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  assert(Ident != nullptr);
  assert(Ident->get("value") != nullptr);
  stak.pop_back();
  
  stak.push_back(llvm::json::Object{{"kind", "VarDecl"},
                                {"type", arr},
                                {"range", },
                                {"loc", },
                                {"name", *(Ident->get("value"))},
                                {"mangledName", *(Ident->get("value"))}
                              });
}
| Ident T_EQUAL InitVal {
  auto Exp = stak.back();
  auto Exp_type = Exp.getAsObject()->getObject("type")->getString("qualType")->str();
	stak.pop_back();
	auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  assert(Ident != nullptr);
  assert(Ident->get("value") != nullptr);
  stak.pop_back();

  int prior = getPriority(Exp_type);

  llvm::json::Array inner{std::move(Exp)};
  
  if (prior != 0) {
    stak.push_back(llvm::json::Object{{"kind", "VarDecl"},
                                  {"type", ""},
                                  {"range", },
                                  {"loc", },
                                  {"name", *(Ident->get("value"))},
                                  {"mangledName", *(Ident->get("value"))},
                                  {"inner", llvm::json::Array{
                                  llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", std::move(inner)},
                                                      {"type", llvm::json::Object{{"qualType", "int"}}}
                                  }}}
                                });
  } else {
    stak.push_back(llvm::json::Object{{"kind", "VarDecl"},
                                  {"type", ""},
                                  {"range", },
                                  {"loc", },
                                  {"name", *(Ident->get("value"))},
                                  {"mangledName", *(Ident->get("value"))},
                                  {"inner", std::move(inner)}
                                });
  }
}
| Ident ArrExp T_EQUAL InitVal {
  auto Exp = stak.back();
	llvm::json::Array inner{std::move(Exp)};
	stak.pop_back();

	auto ArrExp = stak.back().getAsObject();
  assert(ArrExp->get("kind")->getAsString()->equals(llvm::StringRef("ArrExp")));
  assert(ArrExp->get("value")->getAsArray() != nullptr);

  auto value = ArrExp->get("value")->getAsArray();
  std::string arr("");
  for (auto iter = value->begin(); iter != value->end(); ++iter) {
    arr += iter->getAsString()->str() + std::string(",");
  }

  stak.pop_back();

  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  assert(Ident != nullptr);
  assert(Ident->get("value") != nullptr);
  stak.pop_back();
  
  stak.push_back(llvm::json::Object{{"kind", "VarDecl"},
                                {"type", arr},
                                {"loc", },
                                {"range", },
                                {"name", *(Ident->get("value"))},
                                {"mangledName", *(Ident->get("value"))},
																{"inner", std::move(inner)}
															});
}
;


ArrExp
: T_L_SQUARE Exp T_R_SQUARE  {
  auto number = stak.back().getAsObject();
  auto num_kind = number->getString("kind")->str();
  std::string num_type;
  if (num_kind == "IntegerLiteral")
    num_type = number->getString("value")->str();
  stak.pop_back();

  if (num_kind == "IntegerLiteral") {
    stak.push_back(llvm::json::Object{{"kind", "ArrExp"},
                                    {"value", llvm::json::Array{num_type}}});
  } else {
    stak.push_back(llvm::json::Object{{"kind", "ArrExp"},
                                    {"value", llvm::json::Array{"[]"}}
                                  });
  }
}
| ArrExp T_L_SQUARE Exp T_R_SQUARE {
  auto number = stak.back().getAsObject();
  auto num_kind = number->getString("kind")->str();
  std::string num_type;
  if (num_kind == "IntegerLiteral")
    num_type = number->getString("value")->str();
  stak.pop_back(); 

  auto ArrExp = stak.back().getAsObject();

  assert(ArrExp != nullptr);
  assert(ArrExp->get("kind")->getAsString()->equals(llvm::StringRef("ArrExp")));
  assert(ArrExp->get("value")->getAsArray() != nullptr);

  if (num_kind == "IntegerLiteral") {
    ArrExp->getArray("value")->push_back(num_type);
  } else
    ArrExp->getArray("value")->push_back(llvm::StringRef("0"));
}
;


InitVal
: Exp {

}
| T_L_BRACE T_R_BRACE {
  stak.push_back(llvm::json::Object{{"kind", "InitListExpr"}});
}
| T_L_BRACE InitValList T_R_BRACE {
  stak.push_back(llvm::json::Object{{"kind", "InitListExpr"}});
}
;


InitValList
: InitVal {
  // 将栈尾被压入的表达式出栈
  stak.pop_back();
}
| InitValList T_COMMA InitVal {
  stak.pop_back();
}
;


Block
: T_L_BRACE T_R_BRACE {
  stak.push_back(llvm::json::Object{{"kind", "CompoundStmt"},
                                    {"range", }
                                  });
}
| T_L_BRACE BlockItem T_R_BRACE {
  auto inner = stak.back();

  assert(inner.getAsObject()->get("BlockItem") != nullptr);
  assert(inner.getAsObject()->getArray("BlockItem") != nullptr);

  stak.pop_back();
  stak.push_back(llvm::json::Object{{"kind", "CompoundStmt"},
                                    {"range", },
                                    {"inner", *(inner.getAsObject()->get("BlockItem"))}
                                  });
}
;


BlockItem
: Stmt {
  auto Stmt = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"BlockItem", llvm::json::Array{Stmt}}});
}
| BlockItem Stmt {
  llvm::json::Value Stmt = stak.back();
  stak.pop_back();

  auto BlockItem = stak.back().getAsObject();
  assert(BlockItem->get("BlockItem") != nullptr);
  assert(BlockItem->getArray("BlockItem") != nullptr);

  BlockItem->getArray("BlockItem")->push_back(std::move(Stmt));
}
;


Decl
:	VarDecl {
	
}


Stmt
: T_RETURN Exp T_SEMI {
  llvm::json::Value Exp = stak.back();
  stak.pop_back();
  stak.push_back(llvm::json::Object{{"kind", "ReturnStmt"},
                                    {"range", },
                                    {"inner", llvm::json::Array{std::move(Exp)}}
                                  });
}
| T_RETURN T_SEMI {
  stak.push_back(llvm::json::Object{{"kind", "ReturnStmt"},
                                    {"range", },
                                  });
}
| LVal T_EQUAL Exp T_SEMI {
  auto Exp = stak.back();
  auto Exp_type = Exp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  auto LVal = stak.back();
  stak.pop_back();

  int prior = getPriority(Exp_type);
  llvm::json::Array inner{std::move(LVal), std::move(Exp)}; 
  
  if (prior != 0) {
    llvm::json::Object tmp{{"kind", "ImplicitCastExpr"},
                           {"valueCategory", "rvalue"},
                           {"inner", std::move(inner)},
                           {"type", llvm::json::Object{{"qualType", "int"}}}
                          };
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                      {"range", },
                                      {"valueCategory", "rvalue"},
                                      {"opcode", "="},
                                      {"type", llvm::json::Object{{"qualType", "int"}}},
                                      {"inner", llvm::json::Array{std::move(tmp)}}
                                    });
  }
  else
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                      {"range", },
                                      {"valueCategory", "rvalue"},
                                      {"opcode", "="},
                                      {"type", llvm::json::Object{{"qualType", "int"}}},
                                      {"inner", std::move(inner)}
                                    });
} 
| Decl {
	llvm::json::Array Decls;
	auto VarDecl = stak.back().getAsObject();
	assert(VarDecl->getString("kind")->equals("VarDecl"));
	while (VarDecl->getString("kind")->equals("VarDecl")) {
		Decls.push_back(std::move(*VarDecl));
		stak.pop_back();
		VarDecl = stak.back().getAsObject();
		if (VarDecl->get("kind") == nullptr)
			break;
	}

	stak.push_back(llvm::json::Object{{"kind", "DeclStmt"},
                                    {"range", },
																		{"inner", std::move(Decls)}
																	});
}
| Exp T_SEMI{
  
}
| Block {

}
| T_IF T_L_PAREN Cond T_R_PAREN Stmt  %prec IF_THEN {
  auto ident = stak.back();

  if (ident.getAsObject()->get("kind") != nullptr) {
    if (ident.getAsObject()->getString("kind")->equals(llvm::StringRef("IntegerLiteral"))) {
      // 说明stak尾部被提前压入了奇怪的东西
      tmp = ident;
      isElse = false;
      stak.pop_back();
    } else if (ident.getAsObject()->getString("kind")->equals(llvm::StringRef("ident"))) {
      tmp = ident;
      isElse = false;
      stak.pop_back();
    } else if (ident.getAsObject()->getString("kind")->equals(llvm::StringRef("string_literal"))) {
      tmp = ident;
      isElse = false;
      stak.pop_back();
    }
  }

  auto Stmt = stak.back();
  stak.pop_back();

  auto Cond = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "IfStmt"},
                                    {"inner", llvm::json::Array{std::move(Cond), std::move(Stmt)}}
                                  });
}
| T_IF T_L_PAREN Cond T_R_PAREN Stmt T_ELSE Stmt {
  auto Stmt_2 = stak.back();
  stak.pop_back();

  auto Stmt_1 = stak.back();
  stak.pop_back();

  auto Cond = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "IfStmt"},
                                    {"inner", llvm::json::Array{std::move(Cond), std::move(Stmt_1), std::move(Stmt_2)}}
                                  });
}
| T_WHILE T_L_PAREN Cond T_R_PAREN Stmt {
  auto Stmt = stak.back();
  stak.pop_back();

  auto Cond = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "WhileStmt"},
                                    {"inner", llvm::json::Array{std::move(Cond), std::move(Stmt)}}
                                  });
}
| T_BREAK T_SEMI {
  stak.push_back(llvm::json::Object{{"kind", "BreakStmt"}});
}
| T_CONTINUE T_SEMI {
  stak.push_back(llvm::json::Object{{"kind", "ContinueStmt"}});
}
| T_SEMI {
  stak.push_back(llvm::json::Object{{"kind", "NullStmt"}});
}
| T_DO Stmt T_WHILE T_L_PAREN Cond T_R_PAREN T_SEMI {
  auto Cond = stak.back();
  stak.pop_back();

  auto Stmt = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "DoStmt"},
                                    {"inner", llvm::json::Array{std::move(Stmt), std::move(Cond)}}
                                  });
}
;


LVal
: Ident {
  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  stak.pop_back();

  std::string type;
  if (sym_table.get(*(Ident->getString("value"))) != nullptr) {
    type = sym_table.getString(*(Ident->getString("value")))->str();
  } else {
    type = std::string("int");
  }

  stak.push_back(llvm::json::Object{{"kind", "DeclRefExpr"},
                                    {"range", },
                                    {"name", *(Ident->get("value"))},
                                    {"valueCategory", "lvalue"},
                                    {"referencedDecl", },
                                    {"type", llvm::json::Object{{"qualType", type}}}
                                  });
}
| ArrSubExp {

}
| T_L_PAREN LVal T_R_PAREN {
  auto LVal = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "ParenExpr"},
                                    {"valueCategory", "lvalue"},
                                    {"name", *(LVal.getAsObject()->getString("name"))},
                                    {"inner", llvm::json::Array{std::move(LVal)}},
                                    {"type", llvm::json::Object{{"qualType", "int"}}}
                                  });
}
;


ArrSubExp
: PrimaryExp T_L_SQUARE Exp T_R_SQUARE {
  auto Exp = stak.back();
  stak.pop_back();

  auto PrimaryExp = stak.back();
  auto PrimaryExp_type = PrimaryExp.getAsObject()->getObject("type")->getString("qualType")->str();
  if (PrimaryExp_type.find('[') != std::string::npos)
    PrimaryExp_type = PrimaryExp_type.substr(0, PrimaryExp_type.rfind(' '));
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "ArraySubscriptExpr"},
                                    {"inner", llvm::json::Array{std::move(PrimaryExp), std::move(Exp)}},
                                    {"type", llvm::json::Object{{"qualType", PrimaryExp_type}}}
                                  });
}
;


Exp
: AddExp {

}


AddExp
: MulExp {

}
| AddExp T_PLUS MulExp {
  llvm::json::Value MulExp = stak.back();
  auto MulExp_type = MulExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  llvm::json::Value AddExp = stak.back();
  auto AddExp_type = AddExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  int former = getPriority(AddExp_type);
  int later = getPriority(MulExp_type);
  llvm::json::Object type{{"qualType", "int"}};
  if (former < later) {
    auto qualType = MulExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(AddExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}},
                                      std::move(MulExp)
                                    }}
                                  });
  } else if (later < former) {
    auto qualType = AddExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{ std::move(AddExp),
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(MulExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}}
                                    }}
                                  });
  } else {
    auto qualType = AddExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{std::move(AddExp), std::move(MulExp)}}
                                  });
  }
}
| AddExp T_MINUS MulExp {
  llvm::json::Value MulExp = stak.back();
  auto MulExp_type = MulExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  llvm::json::Value AddExp = stak.back();
  auto AddExp_type = AddExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  int former = getPriority(AddExp_type);
  int later = getPriority(MulExp_type);
  llvm::json::Object type{{"qualType", }};
  if (former < later) {
    auto qualType = MulExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(AddExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}},
                                      std::move(MulExp)
                                    }}
                                  });
  } else if (later < former) {
    auto qualType = AddExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{ std::move(AddExp),
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(MulExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}}
                                    }}
                                  });
  } else {
    auto qualType = AddExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{std::move(AddExp), std::move(MulExp)}}
                                  });
  }
}
;


MulExp
: UnaryExp {

}
| MulExp T_STAR UnaryExp {
  llvm::json::Value UnaryExp = stak.back();
  auto UnaryExp_type = UnaryExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  llvm::json::Value MulExp = stak.back();
  auto MulExp_type = MulExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  int former = getPriority(UnaryExp_type);
  int later = getPriority(MulExp_type);
  llvm::json::Object type{{"qualType", }};
  if (former < later) {
    auto qualType = MulExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{ std::move(MulExp),
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(UnaryExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}}
                                    }}
                                  });
  } else if (later < former) {
    auto qualType = UnaryExp_type;
    type["qualType"] = qualType;
   stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(MulExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}},
                                      std::move(UnaryExp)
                                    }}
                                  });
  }else {
    auto qualType = UnaryExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{std::move(MulExp), std::move(UnaryExp)}}
                                  });
  }
}
| MulExp T_SLASH UnaryExp {
  llvm::json::Value UnaryExp = stak.back();
  auto UnaryExp_type = UnaryExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  llvm::json::Value MulExp = stak.back();
  auto MulExp_type = MulExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  int former = getPriority(UnaryExp_type);
  int later = getPriority(MulExp_type);
  llvm::json::Object type{{"qualType", }};
  if (former < later) {
    auto qualType = MulExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{ std::move(MulExp),
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(UnaryExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}}
                                    }}
                                  });
  } else if (later < former) {
    auto qualType = UnaryExp_type;
    type["qualType"] = qualType;
   stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(MulExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}},
                                      std::move(UnaryExp)
                                    }}
                                  });
  }else {
    auto qualType = UnaryExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{std::move(MulExp), std::move(UnaryExp)}}
                                  });
  }
}
| MulExp T_PERCENT UnaryExp {
  llvm::json::Value UnaryExp = stak.back();
  auto UnaryExp_type = UnaryExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  llvm::json::Value MulExp = stak.back();
  auto MulExp_type = MulExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  int former = getPriority(UnaryExp_type);
  int later = getPriority(MulExp_type);
  llvm::json::Object type{{"qualType", }};
  if (former < later) {
    auto qualType = MulExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{ std::move(MulExp),
                                      llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                      {"valueCategory", "rvalue"},
                                                      {"inner", llvm::json::Array{std::move(UnaryExp)}},
                                                      {"type", llvm::json::Object{{"qualType", qualType}}}}
                                    }}
                                  });
  } else if (later < former) {
    auto qualType = UnaryExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                     {"valueCategory", "rvalue"},
                                     {"opcode", "-"},
                                     {"type", std::move(type)},
                                     {"inner", llvm::json::Array{
                                       llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                                       {"valueCategory", "rvalue"},
                                                       {"inner", llvm::json::Array{std::move(MulExp)}},
                                                       {"type", llvm::json::Object{{"qualType", qualType}}}},
                                       std::move(UnaryExp)
                                     }}
                                   });
  }else {
    auto qualType = UnaryExp_type;
    type["qualType"] = qualType;
    stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "-"},
                                    {"type", std::move(type)},
                                    {"inner", llvm::json::Array{std::move(MulExp), std::move(UnaryExp)}}
                                  });
  }
}
;


UnaryExp
: PrimaryExp {

}
// 函数调用使用的是函数指针，因此函数名也相当于一个左值。可能有bug
| Ident T_L_PAREN FuncRParams T_R_PAREN {
  auto tmp = stak.back().getAsObject();
  assert(tmp->getArray("FuncRParams") != nullptr);
  auto list = tmp->getArray("FuncRParams");
  llvm::json::Array FuncRParams;
  for (auto iter = list->begin(); iter != list->end(); ++iter) 
    FuncRParams.push_back(std::move(*iter)); 
  stak.pop_back();

  auto tmp_1 = stak.back();
  auto Ident = tmp_1.getAsObject();
  stak.pop_back();

  std::string type(""), return_type("");
  if (sym_table.get(*(Ident->getString("value"))) != nullptr) {
    type = sym_table.getString(*(Ident->getString("value")))->str();
    return_type = sym_table.getString(*(Ident->getString("value")))->split(' ').first.str();
    auto tmp = sym_table.getString(*(Ident->getString("value")))->split(' ').second;
    auto params_type = tmp.substr(1, tmp.size()-2).split(',');
    
    for (int i = 1; i < FuncRParams.size(); ++i) {
      if (params_type.first.find('[') != llvm::StringLiteral::npos){
        params_type = params_type.second.split(',');
        continue;
      }
      if (!FuncRParams[i].getAsObject()->getObject("type")->getString("qualType")->equals(params_type.first)) {
        FuncRParams[i] = llvm::json::Object{{"kind", "ImplicitCastExpr"},
                                            {"valueCategory", "rvalue"},
                                            {"inner", llvm::json::Array{std::move(FuncRParams[i])}},
                                            {"type", llvm::json::Object{{"qualType", params_type.first.str()}}}
                                          };
      }
      params_type = params_type.second.split(',');
    }
  } else {
    type = std::string("int");
    return_type = std::string("int");
  }

  // 前面已经为函数名留有位置了
  llvm::json::Object Func{{"kind", "ImplicitCastExpr"},
                          {"valueCategory", "rvalue"},
                          {"inner", llvm::json::Array{
                              llvm::json::Object{{"kind", "DeclRefExpr"},
                                                 {"valueCategory", "rvalue"},
                                                 {"type", llvm::json::Object{{"qualType",type}}}
                                                }
                          }},
                          {"type", llvm::json::Object{{"qualType", type}}}
                        };
  FuncRParams.front() = std::move(Func);
  
  stak.push_back(llvm::json::Object{{"kind", "CallExpr"},
                                    {"valueCategory", "rvalue"},
                                    {"inner", std::move(FuncRParams)},
                                    {"type", llvm::json::Object{{"qualType", return_type}}}
                                  });
}
| Ident T_L_PAREN T_R_PAREN {
  auto tmp = stak.back();
  auto Ident = tmp.getAsObject();
  stak.pop_back();

  std::string type, return_type;
  if (sym_table.get(*(Ident->getString("value"))) != nullptr) {
    type = sym_table.getString(*(Ident->getString("value")))->str();
    return_type = sym_table.getString(*(Ident->getString("value")))->split(' ').first.str();
  } else {
    type = std::string("int");
    return_type = std::string("int");
  }

  llvm::json::Object Func{{"kind", "ImplicitCastExpr"},
                          {"valueCategory", "rvalue"},
                          {"inner", llvm::json::Array{
                              llvm::json::Object{{"kind", "DeclRefExpr"},
                                                 {"valueCategory", "rvalue"},
                                                 {"type", llvm::json::Object{{"qualType",type}}}
                                                }
                          }},
                          {"type", llvm::json::Object{{"qualType", type}}}
                        };

  stak.push_back(llvm::json::Object{{"kind", "CallExpr"},
                                    {"valueCategory", "rvalue"},
                                    {"inner", llvm::json::Array{std::move(Func)}},
                                    {"type", llvm::json::Object{{"qualType", return_type}}}
                                  });
}
| UnaryOp UnaryExp {
  auto UnaryExp = stak.back();
  auto UnaryExp_type = UnaryExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  auto tmp = stak.back();
  auto UnaryOp = tmp.getAsObject();
  assert(UnaryOp->getString("kind")->equals(llvm::StringRef("UnaryOp")));
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "UnaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", UnaryOp->getString("opcode")->str()},
                                    {"type", llvm::json::Object{
                                      {"qualType", UnaryExp_type}}},
                                    {"inner", llvm::json::Array{std::move(UnaryExp)}},
                                  });
}
;


FuncRParams
: Exp {
  auto Exp = stak.back();
  stak.pop_back();

  auto type = Exp.getAsObject()->getObject("type")->getString("qualType")->split(' ').first;

  if (type.equals("char")) {
    llvm::json::Object tmp{{"kind", "ImplicitCastExpr"},
                            {"valueCategory", "rvalue"},
                            {"inner", llvm::json::Array{llvm::json::Object{
                                  {"kind", "ImplicitCastExpr"},
                                  {"valueCategory", "rvalue"},
                                  {"inner", llvm::json::Array{std::move(Exp)}},
                                  {"type", llvm::json::Object{{"qualType", "char"}}}
                            }}},
                            {"type", llvm::json::Object{{"qualType", "char"}}}
                          };
    stak.push_back(llvm::json::Object{{"FuncRParams", llvm::json::Array{llvm::json::Object(), std::move(tmp)}}});
  }
  else
    stak.push_back(llvm::json::Object{{"FuncRParams", llvm::json::Array{llvm::json::Object(), Exp}}});
}
| FuncRParams T_COMMA Exp {
  auto Exp = stak.back();
  stak.pop_back();

  auto FuncRParams = stak.back().getAsObject();
  assert(FuncRParams->getArray("FuncRParams") != nullptr);

  auto type = Exp.getAsObject()->getObject("type")->getString("qualType")->split(' ').first;

  if (type.equals("char")) {
    llvm::json::Object tmp{{"kind", "ImplicitCastExpr"},
                            {"valueCategory", "rvalue"},
                            {"inner", llvm::json::Array{llvm::json::Object{
                                  {"kind", "ImplicitCastExpr"},
                                  {"valueCategory", "rvalue"},
                                  {"inner", llvm::json::Array{std::move(Exp)}},
                                  {"type", llvm::json::Object{{"qualType", "char"}}}
                            }}},
                            {"type", llvm::json::Object{{"qualType", "char"}}}
                          };
    FuncRParams->getArray("FuncRParams")->push_back(std::move(tmp));
  }
  else
    FuncRParams->getArray("FuncRParams")->push_back(std::move(Exp));  
}
;


UnaryOp
: T_PLUS {
  stak.push_back(llvm::json::Object{{"kind", "UnaryOp"},
                                    {"opcode", "+"}
                                  });
}
| T_MINUS {
  stak.push_back(llvm::json::Object{{"kind", "UnaryOp"},
                                    {"opcode", "-"}
                                  });
}
| T_EXCLAIM {
  stak.push_back(llvm::json::Object{{"kind", "UnaryOp"},
                                    {"opcode", "!"}
                                  });
}


PrimaryExp
: Number {

}
| T_L_PAREN Exp T_R_PAREN {
  auto Exp = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "ParenExpr"},
                                    {"valueCategory", "rvalue"},
                                    {"type", llvm::json::Object{
                                      {"qualType", Exp.getAsObject()->getObject("type")->getString("qualType")}}},
                                    {"inner", llvm::json::Array{std::move(Exp)}},
                                  });
}
| LVal %prec PrimLVal {
  auto LVal = stak.back();
  auto LVal_type = LVal.getAsObject()->getObject("type")->getString("qualType")->str();
  if (LVal_type.find("const int") != std::string::npos)
    LVal_type = "int";
  stak.pop_back();

  llvm::json::Array inner;
  inner.push_back(LVal);

  stak.push_back(llvm::json::Object{{"range", },
                                    {"kind", "ImplicitCastExpr"},
                                    {"type", llvm::json::Object{
                                      {"qualType", LVal_type}}},
                                    {"valueCategory", "rvalue"},
                                    {"inner", std::move(inner)},
                                    {"castKind", "LValueToRValue"},
                                  });
}
| Strings %prec PrimStrings {

}
;


Number
: T_NUMERIC_CONSTANT {
  auto number = stak.back().getAsObject();
  auto value = number->getString("value")->str();
  auto type = number->getString("type")->str();
  stak.pop_back();
  stak.push_back(llvm::json::Object{{"kind", "IntegerLiteral"},
                                    {"range", },
                                    {"valueCategory", "rvalue"},
                                    {"value", value},
                                    {"type", llvm::json::Object{{"qualType", type}}}
                                  });
}
;


Strings
: T_STRING {
  auto tmp_1 = stak.back();
  auto String = tmp_1.getAsObject();
  stak.pop_back();

  assert(String->get("kind") != nullptr);

  int len = String->getString("value")->size();
  char tmp[33];
  sprintf(tmp, "%d", len);
  std::string type = std::string("char ") + std::string(tmp);
  std::string value = String->getString("value")->str();

  stak.push_back(llvm::json::Object{{"kind", "StringLiteral"},
                                    {"valueCategory", "lvalue"},
                                    {"type", llvm::json::Object{{"qualType", type}}},
                                    {"value", value},
                                  });
}
| Strings T_STRING  {
  auto tmp_1 = stak.back();
  auto String = tmp_1.getAsObject();
  std::string value_2 =  String->getString("value")->str();
  stak.pop_back();

  assert(String->get("kind") != nullptr);

  auto Strings = stak.back().getAsObject();
  // stak.pop_back();

  int len = String->getString("value")->size() + Strings->getString("value")->size();
  char tmp[33];
  sprintf(tmp, "%d", len);
  std::string type = std::string("char ") + std::string(tmp);
  std::string value;
  {
    std::string value_1 = Strings->getString("value")->str();
    value = value_1.substr(0, value_1.size()-1)
    + value_2.substr(1, value_2.size()-1);
  }

  auto inner_type = Strings->getObject("type");
  (*inner_type)["qualType"] = type;
  (*Strings)["value"] = value;
}
;


Cond
: LOrExp {
  
}
;


LOrExp
: LAndExp {

}
| LOrExp T_PIPEPIPE LAndExp {
  auto LAndExp = stak.back();
  auto LAndExp_type = LAndExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  auto LOrExp = stak.back();
  auto LOrExp_type = LOrExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  int former = getPriority(LAndExp_type);
  int later = getPriority(LOrExp_type);
  llvm::json::Object type{{"qualType", }};
  if (former < later) 
    type["qualType"] = LOrExp_type;
  else
    type["qualType"] = LAndExp_type;

  stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "||"},
                                    {"inner", llvm::json::Array{std::move(LOrExp), std::move(LAndExp)}},
                                    {"type", std::move(type)}
                                  });
}
;


LAndExp
: EqExp {

}
| LAndExp T_AMPAMP EqExp {
  auto AddExp = stak.back();
  auto AddExp_type = AddExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  auto RelExp = stak.back();
  auto RelExp_type = RelExp.getAsObject()->getObject("type")->getString("qualType")->str();
  stak.pop_back();

  int former = getPriority(AddExp_type);
  int later = getPriority(RelExp_type);
  llvm::json::Object type{{"qualType", }};
  if (former < later) 
    type["qualType"] = RelExp_type;
  else
    type["qualType"] = AddExp_type;


  stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "&&"},
                                    {"inner", llvm::json::Array{std::move(RelExp), std::move(AddExp)}},
                                    {"type", std::move(type)}
                                  });
}
;


EqExp
: RelExp {

}
| EqExp T_EQUALEQUAL RelExp {
  auto AddExp = stak.back();
  stak.pop_back();

  auto RelExp = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "=="},
                                    {"inner", llvm::json::Array{std::move(RelExp), std::move(AddExp)}},
                                    {"type", llvm::json::Object{{"qualType", "int"}}}
                                  });
} 
| EqExp T_EXCLAIMEQUAL RelExp {
  auto AddExp = stak.back();
  stak.pop_back();

  auto RelExp = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "!="},
                                    {"inner", llvm::json::Array{std::move(RelExp), std::move(AddExp)}},
                                    {"type", llvm::json::Object{{"qualType", "int"}}}
                                  });
}
;


RelExp
: AddExp {

}
| RelExp T_LESS AddExp {
  auto AddExp = stak.back();
  stak.pop_back();

  auto RelExp = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "<"},
                                    {"inner", llvm::json::Array{std::move(RelExp), std::move(AddExp)}},
                                    {"type", llvm::json::Object{{"qualType", "int"}}}
                                  });
}
| RelExp T_GREATER AddExp {
  auto AddExp = stak.back();
  stak.pop_back();

  auto RelExp = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", ">"},
                                    {"inner", llvm::json::Array{std::move(RelExp), std::move(AddExp)}},
                                    {"type", llvm::json::Object{{"qualType", "int"}}}
                                  });
}
| RelExp T_LESSEQUAL AddExp {
  auto AddExp = stak.back();
  stak.pop_back();

  auto RelExp = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", "<="},
                                    {"inner", llvm::json::Array{std::move(RelExp), std::move(AddExp)}},
                                    {"type", llvm::json::Object{{"qualType", "int"}}}
                                  });
}
| RelExp T_GREATEREQUAL AddExp {
  auto AddExp = stak.back();
  stak.pop_back();

  auto RelExp = stak.back();
  stak.pop_back();

  stak.push_back(llvm::json::Object{{"kind", "BinaryOperator"},
                                    {"valueCategory", "rvalue"},
                                    {"opcode", ">="},
                                    {"inner", llvm::json::Array{std::move(RelExp), std::move(AddExp)}},
                                    {"type", llvm::json::Object{{"qualType", "int"}}}
                                  });
}
;


%%