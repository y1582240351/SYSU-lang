%{
#include <cstdio>
#include <cstring>
#include <stdlib.h>
#define YYEOF (0)

int yylex();
void genLabel();
void hex2Dec();

int main() {
  yyout = stderr;
  do {
  } while (yylex() != YYEOF);
}
int yyrow = 1, yycolumn = 1, yycolpre = 1;
int yynewline = 1, yyspace = 0;
char label[100] = "\0", yyfile[100] = "<stdin>";

void genLabel() {
  label[0] = '\0';
  if (yynewline) {
    std::sprintf(label + std::strlen(label), " [StartOfLine]");
    yynewline = 0;
  }

  if (yyspace) {
    // if (std::strlen(label) != 0) {
    //   std::sprintf(label + std::strlen(label), "\t[LeadingSpace]");
    // } else {
    //   std::sprintf(label + std::strlen(label), "[LeadingSpace]");
    // }
    std::sprintf(label + std::strlen(label), " [LeadingSpace]");    
    yyspace = 0;
  }
}

#define YY_USER_ACTION                                                         \
  do {                                                                         \
    yycolumn += yyleng;                                                        \
  } while (0);
%}
%option noyywrap
%%
\n {
  ++yyrow;
  yycolpre = yycolumn;
  yycolumn = 1;
  yynewline = 1;
  yyspace = 0;
}

\/\/.* {
  // --yyrow;
}

\/\*([^*]|(\n|\r)|(\*+([^*\/]|(\n|\r))))*\*+\/ {
  std::fprintf(yyout, "%s\n", yytext);
}

int {
  genLabel();

  std::fprintf(yyout, "int '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

char {
  genLabel();

  std::fprintf(yyout, "char '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

void {
  genLabel();

  std::fprintf(yyout, "void '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
return {
  genLabel();

  std::fprintf(yyout, "return '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

const {
  genLabel();

  std::fprintf(yyout, "const '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

if {
  genLabel();

  std::fprintf(yyout, "if '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

else {
  genLabel();

  std::fprintf(yyout, "else '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

for {
  genLabel();

  std::fprintf(yyout, "for '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

while {
  genLabel();

  std::fprintf(yyout, "while '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

break {
  genLabel();

  std::fprintf(yyout, "break '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

continue {
  genLabel();

  std::fprintf(yyout, "continue '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

switch {
  genLabel();

  std::fprintf(yyout, "switch '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

case {
  genLabel();

  std::fprintf(yyout, "case '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

default {
  genLabel();

  std::fprintf(yyout, "default '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

do {
  genLabel();

  std::fprintf(yyout, "do '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\"(([^\"\n])?(\\\")?)*\" {
  genLabel();

  std::fprintf(yyout, "string_literal '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
\( {
  genLabel();

  std::fprintf(yyout, "l_paren '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);       
  return ~YYEOF;
}
\) {
  genLabel();

  std::fprintf(yyout, "r_paren '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\[ {
  genLabel();

  std::fprintf(yyout, "l_square '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\] {
  genLabel();

  std::fprintf(yyout, "r_square '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
\{ {
  genLabel();

  std::fprintf(yyout, "l_brace '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
\} {
  genLabel();

  std::fprintf(yyout, "r_brace '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
; {
  genLabel();

  std::fprintf(yyout, "semi '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\/ {
  genLabel();

  std::fprintf(yyout, "slash '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\* {
  genLabel();

  std::fprintf(yyout, "star '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

% {
  genLabel();

  std::fprintf(yyout, "percent '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\+ {
  genLabel();

  std::fprintf(yyout, "plus '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

- {
  genLabel();

  std::fprintf(yyout, "minus '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

>= {
  genLabel();

  std::fprintf(yyout, "greaterequal '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\<= {
  genLabel();

  std::fprintf(yyout, "lessequal '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

> {
  genLabel();

  std::fprintf(yyout, "greater '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\< {
  genLabel();

  std::fprintf(yyout, "less '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

== {
  genLabel();

  std::fprintf(yyout, "equalequal '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

!= {
  genLabel();

  std::fprintf(yyout, "exclaimequal '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

! {
  genLabel();

  std::fprintf(yyout, "exclaim '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

&& {
  genLabel();

  std::fprintf(yyout, "ampamp '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

\|\| {
  genLabel();

  std::fprintf(yyout, "pipepipe '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

= {
  genLabel();

  std::fprintf(yyout, "equal '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

, {
  genLabel();

  std::fprintf(yyout, "comma '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}

0x[0-9a-fA-F]+ {
  genLabel();

  // if (yytext[0]=='0' && yytext[1] == 'x') {
  //   hex2Dec();
  // }

  std::fprintf(yyout, "numeric_constant '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
[0-9]+ {
  genLabel();

  std::fprintf(yyout, "numeric_constant '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
[a-zA-Z_][a-zA-Z_0-9]* {
  genLabel();
  
  std::fprintf(yyout, "identifier '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
<<EOF>> {
  std::fprintf(yyout, "eof '%s'\t\tLoc=<%s:%d:%d>\n", yytext, yyfile, yyrow-1,
            yycolpre-1);
  return YYEOF;
}

#.* {
  // --yyrow;
  // // std::fprintf(yyout, "%s\n", yytext);
  // return ~YYEOF;

  // Read the #
  char * token, * file_name;
  token = std::strtok(yytext, " ");

  token = std::strtok(NULL, " ");
  int line_num, oper;
  std::sscanf(token, "%d", &line_num);

  // Read the file name
  file_name = std::strtok(NULL, " ");

  while(token != NULL) {
    token = std::strtok(NULL, " ");
    // No operator
    if (token == NULL) {
      yyrow = line_num; 
      std::sprintf(yyfile, "%s", file_name+1);
      int len = std::strlen(yyfile);
      yyfile[len-1] = '\0';
      break;
    }
    std::sscanf(token, "%d", &oper);
    if (oper == 1 || oper == 2){
        yyrow = line_num; 
        std::sprintf(yyfile, "%s", file_name+1);
        int len = std::strlen(yyfile);
        yyfile[len-1] = '\0';
        break;
    }
  }
  --yyrow;
  return ~YYEOF;
}

[\f\r\t\v ] {
  yyspace = 1;
}

. {
  genLabel();

  std::fprintf(yyout, "unknown '%s'\t%s\tLoc=<%s:%d:%d>\n", yytext, label, yyfile, yyrow,
            yycolumn - yyleng);
  return ~YYEOF;
}
%%