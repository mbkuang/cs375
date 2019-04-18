%{     /* pars1.y    Pascal Parser      Gordon S. Novak Jr.  ; 30 Jul 13   */

/* Copyright (c) 2013 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/* 14 Feb 01; 01 Oct 04; 02 Mar 07; 27 Feb 08; 24 Jul 09; 02 Aug 12 */

/*
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, see <http://www.gnu.org/licenses/>.
  */


/* NOTE:   Copy your lexan.l lexical analyzer to this directory.      */

       /* To use:
                     make parser              has 1 shift/reduce conflict
                     parser                   execute the parser
                     i:=j .
                     ^D                       control-D to end input

                     parser                   execute the parser
                     begin i:=j; if i+j then x:=a+b*c else x:=a*b+c; k:=i end.
                     ^D

                     parser                   execute the parser
                     if x+y then if y+z then i:=j else k:=2.
                     ^D

           You may copy pars1.y to be parse.y and extend it for your
           assignment.  Then use   make parser   as above.
        */

        /* Yacc reports 1 shift/reduce conflict, due to the ELSE part of
           the IF statement, but Yacc's default resolves it in the right way.*/

#include <stdio.h>
#include <ctype.h>
#include "token.h"
#include "lexan.h"
#include "symtab.h"
#include "parse.h"
#include "pprint.h"
#include "string.h"

        /* define the type of the Yacc stack element to be TOKEN */

#define YYSTYPE TOKEN

TOKEN parseresult;

%}

/* Order of tokens corresponds to tokendefs.c; do not change */

%token IDENTIFIER STRING NUMBER   /* token types */

%token PLUS MINUS TIMES DIVIDE    /* Operators */
%token ASSIGN EQ NE LT LE GE GT POINT DOT AND OR NOT DIV MOD IN

%token COMMA                      /* Delimiters */
%token SEMICOLON COLON LPAREN RPAREN LBRACKET RBRACKET DOTDOT

%token ARRAY BEGINBEGIN           /* Lex uses BEGIN */
%token CASE CONST DO DOWNTO ELSE END FILEFILE FOR FUNCTION GOTO IF LABEL NIL
%token OF PACKED PROCEDURE PROGRAM RECORD REPEAT SET THEN TO TYPE UNTIL
%token VAR WHILE WITH


%%

program    :  PROGRAM IDENTIFIER LPAREN idlist RPAREN SEMICOLON cblock DOT  {
                parseresult = makeprogram($2, $4, $7); }
  statement_list  : statement SEMICOLON statement_list  { $$ = cons($1, $3);}
                  | statement
                  ;
  unsigned_constant : NUMBER
                    | NIL
                    | STRING
                    ;
  sign : PLUS | MINUS;
  constant : sign IDENTIFIER { $$ = unaryop($1, $2); }
           | IDENTIFIER
           | sign NUMBER { $$ = unaryop($1, $2); }
           | NUMBER
           | STRING
           ;

  statement  :  BEGINBEGIN statement endpart   { $$ = makeprogn($1,cons($2, $3)); }
             |  IF expr THEN statement endif   { $$ = makeif($1, $2, $4, $5); }
             |  assignment
             |  funcall
             |  FOR assignment TO expr DO statement   { $$ = makefor(1, $1, $2, $3, $4, $5, $6); }
             |  REPEAT statement_list UNTIL expr      { $$ = makerepeat($1, $2, $3, $4); }
             ;
  endpart    :  SEMICOLON statement endpart    { $$ = cons($2, $3); }
             |  END                            { $$ = NULL; }
             ;
  endif      :  ELSE statement                 { $$ = $2; }
             |  /* empty */                    { $$ = NULL; }
             ;
  assignment :  IDENTIFIER ASSIGN expr         { $$ = binop($2, $1, $3); }
             ;
  expr       :  expr compare_op simple_expression   { $$ = binop($2, $1, $3); }
             |  simple_expression 
             ;
  term       :  term times_op factor              { $$ = binop($2, $1, $3); }
             |  factor
             ;
  factor     :  unsigned_constant
             |  variable
             |  LPAREN expr RPAREN             { $$ = $2; }
             |  funcall
             |  NOT factor      { $$ = unaryop($1, $2); }
             ;
  idlist     :  IDENTIFIER COMMA idlist { $$ = cons($1, $3); }
             |  IDENTIFIER    { $$ = cons($1, NULL); }
             ;
  expr_list  :  expr COMMA expr_list           { $$ = cons($1, $3); }
             |  expr
             ;
  variable   :  IDENTIFIER               { $$ = findid($1); }
             ;
  vblock     :  VAR varspecs block       { $$ = $3; }
             |  block
             ;
  varspecs   :  vargroup SEMICOLON varspecs
             |  vargroup SEMICOLON
             ;
  vargroup   :  idlist COLON type { instvars($1, $3); }
             ;
  type       :  simpletype
             ;
  simpletype :  IDENTIFIER   { $$ = findtype($1); }
             ;
  cdef       :  IDENTIFIER EQ constant { instconst($1, $3); }
             ;
  cdef_list  :  cdef SEMICOLON cdef_list
             |  cdef SEMICOLON
             ;
  cblock     :  CONST cdef_list tblock    { $$ = $3; }
             | tblock
             ;
  tdef_list  :  IDENTIFIER EQ TYPE tdef_list
             |  IDENTIFIER EQ TYPE
             ;
  tblock     :  TYPE tdef_list vblock {$$ = $3;}
             |  vblock
             ;
  block      :  BEGINBEGIN statement endpart   { $$ = makeprogn($1,cons($2, $3)); }  
             ;
  funcall    :  IDENTIFIER LPAREN expr_list RPAREN    { $$ = makefuncall($2, $1, $3); }
             ;
  times_op   :  TIMES | DIVIDE | DIV | MOD | AND
             ;

  plus_op    :  PLUS | MINUS | OR
             ;
  simple_expression : sign term { $$ = unaryop($1, $2); }
                    | term
                    | simple_expression plus_op term { $$ = binop($2, $1, $3); }
                    ;
  compare_op  :  EQ | LT | GT | NE | LE | GE | IN
              ;

%%


/* You should add your own debugging flags below, and add debugging
   printouts to your programs.

   You will want to change DEBUG to turn off printouts once things
   are working.
  */

#define DEBUG         0             /* set bits here for debugging, 0 = off  */
#define DB_CONS       1             /* bit to trace cons */
#define DB_BINOP      2             /* bit to trace binop */
#define DB_MAKEIF     4             /* bit to trace makeif */
#define DB_MAKEPROGN  8             /* bit to trace makeprogn */
#define DB_PARSERES  16             /* bit to trace parseresult */

 int labelnumber = 0;  /* sequential counter for internal label numbers */

   /*  Note: you should add to the above values and insert debugging
       printouts in your routines similar to those that are shown here.     */

TOKEN cons(TOKEN item, TOKEN list)           /* add item to front of list */
  { item->link = list;
    if (DEBUG & DB_CONS)
       { printf("cons\n");
         dbugprinttok(item);
         dbugprinttok(list);
       };
    return item;
  }

int get_tok_dt(TOKEN tok) {
  if(tok->basicdt == REAL)
    return -1;
  else if(tok->basicdt == INTEGER)
    return 1;
  else
    return 0;
}

TOKEN binop(TOKEN op, TOKEN lhs, TOKEN rhs)        /* reduce binary operator */
  { 
    op->operands = lhs;          /* link operands to operator       */
    lhs->link = rhs;             /* link second operand to first    */
    rhs->link = NULL;            /* terminate operand list          */
    if (DEBUG & DB_BINOP)
       { printf("binop\n");
         dbugprinttok(op);
         dbugprinttok(lhs);
         dbugprinttok(rhs);
       };
    int lhs_dt = get_tok_dt(lhs);
    int rhs_dt = get_tok_dt(rhs);
    if(lhs_dt == -1 && rhs_dt == -1) {
      op->basicdt = REAL;
    }
    else if(lhs_dt == 1 && rhs_dt == -1) {
      if(op->whichval == ASSIGNOP) {
        op->basicdt = INTEGER;
        TOKEN fixtok = makefix(rhs);
        lhs->link = fixtok;
      }
      else {
        op->basicdt = REAL;
        TOKEN floattok = makefloat(lhs);
        floattok->link = rhs;
      }
    }
    else if(lhs_dt == -1 && rhs_dt == 1) {
      op->basicdt = REAL;
      TOKEN floattok = makefloat(rhs);
      lhs->link = floattok;
    }
    return op;
  }

TOKEN makeif(TOKEN tok, TOKEN exp, TOKEN thenpart, TOKEN elsepart)
  {  tok->tokentype = OPERATOR;  /* Make it look like an operator   */
     tok->whichval = IFOP;
     if (elsepart != NULL) elsepart->link = NULL;
     thenpart->link = elsepart;
     exp->link = thenpart;
     tok->operands = exp;
     if (DEBUG & DB_MAKEIF)
        { printf("makeif\n");
          dbugprinttok(tok);
          dbugprinttok(exp);
          dbugprinttok(thenpart);
          dbugprinttok(elsepart);
        };
     return tok;
   }

TOKEN makeprogn(TOKEN tok, TOKEN statements)
  {  tok->tokentype = OPERATOR;
     tok->whichval = PROGNOP;
     tok->operands = statements;
     if (DEBUG & DB_MAKEPROGN)
       { printf("makeprogn\n");
         dbugprinttok(tok);
         dbugprinttok(statements);
       };
     return tok;
   }

TOKEN makeconst(int number) {
  TOKEN tok = talloc();
  tok->tokentype = NUMBERTOK;
  tok->basicdt = INTEGER;
  tok->intval = number;
  if (DEBUG)
    { printf("makeconst\n");
      dbugprinttok(tok);
    };
  return tok;
}

TOKEN makelabel(int label) {
  TOKEN tok = talloc();
  TOKEN labeltok = makeconst(labelnumber++);
  tok->tokentype = OPERATOR;
  tok->whichval = LABELOP;
  tok->operands = labeltok;
  if (DEBUG)
    { printf("makelabel\n");
      dbugprinttok(tok);
    };
  return tok;
}

TOKEN makeop(int opnum) {
  TOKEN tok = talloc();
  tok->tokentype = OPERATOR;
  tok->whichval = opnum;
  if (DEBUG)
    { printf("makeop\n");
      dbugprinttok(tok);
    };
  return tok;
}

TOKEN makegoto(int label) {
  TOKEN tok = talloc();
  TOKEN gototok = makeconst(label);
  tok->tokentype = OPERATOR;
  tok->whichval = GOTOOP;
  tok->operands = gototok;
  if (DEBUG)
    { printf("makegoto\n");
      dbugprinttok(tok);
    };
  return tok;
}

void instvars(TOKEN idlist, TOKEN typtok) {
  SYMBOL symtype = typtok->symtype;
  while(idlist != NULL) {
    if(idlist->tokentype != IDENTIFIERTOK)
      printf("Expected Identifier?\n");
      SYMBOL sym = insertsym(idlist->stringval);
      sym->kind = VARSYM;
      sym->size = symtype->size;
      sym->basicdt = symtype->basicdt;
      sym->datatype = symtype;
      sym->offset = wordaddress(blockoffs[blocknumber], alignsize(symtype));
      blockoffs[blocknumber] = sym->offset + sym->size;
      idlist = idlist->link;
  }
}

TOKEN findid(TOKEN tok) {
  SYMBOL sym, symtype;
  sym = searchst(tok->stringval);
  if(sym != NULL) {
    tok->symentry = sym;
    if(sym->kind == CONSTSYM) {
      if(sym->basicdt == REAL) {
        tok->tokentype = NUMBERTOK;
        tok->basicdt = REAL;
        tok->realval = sym->constval.realnum;
      }
      else if(sym->basicdt == INTEGER) {
        tok->tokentype = NUMBERTOK;
        tok->basicdt = INTEGER;
        tok->realval = sym->constval.intnum;
      }
         return tok;
    }
    symtype = sym->datatype;
    tok->symtype = symtype;
    if(symtype->kind == BASICTYPE || symtype->kind == POINTERSYM)
      tok->basicdt = symtype->basicdt;
  }
  else
    printf("Could not find tok (findid())\n");

  return tok;
} 

TOKEN findtype(TOKEN tok) {
  SYMBOL sym = searchst(tok->stringval);
  if(sym == NULL)
    printf("tok type not found in symbol table\n");
  tok->symtype = sym;
  return tok;
}

TOKEN makeprogram(TOKEN name, TOKEN args, TOKEN statements) {
  TOKEN prog = talloc();
  prog->tokentype = OPERATOR;
  prog->whichval = PROGRAMOP;
  prog->operands = name;
  TOKEN tok = talloc();
  tok = makeprogn(tok, args);
  name->link = tok;
  tok->link = statements;
  if (DEBUG)
    { printf("makeprogram\n");
      dbugprinttok(prog);
    };
  return prog;
}

TOKEN makefor(int sign, TOKEN tok, TOKEN asg, TOKEN tokb, TOKEN endexpr, 
              TOKEN tokc, TOKEN statement) {
  tok = makeprogn(tok, asg);
  TOKEN lbl = makelabel(labelnumber);
  int label = labelnumber-1;
  asg->link = lbl;

  tokb = makeprogn(tokb, statement);
  TOKEN leo = makeop(LEOP);
  tokc = makeif(tokc, leo, tokb, NULL);

  TOKEN t1 = copytok(asg->operands);
  TOKEN t2 = copytok(t1);
  TOKEN t3 = copytok(t2);
  t1->link = endexpr;
  leo->operands = t1;

  TOKEN pls = makeop(PLUSOP);
  TOKEN assign = makeop(ASSIGNOP);
  t3->link = makeconst(1);
  pls->operands = t3;
  t2->link = pls;

  TOKEN gototok = makegoto(label);
  assign->link = gototok;
  assign->operands = t2;

  statement->link = assign;

  leo->link = tokb;
  tokc->operands = leo;
  lbl->link = tokc;

  if (DEBUG)
    { printf("makefor\n");
      dbugprinttok(tok);
    };

  return tok;
}

TOKEN copytok(TOKEN origtok) {
  TOKEN cpy = talloc();
  //*cpy = *orig
  cpy->tokentype = origtok->tokentype;
  int dt = origtok->basicdt;
  cpy->basicdt = dt;
  if(dt == INTEGER)
    cpy->intval = origtok->intval;
  else if(dt == REAL)
    cpy->realval = origtok->realval;
  else if(dt == STRINGTYPE)
    strcpy(cpy->stringval, origtok->stringval);
  else
    cpy->whichval = origtok->whichval;
  cpy->symtype = origtok->symtype;
  cpy->symentry = origtok->symentry;
  cpy->link = origtok->link;

  if (DEBUG)
    { printf("copytok\n");
      dbugprinttok(cpy);
    };

  return cpy;
}

TOKEN makefuncall(TOKEN tok, TOKEN fn, TOKEN args) {
  tok->tokentype = OPERATOR;
  tok->whichval = FUNCALLOP;
  tok->operands = fn;
  tok->operands->link = args;

  if (DEBUG)
    { printf("makefuncall\n");
      dbugprinttok(tok);
    };

  return tok;
}

/* Added/Changed for Assignment 4 */

TOKEN makefloat(TOKEN tok) {
  if(tok->tokentype == NUMBERTOK) {
    tok->basicdt = REAL;
    tok->realval = (double) (tok->intval);
  }
  else {
    TOKEN floatop = makeop(FLOATOP);
    floatop->operands = tok;
    return floatop;
  }
  if (DEBUG)
    { printf("makefloat\n");
      dbugprinttok(tok);
    };

  return tok;
}

TOKEN makefix(TOKEN tok) {
  if(tok->tokentype == NUMBERTOK) {
    tok->basicdt = INTEGER;
    tok->intval = (int) (tok->realval);
  }
  else {
    TOKEN fixop = makeop(FIXOP);
    fixop->operands = tok;
    return fixop;
  }

  if (DEBUG)
    { printf("makefix\n");
      dbugprinttok(tok);
    };
  return tok;
}

void instconst(TOKEN idtok, TOKEN consttok) {
  SYMBOL sym = insertsym(idtok->stringval);
  sym->kind = CONSTSYM;
  sym->basicdt = consttok->basicdt;
  int type = sym->basicdt;
  if(type == INTEGER) {
    sym->constval.intnum = consttok->intval;
  }
  else if(type == REAL) {
    sym->constval.realnum = consttok->realval;
  }
}

TOKEN unaryop(TOKEN op, TOKEN lhs) {
  op->operands = lhs;
  lhs->link = NULL;
  if (DEBUG)
    { printf("unaryop\n");
      dbugprinttok(op);
    };
  return op;
}

TOKEN makerepeat(TOKEN tok, TOKEN statements, TOKEN tokb, TOKEN expr) {
   TOKEN label = makelabel(labelnumber);
   int current = labelnumber - 1;
   tok = makeprogn(tok, label);
   TOKEN body = tokb;
   body = makeprogn(body, statements);
   label->link = body;
   TOKEN gototok = makegoto(current);
   TOKEN emptytok = makeprogn((TOKEN) talloc(), NULL);
   emptytok->link = gototok;
   TOKEN ifs = talloc();
   ifs = makeif(ifs, expr, emptytok, gototok);
   body->link = ifs;
   if (DEBUG) {
         printf("make repeat\n");
         dbugprinttok(tok);
   }
   return tok;  
}

/* End of added/changed for Assignment 4 */

int wordaddress(int n, int wordsize)
  { return ((n + wordsize - 1) / wordsize) * wordsize; }
 
void yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}

int main(void)          /*  */
  { int res;
    initsyms();
    res = yyparse();
    printst();       /* to shorten, change to:  printstlevel(1);  */
    printf("yyparse result = %8d\n", res);
    if (DEBUG & DB_PARSERES) dbugprinttok(parseresult);
    ppexpr(parseresult);           /* Pretty-print the result tree */
    /* uncomment following to call code generator. */
     /* 
    gencode(parseresult, blockoffs[blocknumber], labelnumber);
 */
  }

