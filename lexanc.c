/* lex1.c         14 Feb 01; 31 May 12; 11 Jan 18       */

/* This file contains code stubs for the lexical analyzer.
   Rename this file to be lexanc.c and fill in the stubs.    */

/* Copyright (c) 2018 Gordon S. Novak Jr. and
   The University of Texas at Austin. */

/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "token.h"
#include "lexan.h"
#include <limits.h>
#include <math.h>
#include <float.h>

/* This file will work as given with an input file consisting only
   of integers separated by blanks:
   make lex1
   lex1
   12345 123    345  357
   */

const int num_reserved_words = 29;
const char* reserved_words[] = {"array", "begin", "case", "const", "do",
                                "downto", "else", "end", "file", "for",
                                "function", "goto", "if", "label", "nil",
                                "of", "packed", "procedure", "program",
                                "record", "repeat", "set", "then", "to",
                                "type", "until", "var", "while", "with"};

const int num_operators = 19;
const char* operators[] = {"+", "-", "*", "/", ":=", "=", "<>", "<", "<=",
                            ">=", ">", "^", ".", "and", "or", "not", "div",
                            "mod", "in"};

const int num_delimiters = 8;
const char* delimiters[] = {",", ";", ":", "(", ")", "[", "]", ".."};

/* Skip blanks and whitespace.  Expand this function to skip comments too. */
void skipblanks ()
  {
    int c;
    while ((c = peekchar()) != EOF) {
      if(c == ' ' || c == '\n' || c == '\t')  //Skip blanks and whitespace
        getchar();
      else if(c == '{') {                     //Skip Comments
        while((c = peekchar()) != EOF && c != '}')
          getchar();
        getchar();
      }
      else if(c == '(') {
        int d = peek2char();
        if(d == '*') {
          getchar();
          getchar();
          while((c = peekchar()) != EOF && (d = peek2char()) != EOF 
                && (c != '*' || d != ')'))
            getchar();
          getchar();
          getchar();
        }
      }
      else
        break;
    }
  }

/* Get identifiers and reserved words */
TOKEN identifier (TOKEN tok)
  {
    int c;
    char s[256];
    int s_len = 0;

    while((c = peekchar()) != EOF && 
          ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') 
            || (c >= 'a' && c <= 'z')) ) {
      c = getchar();
      s[s_len] = c;
      s_len++;
    }
    (s_len >= 16) ? (s[15] = '\0') : (s[s_len] = '\0');
    /* Get reserved word */
    for(int i = 0; i < num_reserved_words; i++) {
      if(strcmp(s, reserved_words[i]) == 0) {
        tok->tokentype = RESERVED;
        tok->whichval = i+1;
        return tok;
      }
    }
    /* Special operators treated as reserved words */
    for(int i = 13; i < num_operators; i++) {
      if(strcmp(s, operators[i]) == 0) {
        tok->tokentype = OPERATOR;
        tok->whichval = i+1;
        return tok;
      }
    }

    /* Else if must be an identifier */
    tok->tokentype = IDENTIFIERTOK;
    strcpy(tok->stringval, s);
    return tok;
  }

TOKEN getstring (TOKEN tok)
  {
    getchar();  //Strings are enclosed by '
    int c;
    char s[256];
    int s_len = 0;

    while( (c = peekchar()) != EOF ) {
      c = getchar();

      if(c == '\'') {
        int d;
        if( (d = peekchar()) != '\'')
          break;
        else
          getchar();
      }

      s[s_len] = c;
      s_len++;
    }

    (s_len >= 16) ? (s[15] = '\0') : (s[s_len] = '\0');

    tok->tokentype = STRINGTOK;
    strcpy(tok->stringval, s);
    return tok;
  }

/* Get Operators and Delimiters */
TOKEN special (TOKEN tok)
  {
    int c;
    int b = 0;  //Is the operator one or two characters
    char op[3];
    int op_len = 0;

    while( (c = peekchar()) != EOF && CHARCLASS[c] == SPECIAL ) {
      c = getchar();
      op[op_len] = c;
      op_len++;

      c = peekchar();
      if(CHARCLASS[c] == SPECIAL) {
        getchar();
        op[op_len] = c;
        op_len++;
      }

      op[op_len] = '\0';

      for(int i = 0; i < num_operators; i++) {
        if(strcmp(op, operators[i]) == 0) {
          tok->tokentype = OPERATOR;
          tok->whichval = i+1;
          return tok;
        }
      }

      for(int i = 0; i < num_delimiters; i++) {
        if(strcmp(op, delimiters[i]) == 0) {
          tok->tokentype = DELIMITER;
          tok->whichval = i+1;
          return tok;
        }
      }
    }
    return tok;
  }

/* Get and convert unsigned numbers of all types. */
TOKEN number (TOKEN tok)
{   
  long num = 0;
  int  c, charval; 
  long exp = 0; 
  long actual_exp = 0;
  int neg_exp = 0; 
  int outOfRange = 0;
  double real_num = 0.0, dec = 0.0;

  while ( (c = peekchar()) != EOF
          && CHARCLASS[c] == NUMERIC) {   
    c = getchar();
    charval = (c - '0');
    if(num > INT_MAX) {
      actual_exp++;
      outOfRange = 1;
    }
    else
      num = num * 10 + charval;
  }

  if(num > INT_MAX) {
    actual_exp++;
    outOfRange = 1;
  }

  int d = peek2char();
  if(c == '.' && d != EOF && CHARCLASS[d] == NUMERIC) {
    outOfRange = 0;
    getchar();
    int fpp = 10; //Floating point position
    while((c = peekchar()) != EOF
          && CHARCLASS[c] == NUMERIC) {
      c = getchar();
      charval = (c - '0');
      dec = dec + ((double) charval/fpp);
      fpp = fpp * 10;
    }
    real_num = (double) num + dec;
  }

  if(c == 'e') {
    getchar();
    c = peekchar();
    if(c == '+')
      getchar();
    else if(c == '-') {
      neg_exp = 1;
      getchar();
    }
    while((c = peekchar()) != EOF
          && CHARCLASS[c] == NUMERIC) {
      c = getchar();
      charval = (c - '0');
      exp = exp*10 + charval;
    }
  }

  if (dec) {
    if (exp) {
      if (neg_exp) {
        actual_exp = actual_exp - exp;
        real_num = real_num / pow (10, actual_exp);
      } 
      else {
        actual_exp = actual_exp + exp;
        real_num = real_num * pow (10, actual_exp);
      }
      tok->tokentype = NUMBERTOK;
      tok->basicdt = REAL;
      tok->realval = real_num;
              if (real_num > FLT_MAX || real_num < FLT_MIN) {
      printf("Floating number out of range \n");
                  tok->tokentype = NUMBERTOK;
      tok->realval = 0.0;
    } 
      return tok;
    } 
    else {
        if (real_num > FLT_MAX || real_num < FLT_MIN) {
          printf("Floating number out of range \n");

        } 
        tok->tokentype = NUMBERTOK;
        tok->basicdt = REAL;
        tok->realval = real_num;
        return tok;
    }
  }
  
  if (exp)  {
    real_num = (double) num;
    if (neg_exp) {
      actual_exp = actual_exp - exp;
      real_num = real_num / pow(10, actual_exp);
    } else {
      actual_exp = actual_exp + exp;
      real_num = real_num * pow(10, actual_exp);
    }
    if (real_num > FLT_MAX || real_num < FLT_MIN) {
      printf("Floating number out of range \n");
    }
    tok->tokentype = NUMBERTOK;
    tok->basicdt = REAL;
    tok->realval = real_num;
    return tok;
  }

  if (outOfRange) {
    printf("Integer number out of range \n");
  }
  tok->tokentype = NUMBERTOK;
  tok->basicdt = INTEGER;
  tok->intval = num;
  return tok;
}
