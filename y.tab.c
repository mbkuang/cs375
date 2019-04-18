/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "parse.y" /* yacc.c:339  */
     /* pars1.y    Pascal Parser      Gordon S. Novak Jr.  ; 30 Jul 13   */

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


#line 129 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    IDENTIFIER = 258,
    STRING = 259,
    NUMBER = 260,
    PLUS = 261,
    MINUS = 262,
    TIMES = 263,
    DIVIDE = 264,
    ASSIGN = 265,
    EQ = 266,
    NE = 267,
    LT = 268,
    LE = 269,
    GE = 270,
    GT = 271,
    POINT = 272,
    DOT = 273,
    AND = 274,
    OR = 275,
    NOT = 276,
    DIV = 277,
    MOD = 278,
    IN = 279,
    COMMA = 280,
    SEMICOLON = 281,
    COLON = 282,
    LPAREN = 283,
    RPAREN = 284,
    LBRACKET = 285,
    RBRACKET = 286,
    DOTDOT = 287,
    ARRAY = 288,
    BEGINBEGIN = 289,
    CASE = 290,
    CONST = 291,
    DO = 292,
    DOWNTO = 293,
    ELSE = 294,
    END = 295,
    FILEFILE = 296,
    FOR = 297,
    FUNCTION = 298,
    GOTO = 299,
    IF = 300,
    LABEL = 301,
    NIL = 302,
    OF = 303,
    PACKED = 304,
    PROCEDURE = 305,
    PROGRAM = 306,
    RECORD = 307,
    REPEAT = 308,
    SET = 309,
    THEN = 310,
    TO = 311,
    TYPE = 312,
    UNTIL = 313,
    VAR = 314,
    WHILE = 315,
    WITH = 316
  };
#endif
/* Tokens.  */
#define IDENTIFIER 258
#define STRING 259
#define NUMBER 260
#define PLUS 261
#define MINUS 262
#define TIMES 263
#define DIVIDE 264
#define ASSIGN 265
#define EQ 266
#define NE 267
#define LT 268
#define LE 269
#define GE 270
#define GT 271
#define POINT 272
#define DOT 273
#define AND 274
#define OR 275
#define NOT 276
#define DIV 277
#define MOD 278
#define IN 279
#define COMMA 280
#define SEMICOLON 281
#define COLON 282
#define LPAREN 283
#define RPAREN 284
#define LBRACKET 285
#define RBRACKET 286
#define DOTDOT 287
#define ARRAY 288
#define BEGINBEGIN 289
#define CASE 290
#define CONST 291
#define DO 292
#define DOWNTO 293
#define ELSE 294
#define END 295
#define FILEFILE 296
#define FOR 297
#define FUNCTION 298
#define GOTO 299
#define IF 300
#define LABEL 301
#define NIL 302
#define OF 303
#define PACKED 304
#define PROCEDURE 305
#define PROGRAM 306
#define RECORD 307
#define REPEAT 308
#define SET 309
#define THEN 310
#define TO 311
#define TYPE 312
#define UNTIL 313
#define VAR 314
#define WHILE 315
#define WITH 316

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);



/* Copy the second part of user declarations.  */

#line 299 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  4
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   151

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  62
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  32
/* YYNRULES -- Number of rules.  */
#define YYNRULES  75
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  132

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   316

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    82,    82,    84,    85,    87,    88,    89,    91,    91,
      92,    93,    94,    95,    96,    99,   100,   101,   102,   103,
     104,   106,   107,   109,   110,   112,   114,   115,   117,   118,
     120,   121,   122,   123,   124,   126,   127,   129,   130,   132,
     134,   135,   137,   138,   140,   142,   144,   146,   148,   149,
     151,   152,   154,   155,   157,   158,   160,   162,   164,   164,
     164,   164,   164,   167,   167,   167,   169,   170,   171,   173,
     173,   173,   173,   173,   173,   173
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "IDENTIFIER", "STRING", "NUMBER", "PLUS",
  "MINUS", "TIMES", "DIVIDE", "ASSIGN", "EQ", "NE", "LT", "LE", "GE", "GT",
  "POINT", "DOT", "AND", "OR", "NOT", "DIV", "MOD", "IN", "COMMA",
  "SEMICOLON", "COLON", "LPAREN", "RPAREN", "LBRACKET", "RBRACKET",
  "DOTDOT", "ARRAY", "BEGINBEGIN", "CASE", "CONST", "DO", "DOWNTO", "ELSE",
  "END", "FILEFILE", "FOR", "FUNCTION", "GOTO", "IF", "LABEL", "NIL", "OF",
  "PACKED", "PROCEDURE", "PROGRAM", "RECORD", "REPEAT", "SET", "THEN",
  "TO", "TYPE", "UNTIL", "VAR", "WHILE", "WITH", "$accept", "program",
  "statement_list", "unsigned_constant", "sign", "constant", "statement",
  "endpart", "endif", "assignment", "expr", "term", "factor", "idlist",
  "expr_list", "variable", "vblock", "varspecs", "vargroup", "type",
  "simpletype", "cdef", "cdef_list", "cblock", "tdef_list", "tblock",
  "block", "funcall", "times_op", "plus_op", "simple_expression",
  "compare_op", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316
};
# endif

#define YYPACT_NINF -47

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-47)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     -43,    15,    26,    -1,   -47,    42,    24,    29,    42,    34,
     -47,   -21,     1,    58,    62,    42,   -47,    57,   -47,   -47,
      -3,     1,    74,    16,     1,   -16,   -47,   -47,    69,    59,
      25,    70,    13,    73,    52,    75,   -47,    16,    16,   -16,
      95,    51,    81,   -47,   -47,   -47,   -47,    36,    16,   -47,
     -47,    36,    18,    80,   -47,   -47,   -47,    84,    50,    86,
       1,   -47,   -47,   136,    58,   -47,    53,   -47,   110,   -47,
      42,   114,   108,    85,   -47,    16,   -47,    82,    80,   -47,
     -47,   -47,   -47,   -47,   -47,   -47,     1,    16,   -47,   -47,
     -47,   -47,   -47,    36,   -47,   -47,   -47,    36,    16,     1,
     -16,   -47,   -47,   -47,    11,   -47,   -47,    62,   -47,   -47,
     -47,   -47,    16,   -47,    55,   -47,    92,    84,   -47,    80,
     114,   -47,   -47,   -47,   -47,   -47,   -47,     1,     1,   -47,
     -47,   -47
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     1,     0,    36,     0,     0,     0,
      35,     0,     0,     0,     0,     0,    55,     0,    51,    41,
       0,     0,     0,     0,     0,     0,    17,    18,     0,     0,
       0,     0,     0,     0,     0,     0,     2,     0,     0,     0,
       0,     0,    39,     7,     5,     8,     9,     0,     0,     6,
      30,     0,     0,    67,    29,    31,    33,    27,     0,     4,
       0,    22,    56,     0,    49,    50,     0,    54,     0,    40,
      43,    25,    38,     0,    15,     0,    34,     0,    66,    69,
      72,    70,    73,    74,    71,    75,     0,     0,    58,    59,
      62,    60,    61,     0,    63,    64,    65,     0,     0,     0,
       0,    11,    14,    13,     0,    47,    48,    53,    46,    44,
      45,    42,     0,    57,     0,    32,    24,    26,    28,    68,
      20,     3,    21,    10,    12,    52,    37,     0,     0,    16,
      19,    23
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -47,   -47,    35,   -47,    72,   -47,   -10,   -38,   -47,   115,
     -20,   -46,   -41,    48,    32,   -47,   104,    76,   -47,   -47,
     -47,   -47,    83,   -47,    38,   118,   116,   -12,   -47,   -47,
      64,   -47
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,    58,    50,    51,   105,    59,    62,   129,    26,
      72,    53,    54,    33,    73,    55,    16,    34,    35,   109,
     110,    29,    30,    17,    32,    18,    19,    56,    93,    97,
      57,    87
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      27,    74,    25,    52,    20,    78,    76,    37,     1,    27,
      60,    39,    27,    12,   123,    13,   124,    71,     3,    42,
      43,    44,    45,    46,    61,    38,     4,     5,    77,    79,
      80,    81,    82,    83,    84,    21,    14,    47,    15,    42,
      43,    44,    85,    22,    48,     6,    23,    12,    27,     8,
     100,   119,   118,     7,    24,   114,    10,    47,     9,    12,
      11,    28,   122,    49,    48,    31,    79,    80,    81,    82,
      83,    84,    15,    86,    27,    36,   116,    40,   120,    85,
      63,    66,    14,    49,    15,    64,    12,    27,    88,    89,
      94,    95,   127,    79,    80,    81,    82,    83,    84,    90,
      68,    70,    91,    92,    96,    37,    85,    75,    98,    38,
     107,   115,    99,   108,   113,    27,    27,   130,   131,    79,
      80,    81,    82,    83,    84,    79,    80,    81,    82,    83,
      84,   128,    85,   112,   121,   104,    67,    41,    85,   101,
     102,   103,    45,    46,   126,   125,   111,   106,    65,     0,
      69,   117
};

static const yytype_int16 yycheck[] =
{
      12,    39,    12,    23,     3,    51,    47,    10,    51,    21,
      26,    21,    24,    34,     3,    36,     5,    37,     3,     3,
       4,     5,     6,     7,    40,    28,     0,    28,    48,    11,
      12,    13,    14,    15,    16,    34,    57,    21,    59,     3,
       4,     5,    24,    42,    28,     3,    45,    34,    60,    25,
      60,    97,    93,     5,    53,    75,     8,    21,    29,    34,
      26,     3,   100,    47,    28,     3,    11,    12,    13,    14,
      15,    16,    59,    55,    86,    18,    86,     3,    98,    24,
      11,    11,    57,    47,    59,    26,    34,    99,     8,     9,
       6,     7,    37,    11,    12,    13,    14,    15,    16,    19,
      27,    26,    22,    23,    20,    10,    24,    56,    58,    28,
      57,    29,    26,     3,    29,   127,   128,   127,   128,    11,
      12,    13,    14,    15,    16,    11,    12,    13,    14,    15,
      16,    39,    24,    25,    99,    63,    32,    22,    24,     3,
       4,     5,     6,     7,   112,   107,    70,    64,    30,    -1,
      34,    87
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    51,    63,     3,     0,    28,     3,    75,    25,    29,
      75,    26,    34,    36,    57,    59,    78,    85,    87,    88,
       3,    34,    42,    45,    53,    68,    71,    89,     3,    83,
      84,     3,    86,    75,    79,    80,    18,    10,    28,    68,
       3,    71,     3,     4,     5,     6,     7,    21,    28,    47,
      65,    66,    72,    73,    74,    77,    89,    92,    64,    68,
      26,    40,    69,    11,    26,    87,    11,    78,    27,    88,
      26,    72,    72,    76,    69,    56,    74,    72,    73,    11,
      12,    13,    14,    15,    16,    24,    55,    93,     8,     9,
      19,    22,    23,    90,     6,     7,    20,    91,    58,    26,
      68,     3,     4,     5,    66,    67,    84,    57,     3,    81,
      82,    79,    25,    29,    72,    29,    68,    92,    74,    73,
      72,    64,    69,     3,     5,    86,    76,    37,    39,    70,
      68,    68
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    62,    63,    64,    64,    65,    65,    65,    66,    66,
      67,    67,    67,    67,    67,    68,    68,    68,    68,    68,
      68,    69,    69,    70,    70,    71,    72,    72,    73,    73,
      74,    74,    74,    74,    74,    75,    75,    76,    76,    77,
      78,    78,    79,    79,    80,    81,    82,    83,    84,    84,
      85,    85,    86,    86,    87,    87,    88,    89,    90,    90,
      90,    90,    90,    91,    91,    91,    92,    92,    92,    93,
      93,    93,    93,    93,    93,    93
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     8,     3,     1,     1,     1,     1,     1,     1,
       2,     1,     2,     1,     1,     3,     5,     1,     1,     6,
       4,     3,     1,     2,     0,     3,     3,     1,     3,     1,
       1,     1,     3,     1,     2,     3,     1,     3,     1,     1,
       3,     1,     3,     2,     3,     1,     1,     3,     3,     2,
       3,     1,     4,     3,     3,     1,     3,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     3,     1,
       1,     1,     1,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 82 "parse.y" /* yacc.c:1646  */
    {
                parseresult = makeprogram((yyvsp[-6]), (yyvsp[-4]), (yyvsp[-1])); }
#line 1485 "y.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 84 "parse.y" /* yacc.c:1646  */
    { (yyval) = cons((yyvsp[-2]), (yyvsp[0]));}
#line 1491 "y.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 92 "parse.y" /* yacc.c:1646  */
    { (yyval) = unaryop((yyvsp[-1]), (yyvsp[0])); }
#line 1497 "y.tab.c" /* yacc.c:1646  */
    break;

  case 12:
#line 94 "parse.y" /* yacc.c:1646  */
    { (yyval) = unaryop((yyvsp[-1]), (yyvsp[0])); }
#line 1503 "y.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 99 "parse.y" /* yacc.c:1646  */
    { (yyval) = makeprogn((yyvsp[-2]),cons((yyvsp[-1]), (yyvsp[0]))); }
#line 1509 "y.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 100 "parse.y" /* yacc.c:1646  */
    { (yyval) = makeif((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[0])); }
#line 1515 "y.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 103 "parse.y" /* yacc.c:1646  */
    { (yyval) = makefor(1, (yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0])); }
#line 1521 "y.tab.c" /* yacc.c:1646  */
    break;

  case 20:
#line 104 "parse.y" /* yacc.c:1646  */
    { (yyval) = makerepeat((yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0])); }
#line 1527 "y.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 106 "parse.y" /* yacc.c:1646  */
    { (yyval) = cons((yyvsp[-1]), (yyvsp[0])); }
#line 1533 "y.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 107 "parse.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 1539 "y.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 109 "parse.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1545 "y.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 110 "parse.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 1551 "y.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 112 "parse.y" /* yacc.c:1646  */
    { (yyval) = binop((yyvsp[-1]), (yyvsp[-2]), (yyvsp[0])); }
#line 1557 "y.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 114 "parse.y" /* yacc.c:1646  */
    { (yyval) = binop((yyvsp[-1]), (yyvsp[-2]), (yyvsp[0])); }
#line 1563 "y.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 117 "parse.y" /* yacc.c:1646  */
    { (yyval) = binop((yyvsp[-1]), (yyvsp[-2]), (yyvsp[0])); }
#line 1569 "y.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 122 "parse.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 1575 "y.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 124 "parse.y" /* yacc.c:1646  */
    { (yyval) = unaryop((yyvsp[-1]), (yyvsp[0])); }
#line 1581 "y.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 126 "parse.y" /* yacc.c:1646  */
    { (yyval) = cons((yyvsp[-2]), (yyvsp[0])); }
#line 1587 "y.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 127 "parse.y" /* yacc.c:1646  */
    { (yyval) = cons((yyvsp[0]), NULL); }
#line 1593 "y.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 129 "parse.y" /* yacc.c:1646  */
    { (yyval) = cons((yyvsp[-2]), (yyvsp[0])); }
#line 1599 "y.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 132 "parse.y" /* yacc.c:1646  */
    { (yyval) = findid((yyvsp[0])); }
#line 1605 "y.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 134 "parse.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1611 "y.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 140 "parse.y" /* yacc.c:1646  */
    { instvars((yyvsp[-2]), (yyvsp[0])); }
#line 1617 "y.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 144 "parse.y" /* yacc.c:1646  */
    { (yyval) = findtype((yyvsp[0])); }
#line 1623 "y.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 146 "parse.y" /* yacc.c:1646  */
    { instconst((yyvsp[-2]), (yyvsp[0])); }
#line 1629 "y.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 151 "parse.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 1635 "y.tab.c" /* yacc.c:1646  */
    break;

  case 54:
#line 157 "parse.y" /* yacc.c:1646  */
    {(yyval) = (yyvsp[0]);}
#line 1641 "y.tab.c" /* yacc.c:1646  */
    break;

  case 56:
#line 160 "parse.y" /* yacc.c:1646  */
    { (yyval) = makeprogn((yyvsp[-2]),cons((yyvsp[-1]), (yyvsp[0]))); }
#line 1647 "y.tab.c" /* yacc.c:1646  */
    break;

  case 57:
#line 162 "parse.y" /* yacc.c:1646  */
    { (yyval) = makefuncall((yyvsp[-2]), (yyvsp[-3]), (yyvsp[-1])); }
#line 1653 "y.tab.c" /* yacc.c:1646  */
    break;

  case 66:
#line 169 "parse.y" /* yacc.c:1646  */
    { (yyval) = unaryop((yyvsp[-1]), (yyvsp[0])); }
#line 1659 "y.tab.c" /* yacc.c:1646  */
    break;

  case 68:
#line 171 "parse.y" /* yacc.c:1646  */
    { (yyval) = binop((yyvsp[-1]), (yyvsp[-2]), (yyvsp[0])); }
#line 1665 "y.tab.c" /* yacc.c:1646  */
    break;


#line 1669 "y.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 176 "parse.y" /* yacc.c:1906  */



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

