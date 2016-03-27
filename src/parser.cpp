// A Bison parser, made by GNU Bison 3.0.4.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2015 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.


// First part of user declarations.
#line 1 "src/syntax.y" // lalr1.cc:404

#ifndef AN_PARSER
#define AN_PARSER

#include <stdlib.h>
#include <stdio.h>
#include <tokens.h>
#include <ptree.h>

#ifndef YYSTYPE
#define YYSTYPE Node*
#endif

/* This has no effect when generating a c++ parser */
/* Setting verbose for a c++ parser requires %error-verbose, set in the next section */
#define YYERROR_VERBOSE

#include "yyparser.h"

/* Defined in lexer.cpp */
extern int yylex(...);

namespace ante{
    extern void error(string& msg, const char *fileName, unsigned int row, unsigned int col);
}

void yyerror(const char *msg);


#line 66 "src/parser.cpp" // lalr1.cc:404

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

#include "yyparser.h"

// User implementation prologue.

#line 80 "src/parser.cpp" // lalr1.cc:412


#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif



// Suppress unused-variable warnings by "using" E.
#define YYUSE(E) ((void) (E))

// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << std::endl;                  \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yystack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE(Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void>(0)
# define YY_STACK_PRINT()                static_cast<void>(0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyla.clear ())

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)


namespace yy {
#line 147 "src/parser.cpp" // lalr1.cc:479

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  parser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr = "";
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
              // Fall through.
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }


  /// Build a parser object.
  parser::parser ()
#if YYDEBUG
     :yydebug_ (false),
      yycdebug_ (&std::cerr)
#endif
  {}

  parser::~parser ()
  {}


  /*---------------.
  | Symbol types.  |
  `---------------*/

  inline
  parser::syntax_error::syntax_error (const std::string& m)
    : std::runtime_error (m)
  {}

  // basic_symbol.
  template <typename Base>
  inline
  parser::basic_symbol<Base>::basic_symbol ()
    : value ()
  {}

  template <typename Base>
  inline
  parser::basic_symbol<Base>::basic_symbol (const basic_symbol& other)
    : Base (other)
    , value ()
  {
    value = other.value;
  }


  template <typename Base>
  inline
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const semantic_type& v)
    : Base (t)
    , value (v)
  {}


  /// Constructor for valueless symbols.
  template <typename Base>
  inline
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t)
    : Base (t)
    , value ()
  {}

  template <typename Base>
  inline
  parser::basic_symbol<Base>::~basic_symbol ()
  {
    clear ();
  }

  template <typename Base>
  inline
  void
  parser::basic_symbol<Base>::clear ()
  {
    Base::clear ();
  }

  template <typename Base>
  inline
  bool
  parser::basic_symbol<Base>::empty () const
  {
    return Base::type_get () == empty_symbol;
  }

  template <typename Base>
  inline
  void
  parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move(s);
    value = s.value;
  }

  // by_type.
  inline
  parser::by_type::by_type ()
    : type (empty_symbol)
  {}

  inline
  parser::by_type::by_type (const by_type& other)
    : type (other.type)
  {}

  inline
  parser::by_type::by_type (token_type t)
    : type (yytranslate_ (t))
  {}

  inline
  void
  parser::by_type::clear ()
  {
    type = empty_symbol;
  }

  inline
  void
  parser::by_type::move (by_type& that)
  {
    type = that.type;
    that.clear ();
  }

  inline
  int
  parser::by_type::type_get () const
  {
    return type;
  }


  // by_state.
  inline
  parser::by_state::by_state ()
    : state (empty_state)
  {}

  inline
  parser::by_state::by_state (const by_state& other)
    : state (other.state)
  {}

  inline
  void
  parser::by_state::clear ()
  {
    state = empty_state;
  }

  inline
  void
  parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.clear ();
  }

  inline
  parser::by_state::by_state (state_type s)
    : state (s)
  {}

  inline
  parser::symbol_number_type
  parser::by_state::type_get () const
  {
    if (state == empty_state)
      return empty_symbol;
    else
      return yystos_[state];
  }

  inline
  parser::stack_symbol_type::stack_symbol_type ()
  {}


  inline
  parser::stack_symbol_type::stack_symbol_type (state_type s, symbol_type& that)
    : super_type (s)
  {
    value = that.value;
    // that is emptied.
    that.type = empty_symbol;
  }

  inline
  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    return *this;
  }


  template <typename Base>
  inline
  void
  parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);

    // User destructor.
    YYUSE (yysym.type_get ());
  }

#if YYDEBUG
  template <typename Base>
  void
  parser::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
    // Avoid a (spurious) G++ 4.8 warning about "array subscript is
    // below array bounds".
    if (yysym.empty ())
      std::abort ();
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " (";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  inline
  void
  parser::yypush_ (const char* m, state_type s, symbol_type& sym)
  {
    stack_symbol_type t (s, sym);
    yypush_ (m, t);
  }

  inline
  void
  parser::yypush_ (const char* m, stack_symbol_type& s)
  {
    if (m)
      YY_SYMBOL_PRINT (m, s);
    yystack_.push (s);
  }

  inline
  void
  parser::yypop_ (unsigned int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  parser::debug_level_type
  parser::debug_level () const
  {
    return yydebug_;
  }

  void
  parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  inline parser::state_type
  parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  inline bool
  parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  inline bool
  parser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  parser::parse ()
  {
    // State.
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The return value of parse ().
    int yyresult;

    // FIXME: This shoud be completely indented.  It is not yet to
    // avoid gratuitous conflicts when merging into the master branch.
    try
      {
    YYCDEBUG << "Starting parse" << std::endl;


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, yyla);

    // A new symbol was pushed on the stack.
  yynewstate:
    YYCDEBUG << "Entering state " << yystack_[0].state << std::endl;

    // Accept?
    if (yystack_[0].state == yyfinal_)
      goto yyacceptlab;

    goto yybackup;

    // Backup.
  yybackup:

    // Try to take a decision without lookahead.
    yyn = yypact_[yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyla.empty ())
      {
        YYCDEBUG << "Reading a token: ";
        try
          {
            yyla.type = yytranslate_ (yylex (&yyla.value));
          }
        catch (const syntax_error& yyexc)
          {
            error (yyexc);
            goto yyerrlab1;
          }
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      goto yydefault;

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", yyn, yyla);
    goto yynewstate;

  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;

  /*-----------------------------.
  | yyreduce -- Do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_(yystack_[yylen].state, yyr1_[yyn]);
      /* If YYLEN is nonzero, implement the default value of the
         action: '$$ = $1'.  Otherwise, use the top of the stack.

         Otherwise, the following line sets YYLHS.VALUE to garbage.
         This behavior is undocumented and Bison users should not rely
         upon it.  */
      if (yylen)
        yylhs.value = yystack_[yylen - 1].value;
      else
        yylhs.value = yystack_[0].value;


      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
      try
        {
          switch (yyn)
            {
  case 3:
#line 114 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[2].value), (yystack_[1].value));}
#line 610 "src/parser.cpp" // lalr1.cc:859
    break;

  case 4:
#line 115 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[1].value), (yystack_[0].value));}
#line 616 "src/parser.cpp" // lalr1.cc:859
    break;

  case 5:
#line 116 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot((yystack_[1].value));}
#line 622 "src/parser.cpp" // lalr1.cc:859
    break;

  case 6:
#line 117 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot((yystack_[0].value));}
#line 628 "src/parser.cpp" // lalr1.cc:859
    break;

  case 21:
#line 145 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (Node*)lextxt;}
#line 634 "src/parser.cpp" // lalr1.cc:859
    break;

  case 22:
#line 148 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (Node*)lextxt;}
#line 640 "src/parser.cpp" // lalr1.cc:859
    break;

  case 23:
#line 151 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkIntLitNode(lextxt);}
#line 646 "src/parser.cpp" // lalr1.cc:859
    break;

  case 24:
#line 154 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkFltLitNode(lextxt);}
#line 652 "src/parser.cpp" // lalr1.cc:859
    break;

  case 25:
#line 157 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkStrLitNode(lextxt);}
#line 658 "src/parser.cpp" // lalr1.cc:859
    break;

  case 26:
#line 160 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_I8,  (char*)"");}
#line 664 "src/parser.cpp" // lalr1.cc:859
    break;

  case 27:
#line 161 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_I16, (char*)"");}
#line 670 "src/parser.cpp" // lalr1.cc:859
    break;

  case 28:
#line 162 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_I32, (char*)"");}
#line 676 "src/parser.cpp" // lalr1.cc:859
    break;

  case 29:
#line 163 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_I64, (char*)"");}
#line 682 "src/parser.cpp" // lalr1.cc:859
    break;

  case 30:
#line 164 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_U8,  (char*)"");}
#line 688 "src/parser.cpp" // lalr1.cc:859
    break;

  case 31:
#line 165 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_U16, (char*)"");}
#line 694 "src/parser.cpp" // lalr1.cc:859
    break;

  case 32:
#line 166 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_U32, (char*)"");}
#line 700 "src/parser.cpp" // lalr1.cc:859
    break;

  case 33:
#line 167 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_U64, (char*)"");}
#line 706 "src/parser.cpp" // lalr1.cc:859
    break;

  case 34:
#line 168 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Isz, (char*)"");}
#line 712 "src/parser.cpp" // lalr1.cc:859
    break;

  case 35:
#line 169 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Usz, (char*)"");}
#line 718 "src/parser.cpp" // lalr1.cc:859
    break;

  case 36:
#line 170 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_F16, (char*)"");}
#line 724 "src/parser.cpp" // lalr1.cc:859
    break;

  case 37:
#line 171 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_F32, (char*)"");}
#line 730 "src/parser.cpp" // lalr1.cc:859
    break;

  case 38:
#line 172 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_F64, (char*)"");}
#line 736 "src/parser.cpp" // lalr1.cc:859
    break;

  case 39:
#line 173 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_C8,  (char*)"");}
#line 742 "src/parser.cpp" // lalr1.cc:859
    break;

  case 40:
#line 174 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_C32, (char*)"");}
#line 748 "src/parser.cpp" // lalr1.cc:859
    break;

  case 41:
#line 175 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Bool, (char*)"");}
#line 754 "src/parser.cpp" // lalr1.cc:859
    break;

  case 42:
#line 176 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Void, (char*)"");}
#line 760 "src/parser.cpp" // lalr1.cc:859
    break;

  case 43:
#line 177 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Data, (char*)(yystack_[0].value));}
#line 766 "src/parser.cpp" // lalr1.cc:859
    break;

  case 44:
#line 178 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_TypeVar, (char*)(yystack_[1].value));}
#line 772 "src/parser.cpp" // lalr1.cc:859
    break;

  case 45:
#line 183 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Ptr,  (char*)"", (yystack_[1].value));}
#line 778 "src/parser.cpp" // lalr1.cc:859
    break;

  case 46:
#line 184 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Array,(char*)"", (yystack_[2].value));}
#line 784 "src/parser.cpp" // lalr1.cc:859
    break;

  case 47:
#line 185 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Func, (char*)"", (yystack_[3].value));}
#line 790 "src/parser.cpp" // lalr1.cc:859
    break;

  case 48:
#line 186 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeNode(TT_Func, (char*)"", (yystack_[2].value));}
#line 796 "src/parser.cpp" // lalr1.cc:859
    break;

  case 49:
#line 187 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[1].value);}
#line 802 "src/parser.cpp" // lalr1.cc:859
    break;

  case 50:
#line 188 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 808 "src/parser.cpp" // lalr1.cc:859
    break;

  case 51:
#line 191 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[2].value), (yystack_[0].value));}
#line 814 "src/parser.cpp" // lalr1.cc:859
    break;

  case 53:
#line 193 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot((yystack_[0].value));}
#line 820 "src/parser.cpp" // lalr1.cc:859
    break;

  case 54:
#line 196 "src/syntax.y" // lalr1.cc:859
    {Node* tmp = getRoot(); 
                        if(tmp == (yystack_[0].value)){//singular type, first type in list equals the last
                            (yylhs.value) = tmp;
                        }else{ //tuple type
                            (yylhs.value) = mkTypeNode(TT_Tuple, (char*)"", tmp);
                        }
                       }
#line 832 "src/parser.cpp" // lalr1.cc:859
    break;

  case 55:
#line 205 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkModNode(Tok_Pub);}
#line 838 "src/parser.cpp" // lalr1.cc:859
    break;

  case 56:
#line 206 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkModNode(Tok_Pri);}
#line 844 "src/parser.cpp" // lalr1.cc:859
    break;

  case 57:
#line 207 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkModNode(Tok_Pro);}
#line 850 "src/parser.cpp" // lalr1.cc:859
    break;

  case 58:
#line 208 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkModNode(Tok_Raw);}
#line 856 "src/parser.cpp" // lalr1.cc:859
    break;

  case 59:
#line 209 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkModNode(Tok_Const);}
#line 862 "src/parser.cpp" // lalr1.cc:859
    break;

  case 60:
#line 210 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkModNode(Tok_Ext);}
#line 868 "src/parser.cpp" // lalr1.cc:859
    break;

  case 61:
#line 211 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkModNode(Tok_Noinit);}
#line 874 "src/parser.cpp" // lalr1.cc:859
    break;

  case 62:
#line 212 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkModNode(Tok_Pathogen);}
#line 880 "src/parser.cpp" // lalr1.cc:859
    break;

  case 63:
#line 215 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[1].value), (yystack_[0].value));}
#line 886 "src/parser.cpp" // lalr1.cc:859
    break;

  case 64:
#line 216 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot((yystack_[0].value));}
#line 892 "src/parser.cpp" // lalr1.cc:859
    break;

  case 65:
#line 219 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = getRoot();}
#line 898 "src/parser.cpp" // lalr1.cc:859
    break;

  case 66:
#line 223 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarDeclNode((char*)(yystack_[2].value), (yystack_[4].value), (yystack_[3].value), (yystack_[0].value));}
#line 904 "src/parser.cpp" // lalr1.cc:859
    break;

  case 67:
#line 224 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarDeclNode((char*)(yystack_[0].value), (yystack_[2].value), (yystack_[1].value),  0);}
#line 910 "src/parser.cpp" // lalr1.cc:859
    break;

  case 68:
#line 225 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarDeclNode((char*)(yystack_[2].value), 0,  (yystack_[3].value), (yystack_[0].value));}
#line 916 "src/parser.cpp" // lalr1.cc:859
    break;

  case 69:
#line 226 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarDeclNode((char*)(yystack_[0].value), 0,  (yystack_[1].value),  0);}
#line 922 "src/parser.cpp" // lalr1.cc:859
    break;

  case 70:
#line 229 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkLetBindingNode((char*)(yystack_[3].value), (yystack_[4].value), (yystack_[3].value), (yystack_[0].value));}
#line 928 "src/parser.cpp" // lalr1.cc:859
    break;

  case 71:
#line 230 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkLetBindingNode((char*)(yystack_[3].value), (yystack_[3].value), 0,  (yystack_[0].value));}
#line 934 "src/parser.cpp" // lalr1.cc:859
    break;

  case 72:
#line 231 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkLetBindingNode((char*)(yystack_[2].value), 0,  (yystack_[3].value), (yystack_[0].value));}
#line 940 "src/parser.cpp" // lalr1.cc:859
    break;

  case 73:
#line 232 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkLetBindingNode((char*)(yystack_[2].value), 0,  0,  (yystack_[0].value));}
#line 946 "src/parser.cpp" // lalr1.cc:859
    break;

  case 74:
#line 236 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarAssignNode((yystack_[2].value), (yystack_[0].value));}
#line 952 "src/parser.cpp" // lalr1.cc:859
    break;

  case 75:
#line 237 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarAssignNode((yystack_[2].value), mkBinOpNode('+', mkUnOpNode('*', (yystack_[2].value)), (yystack_[0].value)));}
#line 958 "src/parser.cpp" // lalr1.cc:859
    break;

  case 76:
#line 238 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarAssignNode((yystack_[2].value), mkBinOpNode('-', mkUnOpNode('*', (yystack_[2].value)), (yystack_[0].value)));}
#line 964 "src/parser.cpp" // lalr1.cc:859
    break;

  case 77:
#line 239 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarAssignNode((yystack_[2].value), mkBinOpNode('*', mkUnOpNode('*', (yystack_[2].value)), (yystack_[0].value)));}
#line 970 "src/parser.cpp" // lalr1.cc:859
    break;

  case 78:
#line 240 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarAssignNode((yystack_[2].value), mkBinOpNode('/', mkUnOpNode('*', (yystack_[2].value)), (yystack_[0].value)));}
#line 976 "src/parser.cpp" // lalr1.cc:859
    break;

  case 79:
#line 243 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[2].value), (yystack_[0].value));}
#line 982 "src/parser.cpp" // lalr1.cc:859
    break;

  case 80:
#line 244 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot((yystack_[0].value));}
#line 988 "src/parser.cpp" // lalr1.cc:859
    break;

  case 81:
#line 247 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = getRoot();}
#line 994 "src/parser.cpp" // lalr1.cc:859
    break;

  case 82:
#line 250 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkDataDeclNode((char*)(yystack_[1].value), (yystack_[0].value));}
#line 1000 "src/parser.cpp" // lalr1.cc:859
    break;

  case 83:
#line 251 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkDataDeclNode((char*)(yystack_[2].value), (yystack_[0].value));}
#line 1006 "src/parser.cpp" // lalr1.cc:859
    break;

  case 84:
#line 252 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkDataDeclNode((char*)(yystack_[1].value), (yystack_[0].value));}
#line 1012 "src/parser.cpp" // lalr1.cc:859
    break;

  case 85:
#line 253 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkDataDeclNode((char*)(yystack_[2].value), (yystack_[0].value));}
#line 1018 "src/parser.cpp" // lalr1.cc:859
    break;

  case 86:
#line 256 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkNamedValNode(mkVarNode((char*)(yystack_[0].value)), (yystack_[1].value));}
#line 1024 "src/parser.cpp" // lalr1.cc:859
    break;

  case 87:
#line 257 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkNamedValNode(0, (yystack_[0].value));}
#line 1030 "src/parser.cpp" // lalr1.cc:859
    break;

  case 89:
#line 261 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[2].value), (yystack_[0].value));}
#line 1036 "src/parser.cpp" // lalr1.cc:859
    break;

  case 90:
#line 262 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot((yystack_[0].value));}
#line 1042 "src/parser.cpp" // lalr1.cc:859
    break;

  case 91:
#line 265 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = getRoot();}
#line 1048 "src/parser.cpp" // lalr1.cc:859
    break;

  case 97:
#line 278 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1054 "src/parser.cpp" // lalr1.cc:859
    break;

  case 98:
#line 279 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1060 "src/parser.cpp" // lalr1.cc:859
    break;

  case 99:
#line 280 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1066 "src/parser.cpp" // lalr1.cc:859
    break;

  case 100:
#line 281 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1072 "src/parser.cpp" // lalr1.cc:859
    break;

  case 101:
#line 284 "src/syntax.y" // lalr1.cc:859
    {setNext((yystack_[2].value), (yystack_[1].value)); (yylhs.value) = getRoot();}
#line 1078 "src/parser.cpp" // lalr1.cc:859
    break;

  case 102:
#line 285 "src/syntax.y" // lalr1.cc:859
    {setNext((yystack_[2].value), (yystack_[1].value)); (yylhs.value) = getRoot();}
#line 1084 "src/parser.cpp" // lalr1.cc:859
    break;

  case 103:
#line 286 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[1].value);}
#line 1090 "src/parser.cpp" // lalr1.cc:859
    break;

  case 104:
#line 287 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[1].value);}
#line 1096 "src/parser.cpp" // lalr1.cc:859
    break;

  case 105:
#line 290 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[1].value), mkVarNode((char*)(yystack_[0].value)));}
#line 1102 "src/parser.cpp" // lalr1.cc:859
    break;

  case 106:
#line 291 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot(mkVarNode((char*)(yystack_[0].value)));}
#line 1108 "src/parser.cpp" // lalr1.cc:859
    break;

  case 107:
#line 299 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[3].value), mkNamedValNode(getRoot(), (yystack_[1].value)));}
#line 1114 "src/parser.cpp" // lalr1.cc:859
    break;

  case 108:
#line 300 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot(mkNamedValNode(getRoot(), (yystack_[1].value)));}
#line 1120 "src/parser.cpp" // lalr1.cc:859
    break;

  case 109:
#line 303 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = getRoot();}
#line 1126 "src/parser.cpp" // lalr1.cc:859
    break;

  case 110:
#line 304 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1132 "src/parser.cpp" // lalr1.cc:859
    break;

  case 111:
#line 307 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkFuncDeclNode((char*)(yystack_[3].value), (yystack_[5].value), (yystack_[4].value), (yystack_[1].value), (yystack_[0].value));}
#line 1138 "src/parser.cpp" // lalr1.cc:859
    break;

  case 112:
#line 308 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkFuncDeclNode((char*)(yystack_[6].value), (yystack_[8].value), (yystack_[7].value), (yystack_[1].value), (yystack_[0].value));}
#line 1144 "src/parser.cpp" // lalr1.cc:859
    break;

  case 113:
#line 309 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkFuncDeclNode((char*)(yystack_[3].value), 0,  (yystack_[4].value), (yystack_[1].value), (yystack_[0].value));}
#line 1150 "src/parser.cpp" // lalr1.cc:859
    break;

  case 114:
#line 310 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkFuncDeclNode((char*)(yystack_[6].value), 0,  (yystack_[7].value), (yystack_[1].value), (yystack_[0].value));}
#line 1156 "src/parser.cpp" // lalr1.cc:859
    break;

  case 115:
#line 313 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkFuncCallNode((char*)(yystack_[1].value), (yystack_[0].value));}
#line 1162 "src/parser.cpp" // lalr1.cc:859
    break;

  case 116:
#line 316 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkRetNode((yystack_[0].value));}
#line 1168 "src/parser.cpp" // lalr1.cc:859
    break;

  case 117:
#line 319 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setElse((IfNode*)(yystack_[3].value), (IfNode*)mkIfNode((yystack_[1].value), (yystack_[0].value)));}
#line 1174 "src/parser.cpp" // lalr1.cc:859
    break;

  case 118:
#line 320 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot(mkIfNode((yystack_[1].value), (yystack_[0].value)));}
#line 1180 "src/parser.cpp" // lalr1.cc:859
    break;

  case 119:
#line 323 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setElse((IfNode*)(yystack_[2].value), (IfNode*)mkIfNode(NULL, (yystack_[0].value)));}
#line 1186 "src/parser.cpp" // lalr1.cc:859
    break;

  case 120:
#line 324 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1192 "src/parser.cpp" // lalr1.cc:859
    break;

  case 121:
#line 325 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot(mkIfNode(NULL, (yystack_[0].value)));}
#line 1198 "src/parser.cpp" // lalr1.cc:859
    break;

  case 122:
#line 326 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot(NULL);}
#line 1204 "src/parser.cpp" // lalr1.cc:859
    break;

  case 123:
#line 329 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkIfNode((yystack_[2].value), (yystack_[1].value), (IfNode*)getRoot());}
#line 1210 "src/parser.cpp" // lalr1.cc:859
    break;

  case 124:
#line 332 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1216 "src/parser.cpp" // lalr1.cc:859
    break;

  case 125:
#line 335 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1222 "src/parser.cpp" // lalr1.cc:859
    break;

  case 126:
#line 338 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1228 "src/parser.cpp" // lalr1.cc:859
    break;

  case 127:
#line 341 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkVarNode((char*)(yystack_[0].value));}
#line 1234 "src/parser.cpp" // lalr1.cc:859
    break;

  case 128:
#line 344 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkUnOpNode('&', (yystack_[0].value));}
#line 1240 "src/parser.cpp" // lalr1.cc:859
    break;

  case 129:
#line 345 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkUnOpNode('*', (yystack_[0].value));}
#line 1246 "src/parser.cpp" // lalr1.cc:859
    break;

  case 130:
#line 346 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('[', mkRefVarNode((char*)(yystack_[3].value)), (yystack_[1].value));}
#line 1252 "src/parser.cpp" // lalr1.cc:859
    break;

  case 131:
#line 347 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkRefVarNode((char*)(yystack_[0].value));}
#line 1258 "src/parser.cpp" // lalr1.cc:859
    break;

  case 132:
#line 350 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1264 "src/parser.cpp" // lalr1.cc:859
    break;

  case 133:
#line 351 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[1].value);}
#line 1270 "src/parser.cpp" // lalr1.cc:859
    break;

  case 134:
#line 352 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1276 "src/parser.cpp" // lalr1.cc:859
    break;

  case 135:
#line 353 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1282 "src/parser.cpp" // lalr1.cc:859
    break;

  case 136:
#line 354 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[1].value);}
#line 1288 "src/parser.cpp" // lalr1.cc:859
    break;

  case 137:
#line 355 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1294 "src/parser.cpp" // lalr1.cc:859
    break;

  case 138:
#line 356 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1300 "src/parser.cpp" // lalr1.cc:859
    break;

  case 139:
#line 357 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1306 "src/parser.cpp" // lalr1.cc:859
    break;

  case 140:
#line 358 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1312 "src/parser.cpp" // lalr1.cc:859
    break;

  case 141:
#line 359 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1318 "src/parser.cpp" // lalr1.cc:859
    break;

  case 142:
#line 360 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBoolLitNode(1);}
#line 1324 "src/parser.cpp" // lalr1.cc:859
    break;

  case 143:
#line 361 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBoolLitNode(0);}
#line 1330 "src/parser.cpp" // lalr1.cc:859
    break;

  case 144:
#line 364 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTupleNode((yystack_[1].value));}
#line 1336 "src/parser.cpp" // lalr1.cc:859
    break;

  case 145:
#line 365 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTupleNode(0);}
#line 1342 "src/parser.cpp" // lalr1.cc:859
    break;

  case 146:
#line 368 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkArrayNode((yystack_[1].value));}
#line 1348 "src/parser.cpp" // lalr1.cc:859
    break;

  case 147:
#line 369 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkArrayNode(0);}
#line 1354 "src/parser.cpp" // lalr1.cc:859
    break;

  case 148:
#line 372 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1360 "src/parser.cpp" // lalr1.cc:859
    break;

  case 149:
#line 373 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = NULL;}
#line 1366 "src/parser.cpp" // lalr1.cc:859
    break;

  case 150:
#line 376 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = getRoot();}
#line 1372 "src/parser.cpp" // lalr1.cc:859
    break;

  case 151:
#line 379 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[2].value), (yystack_[0].value));}
#line 1378 "src/parser.cpp" // lalr1.cc:859
    break;

  case 152:
#line 380 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot((yystack_[0].value));}
#line 1384 "src/parser.cpp" // lalr1.cc:859
    break;

  case 153:
#line 385 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkUnOpNode('*', (yystack_[0].value));}
#line 1390 "src/parser.cpp" // lalr1.cc:859
    break;

  case 154:
#line 386 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkUnOpNode('&', (yystack_[0].value));}
#line 1396 "src/parser.cpp" // lalr1.cc:859
    break;

  case 155:
#line 387 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkUnOpNode('-', (yystack_[0].value));}
#line 1402 "src/parser.cpp" // lalr1.cc:859
    break;

  case 156:
#line 388 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkTypeCastNode((yystack_[2].value), (yystack_[0].value));}
#line 1408 "src/parser.cpp" // lalr1.cc:859
    break;

  case 157:
#line 391 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1414 "src/parser.cpp" // lalr1.cc:859
    break;

  case 158:
#line 394 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('+', (yystack_[2].value), (yystack_[0].value));}
#line 1420 "src/parser.cpp" // lalr1.cc:859
    break;

  case 159:
#line 395 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('-', (yystack_[2].value), (yystack_[0].value));}
#line 1426 "src/parser.cpp" // lalr1.cc:859
    break;

  case 160:
#line 396 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('*', (yystack_[2].value), (yystack_[0].value));}
#line 1432 "src/parser.cpp" // lalr1.cc:859
    break;

  case 161:
#line 397 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('/', (yystack_[2].value), (yystack_[0].value));}
#line 1438 "src/parser.cpp" // lalr1.cc:859
    break;

  case 162:
#line 398 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('%', (yystack_[2].value), (yystack_[0].value));}
#line 1444 "src/parser.cpp" // lalr1.cc:859
    break;

  case 163:
#line 399 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('<', (yystack_[2].value), (yystack_[0].value));}
#line 1450 "src/parser.cpp" // lalr1.cc:859
    break;

  case 164:
#line 400 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('>', (yystack_[2].value), (yystack_[0].value));}
#line 1456 "src/parser.cpp" // lalr1.cc:859
    break;

  case 165:
#line 401 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('^', (yystack_[2].value), (yystack_[0].value));}
#line 1462 "src/parser.cpp" // lalr1.cc:859
    break;

  case 166:
#line 402 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('.', (yystack_[2].value), (yystack_[0].value));}
#line 1468 "src/parser.cpp" // lalr1.cc:859
    break;

  case 167:
#line 403 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(';', (yystack_[3].value), (yystack_[0].value));}
#line 1474 "src/parser.cpp" // lalr1.cc:859
    break;

  case 168:
#line 404 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('[', (yystack_[3].value), (yystack_[1].value));}
#line 1480 "src/parser.cpp" // lalr1.cc:859
    break;

  case 169:
#line 405 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_Where, (yystack_[4].value), mkLetBindingNode((char*)(yystack_[2].value), 0, 0, (yystack_[0].value)));}
#line 1486 "src/parser.cpp" // lalr1.cc:859
    break;

  case 170:
#line 406 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_Eq, (yystack_[2].value), (yystack_[0].value));}
#line 1492 "src/parser.cpp" // lalr1.cc:859
    break;

  case 171:
#line 407 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_NotEq, (yystack_[2].value), (yystack_[0].value));}
#line 1498 "src/parser.cpp" // lalr1.cc:859
    break;

  case 172:
#line 408 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_GrtrEq, (yystack_[2].value), (yystack_[0].value));}
#line 1504 "src/parser.cpp" // lalr1.cc:859
    break;

  case 173:
#line 409 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_LesrEq, (yystack_[2].value), (yystack_[0].value));}
#line 1510 "src/parser.cpp" // lalr1.cc:859
    break;

  case 174:
#line 410 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_Or, (yystack_[2].value), (yystack_[0].value));}
#line 1516 "src/parser.cpp" // lalr1.cc:859
    break;

  case 175:
#line 411 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_And, (yystack_[2].value), (yystack_[0].value));}
#line 1522 "src/parser.cpp" // lalr1.cc:859
    break;

  case 176:
#line 412 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1528 "src/parser.cpp" // lalr1.cc:859
    break;

  case 177:
#line 417 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = getRoot();}
#line 1534 "src/parser.cpp" // lalr1.cc:859
    break;

  case 178:
#line 420 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setNext((yystack_[3].value), (yystack_[0].value));}
#line 1540 "src/parser.cpp" // lalr1.cc:859
    break;

  case 179:
#line 421 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = setRoot((yystack_[0].value));}
#line 1546 "src/parser.cpp" // lalr1.cc:859
    break;

  case 180:
#line 424 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('+', (yystack_[3].value), (yystack_[0].value));}
#line 1552 "src/parser.cpp" // lalr1.cc:859
    break;

  case 181:
#line 425 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('-', (yystack_[3].value), (yystack_[0].value));}
#line 1558 "src/parser.cpp" // lalr1.cc:859
    break;

  case 182:
#line 426 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('*', (yystack_[3].value), (yystack_[0].value));}
#line 1564 "src/parser.cpp" // lalr1.cc:859
    break;

  case 183:
#line 427 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('/', (yystack_[3].value), (yystack_[0].value));}
#line 1570 "src/parser.cpp" // lalr1.cc:859
    break;

  case 184:
#line 428 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('%', (yystack_[3].value), (yystack_[0].value));}
#line 1576 "src/parser.cpp" // lalr1.cc:859
    break;

  case 185:
#line 429 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('<', (yystack_[3].value), (yystack_[0].value));}
#line 1582 "src/parser.cpp" // lalr1.cc:859
    break;

  case 186:
#line 430 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('>', (yystack_[3].value), (yystack_[0].value));}
#line 1588 "src/parser.cpp" // lalr1.cc:859
    break;

  case 187:
#line 431 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('^', (yystack_[3].value), (yystack_[0].value));}
#line 1594 "src/parser.cpp" // lalr1.cc:859
    break;

  case 188:
#line 432 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('.', (yystack_[3].value), (yystack_[0].value));}
#line 1600 "src/parser.cpp" // lalr1.cc:859
    break;

  case 189:
#line 433 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(';', (yystack_[3].value), (yystack_[0].value));}
#line 1606 "src/parser.cpp" // lalr1.cc:859
    break;

  case 190:
#line 434 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode('[', (yystack_[4].value), (yystack_[2].value));}
#line 1612 "src/parser.cpp" // lalr1.cc:859
    break;

  case 191:
#line 435 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_Where, (yystack_[4].value), mkLetBindingNode((char*)(yystack_[2].value), 0, 0, (yystack_[0].value)));}
#line 1618 "src/parser.cpp" // lalr1.cc:859
    break;

  case 192:
#line 436 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_Eq, (yystack_[3].value), (yystack_[0].value));}
#line 1624 "src/parser.cpp" // lalr1.cc:859
    break;

  case 193:
#line 437 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_NotEq, (yystack_[3].value), (yystack_[0].value));}
#line 1630 "src/parser.cpp" // lalr1.cc:859
    break;

  case 194:
#line 438 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_GrtrEq, (yystack_[3].value), (yystack_[0].value));}
#line 1636 "src/parser.cpp" // lalr1.cc:859
    break;

  case 195:
#line 439 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_LesrEq, (yystack_[3].value), (yystack_[0].value));}
#line 1642 "src/parser.cpp" // lalr1.cc:859
    break;

  case 196:
#line 440 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_Or, (yystack_[3].value), (yystack_[0].value));}
#line 1648 "src/parser.cpp" // lalr1.cc:859
    break;

  case 197:
#line 441 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = mkBinOpNode(Tok_And, (yystack_[3].value), (yystack_[0].value));}
#line 1654 "src/parser.cpp" // lalr1.cc:859
    break;

  case 198:
#line 442 "src/syntax.y" // lalr1.cc:859
    {(yylhs.value) = (yystack_[0].value);}
#line 1660 "src/parser.cpp" // lalr1.cc:859
    break;


#line 1664 "src/parser.cpp" // lalr1.cc:859
            default:
              break;
            }
        }
      catch (const syntax_error& yyexc)
        {
          error (yyexc);
          YYERROR;
        }
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, yylhs);
    }
    goto yynewstate;

  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        error (yysyntax_error_ (yystack_[0].state, yyla));
      }


    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
          YYABORT;
        else if (!yyla.empty ())
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyla.clear ();
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:

    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (false)
      goto yyerrorlab;
    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;

  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yyterror_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
                {
                  yyn = yytable_[yyn];
                  if (0 < yyn)
                    break;
                }
            }

          // Pop the current state because it cannot handle the error token.
          if (yystack_.size () == 1)
            YYABORT;

          yy_destroy_ ("Error: popping", yystack_[0]);
          yypop_ ();
          YY_STACK_PRINT ();
        }


      // Shift the error token.
      error_token.state = yyn;
      yypush_ ("Shifting", error_token);
    }
    goto yynewstate;

    // Accept.
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;

    // Abort.
  yyabortlab:
    yyresult = 1;
    goto yyreturn;

  yyreturn:
    if (!yyla.empty ())
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack"
                 << std::endl;
        // Do not try to display the values of the reclaimed symbols,
        // as their printer might throw an exception.
        if (!yyla.empty ())
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
  }

  void
  parser::error (const syntax_error& yyexc)
  {
    error (yyexc.what());
  }

  // Generate an error message.
  std::string
  parser::yysyntax_error_ (state_type yystate, const symbol_type& yyla) const
  {
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    size_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yyla) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yyla.  (However, yyla is currently not documented for users.)
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state
         merging (from LALR or IELR) and default reductions corrupt the
         expected token list.  However, the list is correct for
         canonical LR with one exception: it will still contain any
         token that will not be accepted due to an error action in a
         later state.
    */
    if (!yyla.empty ())
      {
        int yytoken = yyla.type_get ();
        yyarg[yycount++] = yytname_[yytoken];
        int yyn = yypact_[yystate];
        if (!yy_pact_value_is_default_ (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            // Stay within bounds of both yycheck and yytname.
            int yychecklim = yylast_ - yyn + 1;
            int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
            for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck_[yyx + yyn] == yyx && yyx != yyterror_
                  && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      break;
                    }
                  else
                    yyarg[yycount++] = yytname_[yyx];
                }
          }
      }

    char const* yyformat = YY_NULLPTR;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
        YYCASE_(0, YY_("syntax error"));
        YYCASE_(1, YY_("syntax error, unexpected %s"));
        YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    std::string yyres;
    // Argument number.
    size_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += yytnamerr_ (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  const short int parser::yypact_ninf_ = -275;

  const signed char parser::yytable_ninf_ = -1;

  const short int
  parser::yypact_[] =
  {
     -53,  -275,    32,   540,  -275,  -275,  -275,  -275,  -275,  -275,
    -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,
    -275,  -275,  -275,  -275,   746,   746,   841,   746,    22,   841,
      81,    11,  -275,  -275,  -275,  -275,  -275,  -275,  -275,  -275,
       7,   940,     7,   454,  -275,    26,    31,  -275,  -275,    -1,
     -49,    91,  -275,   238,   862,  -275,  -275,  -275,  -275,  -275,
    -275,  -275,  -275,  -275,  -275,  -275,  -275,    -8,  -275,  -275,
    -275,  -275,  -275,   746,   746,   746,   626,   660,   746,   -33,
    -275,  -275,  -275,    16,  -275,  -275,  -275,  -275,  -275,  -275,
    -275,   941,    57,    34,    91,   940,    92,    57,   746,   -52,
      91,   940,   -37,    81,    60,  -275,    54,  -275,    68,  -275,
    -275,  -275,    79,  -275,   626,   746,  -275,  -275,  -275,   919,
      62,   940,   940,   -28,  -275,    81,    11,    91,   746,   746,
     746,   746,   746,  -275,    82,    80,   961,  -275,  -275,  -275,
     -44,    72,    84,    75,  -275,    85,  -275,  -275,   746,   746,
     746,   746,   746,   746,   746,    91,   -53,   746,   746,   746,
     746,   746,   746,   746,   746,   746,   746,   540,   -18,    70,
      91,   746,  -275,    57,   746,    77,   -11,    91,   784,    81,
     101,  -275,    86,    -4,  -275,  -275,  -275,    87,  -275,    94,
    -275,    -1,    -1,   746,   746,   940,   -37,    60,  -275,     2,
    -275,  -275,  -275,  -275,  -275,  -275,   -53,   -53,   -53,   -53,
     -53,   -53,   -53,    91,   -53,   -53,   -53,   -53,   -53,   -53,
     -53,   -53,   -53,   -53,   746,  -275,   746,  -275,  -275,  -275,
      -7,    -7,    -7,    -7,   493,   196,    95,   746,    -7,    -7,
      64,    64,   -32,   -32,   -32,   -32,    89,    93,   540,   112,
      35,   746,    57,    67,  -275,    96,    57,  -275,  -275,   746,
     746,   100,    91,   129,  -275,    49,  -275,  -275,    52,  -275,
     746,    81,  -275,  -275,  -275,   108,  -275,  -275,    91,   120,
      57,   101,  -275,  -275,   746,   746,   940,   746,   746,   746,
     746,   746,   746,   746,   104,   746,   746,   746,   746,   746,
     746,   746,   746,   746,   746,   281,  -275,   746,  1018,  -275,
     125,    65,  -275,  -275,    57,  -275,   746,    57,  -275,  -275,
    -275,   746,  -275,   784,  -275,    81,  -275,  -275,   107,   111,
    -275,    91,   940,  -275,  -275,   115,  -275,    57,   961,   308,
     308,   308,   308,  1051,  1108,   746,  1037,   308,   308,   241,
     241,     6,     6,     6,     6,   116,   -53,  1018,  -275,  -275,
    -275,    57,  -275,  -275,  -275,  -275,   746,   940,  -275,    91,
     114,  -275,  1037,  -275,  -275,  -275,    57,    91,   940,  -275,
      57,  -275
  };

  const unsigned char
  parser::yydefact_[] =
  {
       8,     7,     0,     0,     1,    21,    22,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    40,    41,    42,     0,     0,     0,     0,     0,     0,
       0,     0,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,     0,     8,     6,     0,   131,    43,    50,    53,
      54,     0,    64,    65,     0,    16,    20,    17,    10,    11,
       9,    18,    19,    15,    12,    13,    14,     0,   142,   143,
      23,    24,    25,     0,     0,     0,     0,     0,     0,   127,
     139,   140,   141,     0,   132,   138,   176,   134,   135,   137,
     116,   157,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   100,   131,   129,     0,   128,
       2,     4,     0,     5,     0,     0,    44,   115,    45,     0,
       0,     0,     0,    69,    63,     0,     0,     0,     0,     0,
       0,     0,     0,   198,     0,   177,   179,   155,   153,   145,
       0,     0,   150,   152,   147,     0,   152,   154,     0,     0,
       0,     0,     0,     0,     0,     0,     8,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,    69,
       0,     0,   124,     0,     0,     0,     0,     0,     0,     0,
       0,    84,    95,     0,    98,    49,     3,     0,    48,     0,
      46,    51,    52,   149,     0,   110,     0,     0,    99,    67,
      75,    76,    77,    78,    74,   136,     8,     8,     8,     8,
       8,     8,     8,     0,     8,     8,     8,     8,     8,     8,
       8,     8,     8,     8,     0,   144,     0,   133,   146,   156,
     170,   171,   172,   173,   174,   175,     0,     0,   163,   164,
     158,   159,   160,   161,   162,   165,   166,     0,     0,     6,
       0,     0,     0,   120,   123,    67,     0,   125,    73,     0,
       0,     0,    87,     0,    90,     0,    88,    80,     0,    85,
       0,     0,    96,   130,    47,     0,   148,    68,     0,   109,
       0,     0,    82,    97,   149,     0,   110,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   151,     0,   167,   168,
       4,     0,   103,   104,     0,   121,     0,     0,   126,    72,
      71,     0,    86,     0,    91,     0,    81,    94,    92,     0,
     106,   108,     0,   113,    83,     0,    66,     0,   178,   192,
     193,   194,   195,   196,   197,     0,   189,   185,   186,   180,
     181,   182,   183,   184,   187,   188,     8,   169,   101,   102,
     118,     0,   119,    70,    89,    79,     0,   110,   105,     0,
       0,   111,   191,   190,   117,    93,     0,   107,   110,   114,
       0,   112
  };

  const short int
  parser::yypgoto_[] =
  {
    -275,  -275,    37,    20,   -41,   -35,    -3,   -17,  -275,  -275,
    -275,  -275,    12,  -275,     1,   146,  -275,   -23,   176,  -275,
    -275,  -275,     9,  -275,  -117,  -275,  -171,  -275,   -97,  -173,
     -36,  -161,  -275,  -274,  -275,    -2,  -275,  -275,  -275,  -275,
    -275,  -275,  -275,  -275,    88,   110,   -22,  -275,   -75,   134,
    -275,  -275,    29,   266,  -275,  -275,   145
  };

  const short int
  parser::yydefgoto_[] =
  {
      -1,     2,    43,     3,    44,    45,    79,    47,    80,    81,
      82,    48,    49,    50,    83,    52,    53,    54,    55,    56,
      57,   268,   180,    58,   264,   265,   181,   183,   105,    59,
     168,   331,   279,   280,    60,    84,    62,   253,   254,    63,
      64,    65,    66,    85,    67,    86,    87,    88,   275,   141,
     142,    89,   146,    91,   134,   135,   136
  };

  const unsigned short int
  parser::yytable_[] =
  {
      46,    61,   111,    95,    51,   266,   101,   184,   112,   269,
       5,     1,   337,   102,   104,     6,   128,   129,   130,   131,
     121,   251,   252,    93,   117,   282,    99,    94,   178,   198,
     100,   116,     4,   179,   174,   185,   122,   106,    93,   106,
      46,    61,   108,   148,    51,   164,   165,   114,   123,   166,
     116,    93,   193,    90,    92,   127,    97,   117,   194,   195,
     271,   172,   272,   110,    98,   159,   160,   161,   162,   163,
     164,   165,   116,   118,   166,   260,   103,   140,   132,   119,
     120,    40,   284,   222,   223,     6,   182,   224,   285,   286,
     113,   169,    93,   376,     5,    42,   170,   175,   176,   113,
     283,   313,   177,   148,   380,   143,   316,   317,   196,   197,
     334,   114,   115,   323,   116,   324,    93,   116,    93,    93,
     189,   325,   167,   326,   199,   103,   249,   173,   107,   186,
     109,   359,   250,   191,   192,   115,   171,   257,   161,   162,
     163,   164,   165,   186,   187,   166,   190,   185,   205,   206,
     266,   225,   236,   226,   227,   263,   194,   200,   201,   202,
     203,   204,   267,   259,    46,    61,   178,   255,    51,   228,
     166,   273,   270,   274,   261,    93,   237,   309,   312,   262,
     126,   307,   285,   133,   137,   138,   321,   329,   147,   332,
     345,   358,    93,   366,   370,   247,   278,   224,   367,   124,
     256,   378,    96,   258,   248,   281,   364,   310,   377,   335,
     294,   145,     0,   311,     0,     0,   315,     0,   149,   150,
     318,     0,   276,   277,   151,   152,   287,   288,   289,   290,
     291,   292,   293,     0,   295,   296,   297,   298,   299,   300,
     301,   302,   303,   304,   333,    46,    61,     0,     0,    51,
       0,     0,     0,     0,   328,   306,     0,     0,   229,   322,
       0,     0,     0,     0,     0,     0,   157,   158,   159,   160,
     161,   162,   163,   164,   165,   330,     0,   166,   360,     0,
     314,   362,     0,    93,     0,     0,     0,   278,   319,   320,
      32,    33,    34,    35,    36,    37,    38,    39,     0,   327,
     263,   371,     0,   207,   208,     0,     0,     0,   365,   209,
     210,   211,   212,   276,   336,   219,   220,   221,   222,   223,
      93,     0,   224,     0,   262,   374,     0,     0,   368,    93,
       0,     0,     0,   369,   133,     0,     0,     0,     0,     0,
     379,   213,     0,     0,   381,   361,     0,     0,     0,   214,
     363,   215,   216,   217,   218,   219,   220,   221,   222,   223,
       0,     0,   224,     0,    93,   356,   330,     0,   278,   305,
       0,     0,     0,     0,   368,    93,   373,     0,     0,   278,
     217,   218,   219,   220,   221,   222,   223,     0,     0,   224,
       0,     0,     0,     0,     0,   375,     0,   133,   133,   133,
     133,   133,   133,   133,     0,   133,   133,   133,   133,   133,
     133,   133,   133,   133,   133,   230,   231,   232,   233,   234,
     235,     0,     0,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   338,   339,   340,   341,   342,   343,   344,     0,
     346,   347,   348,   349,   350,   351,   352,   353,   354,   355,
       0,     0,     0,     0,     0,   133,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     372,    24,    25,     0,     0,    26,    27,    28,     0,     0,
       0,     0,    29,   308,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,   149,   150,     0,     1,     0,
       0,   151,   152,     0,   154,     0,     0,     0,    40,     0,
       0,     0,     0,     0,    41,     0,     0,     0,     0,     0,
       0,     0,    42,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,     0,   157,   158,   159,   160,   161,   162,   163,
     164,   165,     0,   357,   166,     0,     0,    24,    25,     0,
       0,    26,    27,    28,     0,     0,     0,     0,    29,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    40,     0,     0,     0,     0,     0,
      41,     0,     0,     0,     0,     0,     0,     0,    42,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    68,    69,
      70,    71,    72,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    73,    68,    69,    70,    71,    72,     0,     0,    74,
      75,     0,     0,     0,     0,   139,    76,    77,     0,     0,
       0,     0,     0,     0,    78,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,     0,     0,    74,    75,     0,     0,     0,     0,     0,
      76,    77,     0,     0,   144,     0,     0,     0,    78,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    68,    69,
      70,    71,    72,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,     0,     0,     0,     0,
       0,    73,     0,     0,     0,     0,     0,     0,     0,    74,
      75,     0,     0,     0,     0,     0,    76,    77,     0,     0,
       0,     0,     0,     0,    78,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,     0,    41,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    32,    33,    34,    35,    36,    37,    38,
      39,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   125,   126,     0,     0,     0,     0,     0,     0,
       0,    41,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,     0,    41,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,     0,   149,   150,     0,     0,     0,     0,   151,
     152,   153,   154,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   207,   208,     0,     0,     0,     0,   209,
     210,   211,   212,     0,     0,     0,     0,     0,   188,    41,
       0,   155,     0,     0,     0,     0,     0,     0,     0,   156,
       0,   157,   158,   159,   160,   161,   162,   163,   164,   165,
      41,   213,   166,     0,     0,     0,     0,     0,     0,   214,
       0,   215,   216,   217,   218,   219,   220,   221,   222,   223,
     149,   150,   224,     0,     0,     0,   151,   152,   153,   154,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   207,
     208,     0,     0,     0,     0,   209,   210,   211,   212,     0,
       0,     0,     0,   207,   208,     0,     0,     0,   155,   209,
     210,     0,   212,     0,     0,     0,     0,     0,   157,   158,
     159,   160,   161,   162,   163,   164,   165,   213,     0,   166,
       0,     0,     0,     0,     0,     0,     0,   215,   216,   217,
     218,   219,   220,   221,   222,   223,     0,     0,   224,     0,
       0,   215,   216,   217,   218,   219,   220,   221,   222,   223,
     207,   208,   224,     0,     0,     0,   209,   210,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   215,   216,
     217,   218,   219,   220,   221,   222,   223,     0,     0,   224
  };

  const short int
  parser::yycheck_[] =
  {
       3,     3,    43,    26,     3,   178,    29,   104,    43,   180,
       3,    64,   286,    30,    31,     4,    24,    25,    26,    27,
      69,    39,    40,    26,    46,   196,    29,    26,    65,   126,
      29,    83,     0,    70,    86,    79,    85,    40,    41,    42,
      43,    43,    41,    87,    43,    77,    78,    80,    51,    81,
      83,    54,    80,    24,    25,    54,    27,    79,    86,    87,
      64,    97,    66,    43,    42,    72,    73,    74,    75,    76,
      77,    78,    83,    74,    81,    86,    65,    76,    86,    80,
      81,    74,    80,    77,    78,     4,   103,    81,    86,    87,
      64,    94,    95,   367,     3,    88,    95,   100,   101,    64,
     197,    66,   101,    87,   378,    76,    39,    40,   125,   126,
     281,    80,    81,    64,    83,    66,   119,    83,   121,   122,
     119,    69,    65,    71,   127,    65,   167,    98,    40,    64,
      42,    66,   167,   121,   122,    81,    44,   173,    74,    75,
      76,    77,    78,    64,   115,    81,    84,    79,    66,    69,
     323,    79,   155,    69,    79,   178,    86,   128,   129,   130,
     131,   132,   179,    86,   167,   167,    65,   170,   167,    84,
      81,    84,    86,    79,   177,   178,   156,    84,    66,   178,
      51,    86,    86,    73,    74,    75,    86,    79,    78,    69,
      86,    66,   195,    86,    79,   166,   195,    81,    87,    53,
     171,    87,    26,   174,   167,   196,   323,   248,   369,   284,
     213,    77,    -1,   248,    -1,    -1,   252,    -1,    22,    23,
     256,    -1,   193,   194,    28,    29,   206,   207,   208,   209,
     210,   211,   212,    -1,   214,   215,   216,   217,   218,   219,
     220,   221,   222,   223,   280,   248,   248,    -1,    -1,   248,
      -1,    -1,    -1,    -1,   271,   226,    -1,    -1,   148,   262,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,   278,    -1,    81,   314,    -1,
     251,   317,    -1,   286,    -1,    -1,    -1,   286,   259,   260,
      52,    53,    54,    55,    56,    57,    58,    59,    -1,   270,
     323,   337,    -1,    22,    23,    -1,    -1,    -1,   325,    28,
      29,    30,    31,   284,   285,    74,    75,    76,    77,    78,
     323,    -1,    81,    -1,   323,   361,    -1,    -1,   331,   332,
      -1,    -1,    -1,   332,   224,    -1,    -1,    -1,    -1,    -1,
     376,    60,    -1,    -1,   380,   316,    -1,    -1,    -1,    68,
     321,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    -1,   367,    84,   369,    -1,   367,   224,
      -1,    -1,    -1,    -1,   377,   378,   356,    -1,    -1,   378,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      -1,    -1,    -1,    -1,    -1,   366,    -1,   287,   288,   289,
     290,   291,   292,   293,    -1,   295,   296,   297,   298,   299,
     300,   301,   302,   303,   304,   149,   150,   151,   152,   153,
     154,    -1,    -1,   157,   158,   159,   160,   161,   162,   163,
     164,   165,   287,   288,   289,   290,   291,   292,   293,    -1,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
      -1,    -1,    -1,    -1,    -1,   345,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     345,    37,    38,    -1,    -1,    41,    42,    43,    -1,    -1,
      -1,    -1,    48,   237,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    -1,    22,    23,    -1,    64,    -1,
      -1,    28,    29,    -1,    31,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    88,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,   307,    81,    -1,    -1,    37,    38,    -1,
      -1,    41,    42,    43,    -1,    -1,    -1,    -1,    48,    -1,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,
      80,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    88,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    32,    33,
      34,    35,    36,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    65,    32,    33,    34,    35,    36,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    79,    80,    81,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
      80,    81,    -1,    -1,    84,    -1,    -1,    -1,    88,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    32,    33,
      34,    35,    36,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    -1,    -1,    -1,    -1,
      -1,    65,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    -1,    80,    81,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    51,    52,    53,    54,    55,
      56,    57,    58,    59,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    -1,    80,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    58,
      59,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    80,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    -1,    80,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    -1,    22,    23,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    22,    23,    -1,    -1,    -1,    -1,    28,
      29,    30,    31,    -1,    -1,    -1,    -1,    -1,    79,    80,
      -1,    60,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      80,    60,    81,    -1,    -1,    -1,    -1,    -1,    -1,    68,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      22,    23,    81,    -1,    -1,    -1,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    22,
      23,    -1,    -1,    -1,    -1,    28,    29,    30,    31,    -1,
      -1,    -1,    -1,    22,    23,    -1,    -1,    -1,    60,    28,
      29,    -1,    31,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    60,    -1,    81,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      22,    23,    81,    -1,    -1,    -1,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81
  };

  const unsigned char
  parser::yystos_[] =
  {
       0,    64,    90,    92,     0,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    37,    38,    41,    42,    43,    48,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      74,    80,    88,    91,    93,    94,    95,    96,   100,   101,
     102,   103,   104,   105,   106,   107,   108,   109,   112,   118,
     123,   124,   125,   128,   129,   130,   131,   133,    32,    33,
      34,    35,    36,    65,    73,    74,    80,    81,    88,    95,
      97,    98,    99,   103,   124,   132,   134,   135,   136,   140,
     141,   142,   141,    95,   103,   106,   107,   141,    42,    95,
     103,   106,    96,    65,    96,   117,    95,   133,   103,   133,
      92,    93,    94,    64,    80,    81,    83,   135,    74,    80,
      81,    69,    85,    95,   104,    50,    51,   103,    24,    25,
      26,    27,    86,   134,   143,   144,   145,   134,   134,    79,
     103,   138,   139,   141,    84,   138,   141,   134,    87,    22,
      23,    28,    29,    30,    31,    60,    68,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    81,    65,   119,    95,
     103,    44,   119,   141,    86,    95,    95,   103,    65,    70,
     111,   115,    96,   116,   117,    79,    64,   141,    79,   103,
      84,   101,   101,    80,    86,    87,    96,    96,   117,    95,
     141,   141,   141,   141,   141,    66,    69,    22,    23,    28,
      29,    30,    31,    60,    68,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    81,    79,    69,    79,    84,   134,
     142,   142,   142,   142,   142,   142,    95,    92,   142,   142,
     142,   142,   142,   142,   142,   142,   142,   141,    91,    93,
      94,    39,    40,   126,   127,    95,   141,   119,   141,    86,
      86,    95,   103,   106,   113,   114,   118,    96,   110,   115,
      86,    64,    66,    84,    79,   137,   141,   141,   103,   121,
     122,   111,   115,   117,    80,    86,    87,    92,    92,    92,
      92,    92,    92,    92,    95,    92,    92,    92,    92,    92,
      92,    92,    92,    92,    92,   145,   141,    86,   142,    84,
      93,    94,    66,    66,   141,   119,    39,    40,   119,   141,
     141,    86,    95,    64,    66,    69,    71,   141,    96,    79,
      95,   120,    69,   119,   115,   137,   141,   122,   145,   145,
     145,   145,   145,   145,   145,    86,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,    84,   142,    66,    66,
     119,   141,   119,   141,   113,    96,    86,    87,    95,   103,
      79,   119,   145,    92,   119,   141,   122,   120,    87,   119,
     122,   119
  };

  const unsigned char
  parser::yyr1_[] =
  {
       0,    89,    90,    91,    91,    91,    91,    92,    92,    93,
      93,    93,    93,    93,    93,    93,    94,    94,    94,    94,
      94,    95,    96,    97,    98,    99,   100,   100,   100,   100,
     100,   100,   100,   100,   100,   100,   100,   100,   100,   100,
     100,   100,   100,   100,   100,   101,   101,   101,   101,   101,
     101,   102,   102,   102,   103,   104,   104,   104,   104,   104,
     104,   104,   104,   105,   105,   106,   107,   107,   107,   107,
     108,   108,   108,   108,   109,   109,   109,   109,   109,   110,
     110,   111,   112,   112,   112,   112,   113,   113,   113,   114,
     114,   115,   116,   116,   116,   116,   117,   118,   118,   118,
     118,   119,   119,   119,   119,   120,   120,   121,   121,   122,
     122,   123,   123,   123,   123,   124,   125,   126,   126,   127,
     127,   127,   127,   128,   129,   130,   131,   132,   133,   133,
     133,   133,   134,   134,   134,   134,   134,   134,   134,   134,
     134,   134,   134,   134,   135,   135,   136,   136,   137,   137,
     138,   139,   139,   140,   140,   140,   140,   141,   142,   142,
     142,   142,   142,   142,   142,   142,   142,   142,   142,   142,
     142,   142,   142,   142,   142,   142,   142,   143,   144,   144,
     145,   145,   145,   145,   145,   145,   145,   145,   145,   145,
     145,   145,   145,   145,   145,   145,   145,   145,   145
  };

  const unsigned char
  parser::yyr2_[] =
  {
       0,     2,     3,     3,     2,     2,     1,     1,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     2,     3,     4,     3,     3,
       1,     3,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     5,     3,     4,     2,
       6,     5,     5,     4,     3,     3,     3,     3,     3,     3,
       1,     3,     4,     5,     3,     4,     2,     1,     1,     3,
       1,     3,     3,     5,     3,     1,     3,     4,     3,     3,
       2,     4,     4,     3,     3,     2,     1,     4,     2,     1,
       0,     6,     9,     5,     8,     2,     2,     4,     3,     3,
       1,     2,     0,     4,     3,     4,     5,     1,     2,     2,
       4,     1,     1,     3,     1,     1,     3,     1,     1,     1,
       1,     1,     1,     1,     3,     2,     3,     2,     1,     0,
       1,     3,     1,     2,     2,     2,     3,     1,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     4,     4,     5,
       3,     3,     3,     3,     3,     3,     1,     1,     4,     1,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       5,     5,     4,     4,     4,     4,     4,     4,     1
  };



  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "$end", "error", "$undefined", "Ident", "UserType", "I8", "I16", "I32",
  "I64", "U8", "U16", "U32", "U64", "Isz", "Usz", "F16", "F32", "F64",
  "C8", "C32", "Bool", "Void", "Eq", "NotEq", "AddEq", "SubEq", "MulEq",
  "DivEq", "GrtrEq", "LesrEq", "Or", "And", "True", "False", "IntLit",
  "FltLit", "StrLit", "Return", "If", "Elif", "Else", "For", "While", "Do",
  "In", "Continue", "Break", "Import", "Let", "Match", "Data", "Enum",
  "Pub", "Pri", "Pro", "Raw", "Const", "Ext", "Noinit", "Pathogen",
  "Where", "Infect", "Cleanse", "Ct", "Newline", "Indent", "Unindent",
  "LOW", "';'", "','", "'<'", "'>'", "'+'", "'-'", "'*'", "'/'", "'%'",
  "'^'", "'.'", "')'", "'('", "'['", "HIGH", "'\\''", "']'", "'|'", "'='",
  "':'", "'&'", "$accept", "top_level_stmt_list", "stmt_list",
  "maybe_newline", "no_nl_stmt", "nl_stmt", "ident", "usertype", "intlit",
  "fltlit", "strlit", "lit_type", "type", "type_expr_", "type_expr",
  "modifier", "modifier_list_", "modifier_list", "var_decl", "let_binding",
  "var_assign", "usertype_list", "generic", "data_decl", "type_decl",
  "type_decl_list", "type_decl_block", "val_init_list", "enum_block",
  "enum_decl", "block", "ident_list", "params", "maybe_params", "fn_decl",
  "fn_call", "ret_stmt", "elif_list", "maybe_elif_list", "if_stmt",
  "while_loop", "do_while_loop", "for_loop", "var", "ref_val", "val",
  "tuple", "array", "maybe_expr", "expr_list", "expr_list_p", "unary_op",
  "expr", "binop", "nl_expr", "nl_expr_list", "expr_block_p", YY_NULLPTR
  };

#if YYDEBUG
  const unsigned short int
  parser::yyrline_[] =
  {
       0,   111,   111,   114,   115,   116,   117,   120,   121,   128,
     129,   130,   131,   132,   133,   134,   138,   139,   140,   141,
     142,   145,   148,   151,   154,   157,   160,   161,   162,   163,
     164,   165,   166,   167,   168,   169,   170,   171,   172,   173,
     174,   175,   176,   177,   178,   183,   184,   185,   186,   187,
     188,   191,   192,   193,   196,   205,   206,   207,   208,   209,
     210,   211,   212,   215,   216,   219,   223,   224,   225,   226,
     229,   230,   231,   232,   236,   237,   238,   239,   240,   243,
     244,   247,   250,   251,   252,   253,   256,   257,   258,   261,
     262,   265,   269,   270,   271,   272,   275,   278,   279,   280,
     281,   284,   285,   286,   287,   290,   291,   299,   300,   303,
     304,   307,   308,   309,   310,   313,   316,   319,   320,   323,
     324,   325,   326,   329,   332,   335,   338,   341,   344,   345,
     346,   347,   350,   351,   352,   353,   354,   355,   356,   357,
     358,   359,   360,   361,   364,   365,   368,   369,   372,   373,
     376,   379,   380,   385,   386,   387,   388,   391,   394,   395,
     396,   397,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   417,   420,   421,
     424,   425,   426,   427,   428,   429,   430,   431,   432,   433,
     434,   435,   436,   437,   438,   439,   440,   441,   442
  };

  // Print the state stack on the debug stream.
  void
  parser::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << std::endl;
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  parser::yy_reduce_print_ (int yyrule)
  {
    unsigned int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):" << std::endl;
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  // Symbol number corresponding to token number t.
  inline
  parser::token_number_type
  parser::yytranslate_ (int t)
  {
    static
    const token_number_type
    translate_table[] =
    {
     0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    76,    88,    83,
      80,    79,    74,    72,    69,    73,    78,    75,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    87,    68,
      70,    86,    71,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    81,     2,    84,    77,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    85,     2,     2,     2,     2,     2,
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
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    82
    };
    const unsigned int user_token_number_max_ = 323;
    const token_number_type undef_token_ = 2;

    if (static_cast<int>(t) <= yyeof_)
      return yyeof_;
    else if (static_cast<unsigned int> (t) <= user_token_number_max_)
      return translate_table[t];
    else
      return undef_token_;
  }


} // yy
#line 2511 "src/parser.cpp" // lalr1.cc:1167
#line 444 "src/syntax.y" // lalr1.cc:1168


/* location parser error
void yy::parser::error(const location& loc, const string& msg){
    ante::error(msg.c_str(), yylexer->fileName, yylexer->getRow(), yylexer->getCol());
} */

void yy::parser::error(const string& msg){
    ante::error(msg.c_str(), yylexer->fileName, yylexer->getRow(), yylexer->getCol());
}

#endif
