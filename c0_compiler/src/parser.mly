%{
  open Ast
%}

// Lexer Tokens:

/* 
    Terminal symbols of digit, letter, ASCII char, and boolean (true/false).
    They are instantiated in Lexer and passed to the parser with containing 
    values to build AST node of the corresponding type.
*/
%token <int> DI
%token <char> LE
%token <char> CC
%token <bool> BC

// Keyword, operator, and punctuation tokens

// null keyword
%token NULL

// Types
%token TYPE_UINT TYPE_INT TYPE_BOOL TYPE_CHAR

// Keywords
%token IF ELSE WHILE RETURN STRUCT TYPEDEF NEW

// Operators
%token PLUS MINUS STAR SLASH AMPERSAND EXCLAMATION
%token ASSIGN LESS LESSEQ GREATER GREATEREQ EQ NEQ AND OR

// Punctuation
%token SEMICOLON COMMA DOT

// Parentheses and brackets
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EOF


%start <prog> prog
%%

// root node of the AST: a program
prog:
    | tyds SEMICOLON vads SEMICOLON fuds EOF { Prog (Some $1, Some $3, $5) }
    | tyds SEMICOLON fuds EOF { Prog (Some $1, None, $3) }
    | vads SEMICOLON fuds EOF { Prog (None, Some $1, $3) }
    | fuds EOF { Prog (None, None, $1) }
;

// Types grammar
tyd:
    | TYPEDEF te na { TyD ($2, $3) }
;

tyds:
    | tyd { TyDS_Single ($1) }
    | tyd SEMICOLON tyds { TyDS_Seq ($1, $3) }
;

ty:
    | TYPE_INT  { Ty_Int }
    | TYPE_BOOL { Ty_Bool }
    | TYPE_CHAR { Ty_Char }
    | TYPE_UINT { Ty_UInt }
    | na { Ty_Na ($1) }
;

te:
    | ty LBRACKET dis RBRACKET { TE_Arr ($1, $3) }
    | ty STAR { TE_Ptr ($1) }
    | STRUCT LBRACE vads RBRACE { TE_Struct ($3) }
;

vads:
    | vad { VaDS_Single ($1) }
    | vad SEMICOLON vads { VaDS_Seq ($1, $3) }
;

vad:
    | ty na { VaD ($1, $2) }
;

// Functions grammar
fuds:
    | fud { FuDS_Single ($1) }
    | fud SEMICOLON fuds { FuDS_Seq ($1, $3) }
;

fud:
    | ty na LPAREN pads RPAREN LBRACE vads SEMICOLON body RBRACE { FuD ($1, $2, $4, $7, $9) }
    | ty na LPAREN pads RPAREN LBRACE body RBRACE { FuD ($1, $2, Some $4, None, $7) }
    | ty na LPAREN RPAREN LBRACE vads SEMICOLON body RBRACE { FuD ($1, $2, None, Some $6, $8) }
    | ty na LPAREN RPAREN LBRACE body RBRACE { FuD ($1, $2, None, None, $6) }
;

pads:
    | vad { PaDS_Single ($1)}
    | vad COMMA pads { PaDS_Seq ($1, $3) }
;

body:
    | rst { Body (None, $1)}
    | sts SEMICOLON rst { Body (Some $1, $3)}
;

// Statements grammar
st:
    | id ASSIGN e   { St_EAssign ($1, $3) }
    | id ASSIGN be  { St_BEAssign ($1, $3) }
    | id ASSIGN CC  { St_CCAssign ($1, $3) }
    | IF be LBRACE sts RBRACE   { St_If ($2, $4) }
    | IF be LBRACE sts RBRACE ELSE LBRACE sts RBRACE    { St_IfElse ($2, $4, $8) }
    | WHILE be LBRACE sts RBRACE    { St_While ($2, $4) }
    | id ASSIGN na LPAREN pas RPAREN    { St_CallAssign ($1, $3, Some $5) }
    | id ASSIGN na LPAREN RPAREN    { St_CallAssign ($1, $3, None) }
    | id ASSIGN NEW na STAR  { St_PtrAssign ($1, $4) }
;

sts:
    | st { StS_Single ($1) }
    | st SEMICOLON sts { StS_Seq ($1, $3) }
;


rst:
    | RETURN e  { RSt_E ($2) }
    | RETURN be { RSt_BE ($2) }
    | RETURN CC { RSt_CC ($2) }
;

pa:
    | e     { Pa_E ($1)}
    | be    { Pa_BE ($1)}
    | CC    { Pa_CC ($1)}
;

pas:
    | pa  { PaS_Single ($1) }
    | pa COMMA pas  { PaS_Seq ($1, $3) }
;


// Boolean expressions grammar
bf:
    | id    { BF_Id ($1) }
    | atom  { BF_Atom ($1) }
    | EXCLAMATION bf   { BF_Not ($2) }
    | LPAREN be RPAREN  { BF_Paren ($2) }
;

bt:
    | bf        { BT_BF ($1) }
    | bt AND bf { BT_And ($1, $3) }
;

be:
    | bt         { BE_BT ($1) }
    | be OR bt   { BE_Or ($1, $3) }

atom:
    | e GREATER e   { Atom_Greater ($1, $3) }
    | e GREATEREQ e { Atom_GreaterEq ($1, $3) }
    | e LESS e  { Atom_Less ($1, $3) }
    | e LESSEQ e    { Atom_LessEq ($1, $3)}
    | e EQ e    { Atom_Eq ($1, $3) }
    | e NEQ e   { Atom_NotEq ($1, $3) }
    | BC    { Atom_BC ($1) }
;

// Arithmetic expressions grammar
f:
    | id    { F_Id ($1) }
    | MINUS f   { F_Neg ($2) }
    | LPAREN e RPAREN   { F_Paren ($2) }
    | c { F_C ($1) }
;

t:
    | f { T_F ($1) }
    | t STAR f  { T_Mul ($1, $3)}
    | t SLASH f { T_Div ($1, $3)}
;

e:
    | t { E_T ($1) }
    | e PLUS t  { E_Add ($1, $3) }
    | e MINUS t { E_Sub ($1, $3) }
;


// Constants grammar
dis:
    | DI    { DiS_Single ($1) }
    | DI dis    { DiS_Seq ($1, $2) }
;

dile:
    | LE    { DiLe_Le ($1) }
    | DI    { DiLe_Di ($1) }
;

diles:
    | dile  { DiLeS_Single ($1) }
    | dile diles  { DiLeS_Seq ($1, $2) }
;

na:
    | LE    { Na_Single ($1) }
    | LE diles  { Na_Seq ($1, $2) }
;

c:
    | dis { C_DiS ($1) }
    | dis LE  {
        if $2 = 'u' then C_DiSu ($1)
        else raise (Parse_error "Expected 'u' for unsigned integer")
    }
    | NULL  { C_Null }
;

id:
    | na    { Id_Na ($1) }
    | id DOT na  { Id_DotNa ($1, $3)}
    | id LBRACKET e RBRACKET    { Id_Brac ($1, $3) }
    | id STAR   { Id_Deref ($1) }
    | id AMPERSAND  { Id_AddrOf ($1) }
;
%%
