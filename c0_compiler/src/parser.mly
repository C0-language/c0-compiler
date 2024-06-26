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


%start <Ast.prog> prog
%%

// root node of the AST: a program
prog:
    | tyds SEMICOLON vads SEMICOLON fuds EOF { Ast.Prog (Some $1, Some $3, $5) }
    | tyds SEMICOLON fuds EOF { Ast.Prog (Some $1, None, $3) }
    | vads SEMICOLON fuds EOF { Ast.Prog (None, Some $1, $3) }
    | fuds EOF { Ast.Prog (None, None, $1) }


// Types grammar
tyd:
    | TYPEDEF te na { Ast.TyD ($2, $3) }


tyds:
    | tyd { Ast.TyDS_Single ($1) }
    | tyd SEMICOLON tyds { Ast.TyDS_Seq ($1, $3) }


ty:
    | TYPE_INT  { Ast.Ty_Int }
    | TYPE_BOOL { Ast.Ty_Bool }
    | TYPE_CHAR { Ast.Ty_Char }
    | TYPE_UINT { Ast.Ty_UInt }
    | na { Ast.Ty_Na ($1) }


te:
    | ty LBRACKET dis RBRACKET { Ast.TE_Arr ($1, $3) }
    | ty STAR { Ast.TE_Ptr ($1) }
    | STRUCT LBRACE vads RBRACE { Ast.TE_Struct ($3) }


vads:
    | vad { Ast.VaDS_Single ($1) }
    | vad SEMICOLON vads { Ast.VaDS_Seq ($1, $3) }


vad:
    | ty na { Ast.VaD ($1, $2) }


// Functions grammar
fuds:
    | fud { Ast.FuDS_Single ($1) }
    | fud SEMICOLON fuds { Ast.FuDS_Seq ($1, $3) }


fud:
    | ty na LPAREN pads RPAREN LBRACE vads SEMICOLON body RBRACE { Ast.FuD ($1, $2, Some $4, Some $7, $9) }
    | ty na LPAREN pads RPAREN LBRACE body RBRACE { Ast.FuD ($1, $2, Some $4, None, $7) }
    | ty na LPAREN RPAREN LBRACE vads SEMICOLON body RBRACE { Ast.FuD ($1, $2, None, Some $6, $8) }
    | ty na LPAREN RPAREN LBRACE body RBRACE { Ast.FuD ($1, $2, None, None, $6) }


pads:
    | vad { Ast.PaDS_Single ($1)}
    | vad COMMA pads { Ast.PaDS_Seq ($1, $3) }


body:
    | rst { Ast.Body (None, $1)}
    | sts SEMICOLON rst { Ast.Body (Some $1, $3)}


// Statements grammar
st:
    | id ASSIGN e   { Ast.St_EAssign ($1, $3) }
    | id ASSIGN be  { Ast.St_BEAssign ($1, $3) }
    | id ASSIGN CC  { Ast.St_CCAssign ($1, (Ast.CC $3)) }
    | IF be LBRACE sts RBRACE   { Ast.St_If ($2, $4) }
    | IF be LBRACE sts RBRACE ELSE LBRACE sts RBRACE    { Ast.St_IfElse ($2, $4, $8) }
    | WHILE be LBRACE sts RBRACE    { Ast.St_While ($2, $4) }
    | id ASSIGN na LPAREN pas RPAREN    { Ast.St_CallAssign ($1, $3, Some $5) }
    | id ASSIGN na LPAREN RPAREN    { Ast.St_CallAssign ($1, $3, None) }
    | id ASSIGN NEW na STAR  { Ast.St_PtrAssign ($1, $4) }


sts:
    | st { Ast.StS_Single ($1) }
    | st SEMICOLON sts { Ast.StS_Seq ($1, $3) }

rst:
    | RETURN e  { Ast.RSt_E ($2) }
    | RETURN be { Ast.RSt_BE ($2) }
    | RETURN CC { Ast.RSt_CC (Ast.CC $2) }


pa:
    | e     { Ast.Pa_E ($1)}
    | be    { Ast.Pa_BE ($1)}
    | CC    { Ast.Pa_CC (Ast.CC $1)}


pas:
    | pa  { Ast.PaS_Single ($1) }
    | pa COMMA pas  { Ast.PaS_Seq ($1, $3) }



// Boolean expressions grammar
bf:
    | id    { Ast.BF_Id ($1) }
    | atom  { Ast.BF_Atom ($1) }
    | EXCLAMATION bf   { Ast.BF_Not ($2) }
    | LPAREN be RPAREN  { Ast.BF_Paren ($2) }


bt:
    | bf        { Ast.BT_BF ($1) }
    | bt AND bf { Ast.BT_And ($1, $3) }


be:
    | bt         { Ast.BE_BT ($1) }
    | be OR bt   { Ast.BE_Or ($1, $3) }

atom:
    | e GREATER e   { Ast.Atom_Greater ($1, $3) }
    | e GREATEREQ e { Ast.Atom_GreaterEq ($1, $3) }
    | e LESS e  { Ast.Atom_Less ($1, $3) }
    | e LESSEQ e    { Ast.Atom_LessEq ($1, $3)}
    | e EQ e    { Ast.Atom_Eq ($1, $3) }
    | e NEQ e   { Ast.Atom_NotEq ($1, $3) }
    | BC    { Ast.Atom_BC (Ast.BC $1) }


// Arithmetic expressions grammar
f:
    | id    { Ast.F_Id ($1) }
    | MINUS f   { Ast.F_Neg ($2) }
    | LPAREN e RPAREN   { Ast.F_Paren ($2) }
    | c { Ast.F_C ($1) }


t:
    | f { T_F ($1) }
    | t STAR f  { Ast.T_Mul ($1, $3)}
    | t SLASH f { Ast.T_Div ($1, $3)}


e:
    | t { E_T ($1) }
    | e PLUS t  { Ast.E_Add ($1, $3) }
    | e MINUS t { Ast.E_Sub ($1, $3) }



// Constants grammar
dis:
    | DI    { Ast.DiS_Single (Ast.Di $1) }
    | DI dis    { Ast.DiS_Seq ((Ast.Di $1), $2) }


dile:
    | LE    { Ast.DiLe_Le (Ast.Le $1) }
    | DI    { Ast.DiLe_Di (Ast.Di $1) }


diles:
    | dile  { Ast.DiLeS_Single ($1) }
    | dile diles  { Ast.DiLeS_Seq ($1, $2) }


na:
    | LE    { Ast.Na_Single (Ast.Le $1) }
    | LE diles  { Ast.Na_Seq ((Ast.Le $1), $2) }


c:
    | dis { Ast.C_DiS ($1) }
    | dis LE  {
        if $2 = 'u' then Ast.C_DiSu ($1)
        else failwith "Invalid character for unsigned integer"
    }
    | NULL  { Ast.C_Null }


id:
    | na    { Ast.Id_Na ($1) }
    | id DOT na  { Ast.Id_DotNa ($1, $3)}
    | id LBRACKET e RBRACKET    { Ast.Id_Brac ($1, $3) }
    | id STAR   { Ast.Id_Deref ($1) }
    | id AMPERSAND  { Ast.Id_AddrOf ($1) }

%%
