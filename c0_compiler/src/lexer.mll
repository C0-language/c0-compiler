{
    (* Use Tokens defined in the Parser *)
    open Parser
    
    (* Exception for lexer errors *)
    exception LexerError of string

    let lex_error msg lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    let err = Printf.sprintf "Error at line %d, position %d: %s"
        pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) msg in 
        raise (LexerError err)

    (* Define comment handlers *)
(* 
    (* Single-line comment, ends at the end of the line *)
    let rec singleline_comment lexbuf =
        match%ocamllex lexbuf with
        | '\n' -> tokenize lexbuf (* End of line terminates the comment *)
        | _ -> singleline_comment lexbuf

    (* Multi-line comment, ends with "*/" *)
    let rec multiline_comment lexbuf =
        match%ocamllex lexbuf with
        | "*/" -> tokenize lexbuf (* End of comment *)
        | eof -> failwith "Unterminated multiline comment"
        | _ -> multiline_comment lexbuf *)
}

(* Define the tokens of the language *)
rule tokenize = parse
    (* skip whitespace *)
    | [' ' '\t' '\r' '\n']+ { tokenize lexbuf }

    (* Add comments pattern *)
    (* | "//" { singleline_comment lexbuf; tokenize lexbuf } (* Single-line comment *)
    | "/*" { multiline_comment lexbuf; tokenize lexbuf } Multi-line comment *)

    (* Digit Constant *)
    | ['0'-'9'] as digit { DI (int_of_char digit) }
    
    (* Letter Constant *)
    | ['a'-'z' 'A'-'Z' '_'] as letter { LE letter }

    (* Character constants (printable ASCII and some escape sequences) *)
    | "'" ( [' ' '!'-'~'] | '\\' ['\\' ''' 'n' 't' 'r'] ) "'"
    { 
        let c = Lexing.lexeme lexbuf in
            match c with
                | "'\\n'" -> CC '\n'
                | "'\\t'" -> CC '\t'
                | "'\\r'" -> CC '\r'
                | "'\\''" -> CC '\''
                | "'\\\\'" -> CC '\\'
                | _ -> CC (Lexing.lexeme_char lexbuf 1) 
    }

    (* null pointer reserved word *)
    | "null" { NULL }

    (* Type keywords *)
    | "uint" { TYPE_UINT }
    | "int"  { TYPE_INT }
    | "bool" { TYPE_BOOL }
    | "char" { TYPE_CHAR }

    (* Boolean constants *)
    | "true" { BC true }
    | "false" { BC false }

    (* Structure keywords *)
    | "if"   { IF }
    | "else" { ELSE }
    | "while" { WHILE }
    | "return" { RETURN }
    | "struct" { STRUCT }
    | "typedef" { TYPEDEF }
    | "new" { NEW }

    (* Operator & Punctuation symbols *)
    | "<=" { LESSEQ }
    | ">=" { GREATEREQ }
    | "==" { EQ }
    | "!=" { NEQ }
    | "&&" { AND }
    | "||" { OR }


    | '+' { PLUS }
    | '-' { MINUS }
    | '*' { STAR }
    | '/' { SLASH }

    | '&' { AMPERSAND }
    | '!' { EXCLAMATION }

    | '=' { ASSIGN }
    | '<' { LESS }
    | '>' { GREATER }

    | ';' { SEMICOLON }
    | ',' { COMMA }
    | '.' { DOT }

    | '(' { LPAREN }
    | ')' { RPAREN }
    | '{' { LBRACE }
    | '}' { RBRACE }
    | '[' { LBRACKET }
    | ']' { RBRACKET }
        
    (* End of file *)
    | eof { EOF }
    | _ as c { raise (LexerError (Printf.sprintf "Unexpected character: %c" c)) }
    
