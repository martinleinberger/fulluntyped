// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | RCURLY
  | LCURLY
  | RPARENS
  | LPARENS
  | EQ
  | COMMA
  | DOT
  | LAMBDA
  | IN
  | LET
  | ELSE
  | THEN
  | IF
  | STRINGV of (string)
  | FALSE
  | TRUE
  | IDENTIFIER of (string)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_RCURLY
    | TOKEN_LCURLY
    | TOKEN_RPARENS
    | TOKEN_LPARENS
    | TOKEN_EQ
    | TOKEN_COMMA
    | TOKEN_DOT
    | TOKEN_LAMBDA
    | TOKEN_IN
    | TOKEN_LET
    | TOKEN_ELSE
    | TOKEN_THEN
    | TOKEN_IF
    | TOKEN_STRINGV
    | TOKEN_FALSE
    | TOKEN_TRUE
    | TOKEN_IDENTIFIER
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_File
    | NONTERM_Term
    | NONTERM_Constant
    | NONTERM_RecordFields
    | NONTERM_Field
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ( AST.Toplevel ) 
