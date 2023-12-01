type error =
  | UnexpectedToken of Lexer.token
  | UnexpectedEOF

exception Error of error CodeMap.Span.located

val print_error : error -> out_channel -> unit

val parse_command : Lexer.t -> Ast.command CodeMap.Span.located
val parse_condition : Lexer.t -> Ast.condition CodeMap.Span.located
val parse_direction : Lexer.t -> Ast.direction CodeMap.Span.located
val parse_expression : Lexer.t -> Ast.expression CodeMap.Span.located
val parse_lr : Lexer.t -> Ast.lr CodeMap.Span.located
val parse_program : Lexer.t -> Ast.program CodeMap.Span.located
val parse_valeur : Lexer.t -> Ast.valeur CodeMap.Span.located

