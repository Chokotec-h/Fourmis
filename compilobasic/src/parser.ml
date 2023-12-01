open CodeMap

type error =
  | UnexpectedToken of Lexer.token
  | UnexpectedEOF

exception Error of error CodeMap.Span.located

let print_error e fmt =
  match e with
  | UnexpectedToken token -> Printf.fprintf fmt "unexpected %t" (Lexer.print_token_debug token)
  | UnexpectedEOF -> Printf.fprintf fmt "unexpected end of file"

let rec consume span lexer =
  match lexer () with
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))
  | Seq.Cons ((token, span'), lexer') ->
    (token, span'), (Span.union span span'), lexer'

and parse_nt_command span lexer = 
  match lexer () with 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Attack, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Attack, _), span, lexer -> 
        (Ast.Attack, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Dig, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Dig, _), span, lexer -> 
        (Ast.Dig, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Digdown, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Digdown, _), span, lexer -> 
        (Ast.DigDown, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Digup, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Digup, _), span, lexer -> 
        (Ast.DigUp, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Drop, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Drop, _), span, lexer -> 
        (Ast.Drop, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Fill, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Fill, _), span, lexer -> 
        (Ast.Fill, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Filldown, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Filldown, _), span, lexer -> 
        (Ast.FillDown, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Fillup, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Fillup, _), span, lexer -> 
        (Ast.FillUp, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Grab, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Grab, _), span, lexer -> 
        (Ast.Grab, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Mark, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Mark, _), span, lexer -> 
        let arg0, lexer = parse_int (Span.next span) lexer in 
        let span = Span.union span (snd arg0) in 
        (Ast.Mark (arg0), span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Move, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Move, _), span, lexer -> 
        (Ast.Move, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Movedown, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Movedown, _), span, lexer -> 
        (Ast.MoveDown, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Moveup, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Moveup, _), span, lexer -> 
        (Ast.MoveUp, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Nop, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Nop, _), span, lexer -> 
        (Ast.Nope, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Pickup, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Pickup, _), span, lexer -> 
        (Ast.Pickup, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Turn, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Turn, _), span, lexer -> 
        let arg0, lexer = parse_nt_lr (Span.next span) lexer in 
        let span = Span.union span (snd arg0) in 
        (Ast.Turn (arg0), span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Unmark, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Unmark, _), span, lexer -> 
        let arg0, lexer = parse_int (Span.next span) lexer in 
        let span = Span.union span (snd arg0) in 
        (Ast.Unmark (arg0), span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end
  | Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))

and parse_nt_expression span lexer = 
  match lexer () with 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Unmark, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Turn, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Pickup, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Nop, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Moveup, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Movedown, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Move, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Mark, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Grab, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Fillup, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Filldown, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Fill, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Drop, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Digup, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Digdown, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Dig, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Attack, _), _)  -> 
    let arg0, lexer = parse_nt_command (Span.next span) lexer in 
    let span = Span.union span (snd arg0) in 
    (Ast.Do (arg0), span), lexer

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Moveelse, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Moveelse, _), span, lexer -> 
        begin match consume span lexer with 
          | (Lexer.Begin '{', _), span, lexer -> 
            let arg0, lexer = non_empty_iter_parse_nt_expression (Lexer.Operator ";") (Span.next span) lexer in 
            let span = Span.union span (snd arg0) in 
            begin match consume span lexer with 
              | (Lexer.End '}', _), span, lexer -> 
                (Ast.MoveElse (arg0), span), lexer
              | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
            end
          | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
        end
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Pickelse, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Pickelse, _), span, lexer -> 
        begin match consume span lexer with 
          | (Lexer.Begin '{', _), span, lexer -> 
            let arg0, lexer = non_empty_iter_parse_nt_expression (Lexer.Operator ";") (Span.next span) lexer in 
            let span = Span.union span (snd arg0) in 
            begin match consume span lexer with 
              | (Lexer.End '}', _), span, lexer -> 
                (Ast.PickElse (arg0), span), lexer
              | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
            end
          | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
        end
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.While, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.While, _), span, lexer -> 
        begin match consume span lexer with 
          | (Lexer.Begin '(', _), span, lexer -> 
            let arg0, lexer = parse_nt_condition (Span.next span) lexer in 
            let span = Span.union span (snd arg0) in 
            begin match consume span lexer with 
              | (Lexer.End ')', _), span, lexer -> 
                begin match consume span lexer with 
                  | (Lexer.Keyword Lexer.Keyword.Do, _), span, lexer -> 
                    begin match consume span lexer with 
                      | (Lexer.Begin '{', _), span, lexer -> 
                        let arg1, lexer = non_empty_iter_parse_nt_expression (Lexer.Operator ";") (Span.next span) lexer in 
                        let span = Span.union span (snd arg1) in 
                        begin match consume span lexer with 
                          | (Lexer.End '}', _), span, lexer -> 
                            (Ast.While (arg0, arg1), span), lexer
                          | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
                        end
                      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
                    end
                  | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
                end
              | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
            end
          | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
        end
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end
  | Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))

and parse_nt_condition span lexer = 
  match lexer () with 
  | Seq.Cons ((Lexer.Operator "=", _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Operator "=", _), span, lexer -> 
        let arg0, lexer = parse_nt_direction (Span.next span) lexer in 
        let span = Span.union span (snd arg0) in 
        let arg1, lexer = parse_nt_valeur (Span.next span) lexer in 
        let span = Span.union span (snd arg1) in 
        (Ast.Eq (arg0, arg1), span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Operator "!=", _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Operator "!=", _), span, lexer -> 
        let arg0, lexer = parse_nt_direction (Span.next span) lexer in 
        let span = Span.union span (snd arg0) in 
        let arg1, lexer = parse_nt_valeur (Span.next span) lexer in 
        let span = Span.union span (snd arg1) in 
        (Ast.Neq (arg0, arg1), span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Random, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Random, _), span, lexer -> 
        let arg0, lexer = parse_int (Span.next span) lexer in 
        let span = Span.union span (snd arg0) in 
        (Ast.Random (arg0), span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.True, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.True, _), span, lexer -> 
        (Ast.True, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end
  | Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))

and parse_nt_lr span lexer = 
  match lexer () with 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Left, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Left, _), span, lexer -> 
        (Ast.TurnL, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Right, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Right, _), span, lexer -> 
        (Ast.TurnR, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end
  | Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))

and parse_nt_valeur span lexer = 
  match lexer () with 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Empty, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Empty, _), span, lexer -> 
        (Ast.Empty, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Enemy, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Enemy, _), span, lexer -> 
        (Ast.Enemy, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Enemyhome, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Enemyhome, _), span, lexer -> 
        (Ast.EnemyHome, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Enemymarker, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Enemymarker, _), span, lexer -> 
        (Ast.EnemyMarker, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Enemywithfood, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Enemywithfood, _), span, lexer -> 
        (Ast.EnemyWithFood, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Food, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Food, _), span, lexer -> 
        (Ast.Food, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Friend, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Friend, _), span, lexer -> 
        (Ast.Friend, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Friendwithfood, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Friendwithfood, _), span, lexer -> 
        (Ast.FriendWithFood, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Grabbed, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Grabbed, _), span, lexer -> 
        (Ast.Grabbed, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Holeabove, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Holeabove, _), span, lexer -> 
        (Ast.HoleAbove, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Holebelow, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Holebelow, _), span, lexer -> 
        (Ast.HoleBelow, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Home, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Home, _), span, lexer -> 
        (Ast.Home, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Marker, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Marker, _), span, lexer -> 
        let arg0, lexer = parse_int (Span.next span) lexer in 
        let span = Span.union span (snd arg0) in 
        (Ast.Marker (arg0), span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Rock, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Rock, _), span, lexer -> 
        (Ast.Rock, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Surface, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Surface, _), span, lexer -> 
        (Ast.Surface, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Underground, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Underground, _), span, lexer -> 
        (Ast.Underground, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end
  | Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))

and parse_nt_direction span lexer = 
  match lexer () with 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Above, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Above, _), span, lexer -> 
        (Ast.Above, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Ahead, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Ahead, _), span, lexer -> 
        (Ast.Ahead, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Below, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Below, _), span, lexer -> 
        (Ast.Below, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Here, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Here, _), span, lexer -> 
        (Ast.Here, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Leftahead, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Leftahead, _), span, lexer -> 
        (Ast.LeftAhead, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end

  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Rightahead, _), _)  -> 
    begin match consume span lexer with 
      | (Lexer.Keyword Lexer.Keyword.Rightahead, _), span, lexer -> 
        (Ast.RightAhead, span), lexer
      | (token, token_span), _, _ -> raise (Error (UnexpectedToken token, token_span))
    end
  | Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))

and parse_nt_program span lexer = 
  match lexer () with 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.While, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Unmark, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Turn, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Pickup, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Pickelse, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Nop, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Moveup, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Moveelse, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Movedown, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Move, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Mark, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Grab, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Fillup, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Filldown, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Fill, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Drop, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Digup, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Digdown, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Dig, _), _) 
  | Seq.Cons ((Lexer.Keyword Lexer.Keyword.Attack, _), _)  -> 
    let arg0, lexer = non_empty_iter_parse_nt_expression (Lexer.Operator ";") (Span.next span) lexer in 
    let span = Span.union span (snd arg0) in 
    (Ast.Program (arg0), span), lexer
  | Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))

and parse_int span lexer =
  match lexer () with
  | Seq.Cons ((Int i, token_span), lexer) -> (i, token_span), lexer
  | Seq.Cons ((token, token_span), _) -> raise (Error (UnexpectedToken token, token_span))
  | Seq.Nil -> raise (Error (UnexpectedEOF, span))

and non_empty_iter_parse_nt_expression sep span lexer =
  let item, lexer = parse_nt_expression span lexer in
  match lexer () with
  | Seq.Cons ((token, token_span), lexer) when token = sep ->
    let span = Span.union token_span (snd item) in
    let (items, span'), lexer = non_empty_iter_parse_nt_expression sep (Span.next span) lexer in
    let span = Span.union span span' in
    (item::items, span), lexer
  | _ -> ([item], snd item), lexer

let parse_command lexer =
  let res, _ = parse_nt_command Span.default lexer in
  res

let parse_condition lexer =
  let res, _ = parse_nt_condition Span.default lexer in
  res

let parse_direction lexer =
  let res, _ = parse_nt_direction Span.default lexer in
  res

let parse_expression lexer =
  let res, _ = parse_nt_expression Span.default lexer in
  res

let parse_lr lexer =
  let res, _ = parse_nt_lr Span.default lexer in
  res

let parse_program lexer =
  let res, _ = parse_nt_program Span.default lexer in
  res

let parse_valeur lexer =
  let res, _ = parse_nt_valeur Span.default lexer in
  res


