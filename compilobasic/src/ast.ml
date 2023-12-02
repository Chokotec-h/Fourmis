open CodeMap

type command = 
  | Nope
  | Move
  | MoveUp
  | MoveDown
  | Dig
  | Fill
  | DigUp
  | DigDown
  | FillUp
  | FillDown
  | Grab
  | Attack
  | Turn of (lr Span.located)
  | Pickup
  | Mark of (int Span.located)
  | Unmark of (int Span.located)
  | Drop

and condition = 
  | Eq of (direction Span.located) * (valeur Span.located)
  | Neq of (direction Span.located) * (valeur Span.located)
  | Random of (int Span.located)
  | True

and direction = 
  | Here
  | LeftAhead
  | RightAhead
  | Ahead
  | Above
  | Below

and expression = 
  | Do of (command Span.located)
  | MoveElse of (expression Span.located list Span.located)
  | PickElse of (expression Span.located list Span.located)
  | While of (condition Span.located) * (expression Span.located list Span.located)
  | If of (condition Span.located) * (expression Span.located list Span.located) * (expression Span.located list Span.located)

and lr = 
  | TurnL
  | TurnR

and program = 
  | Program of (expression Span.located list Span.located)

and valeur = 
  | Friend
  | Enemy
  | Grabbed
  | FriendWithFood
  | EnemyWithFood
  | Food
  | Rock
  | Empty
  | Underground
  | Surface
  | HoleAbove
  | HoleBelow
  | Marker of (int Span.located)
  | EnemyMarker
  | Home
  | EnemyHome

let rec print__indent n out =
  if n <= 0 then () else begin
    Printf.fprintf out "  ";
    print__indent (n-1) out
  end

and print_nt_command indent t out = 
  match t with
  | Nope -> Printf.fprintf out "nop " 
  | Move -> Printf.fprintf out "move " 
  | MoveUp -> Printf.fprintf out "moveup " 
  | MoveDown -> Printf.fprintf out "movedown " 
  | Dig -> Printf.fprintf out "dig " 
  | Fill -> Printf.fprintf out "fill " 
  | DigUp -> Printf.fprintf out "digup " 
  | DigDown -> Printf.fprintf out "digdown " 
  | FillUp -> Printf.fprintf out "fillup " 
  | FillDown -> Printf.fprintf out "filldown " 
  | Grab -> Printf.fprintf out "grab " 
  | Attack -> Printf.fprintf out "attack " 
  | Turn ((arg0, _)) -> Printf.fprintf out "turn %t" (print_nt_lr indent arg0) 
  | Pickup -> Printf.fprintf out "pickup " 
  | Mark ((arg0, _)) -> Printf.fprintf out "mark %t" (print_int indent arg0) 
  | Unmark ((arg0, _)) -> Printf.fprintf out "unmark %t" (print_int indent arg0) 
  | Drop -> Printf.fprintf out "drop " 

and print_nt_expression indent t out = 
  match t with
  | Do ((arg0, _)) -> Printf.fprintf out "%t" (print_nt_command indent arg0) 
  | MoveElse ((arg0, _)) -> Printf.fprintf out "moveelse { %t} " (non_empty_iter_print_nt_expression (Lexer.Operator ";") indent arg0) 
  | PickElse ((arg0, _)) -> Printf.fprintf out "pickelse { %t} " (non_empty_iter_print_nt_expression (Lexer.Operator ";") indent arg0) 
  | While ((arg0, _), (arg1, _)) -> Printf.fprintf out "while ( %t) do { %t} " (print_nt_condition indent arg0) (non_empty_iter_print_nt_expression (Lexer.Operator ";") indent arg1) 
  | If ((arg0, _), (arg1, _), (arg2, _)) -> Printf.fprintf out "if ( %t) then { %t} else { %t} " (print_nt_condition indent arg0) (non_empty_iter_print_nt_expression (Lexer.Operator ";") indent arg1) (non_empty_iter_print_nt_expression (Lexer.Operator ";") indent arg2) 

and print_nt_condition indent t out = 
  match t with
  | Eq ((arg0, _), (arg1, _)) -> Printf.fprintf out "= %t%t" (print_nt_direction indent arg0) (print_nt_valeur indent arg1) 
  | Neq ((arg0, _), (arg1, _)) -> Printf.fprintf out "!= %t%t" (print_nt_direction indent arg0) (print_nt_valeur indent arg1) 
  | Random ((arg0, _)) -> Printf.fprintf out "random %t" (print_int indent arg0) 
  | True -> Printf.fprintf out "true " 

and print_nt_lr _ t out = 
  match t with
  | TurnL -> Printf.fprintf out "left " 
  | TurnR -> Printf.fprintf out "right " 

and print_nt_valeur indent t out = 
  match t with
  | Friend -> Printf.fprintf out "friend " 
  | Enemy -> Printf.fprintf out "enemy " 
  | Grabbed -> Printf.fprintf out "grabbed " 
  | FriendWithFood -> Printf.fprintf out "friendwithfood " 
  | EnemyWithFood -> Printf.fprintf out "enemywithfood " 
  | Food -> Printf.fprintf out "food " 
  | Rock -> Printf.fprintf out "rock " 
  | Empty -> Printf.fprintf out "empty " 
  | Underground -> Printf.fprintf out "underground " 
  | Surface -> Printf.fprintf out "surface " 
  | HoleAbove -> Printf.fprintf out "holeabove " 
  | HoleBelow -> Printf.fprintf out "holebelow " 
  | Marker ((arg0, _)) -> Printf.fprintf out "marker %t" (print_int indent arg0) 
  | EnemyMarker -> Printf.fprintf out "enemymarker " 
  | Home -> Printf.fprintf out "home " 
  | EnemyHome -> Printf.fprintf out "enemyhome " 

and print_nt_direction _ t out = 
  match t with
  | Here -> Printf.fprintf out "here " 
  | LeftAhead -> Printf.fprintf out "leftahead " 
  | RightAhead -> Printf.fprintf out "rightahead " 
  | Ahead -> Printf.fprintf out "ahead " 
  | Above -> Printf.fprintf out "above " 
  | Below -> Printf.fprintf out "below " 

and print_nt_program indent t out = 
  match t with
  | Program ((arg0, _)) -> Printf.fprintf out "%t" (non_empty_iter_print_nt_expression (Lexer.Operator ";") indent arg0) 

and print_int _ t out =
  Printf.fprintf out "%d " t

and non_empty_iter_print_nt_expression sep indent l out =
  Printf.fprintf out "
";
  let rec print l out =
    match l with
    | [] -> ()
    | (e, _)::[] ->
      print__indent (indent + 1) out;
      print_nt_expression (indent + 1) e out
    | (e, _)::l ->
      print__indent (indent + 1) out;
      Printf.fprintf out "%t%t\n" (print_nt_expression (indent + 1) e) (Lexer.print_token sep);
      print l out
  in
  print l out;
  Printf.fprintf out "
";
  print__indent indent out

let print_command = print_nt_command 0

let print_condition = print_nt_condition 0

let print_direction = print_nt_direction 0

let print_expression = print_nt_expression 0

let print_lr = print_nt_lr 0

let print_program = print_nt_program 0

let print_valeur = print_nt_valeur 0


