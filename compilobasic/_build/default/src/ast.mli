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

val print_command : command -> out_channel -> unit
val print_condition : condition -> out_channel -> unit
val print_direction : direction -> out_channel -> unit
val print_expression : expression -> out_channel -> unit
val print_lr : lr -> out_channel -> unit
val print_program : program -> out_channel -> unit
val print_valeur : valeur -> out_channel -> unit

