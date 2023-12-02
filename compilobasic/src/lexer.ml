open CodeMap
open Unicode
open UString

module Keyword = struct
  type t = 
    | Above 
    | Ahead 
    | Attack 
    | Below 
    | Dig 
    | Digdown 
    | Digup 
    | Do 
    | Drop 
    | Else 
    | Empty 
    | Enemy 
    | Enemyhome 
    | Enemymarker 
    | Enemywithfood 
    | Fill 
    | Filldown 
    | Fillup 
    | Food 
    | Friend 
    | Friendwithfood 
    | Grab 
    | Grabbed 
    | Here 
    | Holeabove 
    | Holebelow 
    | Home 
    | If 
    | Ifs 
    | Left 
    | Leftahead 
    | Mark 
    | Marker 
    | Move 
    | Movedown 
    | Moveelse 
    | Moveup 
    | Nop 
    | Pickelse 
    | Pickup 
    | Random 
    | Right 
    | Rightahead 
    | Rock 
    | Surface 
    | Then 
    | True 
    | Turn 
    | Underground 
    | Unmark 
    | While 
  let print t fmt = 
    match t with 
    | Above -> Printf.fprintf fmt "above" 
    | Ahead -> Printf.fprintf fmt "ahead" 
    | Attack -> Printf.fprintf fmt "attack" 
    | Below -> Printf.fprintf fmt "below" 
    | Dig -> Printf.fprintf fmt "dig" 
    | Digdown -> Printf.fprintf fmt "digdown" 
    | Digup -> Printf.fprintf fmt "digup" 
    | Do -> Printf.fprintf fmt "do" 
    | Drop -> Printf.fprintf fmt "drop" 
    | Else -> Printf.fprintf fmt "else" 
    | Empty -> Printf.fprintf fmt "empty" 
    | Enemy -> Printf.fprintf fmt "enemy" 
    | Enemyhome -> Printf.fprintf fmt "enemyhome" 
    | Enemymarker -> Printf.fprintf fmt "enemymarker" 
    | Enemywithfood -> Printf.fprintf fmt "enemywithfood" 
    | Fill -> Printf.fprintf fmt "fill" 
    | Filldown -> Printf.fprintf fmt "filldown" 
    | Fillup -> Printf.fprintf fmt "fillup" 
    | Food -> Printf.fprintf fmt "food" 
    | Friend -> Printf.fprintf fmt "friend" 
    | Friendwithfood -> Printf.fprintf fmt "friendwithfood" 
    | Grab -> Printf.fprintf fmt "grab" 
    | Grabbed -> Printf.fprintf fmt "grabbed" 
    | Here -> Printf.fprintf fmt "here" 
    | Holeabove -> Printf.fprintf fmt "holeabove" 
    | Holebelow -> Printf.fprintf fmt "holebelow" 
    | Home -> Printf.fprintf fmt "home" 
    | If -> Printf.fprintf fmt "if" 
    | Ifs -> Printf.fprintf fmt "ifs" 
    | Left -> Printf.fprintf fmt "left" 
    | Leftahead -> Printf.fprintf fmt "leftahead" 
    | Mark -> Printf.fprintf fmt "mark" 
    | Marker -> Printf.fprintf fmt "marker" 
    | Move -> Printf.fprintf fmt "move" 
    | Movedown -> Printf.fprintf fmt "movedown" 
    | Moveelse -> Printf.fprintf fmt "moveelse" 
    | Moveup -> Printf.fprintf fmt "moveup" 
    | Nop -> Printf.fprintf fmt "nop" 
    | Pickelse -> Printf.fprintf fmt "pickelse" 
    | Pickup -> Printf.fprintf fmt "pickup" 
    | Random -> Printf.fprintf fmt "random" 
    | Right -> Printf.fprintf fmt "right" 
    | Rightahead -> Printf.fprintf fmt "rightahead" 
    | Rock -> Printf.fprintf fmt "rock" 
    | Surface -> Printf.fprintf fmt "surface" 
    | Then -> Printf.fprintf fmt "then" 
    | True -> Printf.fprintf fmt "true" 
    | Turn -> Printf.fprintf fmt "turn" 
    | Underground -> Printf.fprintf fmt "underground" 
    | Unmark -> Printf.fprintf fmt "unmark" 
    | While -> Printf.fprintf fmt "while" 


end

type token = 
  | Keyword of Keyword.t 
  | Begin of char 
  | End of char 
  | Operator of Utf8String.t 
  | Int of int 

let print_token t fmt = 
  match t with 
  | Keyword kw -> Keyword.print kw fmt
  | Begin d -> Printf.fprintf fmt "%c" d 
  | End d -> Printf.fprintf fmt "%c" d 
  | Operator name -> Printf.fprintf fmt "%s" name 
  | Int i -> Printf.fprintf fmt "%d" i 

let print_token_debug t fmt = 
  match t with 
  | Keyword kw -> Printf.fprintf fmt "keyword `%t`" (Keyword.print kw)
  | Begin d -> Printf.fprintf fmt "opening `%c`" d 
  | End d -> Printf.fprintf fmt "closing `%c`" d 
  | Operator name -> Printf.fprintf fmt "operator `%s`" name 
  | Int i -> Printf.fprintf fmt "integer `%d`" i 

type error = UnknownToken of Utf8String.t

exception Error of error CodeMap.Span.located

let print_error e fmt =
  match e with
  | UnknownToken name -> Printf.fprintf fmt "unknown token `%s`" name

type t = token CodeMap.Span.located Seq.t

let delimiter_opt str =
  match str with
  | "(" -> Some (Begin '(')
  | ")" -> Some (End ')')
  | "{" -> Some (Begin '{')
  | "}" -> Some (End '}')
  | _ -> None

let keyword_opt str =
  match str with
  | "above" -> Some (Keyword Keyword.Above)
  | "ahead" -> Some (Keyword Keyword.Ahead)
  | "attack" -> Some (Keyword Keyword.Attack)
  | "below" -> Some (Keyword Keyword.Below)
  | "dig" -> Some (Keyword Keyword.Dig)
  | "digdown" -> Some (Keyword Keyword.Digdown)
  | "digup" -> Some (Keyword Keyword.Digup)
  | "do" -> Some (Keyword Keyword.Do)
  | "drop" -> Some (Keyword Keyword.Drop)
  | "else" -> Some (Keyword Keyword.Else)
  | "empty" -> Some (Keyword Keyword.Empty)
  | "enemy" -> Some (Keyword Keyword.Enemy)
  | "enemyhome" -> Some (Keyword Keyword.Enemyhome)
  | "enemymarker" -> Some (Keyword Keyword.Enemymarker)
  | "enemywithfood" -> Some (Keyword Keyword.Enemywithfood)
  | "fill" -> Some (Keyword Keyword.Fill)
  | "filldown" -> Some (Keyword Keyword.Filldown)
  | "fillup" -> Some (Keyword Keyword.Fillup)
  | "food" -> Some (Keyword Keyword.Food)
  | "friend" -> Some (Keyword Keyword.Friend)
  | "friendwithfood" -> Some (Keyword Keyword.Friendwithfood)
  | "grab" -> Some (Keyword Keyword.Grab)
  | "grabbed" -> Some (Keyword Keyword.Grabbed)
  | "here" -> Some (Keyword Keyword.Here)
  | "holeabove" -> Some (Keyword Keyword.Holeabove)
  | "holebelow" -> Some (Keyword Keyword.Holebelow)
  | "home" -> Some (Keyword Keyword.Home)
  | "if" -> Some (Keyword Keyword.If)
  | "ifs" -> Some (Keyword Keyword.Ifs)
  | "left" -> Some (Keyword Keyword.Left)
  | "leftahead" -> Some (Keyword Keyword.Leftahead)
  | "mark" -> Some (Keyword Keyword.Mark)
  | "marker" -> Some (Keyword Keyword.Marker)
  | "move" -> Some (Keyword Keyword.Move)
  | "movedown" -> Some (Keyword Keyword.Movedown)
  | "moveelse" -> Some (Keyword Keyword.Moveelse)
  | "moveup" -> Some (Keyword Keyword.Moveup)
  | "nop" -> Some (Keyword Keyword.Nop)
  | "pickelse" -> Some (Keyword Keyword.Pickelse)
  | "pickup" -> Some (Keyword Keyword.Pickup)
  | "random" -> Some (Keyword Keyword.Random)
  | "right" -> Some (Keyword Keyword.Right)
  | "rightahead" -> Some (Keyword Keyword.Rightahead)
  | "rock" -> Some (Keyword Keyword.Rock)
  | "surface" -> Some (Keyword Keyword.Surface)
  | "then" -> Some (Keyword Keyword.Then)
  | "true" -> Some (Keyword Keyword.True)
  | "turn" -> Some (Keyword Keyword.Turn)
  | "underground" -> Some (Keyword Keyword.Underground)
  | "unmark" -> Some (Keyword Keyword.Unmark)
  | "while" -> Some (Keyword Keyword.While)
  | _ -> None

let operator_opt str =
  match str with
  | "!=" -> Some (Operator "!=")
  | ";" -> Some (Operator ";")
  | "=" -> Some (Operator "=")
  | _ -> None

let int_opt str =
  match int_of_string_opt str with
  | Some i -> Some (Int i)
  | None -> None

let ident_opt _ = None

let token_of_buffer span str =
  match delimiter_opt str with
  | Some token -> token
  | None ->
    begin match int_opt str with
      | Some token -> token
      | None ->
        begin match keyword_opt str with
          | Some token -> token
          | None ->
            begin match ident_opt str with
              | Some token -> token
              | None ->
                begin match operator_opt str with
                  | Some token -> token
                  | None ->
                    raise (Error (UnknownToken str, span))
                end
            end
        end
    end

let consume span chars =
  begin match chars () with
    | Seq.Nil -> (span, Seq.Nil)
    | Seq.Cons (c, chars) ->
      (* Add [c] to the [span]. *)
      let span = Span.push c span in
      (span, Seq.Cons (c, chars))
  end

let create input =
  let rec next span chars () =
    begin match consume span chars with
      | _, Seq.Nil -> Seq.Nil
      | span, Seq.Cons (c, chars) ->
        begin match UChar.to_int c with
          | 0x28 | 0x29 |0x5b | 0x5d | 0x7b | 0x7d -> (* ( ) [ ] { } *)
            return span chars (Utf8String.push c "")
          | _ when UChar.is_whitespace c || UChar.is_control c ->
            next (Span.next span) chars ()
          | _ when UChar.is_alphanumeric c ->
            read_alphanumeric span (c, chars)
          | _ ->
            read_operator span (c, chars)
        end
    end
  and return span chars buffer =
    let token = token_of_buffer span buffer in
    Seq.Cons (Span.located span token, next (Span.next span) chars)
  and read_alphanumeric span (c, chars) =
    let rec read span chars buffer =
      match consume span chars with
      | _, Seq.Nil -> return span chars buffer
      | span', Seq.Cons (c, chars') ->
        if UChar.is_whitespace c || UChar.is_control c || not (UChar.is_alphanumeric c) then
          return span chars buffer
        else
          read span' chars' (Utf8String.push c buffer)
    in
    read span chars (Utf8String.push c "")
  and read_operator span (c, chars) =
    let rec read span chars buffer =
      match consume span chars with
      | _, Seq.Nil -> return span chars buffer
      | span', Seq.Cons (c, chars') ->
        begin match UChar.to_int c with
          | 0x28 | 0x29 |0x5b | 0x5d | 0x7b | 0x7d -> (* ( ) [ ] { } *)
            return span chars buffer
          | _ when UChar.is_whitespace c || UChar.is_control c || UChar.is_alphanumeric c ->
            return span chars buffer
          | _ ->
            read span' chars' (Utf8String.push c buffer)
        end
    in
    read span chars (Utf8String.push c "")
  in
  next Span.default input

(* Create a sequence of chars from an input channel. *)
let seq_of_channel input =
  let rec next mem () =
    match !mem with
    | Some res -> res
    | None ->
      let res =
        try
          let c = input_char input in
          Seq.Cons (c, next (ref None))
        with
        | End_of_file -> Seq.Nil
      in
      mem := Some res;
      res
  in
  next (ref None)

let of_channel chan =
  let input = seq_of_channel chan in
  let utf8_input = Unicode.Encoding.utf8_decode input in
  create utf8_input

