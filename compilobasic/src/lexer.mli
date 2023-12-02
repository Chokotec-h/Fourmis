open Unicode
open UString

module Keyword : sig
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
    | For 
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
  val print : t -> out_channel -> unit
end

type token = 
  | Keyword of Keyword.t 
  | Begin of char 
  | End of char 
  | Operator of Utf8String.t 
  | Int of int 

val print_token : token -> out_channel -> unit
val print_token_debug : token -> out_channel -> unit

type error = UnknownToken of Utf8String.t

exception Error of error CodeMap.Span.located

val print_error : error -> out_channel -> unit

type t = token CodeMap.Span.located Seq.t

val create : UChar.t Seq.t -> t
val of_channel : in_channel -> t

