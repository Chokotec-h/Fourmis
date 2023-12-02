open Printf


(*================================================================================*)
(* Utilitaires                                                                    *)
(*================================================================================*)

(* Fonctions utilitaires pour rajouter des messages d'erreur utilisables *)
let ouvrir_in_check (filename: string) : in_channel =
  try open_in filename
  with
  | Sys_error(e) -> eprintf "Ne peut pas ouvrir le fichier \"%s\" %s\n" filename e; exit 1 

let ouvrir_out_check (filename: string) : out_channel = 
  try open_out filename
  with
  | Sys_error(e) -> eprintf "Ne peut pas ouvrir le fichier \"%s\" %s\n" filename e; exit 1 

(* Fonctions utilitaires pour se débarasser des Span.located 
   La fonction [Parse.parse_program] de la bibliothèque de parsing fournie
   produit un AST du programme qui a une forme un peu spéciale : 
   chaque noeud de l'AST est en fait un couple [(n,loc)] avec [n] le noeud réel
   et [loc] un [CodeMap.Span.t] qui représente une information la position
   dans le programme du noeud. 
   [loc] est très utile si vous voulez que votre compilateur fasse des messages d'erreur
   super précis. Ce n'est peut-être pas essentiel pour un hackaton de 2 jours d'où ces
   fonctions d'unwrap et la présence de nombreux [(expr,_)] dans le code.
   Les cas Mark et Unmark de la fonction [comp_command] présentent l'usage basique
   des [CodeMap.Span.t] si ça vous intéresse quand même.*)
let unwrap_expr (e: Ast.expression CodeMap.Span.located) : Ast.expression = 
    match e with 
    | (expr, _) -> expr 

let unwrap_expr_list : Ast.expression CodeMap.Span.located list -> Ast.expression list =
  List.map unwrap_expr






(*================================================================================*)
(* Compilation                                                                    *)
(*================================================================================*)
(*Les fonctions [comp_something] prennent en entrée un argument
   de type [Ast.something] et renvoient une [string] ou écrivent
   dans un channel [oc].*)



let i = ref 0 (*Compteur pour créer des labels frais*)


(** Compile un [AST.direction]*)
let comp_direction (dir: Ast.direction) : string =
    match dir with
        | Ast.Here ->               "Here"
        | Ast.LeftAhead ->          "LeftAhead"
        | Ast.RightAhead ->         "RightAhead"
        | Ast.Ahead ->              "Ahead"
        | Ast.Above ->              "Above"
        | Ast.Below ->              "Below"

(** Compile un [AST.lr]*)
let comp_lr (dir: Ast.lr) : string =
    match dir with
        | Ast.TurnL ->              "Left"
        | Ast.TurnR ->              "Right"
 
(** Compile un [AST.valeur]*)
let comp_valeur (valeur: Ast.valeur) : string =
    match valeur with
        | Ast.Friend ->             "Friend"
        | Ast.Enemy ->              "Enemy"
        | Ast.Grabbed ->            "Grabbed"
        | Ast.FriendWithFood ->     "FriendWithFood"
        | Ast.EnemyWithFood ->      "EnemyWithFood"
        | Ast.Food ->               "Food"
        | Ast.Rock ->               "Rock"
        | Ast.Empty ->              "Empty"
        | Ast.Underground ->        "Underground"
        | Ast.Surface ->            "Surface"
        | Ast.HoleAbove ->          "HoleAbove"
        | Ast.HoleBelow ->          "HoleBelow"
        | Ast.Marker(i,_) ->        "Marker " ^ (string_of_int i)
        | Ast.EnemyMarker ->        "EnemyMarker"
        | Ast.Home ->               "Home"
        | Ast.EnemyHome ->          "EnemyHome"


(** Compile un [AST.command]*)
let comp_command (commande: Ast.command) (oc: out_channel) : unit =
    match commande with
        | Ast.Nope ->               ()
        
        (* Pour ces commandes très basiques, la compilation revient juste à écrire la
           l'instruction correspondante dans le fichier brain.*)
        | Ast.Turn(direction,_) ->  fprintf oc  "  Turn%s\n" (comp_lr direction)
        | Ast.Drop ->               fprintf oc "  Drop\n"
        | Ast.FillUp ->
                fprintf oc "  FillUp\n"
        | Ast.FillDown ->
                fprintf oc "  FillDown\n"

        (* Vous avez ici un exemple d'usage de [loc].*)
        | Ast.Mark(i, loc) ->         
          if i > 7 || i < 0 
          then (
            eprintf "%tOn ne peut faire mark i que pour 0 <= i <= 7\n" (CodeMap.Span.print loc); 
            exit 1) 
          else fprintf oc "  Mark %d\n" i
        | Ast.Unmark(i, loc) ->         
          if i > 7 || i < 0 
          then (
            eprintf "%tOn ne peut faire unmark i que pour 0 <= i <= 7\n" (CodeMap.Span.print loc); 
            exit 1) 
          else fprintf oc "  Unmark %d\n" i

        (* Full steam ahead! Ces instructions peuvent échouer mais sont ici compilées
           pour se rendre au label suivant peut importe qu'on réussisse ou non.
           Move et Pickup ont aussi une version [Ast.expression] avec gestion d'échec.
           Allez voir dans [comp_expression] pour plus de détails.*)
        | Ast.Pickup ->
                fprintf oc "  PickUp label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.Move ->
                fprintf oc "  Move label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.MoveUp ->
                fprintf oc "  MoveUp label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.MoveDown ->
                fprintf oc "  MoveDown label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.Dig ->
                fprintf oc "  Dig label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.Fill ->
                fprintf oc "  Fill label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.DigUp ->
                fprintf oc "  DigUp label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.DigDown ->
                fprintf oc "  DigDown label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.Grab ->
                fprintf oc "  Grab label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i
        | Ast.Attack ->
                fprintf oc "  Attack label_%d\n  Goto label_%d\nlabel_%d:\n" !i !i !i;
                incr i

(* Compile une condition. [c] représente le numéro de label
   vers lequel il faut se rendre si la condition est vraie, [c+1]
   si elle est fausse.*)
let comp_condition (cond: Ast.condition) (c: int) (oc: out_channel) : unit =
    match cond with
        | Ast.Eq((direction,_),(valeur,_)) ->       
            fprintf oc "  Sense %s label_%d label_%d %s\n" (comp_direction direction) (c) (c+1) (comp_valeur valeur)
        | Ast.Neq((direction,_),(valeur,_)) ->
            fprintf oc "  Sense %s label_%d label_%d %s\n" (comp_direction direction) (c+1) (c) (comp_valeur valeur)
        | Ast.Random(p,_) ->
                        fprintf oc "  Roll %d label_%d label_%d\n" (p) (c) (c+1)
        (*| Ast.And((cond1,_),(cond2,_)) ->
            fprintf oc "  Sense %s label_%d label_%d %s\n" (comp_direction direction) (c) (c+1) (comp_valeur valeur);
            fprintf oc "label_%d\n" ();
            comp_condition cond2 *)
        | _ -> failwith "cas impossible"


(* Compile une expression :
   on s'attend à être déjà à l'intérieur d'un label,
   ce qui signifie que chaque expression termine sa compilation en écrivant
   un label frais pour la suivante.*)
let rec comp_expression (exp: Ast.expression) (oc: out_channel) : unit =
  let comp_many_expr : Ast.expression CodeMap.Span.located list -> unit = 
    List.iter (fun (exp,_) -> comp_expression exp oc) 
  in match exp with
        | Ast.Do(commande,_) -> comp_command commande oc

        (* Version avec gestion d'erreur de Move et Pick*)
        | Ast.MoveElse(exp_l,_) ->
            begin
                let c = !i in i := c + 2;
                fprintf oc "  Move label_%d\n" c;
                fprintf oc "  Goto label_%d\n" (c+1);
                fprintf oc "label_%d:\n" c;
                comp_many_expr exp_l;
                fprintf oc "  Goto label_%d\n" (c+1);
                fprintf oc "label_%d:\n" (c+1);
            end
        | Ast.PickElse(exp_l,_) ->
            begin
                let c = !i in i := c + 2;
                fprintf oc "  Pickup label_%d\n" c;
                fprintf oc "  Goto label_%d\n" (c+1);
                fprintf oc "label_%d:\n" c;
                comp_many_expr exp_l;
                fprintf oc "  Goto label_%d\n" (c+1);
                fprintf oc "label_%d:\n" (c+1);
            end

        (* Compilation du while true : on crée un label [c+1]
           qui contient le corps de la boucle et un goto final vers
           lui même*)
        | Ast.While((Ast.True,_),(exp,_)) -> 
            let c = !i in
            fprintf oc "  Goto label_%d\n" (c+1);
            fprintf oc "label_%d:\n" (c+1);
            i := !i + 3;
            comp_many_expr exp;
            fprintf oc "  Goto label_%d\n" (c+1) ;
            fprintf oc "label_%d:\n" (c+2)
        
        (* Compilation du while : on crée un label où la condition est évaluée,
           un label avec le corps de boucle qui se termine par un retour au label
           de condition, et le label de sortie.*)
        | Ast.While((cond,_),(exp,_)) -> 
            let c = !i in
            fprintf oc "  Goto label_%d\n" (c+1);
            (* on réserve [c+1] [c+2] et [c+3] pour notre boucle. les labels frais commencent à [c+4]*)
            i := !i + 4; 
            (* label de condition c+1*)
            fprintf oc "label_%d:\n" (c+1);
            comp_condition cond (c+2) oc;
            (* label de corps c+2*)
            fprintf oc "label_%d:\n" (c+2);
            comp_many_expr exp;
            fprintf oc "  Goto label_%d\n" (c+1);
            (* label de sortie c+3*)
            fprintf oc "label_%d:\n" (c+3)
        | Ast.If((cond,_),(e_then,_),(e_else,_)) ->
            let c = !i in
            (* on réserve [c+1] [c+2] [c+3] pour notre if. les labels frais commencent à [c+4]*)
            i := !i + 4; 
            comp_condition cond (c+1) oc;
            (* label de if c+1*)
            fprintf oc "label_%d:\n" (c+1);
            comp_many_expr e_then;
            fprintf oc "  Goto label_%d\n" (c+3);
            (* label de then c+2*)
            fprintf oc "label_%d:\n" (c+2);
            comp_many_expr e_else;
            fprintf oc "  Goto label_%d\n" (c+3);
            (* label de sortie c+3*)
            fprintf oc "label_%d:\n" (c+3)

        | Ast.Ifs((cond,_),(e_then,_)) ->
            let c = !i in
            (* on réserve [c+1] [c+2] pour notre boucle. les labels frais commencent à [c+3]*)
            i := !i + 3; 
            comp_condition cond (c+1) oc;
            (* label de if c+1*)
            fprintf oc "label_%d:\n" (c+1);
            comp_many_expr e_then;
            fprintf oc "  Goto label_%d\n" (c+2);
            (* label de sorti c+3*)
            fprintf oc "label_%d:\n" (c+2)

(* Compile un programme *)
let comp_program (program: Ast.program) (oc: out_channel) : unit =
    match program with
        | Ast.Program(expression_l,_) ->
                fprintf oc "debut:\n";
                List.iter (fun x -> comp_expression x oc) (unwrap_expr_list expression_l);
                fprintf oc "  Goto debut\n"


(*================================================================================*)
(* main                                                                           *)
(*================================================================================*)
 
(* Réalise l'entièreté de la compilation : gestion des fichiers, lexing, parsing, traitement.*)
let process_file (filename: string) (output: string) : unit =
  (* Ouvre le fichier et créé un lexer. *)
  let file = ouvrir_in_check filename in
    let lexer = Lexer.of_channel file in
  (* Parse le fichier. *)
    let (program, _) = Parser.parse_program lexer in
    (* printf "successfully parsed the following program at position %t:\n%t\n" (CodeMap.Span.print span) (Ast.print_program program); *)
  let out = ouvrir_out_check output in  
  comp_program program out;
  close_out out


let print_usage () : unit = 
  eprintf "Usage :\n\t%s <entree> [-o sortie]\n" Sys.argv.(0);
  eprintf "\tSi sortie n'est pas précisée, sortie est mise à cervo.brain\n"; 
  exit 1

let process_cli (input_file: string ref) (output_file: string ref) : unit = 
  let speclist = [("-o", Arg.Set_string output_file, "Nom du fichier de sortie")] 
    in let add_input (filename: string) : unit = match !input_file with  
        "" -> input_file := filename
      | _ -> failwith "" 
    in Arg.parse speclist add_input "Usage :\n\t%s <entree> [-o sortie]\n"

(* Le point de départ du compilateur. *)
let _ =
  let name_in = ref "" and name_out = ref "" in
  process_cli name_in name_out; 
  if !name_in = "" then ( 
    eprintf "Pas de fichier d'entrée fourni\n"; print_usage ();
  );
  
  if !name_out = "" then (
    name_out := "cervo.brain"
  );

  try
    process_file !name_in !name_out;
  with
    | Lexer.Error (e, span) ->
      eprintf "Lex error: %t: %t\n" (CodeMap.Span.print span) (Lexer.print_error e)
    | Parser.Error (e, span) ->
      eprintf "Parse error: %t: %t\n" (CodeMap.Span.print span) (Parser.print_error e)
