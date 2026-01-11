let valid_chars = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" 
;;

(* zipper dont les éléments sont de type 'a et l'élément courrant de type 'b *)

type ('a,'b) zipper =
  { before:  'a list (* liste des éléments précédents dans l'ordre inversé *)
    ; current: 'b
    ; after:   'a list (* liste des éléments précédents dans l'ordre *)
    ; pos:     int (* position courrante *)
    }
;;

type cursor = Cursor;;

type line   = (char, cursor) zipper;;

type buffer = (line, line) zipper;;

type action =
    | Up
    | Left
    | Down
    | Right
    | Char of char
    | Newline
    | Delete
    | Backspace
;;

let empty_line = { before = []; current = Cursor; after = []; pos = 0 };;

let empty_buf  = { before = []; current = empty_line; after = []; pos = 0 };;

(** NE RIEN MODIFIER AVANT CE POINT **)

let sx = 800 (* LARGEUR DE LA FENETRE GRAPHIQUE EN POINTS *)

let sy = 600  (* HAUTEUR DE LA FENETRE GRAPHIQUE EN POINTS *)

(* get_current : ('a, 'b) zipper -> 'b
    @requires Rien
    @ensures Retourne l'élément courant du zipper
    @raises Rien
*)
let get_current z = z.current;;

(* get_pos : ('a, 'b) zipper -> int
    @requires Rien
    @ensures Retourne la position courante du zipper
    @raises Rien
*)
let get_pos z = z.pos;;

(* fold_zipper : ('a -> 'c -> 'c) -> ('b -> 'c -> 'c) -> ('a, 'b) zipper -> 'c -> 'c
    @requires Rien
    @ensures Applique un fold sur le zipper dans l’ordre : before, current, after
    @raises Rien
*)
let fold_zipper f g z acc0= (*j'ai utilisé cet ordre pour garder le même typage que celui de l'énoncé*)
let acc= List.fold_right f z.before acc0 in
List.fold_left (fun acc x -> f x acc) (g z.current acc) z.after
;;

(* update_with : ('b -> 'c) -> ('a, 'b) zipper -> ('a, 'c) zipper
    @requires Rien
    @ensures Retourne un nouveau zipper identique à z avec f appliqué à l'élément courant
    @raises Rien
*)
let update_with f z = {
  before = z.before;
  current = f z.current;
  after = z.after;
  pos=z.pos
};;

(*J'ai choisir de faire deux fonctions opérant sur le type line, et deux sur le type buffer*)

(* move_left_line : line -> line
    @requires Rien
    @ensures Déplace le curseur d’un caractère vers la gauche dans la ligne et retourne la ligne inchangée si le curseur est déjà au début.
    @raises Rien
*)
let move_left_line (l : line) : line =
  match l.before with
  | [] -> l
  | p :: q -> {
      before = q;
      current = Cursor;
      after = p :: l.after;
      pos = l.pos - 1
    }
;;
(* move_right_line : line -> line
    @requires Rien
    @ensures Déplace le curseur d’un caractère vers la droite dans la ligne et retourne la ligne inchangée si le curseur est déjà à la fin.
    @raises Rien
*)
let move_right_line (l : line) : line =
  match l.after with
  | [] -> l
  | p:: q -> {
      before = p :: l.before;
      current = Cursor;
      after = q;
      pos = l.pos + 1
    }
;;


(* move_left_buffer : buffer -> buffer
    @requires Rien
    @ensures Applique un déplacement vers la gauche dans la ligne courante du buffer et retourne le buffer inchangé si le curseur est déjà au début de la ligne.
    @raises Rien
*)
let move_left_buffer (buf : buffer) : buffer =
  update_with move_left_line buf
;;

(* move_right_buffer : buffer -> buffer
    @requires Rien
    @ensures Applique un déplacement vers la droite dans la ligne courante du buffer et retourne le buffer inchangé si le curseur est déjà à la fin de la ligne.
    @raises Rien
*)
let move_right_buffer (buf : buffer) : buffer =
  update_with move_right_line buf
;;

(* move_left1 : buffer -> buffer
    @requires Rien
    @ensures Déplace le curseur vers la gauche dans la ligne courante (retourne le buffer inchangé si le curseur est déjà au début de la ligne)
    @raises Rien
*)
let move_left1 (buf : buffer) : buffer =
  { before=buf.before;
    after=buf.after;
    pos=buf.pos;

    current= match buf.current.before with
  | [] -> buf.current
  | p :: q ->
      {
        before = q;
        current = Cursor;
        after = p :: buf.current.after;
        pos = buf.current.pos - 1
      };
}
  ;;


(* move_right1 : buffer -> buffer
    @requires Rien
    @ensures Déplace le curseur vers la droite dans la ligne courante (retourne le buffer inchangé si le curseur est déjà à la fin de la ligne)
    @raises Rien
*)
let move_right1 (buf : buffer) : buffer =
  { before=buf.before;
    after=buf.after;
    pos=buf.pos;
    current=
    match buf.current.after with
  | [] -> buf.current
  | p :: q ->
      {
        before = p::buf.current.before;
        current = Cursor;
        after = q;
        pos = buf.current.pos + 1
      };
    
    
};;

(*
take : int -> 'a list -> 'a list * 'a list
    @requires Rien
    @ensures Retourne un couple dont le premier élément est la liste des n premiers éléments de la liste et le second est le reste de la liste
    @raises Rien
*)
let rec take n li =
  match n, li with
  | 0, l -> ([], l)
  | p, [] -> ([], [])
  | n, p :: q -> let (taken, rest) = take (n - 1) q in (p::taken, rest);;


(* move_up : buffer -> buffer
    @requires Rien
    @ensures Déplace le curseur vers le haut dans le fichier (retourne le buffer inchangé si la ligne courante est la première)
    @raises Rien
*)
let move_up    (buf : buffer) : buffer  =
let length = List.length buf.current.before in match buf.before with
| [] -> buf
| p::q ->
{
    after=buf.current::buf.after;
    pos=buf.pos-1;
    before=q;
    current={
      before=List.rev(fst (take length ((List.rev p.before)@p.after)));
      current=Cursor;
      after=snd(take length ((List.rev p.before)@p.after));
      pos=min length (List.length p.before + List.length p.after);

    }
}
  ;;


(* move_down : buffer -> buffer
    @requires Rien
    @ensures Déplace le curseur vers le bas dans le fichier (retourne le buffer inchangé si la ligne courante est la dernière)
    @raises Rien
*)
let move_down  (buf : buffer) : buffer  =  
let length = List.length buf.current.before in match buf.after with
| [] -> buf
| p::q ->
{
    pos=buf.pos+1;
    before=buf.current::buf.before;
    current={
      before=List.rev (fst(take length  ((List.rev p.before)@p.after)));
      current=Cursor;
      after=snd(take length ((List.rev p.before)@p.after));
      pos=min length (List.length p.before + List.length p.after);

    };
    after=q
}
  ;;


(* insert_char : char -> buffer -> buffer
    @requires c doit appartenir à valid_chars
    @ensures Ajoute c à la position courante dans la ligne et avance la position de 1
    @raises Rien
*)

let insert_char c (buf : buffer) : buffer  =
  let insert c line =
      {
        before = c :: line.before;
        current = line.current;
        after = line.after;
        pos = line.pos + 1
      }
    in
    update_with (insert c) buf
    ;;


(* do_supr : buffer -> buffer
    @requires Rien
    @ensures Supprime le caractère situé juste après la position courante
    @raises Rien
*)
let do_supr (buf : buffer) : buffer  =
  let supr line =
    match line.after with
    | [] -> line
    | _ :: p -> { 
        before = line.before;
        current = line.current;
        after = p;
        pos = line.pos }
  in
  update_with supr buf
;;

(* do_backspace1 : buffer -> buffer
    @requires Rien
    @ensures Supprime le caractère situé juste avant la position courante et recule le curseur d’un caractère
    @raises Rien
*)
let do_backspace1 (buf : buffer) : buffer =
  let supr line =
    match line.before with
    | [] -> line
    | _ :: p -> {
        before = p;
        current = line.current;
        after = line.after;
        pos = line.pos - 1
      }
  in
  update_with supr buf
;;


(* create_newline : buffer -> buffer
    @requires Rien
    @ensures Coupe la ligne courante en deux lignes au niveau du curseur de la ligne et
    insère une nouvelle ligne (le curseur du buffer se trouve sur cette nouvelle ligne)
    @raises Rien
*)
let create_newline (buf : buffer) : buffer  =
  let l = get_current buf in
  let new_before = { 
    before = l.before;
    current = Cursor; 
    after = []; 
    pos = List.length l.before } in
  let new_after = { 
    before = []; 
    current = Cursor; 
    after = l.after; 
    pos = 0 } in
  {
    before = new_before :: buf.before;
    current = new_after;
    after = buf.after;
    pos = buf.pos + 1
  }
;;


(** 4.3 Troisième partie **)

(* move_to_end_of_line : buffer -> buffer
    @requires Rien
    @ensures Met le curseur à la fin de la ligne courante
    @raises Rien
*)
let rec move_to_end_of_line (buf : buffer) : buffer =
  let rec go_right l =
    match l.after with
    | [] -> l
    | c :: p ->
        go_right {
          before = c :: l.before;
          current = Cursor;
          after = p;
          pos = l.pos + 1
        }
  in
  update_with go_right buf;;

(* move_to_start_of_line : buffer -> buffer
    @requires Rien
    @ensures Met le curseur au début de la ligne courante
    @raises Rien
*)
let rec move_to_start_of_line (buf : buffer) : buffer =
  let rec go_left l =
    match l.before with
    | [] -> l
    | c :: p ->
        go_left {
          before = p;
          current = Cursor;
          after = c :: l.after;
          pos = l.pos - 1
        }
  in
  update_with go_left buf;;

(* move_left : buffer -> buffer
    @requires Rien
    @ensures Déplace le curseur vers la gauche. On passe à la ligne précédente si le curseur se trouve en début de ligne
    @raises Rien
*)
let move_left (buf : buffer) : buffer  =
  let line = get_current buf in
  match line.before, buf.before with
  | [], prev_line :: p -> move_to_end_of_line {
    before = p;
    current = prev_line;
    after = line :: buf.after;
    pos = buf.pos - 1
  } 
  
    
  | _ -> move_left1 buf;;

(* move_right : buffer -> buffer
    @requires Rien
    @ensures Déplace le curseur vers la droite. On passe à la ligne suivante si le curseur se trouve en fin de ligne
    @raises Rien
*)
let move_right buf =
  let line = get_current buf in
  match line.after, buf.after with
  | [], next_line :: n ->
  move_to_start_of_line
      {
        before = line :: buf.before;
        current = next_line;
        after = n;
        pos = buf.pos + 1
      }
  | _ ->  move_right1 buf;;

(* do_backspace : buffer -> buffer
    @requires Rien
    @ensures Supprime le caractère avant le curseur. Fusionne avec la ligne précédente si le curseur est en début de ligne
    @raises Rien
*)
let do_backspace buf =
  let line = get_current buf in
  match line.before, buf.before with
  | [], prev_line :: prevs ->
      {
        before = prevs;

        current = {
        before = line.before@ (List.rev prev_line.after) @prev_line.before;
        current = Cursor;
        after = line.after;
        pos = List.length prev_line.before + List.length prev_line.after + line.pos};

        after = buf.after;
        pos = buf.pos - 1
      }
  | _ -> do_backspace1 buf
  ;;

(* do_suppr : buffer -> buffer
    @requires Rien
    @ensures Supprime le caractère après le curseur. Fusionne avec la ligne suivante si le curseur est en fin de ligne
    @raises Rien
*)
let do_suppr buf =
  let line = get_current buf in
  match line.after, buf.after with
  | [], next_line :: nexts ->
      {
        before = buf.before;

        current =
        {
        before = line.before;
        current = Cursor;
        after = line.after @ (List.rev next_line.before) @ next_line.after;
        pos = line.pos};

        after = nexts;
        pos = buf.pos
      }
  | _ -> do_supr buf
;;






(***** NE RIEN MODIFIER À PARTIR DE CE POINT **)       

let apply_action a buf =
    match a with
    | Up        -> move_up    buf
    | Left      -> move_left  buf
    | Down      -> move_down  buf
    | Right     -> move_right buf
    | Char ch   -> insert_char ch buf
    | Newline   -> create_newline buf
    | Delete    -> do_suppr buf
    | Backspace -> do_backspace buf
;;
let wopen () =
  let args = Printf.sprintf " %dx%d" sx sy in
  let _ = Graphics.open_graph args in
  let _ = Graphics.set_window_title "test" in
  ()

let font_width,font_height = 18,18

let line_height = font_height + 4

let line_width = font_width + 4
             
let default_char = Char.chr 167 



                 
let draw_square col row color c =
  let _ =
    Graphics.moveto (col*line_width+4) (Graphics.size_y () - row * line_height +2) in
  let _ = Graphics.set_color color in
  let _ = Graphics.fill_rect (col*(line_width)) (Graphics.size_y () - row * (line_width)) (line_width) (line_height) in
  let _ = Graphics.set_color Graphics.black in
  let _ = Graphics.draw_rect (col*(line_width)) (Graphics.size_y () - row * (line_width)) (line_width) (line_height)
  in
  Graphics.draw_char c



let draw_line is_current row l =
  let print i c =
    let _ = draw_square i row Graphics.white c in
    i+1
  in
  let col = List.fold_right (fun c i -> print i c) l.before 0 in
  let _ = List.fold_left print col l.after in 
  let _ =
    if is_current
    then
    let _ = Graphics.set_color Graphics.red in
      let _ = Graphics.fill_rect (col*(line_width)-2) (Graphics.size_y () - row * (line_width)) (4) (line_height) in
      Graphics.set_color Graphics.black
    else ()
  in
  ()

let draw_buffer buf =
  let print b j l =
    let _ = Format.printf "line : %d@." j in
    let _ = draw_line b j l in
    j+1
  in
  let row = List.fold_right (fun l j -> print false j l) buf.before 1 in
  let _ = print true row buf.current in
  List.fold_left (print false) (row+1) buf.after
  
  
let rec loop  buf =
  let _ = Graphics.clear_graph () in 
  let _ = draw_buffer buf in 
  let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
  let ch = ev.Graphics.key in
  if Char.code ch = 27 (* esc *) 
  then ()
  else 
    let laction = [
        Char.chr 26,Up;
        Char.chr 19,Down;
        Char.chr 17,Left;
        Char.chr 4,Right;
        Char.chr 13,Newline;
        Char.chr 127,Delete;
        Char.chr 8,Backspace
      ]
    in
    let buf1 = 
      match List.assoc_opt ch laction with
      | Some a -> apply_action a buf
      | None ->
                  if String.contains valid_chars ch
         then apply_action  (Char ch) buf
         else
           let code = Char.code ch in
           let msg = if code >= 1 && code <= 26
                     then Format.sprintf " (CTRL + %c)" (Char.chr (Char.code 'A' + code -1 ))
                     else ""
           in
           let _ = 
             Format.fprintf Format.err_formatter
               "Invalid char : ascii code %d %s@."
               code
               msg
           in 
           buf
    in
    loop buf1
  
let main () =
  let _ = wopen () in
  let _ = loop empty_buf in 
  let _ = Graphics.close_graph () in
  ()

let _ = main  ()
