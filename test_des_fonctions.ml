(*
Tests des foncions de projet.ml (à copier dans le fichier projet.ml


Test de get_current pour une ligne
let () =
  let zip = { before = ['a']; current = Cursor; after = ['b';'c']; pos = 1 } in
  assert (get_current zip = Cursor);;

Test de get_current pour un buffer
  let () =
  let line = { before = ['a']; current = Cursor; after = ['b';'c']; pos = 1 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  assert (get_current buf = line);;

Test de get_pos pour une ligne
  let () =
  let line = { before = ['a']; current = Cursor; after = ['b']; pos = 2 } in
  assert (get_pos line = 2);;

Test de get_pos pour un buffer
  let () =
  let line1 : line = { before = []; current = Cursor; after = ['a']; pos = 0 } in
  let line2 : line = { before = ['b']; current = Cursor; after = []; pos = 1 } in
  let buf : buffer = { before = [line1]; current = line2; after = []; pos = 1 } in
  assert (get_pos buf = 1);;

Test de fold_zipper pour une ligne
let () =
  let line = { before = ['b';'a']; current = Cursor; after = ['c']; pos = 2 } in
  let count = fold_zipper (fun _ acc -> acc + 1) (fun _ acc -> acc + 1) line 0 in
  assert (count = 4);;

Test de fold_zipper pour un buffer
let () =
  let line1 = { before = ['a']; current = Cursor; after = ['b']; pos = 1 } in
  let line2 = { before = ['x']; current = Cursor; after = ['y';'z']; pos = 1 } in
  let buf  = { before = [line1]; current = line2; after = []; pos = 1 } in
  let count_line l acc =
    fold_zipper (fun _ a -> a + 1) (fun _ a -> a + 1) l acc
  in
  let total = fold_zipper count_line count_line buf 0 in
  assert (total = 7);; 

  Test de update_with
  let () =
  let line = { before = ['a']; current = Cursor; after = ['b']; pos = 1 } in
  let f l = { before=l.before; current = l.current ; after = l.after; pos = 1 } in
  let updated = update_with f { before = []; current = line; after = []; pos = 0 } in
  assert ((get_current updated).pos = 1);;

Tests de move_left_line
let () =
  let line = { before = ['b'; 'a']; current = Cursor; after = ['c']; pos = 2 } in
  let result = move_left_line line in
  assert (result.before = ['a']);
  assert (result.after = 'b' :: ['c']);
  assert (result.pos = 1);;

let () =
  let line = { before = []; current = Cursor; after = ['a'; 'b']; pos = 0 } in
  let result = move_left_line line in
  assert (result = line);;

Test de move_right_line
let () =
  let line = { before = ['b'; 'a']; current = Cursor; after = ['c'; 'd']; pos = 2 } in
  let result = move_right_line line in
  assert (result.before = 'c' :: ['b'; 'a']);
  assert (result.after = ['d']);
  assert (result.pos = 3);;

let () =
  let line = { before = ['a']; current = Cursor; after = []; pos = 1 } in
  let result = move_right_line line in
  assert (result = line);;

Test de move_left_buffer
let () =
  let line = { before = ['b'; 'a']; current = Cursor; after = ['c']; pos = 2 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let result = move_left_buffer buf in
  assert (result.current.before = ['a']);
  assert (result.current.after = 'b' :: ['c']);
  assert (result.current.pos = 1);;

Test de move_right_buffer
  let () =
  let line = { before = ['b'; 'a']; current = Cursor; after = ['c'; 'd']; pos = 2 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let result = move_right_buffer buf in
  assert (result.current.before = 'c' :: ['b'; 'a']);
  assert (result.current.after = ['d']);
  assert (result.current.pos = 3);;


Test de move_left1
  let () =
  let buf = {
    before = [];
    current = { before = ['a']; current = Cursor; after = ['b']; pos = 1 };
    after = [];
    pos = 0
  } in
  let result = move_left1 buf in
  assert (result.current.pos = 0);;

Test de move_right1
let () =
  let buf = {
    before = [];
    current = { before = ['a']; current = Cursor; after = ['b']; pos = 1 };
    after = [];
    pos = 0
  } in
  let result = move_right1 buf in
  assert (result.current.pos = 2);;

Test de move_up
let () =
  let line1 = { before = ['a']; current = Cursor; after = []; pos = 1 } in
  let line2 = { before = []; current = Cursor; after = ['b']; pos = 0 } in
  let buf = { before = [line1]; current = line2; after = []; pos = 1 } in
  let moved = move_up buf in
  assert (moved.pos = 0 && moved.current.pos = 0);;

Test de move_down
let () =
  let line1 = { before = ['a']; current = Cursor; after = []; pos = 1 } in
  let line2 = { before = []; current = Cursor; after = ['b']; pos = 0 } in
  let buf = { before = []; current = line1; after = [line2]; pos = 0 } in
  let moved = move_down buf in
  assert (moved.pos = 1 && moved.current.pos = 1);;

Test de insert_char
  let () =
  let line = { before = []; current = Cursor; after = []; pos = 0 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let buf' = insert_char 'x' buf in
  assert (buf'.current.before = ['x'] && buf'.current.pos = 1);;

Test de do_supr
  let () =
  let line = { before = ['a']; current = Cursor; after = ['b';'c']; pos = 1 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let result = do_supr buf in
  assert (result.current.after = ['c']);;

Test de do_backspace en milieu de ligne
  let () =
  let curr = { before = ['x'; 'y']; current = Cursor; after = ['z']; pos = 2 } in
  let buf = { before = []; current = curr; after = []; pos = 0 } in
  let result = do_backspace buf in
  assert (result.current.before = ['y']);
  assert (result.current.after = ['z']);
  assert (result.current.pos = 1);;

Test de do_backspace en début de ligne
let () =
  let prev = { before = ['a']; current = Cursor; after = []; pos = 1 } in
  let curr = { before = []; current = Cursor; after = ['b']; pos = 0 } in
  let buf = { before = [prev]; current = curr; after = []; pos = 1 } in
  let result = do_backspace buf in
  assert (result.pos = 0);
  assert (result.current.before = ['a']);
  assert (result.current.after = ['b']);;

  Test de create_newline
  let () =
  let line = { before = ['a']; current = Cursor; after = ['b']; pos = 1 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let buf' = create_newline buf in
  assert (List.length buf'.before = 1 && buf'.current.before = [] && buf'.current.after = ['b']);;

  Test de insert_char
  let () =
  let line = { before = []; current = Cursor; after = []; pos = 0 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let buf' = insert_char 'x' buf in
  assert (buf'.current.before = ['x'] && buf'.current.pos = 1);;

  Test de do_supr en milieu de ligne
  let () =
  let line = { before = ['a']; current = Cursor; after = ['b'; 'c']; pos = 1 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let result = do_suppr buf in
  assert (result.current.before = ['a']);
  assert (result.current.after = ['c']);
  assert (result.current.pos = 1);;


Test de do_supr en fin de ligne
  let () =
  let curr = { before = ['a']; current = Cursor; after = []; pos = 1 } in
  let next = { before = ['x'; 'y']; current = Cursor; after = ['z']; pos = 2 } in
  let buf = { before = []; current = curr; after = [next]; pos = 0 } in
  let result = do_suppr buf in
  assert (result.current.before = ['a']);
  assert (result.current.after = ['y'; 'x'; 'z']);
  assert (result.current.pos = 1);
  assert (result.after = []);;

Test de move_to_start_of_line
  let () =
  let line = { before = ['c'; 'b'; 'a']; current = Cursor; after = ['d'; 'e']; pos = 3 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let result = move_to_start_of_line buf in
  assert (result.current.before = []);
  assert (result.current.after = ['a'; 'b'; 'c'; 'd'; 'e']);
  assert (result.current.pos = 0);;

Test de move_to_end_of_line
let () =
  let line = { before = ['b'; 'a']; current = Cursor; after = ['c'; 'd'; 'e']; pos = 2 } in
  let buf = { before = []; current = line; after = []; pos = 0 } in
  let result = move_to_end_of_line buf in
  assert (result.current.before = ['e'; 'd'; 'c'; 'b'; 'a']);
  assert (result.current.after = []);
  assert (result.current.pos = 5);;

  *)