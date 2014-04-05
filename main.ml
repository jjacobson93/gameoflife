#load "graphics.cma"
#load "unix.cma"

(* matrix helper functions *)
let matrix = Array.make_matrix;;
let matrix_copy m = Array.map Array.copy m;; (* copies 2d array (matrix) *)

let square_size = 15;;

let random m n uni = 
	Random.self_init();
	for i = 0 to m do
		for j = 0 to n do
			uni.(i).(j) <- Random.int 2
		done
	done;;
	

let glider_gun = [(1, 26); (2, 24); (2, 26); (3, 14); (3, 15);
	(3, 22); (3, 23); (3, 36); (3, 37); (4, 13); (4, 17); 
	(4, 22); (4, 23); (4, 36); (4, 37); (5, 2); (5, 3); 
	(5, 12); (5, 18); (5, 22); (5, 23); (6, 2); (6, 3); 
	(6, 12); (6, 16); (6, 18); (6, 19); (6, 24); (6, 26); 
	(7, 12); (7, 18); (7, 26); (8, 13); (8, 17); (9, 14); (9, 15)];;

(* Set cells from a list of tuples *)
let set_cells cells uni =
	List.iter (fun (i, j) -> uni.(i).(j) <- 1) cells;;

(* the core of the game: checks if cell i,j is alive or dead *)
let live_or_die i j uni =
	let m = Array.length uni and
		n = Array.length uni.(i) in
		let neighbors =
			let alive x y = 
				if x >= 0 && y >= 0 && x < m && y < n then 
						(* Printf.printf "(%d, %d) is alive? %d\n" x y uni.(x).(y); *)
						uni.(x).(y)
				else 0 in
				(alive (i - 1) (j - 1)) +
				(alive (i - 1) j) +
				(alive (i - 1) (j + 1)) +
				(alive i (j - 1)) +
				(alive i (j + 1)) +
				(alive (i + 1) (j - 1)) +
				(alive (i + 1) j) +
				(alive (i + 1) (j + 1)) in

				let current = uni.(i).(j) in
				if current == 1 && neighbors < 2 
					then 0 (* dies of loneliness *)
				else if current == 1 && neighbors > 3 
					then 0 (* dies of overcrowding *)
				else if current == 1 && (neighbors == 2 || neighbors == 3)
					then 1 (* stays alive *)
				else if current == 0 && neighbors == 3
					then 1 (* born from 3 neighbors *)
				else 0;; (* stays dead *)

(* Runs an iteration and returns a new universe *)
let run_iteration uni =
	let m = (Array.length uni) and 
		n = (Array.length uni.(0)) and
		new_uni = matrix_copy uni in
		for x = 0 to m - 1 do
			for y = 0 to n - 1 do
				new_uni.(x).(y) <- live_or_die x y uni;
			done
		done;
	new_uni;;

(* Draws grid with given universe *)
let draw_grid uni = 
	Graphics.clear_graph();
	let m = (Array.length uni) and 
		n = (Array.length uni.(0)) in
		for j = 0 to n - 1 do
			for i = 0 to m - 1 do
				let x = j * square_size and
					y = ((m - 1) - i) * square_size in
					if uni.(i).(j) == 1 then begin
						Graphics.set_color Graphics.blue;
						Graphics.fill_rect x y square_size square_size; 
					end
			done
		done;;

(* main code *)
(* Usage: life [option] [gun|random] width height *)

let () = 
	let argc = Array.length Sys.argv in
	let opt = if argc > 1 then Sys.argv.(1) else "NONE" in
	let file_name = if opt == "-t" && argc > 2 then
		Sys.argv.(2) else "NONE" in

	let width = if opt != "-t" && argc > 3 then
		max 40 (int_of_string Sys.argv.(3))
		else if opt == "-t" && argc > 4 then
		max 40 (int_of_string Sys.argv.(4))
		else 40 in
	let height = if opt != "-t" && argc > 4 then
		max 22 (int_of_string Sys.argv.(4))
		else if opt == "-t" && argc > 5 then
		max 22 (int_of_string Sys.argv.(5))
		else 22 in

	Graphics.open_graph (Printf.sprintf " %dx%d" (width * square_size) (height * square_size));
	let universe = ref (matrix height width 0) in
	set_cells glider_gun !universe;
	while true do
		draw_grid !universe;
		Unix.sleep 1;
		universe := matrix_copy (run_iteration !universe);
	done;;