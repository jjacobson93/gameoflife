(* matrix helper functions *)
let matrix = Array.make_matrix;;
let matrix_copy m = Array.map Array.copy m;; (* copies 2d array (matrix) *)

let square_size = 15;;

(* cell patterns *)
let glider_gun = [(1, 26); (2, 24); (2, 26); (3, 14); (3, 15);
	(3, 22); (3, 23); (3, 36); (3, 37); (4, 13); (4, 17); 
	(4, 22); (4, 23); (4, 36); (4, 37); (5, 2); (5, 3); 
	(5, 12); (5, 18); (5, 22); (5, 23); (6, 2); (6, 3); 
	(6, 12); (6, 16); (6, 18); (6, 19); (6, 24); (6, 26); 
	(7, 12); (7, 18); (7, 26); (8, 13); (8, 17); (9, 14); (9, 15)];;

let spaceship = [(5, 2); (7, 2); (5, 5); (6, 6); (7, 6);
	(8, 3); (8, 4); (8, 5); (8, 6)];;

let cooldesign = [(5, 5); (6, 5); (7, 5); (9, 5); (10, 5); (11, 5);
	(5, 10); (6, 10); (7, 10); (9, 10); (10, 10); (11, 10);
	(5, 12); (6, 12); (7, 12); (9, 12); (10, 12); (11, 12);
	(5, 17); (6, 17); (7, 17); (9, 17); (10, 17); (11, 17);
	(3, 7); (3, 8); (3, 9); (3, 13); (3, 14); (3, 15);
	(8, 7); (8, 8); (8, 9); (8, 13); (8, 14); (8, 15);
	(10, 7); (10, 8); (10, 9); (10, 13); (10, 14); (10, 15);
	(15, 7); (15, 8); (15, 9); (15, 13); (15, 14); (15, 15);];;

let pulsar = [(5, 5); (6, 5); (7, 5); (11, 5); (12, 5); (13, 5);
	(5, 10); (6, 10); (7, 10); (11, 10); (12, 10); (13, 10);
	(5, 12); (6, 12); (7, 12); (11, 12); (12, 12); (13, 12);
	(5, 17); (6, 17); (7, 17); (11, 17); (12, 17); (13, 17);
	(3, 7); (3, 8); (3, 9); (3, 13); (3, 14); (3, 15);
	(8, 7); (8, 8); (8, 9); (8, 13); (8, 14); (8, 15);
	(10, 7); (10, 8); (10, 9); (10, 13); (10, 14); (10, 15);
	(15, 7); (15, 8); (15, 9); (15, 13); (15, 14); (15, 15);];;

(* Set cells from a list of tuples *)
let set_cells cells offset uni =
	List.iter (fun (i, j) -> uni.(i + offset).(j + offset) <- 1) cells;;

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

(* Recursive wrapper for Thread.delay to handle errors *)
let rec thread_delay sec =
	try
		Thread.delay sec
	with
		Unix.Unix_error(Unix.EINTR, _, _) -> thread_delay sec;;

(* main code *)
(* Usage: life [glidergun|spaceship|pulsar|cool] width height *)

let () = 
	let argc = Array.length Sys.argv in
	let opt = if argc < 2 then glider_gun else
		match Sys.argv.(1) with
		 | "glidergun" -> glider_gun
		 | "spaceship" -> spaceship
		 | "pulsar" -> pulsar
		 | "cool" -> cooldesign
		 | "-h" -> Printf.printf
            "Usage: %s [glidergun|spaceship|pulsar|cool] width height delay\n" Sys.argv.(0);
            exit 0
		 | _ -> glider_gun in

	let width = if argc > 2 then
		max 40 (int_of_string Sys.argv.(2))
		else 80 in
	let height = if argc > 3 then
		max 30 (int_of_string Sys.argv.(3))
		else 40 in
	let delay = if argc > 4 then
		max 0.05 (try (float_of_string Sys.argv.(4)) with _ -> 0.05)
		else 0.1 in
	let universe = ref (matrix height width 0) in
	
	Graphics.open_graph (Printf.sprintf " %dx%d" (width * square_size) (height * square_size));
	set_cells opt 5 !universe;
	while true do
		draw_grid !universe;
		thread_delay delay;
		universe := matrix_copy (run_iteration !universe);
	done;;