#load "graphics.cma"
#load "unix.cma";;

Graphics.open_graph " 640x480";;

let matrix = Array.make_matrix;;

let square_size = 15;;
let frame_width = Graphics.size_x ();;
let frame_height = Graphics.size_y ();;

(* let universe = matrix (frame_height/square_size) (frame_width/square_size) 0;; *)


let universe =
	[|
	[|0; 0; 0; 0; 1; 0; 0; 0; 0; 0|]; [|0; 0; 0; 1; 0; 0; 0; 0; 0; 0|];
	[|0; 0; 0; 1; 0; 1; 0; 0; 0; 0|]; [|0; 0; 0; 0; 1; 0; 0; 0; 1; 0|];
	[|0; 0; 0; 0; 1; 0; 0; 0; 1; 0|]; [|0; 0; 0; 1; 0; 1; 0; 0; 1; 0|];
	[|0; 1; 1; 0; 0; 0; 0; 1; 1; 0|]; [|0; 0; 0; 0; 0; 1; 0; 0; 0; 0|];
	[|0; 0; 1; 0; 0; 0; 0; 0; 0; 0|]; [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|]
	|];;

(* let draw_grid _ =
	for i = 0 to 150/square_size do
		for j = 0 to 150/square_size do
			let x = i*square_size and
			y = j*square_size in
				Graphics.draw_rect x y square_size square_size
		done
	done;; *)

let live_or_die i j uni =
	let neighbors = 
		if i > 0 && j > 0 then uni.(i - 1).(j - 1) else 0 +
		if i > 0 then uni.(i - 1).(j) else 0 +
		if i > 0 && (j + 1) < (Array.length uni.(i - 1)) then uni.(i - 1).(j + 1) else 0 +
		if j > 0 then uni.(i).(j - 1) else 0 +
		if (j + 1) < (Array.length uni.(i)) then uni.(i).(j + 1) else 0 +
		if j > 0 && (i + 1) < (Array.length uni) then uni.(i + 1).(j - 1) else 0 +
		if (i + 1) < (Array.length uni) then uni.(i + 1).(j) else 0 +
		if (i + 1) < (Array.length uni) && (j + 1) < (Array.length uni.(i)) then uni.(i + 1).(j + 1) else 0
		and
		current = uni.(i).(j) in
			if current == 1 then
				if neighbors == 2 || neighbors == 3 then 1
				else 0
			else if current == 0 && neighbors == 3 then 1
			else 0;;
		

let rec draw_grid uni = 
	Printf.printf "Draw grid called\n";
	(* Initialize indexes *)
	let i = ref 0 and j = ref 0 and
		new_universe = uni in
	let f e = 
		let g h =
			(* This is the fancy stuff right here *)
			(* h is an integer in the array *)
			let x = !i * square_size and y = !j * square_size in
				Graphics.set_color Graphics.black;
				Graphics.draw_rect x y square_size square_size;
				if h == 1 then begin
					Graphics.set_color Graphics.blue;
					Graphics.fill_rect x y square_size square_size;
				end;
				let state = live_or_die !i !j uni in
					new_universe.(!i).(!j) <- state;
				(* End fancy stuff *)
				i := !i + 1;
				in
					(* Iterate through each element of the row *)
					Array.iter g e;
					i := 0;
					j := !j + 1;
					in
						(* Iterate through each row of the universe *)
						Array.iter f uni;
						Unix.sleep 1;
						draw_grid new_universe;;

(* let run_iteration _ = 
	(* Initialize indexes *)
	let i = ref 0 and j = ref 0 in
	Graphics.set_color Graphics.blue;
	let f e = 
		let g h =
			(* This is the fancy stuff right here *)
			(* h is an integer in the array *)
			let x = !i * square_size and y = !j * square_size in
				if h == 1 then Graphics.fill_rect x y square_size square_size;
				(* End fancy stuff *)
				i := !i + 1; 
				in
					(* Iterate through each element of the row *)
					Array.iter g e;
					i := 0;
					j := !j + 1; 
					in
						(* Iterate through each row of the universe *)
						Array.iter f universe;; *)


draw_grid universe;;
read_line();;