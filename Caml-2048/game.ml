#load "unix.cma";;
#load "graphics.cma";;
open Graphics;;


let n = 4 ;;

let table = Array.make_matrix n n 0;;
	
let print_matrix input_matrix = 
	for i = Array.length input_matrix - 1 downto 0 do
		for j = 0 to Array.length input_matrix - 1 do
			print_int input_matrix.(i).(j);
			print_string "\t"
		done;
	print_newline();
	print_newline();
	done;
print_newline();
;;

let matrix_to_list input_matrix =
	let rec aux i j n =
		if(i == n) then []
		else begin
			if(j < n-1) then input_matrix.(i).(j)::(aux i (j+1) n)
			else input_matrix.(i).(j)::(aux (i+1) 0 n)
		end
	in aux 0 0 (Array.length input_matrix)
;;

let find_zeroes input_matrix =
	let rec aux i j n =
		if(i == n) then []
		else begin
			if(input_matrix.(i).(j) == 0) then begin
				if(j < n-1) then (i,j)::(aux i (j+1) n)
				else (i,j)::(aux (i+1) 0 n)
			end else begin
				if(j < n-1) then (aux i (j+1) n)
				else (aux (i+1) 0 n)
			end
		end
	in aux 0 0 (Array.length input_matrix)
;;

let get_random_elt my_list = List.nth my_list (Random.int (List.length my_list)) ;;

let add_element input_matrix =
	let (i,j) = get_random_elt (find_zeroes input_matrix) in input_matrix.(i).(j) <- 1 ;;

let player_move_right input_matrix =
	for i = 0 to Array.length input_matrix - 1 do
		for k = 0 to 1 do
			for j = Array.length input_matrix - 1 downto 1 do
				let tmp_left = input_matrix.(i).(j-1) in
				if(input_matrix.(i).(j)==0) then begin
					input_matrix.(i).(j) <- tmp_left;
					input_matrix.(i).(j-1) <- 0;
				end else if (tmp_left==input_matrix.(i).(j)) then begin 
					input_matrix.(i).(j) <- tmp_left*2;
					input_matrix.(i).(j-1) <- 0;
				end
			done;
		done;
	done;
;;


let player_move_left input_matrix =
	for i = 0 to Array.length input_matrix - 1 do
		for k = 0 to 1 do
			for j = 0 to Array.length input_matrix - 2 do
				let tmp_left = input_matrix.(i).(j+1) in
				if(input_matrix.(i).(j)==0) then begin
					input_matrix.(i).(j) <- tmp_left;
					input_matrix.(i).(j+1) <- 0;
				end else if (tmp_left==input_matrix.(i).(j)) then begin 
					input_matrix.(i).(j) <- tmp_left*2;
					input_matrix.(i).(j+1) <- 0;
				end
			done;
		done;
	done;
;;


let player_move_up input_matrix =
	for j = 0 to Array.length input_matrix - 1 do
		for k = 0 to 1 do
			for i = Array.length input_matrix - 1 downto 1 do
				let tmp_left = input_matrix.(i-1).(j) in
				if(input_matrix.(i).(j)==0) then begin
					input_matrix.(i).(j) <- tmp_left;
					input_matrix.(i-1).(j) <- 0;
				end else if (tmp_left==input_matrix.(i).(j)) then begin 
					input_matrix.(i).(j) <- tmp_left*2;
					input_matrix.(i-1).(j) <- 0;
				end
			done;
		done;
	done;
;;


let player_move_up input_matrix =
	for j = 0 to Array.length input_matrix - 1 do
		for k = 0 to 1 do
			for i = Array.length input_matrix - 1 downto 1 do
				let tmp_left = input_matrix.(i-1).(j) in
				if(input_matrix.(i).(j)==0) then begin
					input_matrix.(i).(j) <- tmp_left;
					input_matrix.(i-1).(j) <- 0;
				end else if (tmp_left==input_matrix.(i).(j)) then begin 
					input_matrix.(i).(j) <- tmp_left*2;
					input_matrix.(i-1).(j) <- 0;
				end
			done;
		done;
	done;
;;



let player_move_down input_matrix =
	for j = 0 to Array.length input_matrix - 1 do
		for k = 0 to 1 do
			for i = 0 to Array.length input_matrix - 2 do
				let tmp_left = input_matrix.(i+1).(j) in
				if(input_matrix.(i).(j)==0) then begin
					input_matrix.(i).(j) <- tmp_left;
					input_matrix.(i+1).(j) <- 0;
				end else if (tmp_left==input_matrix.(i).(j)) then begin 
					input_matrix.(i).(j) <- tmp_left*2;
					input_matrix.(i+1).(j) <- 0;
				end
			done;
		done;
	done;
;;
	
Random.init 11;

let key_pressed_player button_pressed table =
    (match button_pressed with
	  "j" -> player_move_left table
      |"l" -> player_move_right table 
      |"i" -> player_move_up table
      |"k" -> player_move_down table
      |_ ->  ())
    in

add_element table;
print_matrix table;
print_newline();

let main_loop a =

    let rec aux over =
		let e = read_line () in
		key_pressed_player e table;
			
		add_element table;
		print_matrix table;
		print_newline();
		aux over;
    in
    aux false
in

main_loop ();
	


print_newline();
