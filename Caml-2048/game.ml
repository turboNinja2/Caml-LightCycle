type 'a matrix = 'a array array

let n = 4 ;; 
let table = Array.make_matrix n n 0;;

let indexedTraversal
    (add : int -> int -> 'a -> 'b -> 'b)
    (ini : 'b)
    (mat : 'a matrix)
    : 'b =
  let m = Array.length mat     in
  let n = Array.length mat.(0) in
  let rec aux (i : int) (j : int) (acc : 'b) =
    let acc' = add i j mat.(i).(j) acc in
         if j < n - 1 then aux i (j + 1) acc'
    else if i < m - 1 then aux (i + 1) 0 acc'
    else acc'
  in aux 0 0 ini
;;  
  
let find_zeroes (m : int matrix) : (int * int) list =
  let testElt i j x ijs = if x == 0 then (i,j)::ijs else ijs
  in List.rev (indexedTraversal testElt [] m)
;;
  
let print_matrix input_matrix = 
	for i = Array.length input_matrix - 1 downto 0 do
		for j = 0 to Array.length input_matrix - 1 do
			print_int input_matrix.(i).(j);
			print_string "\t"
		done;
	print_newline(); print_newline();
	done;
print_newline();
;;

let get_random_elt my_list = List.nth my_list (Random.int (List.length my_list)) ;;

let add_element input_matrix =
	let (i,j) = get_random_elt (find_zeroes input_matrix) in input_matrix.(i).(j) <- 1 ;;

	
let player_move_horiz input_matrix x =
	let successful_move = ref false in
	let offset_x = (x+1) / 2 in 
	for i = 0 to Array.length input_matrix - 1 do
		for k = 0 to 2 do
			for j = (Array.length input_matrix - 2 + offset_x) downto offset_x do
				let tmp_left = input_matrix.(i).(j-x) in
				if(input_matrix.(i).(j)==0) then begin
					input_matrix.(i).(j) <- tmp_left;
					input_matrix.(i).(j-x) <- 0;
					successful_move := true;
				end
			done;
		done;
			for j = (Array.length input_matrix - 2 + offset_x) downto offset_x do
				let tmp_left = input_matrix.(i).(j-x) in
				if (tmp_left==input_matrix.(i).(j)) then begin 
					input_matrix.(i).(j) <- tmp_left*2;
					input_matrix.(i).(j-x) <- 0;
					successful_move := true;
				end
			done;
				for j = (Array.length input_matrix - 2 + offset_x) downto offset_x do
					let tmp_left = input_matrix.(i).(j-x) in
					if(input_matrix.(i).(j)==0) then begin
						input_matrix.(i).(j) <- tmp_left;
						input_matrix.(i).(j-x) <- 0;
						successful_move := true;
					end
				done;
	done;
successful_move	;;

let player_move_verti input_matrix y =
	let offset_y = (y+1) / 2 in 
	let successful_move = ref false in
	for j = 0 to Array.length input_matrix - 1 do
		for k = 0 to 2 do
			for i = (Array.length input_matrix - 2 + offset_y) downto offset_y do
				let tmp_left = input_matrix.(i-y).(j) in
				if(input_matrix.(i).(j)==0) then begin
					input_matrix.(i).(j) <- tmp_left;
					input_matrix.(i-y).(j) <- 0;
					successful_move := true;
				end
			done;
		done;	
			for i = (Array.length input_matrix - 2 + offset_y) downto offset_y do
				let tmp_left = input_matrix.(i-y).(j) in
				if (tmp_left==input_matrix.(i).(j)) then begin 
					input_matrix.(i).(j) <- tmp_left*2;
					input_matrix.(i-y).(j) <- 0;
					successful_move := true;
				end
			done;
				for i = (Array.length input_matrix - 2 + offset_y) downto offset_y do
					let tmp_left = input_matrix.(i-y).(j) in
					if(input_matrix.(i).(j)==0) then begin
						input_matrix.(i).(j) <- tmp_left;
						input_matrix.(i-y).(j) <- 0;
						successful_move := true;
					end
				done;
	done;
successful_move ;;



let player_move_right input_matrix = player_move_horiz input_matrix 1 ;;
let player_move_left input_matrix  = player_move_horiz input_matrix (-1) ;;
let player_move_up input_matrix    = player_move_verti input_matrix 1 ;;
let player_move_down input_matrix  = player_move_verti input_matrix (-1) ;;

Random.init 11;

let key_pressed_player button_pressed table =
    (match button_pressed with
	  "j" -> player_move_left table
      |"l" -> player_move_right table 
      |"i" -> player_move_up table
      |"k" -> player_move_down table
      |_ -> ref false)
    in

let main_loop a =
    let rec aux over =
		let e = read_line () in
		if !(key_pressed_player e table) then add_element table;
		print_matrix table;
		print_newline();
		aux over;
    in
    aux false
in

add_element table;
print_matrix table;
print_newline();
main_loop ();
