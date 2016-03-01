#use "keyboard.ml"
#use "draw.ml";;
#use "player.ml";;
#use "threading_tools.ml";;
#use "game_settings.ml"

#load "unix.cma";;
open Graphics;;

    
let box_player_1 = {
	w=tile_size;
	bw=tile_size/2;
	h=tile_size;
	r=Top; 
	b1_col = 657900; 
	b2_col = 657900; 
	b_col=657900};;	

let box_player_2 = {
	w=tile_size;
	bw=tile_size/2;
	h=tile_size;
	r=Top; 
	b1_col = 35700; 
	b2_col = 35700; 
	b_col=35700};;

let player_1 = {x=((n_tiles-1)/2)*tile_size; y=tile_size ;s_y=speed; s_x=0} ;;

let player_2 = {x=((n_tiles-1)/2)*tile_size; y=(n_tiles-1)*tile_size; s_y= (-1)*speed; s_x=0} ;;

draw_background n_tiles tile_size;
	
let draw_players box_player_1 box_player_2 player_1 player_2 =    
    draw_box box_player_1 player_1.x player_1.y;
    draw_box box_player_2 player_2.x player_2.y;
    in

let key_pressed_player_1 button_pressed player =
    (match button_pressed with
      'q' ->  if player.s_x <> speed then {x = player.x; y = player.y; s_x = (-1)*speed; s_y = 0} else player;
      |'d' -> if player.s_x <> (-1)*speed then {x = player.x; y =player.y; s_x = speed; s_y = 0} else player;
      |'z' -> if player.s_y <> (-1)*speed then  {x = player.x; y =player.y; s_x = 0; s_y = speed} else player;
      |'s' -> if player.s_y <> speed then {x = player.x; y =player.y; s_x = 0; s_y = (-1)*speed} else player;
      |_ ->  player)
    in

let key_pressed_player_2 button_pressed player =
    (match button_pressed with
	  'j' ->  if player.s_x <> speed then {x = player.x; y = player.y; s_x = (-1)*speed; s_y = 0} else player;
      |'l' -> if player.s_x <> (-1)*speed then {x = player.x; y =player.y; s_x = speed; s_y = 0} else player;
      |'i' -> if player.s_y <> (-1)*speed then  {x = player.x; y =player.y; s_x = 0; s_y = speed} else player;
      |'k' -> if player.s_y <> speed then {x = player.x; y =player.y; s_x = 0; s_y = (-1)*speed} else player;
      |_ ->  player)
    in

let is_out player =
	(player.x > tile_size*n_tiles) || (player.y > tile_size*n_tiles) || (player.x < 0) || (player.y < 0)
	in

let is_on_wall player walls =
	List.mem (player.x,player.y) walls
	in

let has_lost player walls =
	(is_out player) || (is_on_wall player walls)
	in

let  main_loop player_1 player_2 =

    let rec aux player_1 player_2 walls over =
        draw_players box_player_1 box_player_2 player_1 player_2;
        
		
		let e = Graphics.wait_next_event [Graphics.Poll] in
		if e.Graphics.keypressed then
			ignore (Graphics.wait_next_event [Graphics.Key_pressed]);
         
		let player_1_bis = key_pressed_player_1 e.Graphics.key player_1 and
			player_2_bis = key_pressed_player_2 e.Graphics.key player_2 in
			
		minisleep refresh_rate;
		
        let player_1_next = update_player_position player_1_bis and
            player_2_next = update_player_position player_2_bis in
		
        if (has_lost player_1_next walls) then begin
            minisleep 3.0; 
            exit 0 
        end else ();
		if (has_lost player_2_next walls) then begin 
            minisleep 3.0; 
            exit 0; 
        end else ();
		
        aux player_1_next player_2_next ((player_1_next.x,player_1_next.y)::(player_2_next.x,player_2_next.y)::walls) over ;
    in
    aux player_1 player_2 [] false
in

main_loop player_1 player_2;
	
