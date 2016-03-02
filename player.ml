type player = {x:int; y:int; s_x:int; s_y:int} ;;

let update_player_position pl = {
    x = pl.x + pl.s_x ;
    y = pl.y + pl.s_y ;
    s_x = pl.s_x ;
    s_y = pl.s_y}
	;;
	
let is_out pl tile_size n_tiles = pl.x > tile_size * n_tiles || pl.y > tile_size * n_tiles || pl.x < 0 || pl.y < 0 ;;

let is_on_wall pl walls = List.mem (pl.x,pl.y) walls ;;

let has_lost pl walls tile_size n_tiles = (is_out pl tile_size n_tiles) || (is_on_wall pl walls) ;;