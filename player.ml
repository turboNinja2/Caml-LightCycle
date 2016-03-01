type player = {x:int; y:int; s_x:int; s_y:int} ;;

let update_player_position pl = {
    x = pl.x + pl.s_x ;
    y = pl.y + pl.s_y ;
    s_x = pl.s_x ;
    s_y = pl.s_y};;