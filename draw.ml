#load "unix.cma";;
open Graphics;;

#load "graphics.cma";;
Graphics.open_graph " ";;
Graphics.set_window_title "Test !";;
Graphics.plot 50 50;;

type relief = Top | Bot | Flat;;


type box_config =
{   w:int; h:int; bw:int; mutable r:relief;
	b1_col : Graphics.color;
	b2_col : Graphics.color;
	b_col : Graphics.color} ;;

	
let draw_rect x0 y0 w h =
	let (a,b) = Graphics.current_point()
		and x1 = x0+w 
		and y1 = y0+h
	in
	Graphics.moveto x0 y0;
	Graphics.lineto x0 y1; Graphics.lineto x1 y1;
	Graphics.lineto x1 y0; Graphics.lineto x0 y0;
	Graphics.moveto a b ;;
	
	
let draw_background n_tiles tile_size =
	for i=1 to n_tiles do
		for j=1 to n_tiles do
			draw_rect (i*tile_size) (j*tile_size) tile_size tile_size
		done
	done ;;


let draw_box_outline bcf col x1 y1=
	Graphics.set_color col;
	draw_rect x1 y1 bcf.w bcf.h ;;

	
let draw_box bcf x1 y1 =
	let x2 = x1+bcf.w and y2 = y1+bcf.h in
	let ix1 = x1+bcf.bw and ix2 = x2-bcf.bw
	and iy1 = y1+bcf.bw and iy2 = y2-bcf.bw in
	let border1 g =
		Graphics.set_color g;
		Graphics.fill_poly
		[| (x1,y1);(ix1,iy1);(ix2,iy1);(ix2,iy2);(x2,y2);(x2,y1) |]
	in
	let border2 g =
		Graphics.set_color g;
		Graphics.fill_poly
		[| (x1,y1);(ix1,iy1);(ix1,iy2);(ix2,iy2);(x2,y2);(x1,y2) |]
	in
	Graphics.set_color bcf.b_col;
	( match bcf.r with
		Top ->
		Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
		border1 bcf.b1_col;
		border2 bcf.b2_col
		| Bot ->
		Graphics.fill_rect ix1 iy1 (ix2-ix1) (iy2-iy1);
		border1 bcf.b2_col;
		border2 bcf.b1_col
		| Flat ->
		Graphics.fill_rect x1 y1 bcf.w bcf.h );
		draw_box_outline bcf Graphics.black x1 y1 	
	;;
