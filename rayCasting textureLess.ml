#load "graphics.cma";;
open Graphics;;

let niceGreen = rgb 43 113 40;;
let niceGreenShade = rgb 21 56 20;;

let niceBlue = rgb 37 150 190;;
let niceBlueShade = rgb 18 75 95;;

let niceRed = rgb 141 23 23;;
let niceRedShade = rgb 70 11 11;;

let shade color  =
	let b = color mod 256 in
	let g = ((color - b)/256) mod 256 in
	let r = ((color-b)/(256*256)) - g/256 in
	rgb (r/2) (g/2) (b/2);;

type vector = {x:float; y:float;};;


let add v1 v2 =
	let x1, y1= v1.x, v1.y in
	let x2, y2= v2.x, v2.y in
	{x=x1+.x2; y=y1+.y2};;

let scale a v1 =
	let x1, y1= v1.x, v1.y in
	{x=x1*.a; y=y1*.a};;

let neg v1 = scale (-1.) v1;;

let sub v1 v2 = (*return v1 - v2*)
	add v1 (neg v2);;

let length v =
	let x, y= v.x, v.y in
	sqrt(x *. x +. y *. y);;

let dot u v =
	let x, y= u.x, u.y in
	let a, b= v.x, v.y in
	x*.a +. y*.b;;

let normalize v = scale (1. /. length v) v;;

let rotate u a =
	let x, y = u.x, u.y in
	{x= x*.cos a -. y*.sin a; y= x*.sin a +. y*. cos a};;

let colorMatch n shade = match n with
	1 -> white
	|2 -> if shade = 0 then niceGreen else niceGreenShade
	|3 -> if shade = 0 then niceBlue else niceBlueShade
	|4 -> if shade = 0 then niceRed else niceRedShade
	|_ -> black;;

let matrixInit w h f =
	let mat = Array.make_matrix w h 0 in
	for i = 0 to w-1 do
		for j = 0 to h-1 do
			mat.(i).(j) <- f i j
		done;
	done;
	mat;;

let frac x = x -. Float.floor x;;

let w = 640;;
let h = 640;;


let texWidth = 64;;
let texHeight = 64;;

let blackScreen = matrixInit w h (fun i j -> black);;
let screenBuffer = matrixInit w h (fun i j -> black);;

let cleanScreen () =
	for i = 0 to w-1 do
		for j = 0 to h-1 do
			screenBuffer.(i).(j) <- black;
		done;
	done;
	;;

let drawScreen () =
	for i = 0 to w-1 do
		for j=0 to h-1 do
			set_color screenBuffer.(i).(j);
			plot i j;
		done;
	done;
	;;

let texture = Array.init 10 (fun i -> Array.make_matrix texWidth texHeight black);;
texture.(0) <- Array.make_matrix texWidth texHeight white;;
texture.(1) <- matrixInit texWidth texHeight (fun i j -> if (i + j) mod 2 = 0 then niceGreen else niceGreenShade);;
texture.(2) <- matrixInit texWidth texHeight (fun i j -> if (i + j) mod 2 = 0 then niceBlue else niceBlueShade);;
texture.(3) <- matrixInit texWidth texHeight (fun i j -> if (i + j) mod 2 = 0 then niceRed else niceRedShade);;
let worldMap = matrixInit h w (fun i j -> if i = 0 || i = h-1 || j = 0 || j = w-1 then 1 else  0) ;;
let maxIter = 100;;


let ray {x=posX; y=posY} {x=rayDirX; y=rayDirY} x =

	let mapX = ref (int_of_float posX) in
	let mapY = ref (int_of_float posY) in
	
	let sideDistX = ref 0. in
	let sideDistY = ref 0. in
	
	let deltaDistX = Float.abs(1. /. rayDirX) in
	let deltaDistY = Float.abs(1. /. rayDirY) in
	let perpWallDist = ref 0. in
	
	let stepX = ref 0 in
	let stepY = ref 0 in
	
	let hit = ref true in
	let side = ref 0 in
	
	if rayDirX < 0. then 
		begin
		stepX := -1;
		sideDistX := (posX -. float_of_int !mapX) *. deltaDistX;
		end
	else
		begin
		stepX := 1;
		sideDistX := (float_of_int !mapX +. 1.0 -. posX) *. deltaDistX;
		end;
		
	if rayDirY < 0. then 
		begin
		stepY := -1;
		sideDistY := (posY -. float_of_int !mapY) *. deltaDistY;
		end
	else
		begin
		stepY := 1;
		sideDistY := (float_of_int !mapY +. 1.0 -. posY) *. deltaDistY;
		end;
		
	let iter = ref 0 in
	
	while !hit && !iter < maxIter do
		iter := !iter + 1;
		if !sideDistX < !sideDistY then
			begin
			sideDistX := !sideDistX +. deltaDistX;
			mapX := !mapX + !stepX;
			side := 0;
			end
		else
			begin
			sideDistY := !sideDistY +. deltaDistY;
			mapY := !mapY + !stepY;
			side := 1;
			end;
			
		let a,b = !mapX, !mapY in
		if a < 0 || a > w-1 || b < 0 || b > h-1 || worldMap.(a).(b) > 0 then hit := false;
		
	done;
	
	let perpWallDist = if !side = 0 then !sideDistX -. deltaDistX else !sideDistY -. deltaDistY in
	let lineHeight = int_of_float (float_of_int h /. perpWallDist) in
	
	lineHeight, colorMatch (if !mapX < w-1 && !mapY < h-1 then worldMap.(!mapX).(!mapY) else 1) !side
	
	
	;;

let rayCast camPos dirVec screen =
	
	for x = 0 to  w-1 do
		let cameraX = 2. *. (float_of_int x) /. (float_of_int w) -. 1. in
		let curRay = add dirVec (scale cameraX screen) in
		
		let l,c = ray camPos curRay x in
		set_color c;
		moveto x (h/2 - l/2);
		lineto x (h/2 + l/2);
		
	done;
	cleanScreen ();
	;;

let fov = 70.;;
let camPos = ref {x= 20.; y= 10.};;
let dirVec = ref {x=(-1.); y= 0.};;
let screenDir = ref {x=0.; y= tan (0.5 *. fov *.Float.pi /. 180.)};;
let speed = ref 0.1;;
let rotSpeed = ref 0.1;;
let dx = {x= !speed;y=0.};;
let dy = {x=0.;y= !speed};;

for i = 12 to 25 do
	worldMap.(i).(5) <- 2;
	done;;

for j = 1 to 24 do
	worldMap.(17).(j) <- 3
	done;;

let move s =
	let posX, posY = !camPos.x, !camPos.y in
	let dirX, dirY = !dirVec.x, !dirVec.y in
	if s = 'w' then
		begin
		if(worldMap.(int_of_float(posX +. dirX *. !speed)).(int_of_float(posY)) = 0) then camPos := add !camPos {x=dirX *. !speed; y=0.};
		if(worldMap.(int_of_float(posX)).(int_of_float(posY +. dirY *. !speed)) = 0) then camPos := add !camPos {x=0.; y=dirY *. !speed};
		end
	else if s = 's' then
		begin
		if(worldMap.(int_of_float(posX -. dirX *. !speed)).(int_of_float(posY)) = 0) then camPos := sub !camPos {x=dirX *. !speed; y=0.};
		if(worldMap.(int_of_float(posX)).(int_of_float(posY -. dirY *. !speed)) = 0) then camPos := sub !camPos {x=0.; y=dirY *. !speed};
		end
	else if s = 'a' then
		begin
		dirVec := rotate !dirVec !rotSpeed;
		screenDir := rotate !screenDir !rotSpeed;
		end
	else if s = 'd' then
		begin
		dirVec := rotate !dirVec (-. !rotSpeed);
		screenDir := rotate !screenDir (-. !rotSpeed);
		end;;

let clearGraph () = 
	set_color black;
	fill_rect 0 0 h w;
	;;

let time = ref (Sys.time ());;
let textSize = 25;;

open_graph (" " ^ string_of_int w ^ "x" ^ string_of_int h);
auto_synchronize false;
set_text_size textSize;
rayCast !camPos !dirVec !screenDir;;

let fps () =
	let t = Sys.time () in
	set_color white;
	moveto 0 0;
	draw_string (string_of_float (1. /. (t-. !time)));
	time:= t;
	;;

let rec main () = 
	
	
	if Graphics.key_pressed () then
		begin
		let s = Graphics.read_key () in
		match s with
	
			|'x' -> close_graph ()
			|_ -> move s
		end;
	clearGraph ();
	rayCast !camPos !dirVec !screenDir;
	fps ();
	synchronize ();
	main ()
	
	;;

main ();;
!time;;
Float.rem (-1.) 10.;;

