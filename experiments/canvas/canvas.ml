type pixel = {r:char; g:char; b:char}
let default_pixel = {r=Char.chr 255; g=Char.chr 255; b=Char.chr 255}

type t = 
  | N of pixel
  | B of {tl_t: t; tr_t: t; 
          bl_t: t; br_t: t} (* 4 quadrants *)

type canvas = {max_x:int; max_y:int; t:t}
type loc = {x:int; y:int}

let blank = N default_pixel

let plain px = N px

let new_canvas max_x max_y = 
  {max_x=max_x; max_y=max_y; t=blank}

let rec set_px canvas loc px = 
  if canvas.max_x<=loc.x && canvas.max_y<=loc.y 
  then N px
  else 
    let mid_x = canvas.max_x/2 in
    let mid_y = canvas.max_y/2 in 
      match (loc.x <= mid_x, loc.y <= mid_y) with
        | (true,true) -> (* top-left quadrant *)
            let tl_t = match canvas.t with 
              | N px -> N px | B {tl_t} -> tl_t in
            let tl_c = {max_x=mid_x; max_y=mid_y; t=tl_t} in
            let tl_t' = set_px tl_c loc px in
            let t' = match canvas.t with
              | N px -> B {tl_t=tl_t'; tr_t=N px; 
                           bl_t=N px; br_t=N px}
              | B y -> B {y with tl_t=tl_t'} in
              t'
        | (false,true) -> (* top-right quadrant *)
            let tr_t = match canvas.t with 
              | N px -> N px | B {tr_t} -> tr_t in
            let tr_c = {max_x=canvas.max_x - mid_x; 
                        max_y=mid_y; t=tr_t} in
            let loc' = {loc with x=loc.x - mid_x} in
            let tr_t' = set_px tr_c loc' px in
            let t' = match canvas.t with
              | N px -> B {tl_t=N px; tr_t=tr_t'; 
                           bl_t=N px; br_t=N px}
              | B y -> B {y with tr_t=tr_t'} in
              t'
        | (true,false) -> (* bottom-left quadrant *)
            let bl_t = match canvas.t with 
              | N px -> N px | B {bl_t} -> bl_t in
            let bl_c = {max_x=mid_x; 
                        max_y=canvas.max_y - mid_y; 
                        t=bl_t} in
            let loc' = {loc with y=loc.y - mid_y} in
            let bl_t' = set_px bl_c loc' px in
            let t' = match canvas.t with
              | N px -> B {tl_t=N px; tr_t=N px; 
                           bl_t=bl_t'; br_t=N px}
              | B y -> B {y with bl_t=bl_t'} in
              t'
        | (false,false) -> (* bottom-right quadrant *)
            let br_t = match canvas.t with 
              | N px -> N px | B {br_t} -> br_t in
            let br_c = {max_x=canvas.max_x - mid_x;
                        max_y=canvas.max_y - mid_y; 
                        t=br_t} in
            let loc' = {x=loc.x-mid_x; y=loc.y-mid_y} in
            let br_t' = set_px br_c loc' px in
            let t' = match canvas.t with
              | N px -> B {tl_t=N px; tr_t=N px; 
                           bl_t=N px; br_t=br_t'}
              | B y -> B {y with br_t=br_t'} in
              t'

let set_px canvas loc px = 
  if loc.x > canvas.max_x || loc.y > canvas.max_y then
    failwith "set_px: location out of canvas bounds"
  else
    let t' = set_px canvas loc px in
      {canvas with t=t'}

let rec get_px canvas loc = match canvas.t with
  | N px -> px
  | B y -> 
    let mid_x = canvas.max_x/2 in
    let mid_y = canvas.max_y/2 in 
      match (loc.x <= mid_x, loc.y <= mid_y) with
        | (true,true) ->
            let tl_t = match canvas.t with 
              | N px -> N px | B {tl_t} -> tl_t in
            let tl_c = {max_x=mid_x; max_y=mid_y; t=tl_t} in
              get_px tl_c loc
        | (false,true) -> 
            let tr_t = match canvas.t with 
              | N px -> N px | B {tr_t} -> tr_t in
            let tr_c = {max_x=canvas.max_x - mid_x; 
                        max_y=mid_y; t=tr_t} in
            let loc' = {loc with x=loc.x - mid_x} in
              get_px tr_c loc'
        | (true,false) ->
            let bl_t = match canvas.t with 
              | N px -> N px | B {bl_t} -> bl_t in
            let bl_c = {max_x=mid_x; 
                        max_y=canvas.max_y - mid_y; 
                        t=bl_t} in
            let loc' = {loc with y=loc.y - mid_y} in
              get_px bl_c loc'
        | (false,false) -> 
            let br_t = match canvas.t with 
              | N px -> N px | B {br_t} -> br_t in
            let br_c = {max_x=canvas.max_x - mid_x;
                        max_y=canvas.max_y - mid_y; 
                        t=br_t} in
            let loc' = {x=loc.x-mid_x; y=loc.y-mid_y} in
              get_px br_c loc'

(*
 * RGB color mixing algorithm.
 *)
let color_mix px1 px2 : pixel = 
  let f = Char.code in
  let h x y = Char.chr @@ (x + y)/ 2 in
  let (r1,g1,b1) = (f px1.r, f px1.g, f px1.b) in
  let (r2,g2,b2) = (f px2.r, f px2.g, f px2.b) in
  let (r,g,b) = (h r1 r2, h g1 g2, h b1 b2) in
    {r=r; g=g; b=b}

let b_of_n px = 
  B {tl_t=N px; tr_t=N px; bl_t=N px; br_t=N px}
    
let make_b (tl,tr,bl,br) = 
  B {tl_t=tl; tr_t=tr; bl_t=bl; br_t=br}

let rgb px = {r=px; g=px; b=px}

let rec merge old v1 v2 = 
  if v1=v2 then v1
  else if v1=old then v2
  else if v2=old then v1
  else match (old,v1,v2) with
    (*
     * The first three rules isomorphize old, v1 and v2.
     *)
    | (_, B _, N px2) -> merge old v1 @@ b_of_n px2
    | (_, N px1, B _) -> merge old (b_of_n px1) v2
    | (N px, B _, B _) -> merge (b_of_n px) v1 v2
    | (B x, B x1, B x2) ->
        let tl_t' = merge x.tl_t x1.tl_t x2.tl_t in
        let tr_t' = merge x.tr_t x1.tr_t x2.tr_t in
        let bl_t' = merge x.bl_t x1.bl_t x2.bl_t in
        let br_t' = merge x.br_t x1.br_t x2.br_t in
          B {tl_t=tl_t'; tr_t=tr_t'; bl_t=bl_t'; br_t=br_t'}
    | (_, N px1, N px2) -> 
        (* pixels are merged by mixing colors *)
        let px' = color_mix px1 px2 in N px'

let rec print min_x min_y max_x max_y t = 
  if min_x > max_x || min_y > max_y then ()
  else match t with 
    | N px when not (px = default_pixel) -> 
        if min_x = max_x && min_y = max_y 
        then Printf.printf "<%d,%d>: (%d,%d,%d)\n" min_x min_y 
               (Char.code px.r) (Char.code px.g) (Char.code px.b)
        else Printf.printf "<%d,%d> to <%d,%d>: (%d,%d,%d)\n"
                min_x min_y max_x max_y (Char.code px.r)
                (Char.code px.g) (Char.code px.b) 
    | N px -> ()
    | B {tl_t; tr_t; bl_t; br_t} -> 
        let (mid_x, mid_y) = (min_x + (max_x - min_x + 1)/2, 
                              min_y + (max_y - min_y + 1)/2) in
        begin
          print min_x min_y mid_x mid_y tl_t;
          print (mid_x+1) min_y max_x mid_y tr_t;
          print min_x (mid_y+1) mid_x max_y bl_t;
          print (mid_x+1) (mid_y+1) max_x max_y br_t;
        end 

let print {max_x; max_y; t} = print 0 0 max_x max_y t

let print c = 
  for x=1 to c.max_x do
    for y=1 to c.max_y do
      let px = get_px c {x=x; y=y} in
        if not (px = default_pixel)
        then Printf.printf "<%d,%d>: (%d,%d,%d)\n" x y 
               (Char.code px.r) (Char.code px.g) (Char.code px.b)
        else ()
    done
  done

let main () =
  let c = new_canvas 128 128 in
  let loc = {x=93; y=127} in
  let c' = set_px c loc @@ rgb @@ Char.chr 23 in
  let _ = print c' in
  let px = get_px c' loc  in
  let _ = Printf.printf "px(%d,%d)=(%d,%d,%d)\n" loc.x loc.y
          (Char.code px.r) (Char.code px.g) (Char.code px.b) in
  let loc' = {x=45; y=78} in
  let px' = get_px c loc' in
  let _ = Printf.printf "px(%d,%d)=(%d,%d,%d)\n" loc'.x loc'.y
          (Char.code px'.r) (Char.code px'.g) (Char.code px'.b) in
  let c' = set_px c {x=98;y=17} @@ rgb @@ Char.chr 23 in
  let _ = print c' in
    ();;

(* main ();; *)
