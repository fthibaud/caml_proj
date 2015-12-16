(*----Constantes----*)

(*accélération gravitationnelle en m/s²*)
let g = 9.81;;

(*définition de pi = 3.1415926*)
let pi = 4. *. atan 1.;;


(*----Types----*)

(*enregistrement de type avion*)
type plane = {
    banks : float list;
    speed : float
};;

(*type point*)
type point = float*float;;

(*enregistrement de type position avion : contient les coordonnées du point courant, du point précédent, le vecteur vitesse courant*)
type planepos = {
    currpos : point;
    precpos : point;
    speedvector : point
};;

(*type géométrique*)
type geom =
    Circle of point*float
  | Polygon of point list;; (*Nécessairement convexe*)

(*type nuage à définir*)
type clouds = geom list;;

(*----Paramètres----*)

(*pas de temps en seconde*)
let dt = 1.

(*on créé un avion de test; angles en degrés et vitesse en m/s (basé sur A320)*)
let a_trois_vingt = {banks = [0.; 10.; 20.; 30.]; speed = 230.};;

(*position de départ*)
let departure = {currpos = (0., 0.); precpos = (0., 0.); speedvector = (0., a_trois_vingt.speed)};;

(*position d'arrivée*)
let arrival = {currpos = (12000., 12000.); precpos = (12000., 12000.); speedvector = (0., a_trois_vingt.speed)};;

(*liste des nuages*)
let clouds = [Circle ((1000.,1000.),500.); Polygon[(2000.,2000.);(2000.,4000.);(4000.,4000.);(4000.,2000.)]];;


(*----Fonctions géométriques----*)

(*conversion degrés en radians*)
let degtorad = fun deg ->
  deg*.pi/.180.;;

exception Pas_de_solution_reelle;;
(*solveur de polynome de degré 2*)
let solve2 = fun a b c ->
  let delta = b**2. -. 4.*.a*.c in
  if delta > 0. then
    let x1 = (-.b -. sqrt delta)/.(2.*.a) in
    let x2 = (-.b +. sqrt delta)/.(2.*.a) in
    (x1,x2);
  else
    if delta = 0. then
      let x = (-.b/.(2.*.a)) in
      (x,x);       
    else
      raise Pas_de_solution_reelle;;

(*fonction qui renvoie les coordonnées des centres de cercles de rayon donné tangent à une droite donnée en un point donné - il y en a deux*)
let center = fun radius dirvector tgpoint ->
  let (u, v) = dirvector in
  let (s, t) = tgpoint in
  (*on cherche l'équation de la perpendiculaire de la droite portée par dirvector en tgpoint i.e. ux+vy=w avec w qui vérifie les coordonnées de tgpoint*)
  let w = u*.s+.v*.t in
  (*on cherche les deux intersections entre la droite définie précédement et le cercle de centre tgpoint et de rayon radius*)
  (*cela revient à chercher les solutions de ax² + bx + c*)
  let a = 1. +. (u**2.) /. (v**2.) in
  let b = 2. *. (t*.u/.v -. s -. u*.w/.(v**2.)) in
  let c = s**2. +. t**2. +. (w**2.) /. (v**2.) -. 2.*.w*.t/.v -. radius**2. in
  let (x1,x2)= solve2 a b c in
  let ys = fun xs ->
    (w -. u*.xs)/.v in
  let y1 = ys x1 in
  let y2 = ys x2 in
  [(x1,y1);(x2,y2)];;
  
(*fonction renvoyant les intersections de deux cercles*)
let intersect_circles = fun center0 r0 center1 r1 ->
  let (x0,y0) = center0 in
  let (x1,y1) = center1 in
  if y0=y1 
  then
    let x = (r1**2.-.r0**2.-.x1**2.+.x0**2.) /. (2.*.(x0-.x1)) in
    let a = 1. in
    let b = -.1.*.2.*.y1 in
    let c = x1**2. +. x**2. -. 2.*.x1*.x +. y1**2. -. r1**2. in
    let (y1,y2) = solve2 a b c in
    let p1 = (x,y1) in
    let p2 = (x,y2) in
    (p1,p2);
  else
    let n = (r1**2.-.r0**2.-.x1**2.+.x0**2.-.y1**2.+.y0**2.) /. (2.*.(y0-.y1)) in
    let div = ((x0-.x1)/.(y0-.y1)) in
    let a = 1. +. div**2. in
    let b = 2.*.y0*.div -. 2.*.n*.div -. 2.*.x0 in
    let c = x0**2.+.y0**2.+.n**2.-.r0**2.-.2.*.y0*.n in
    let (x1,x2) = solve2 a b c in
    let y = fun x ->
      n -. x*.div in
    let p1 = (x1,y x1) in
    let p2 = (x2,y x2) in
    (p1,p2);;
  
(*fonction renvoyant une position après le virage*)
let pos_circle = fun radius dirvector tgpoint center distance ->
  let theta = distance /. radius in
  let cote = sqrt (2. *. radius**2. *. (1. -. cos theta)) in
  let (p1,p2) = intersect_circles tgpoint cote center radius in
  let (a,b) = dirvector in
  let (c,d) = p1 in
  let (t1,t2) = tgpoint in
  let (u,v) = (c-.t1,d-.t2) in
  let ps1 = u*.a +. v*.b in
  if ps1 > 0. then p1
  else p2;;

(*fonction qui calcule la norme d'un vecteur*)
let norme = fun v ->
  let (d1,d2) = v in
  sqrt (d1**2. +. d2**2.);;

(*fonction renvoyant la position après avoir avancé tout droit*)
let pos_vector = fun dirvector tgpoint distance ->
  let (t1,t2) = tgpoint in
  let (d1,d2) = dirvector in
  let norme = sqrt (d1**2. +. d2**2.) in
  let (u,v) = (distance*.d1/.norme,distance*.d2/.norme) in
  let (x,y) = (u+.t1,v+.t2) in
  (x,y);;

(*fonction qui teste vérifie qu'un point n'appartient pas à une géométrie*)
let not_in_geom = fun geom point ->
  match geom with
    Circle ((c1,c2),r) ->
      begin
        let (x,y) = point in
        let dist = sqrt ((x-.c1)**2. +. (y-.c2)**2.) in
        dist > r;
      end
  | Polygon l ->
      begin
        let (sumx,sumy) =  List.fold_left (fun (x1,y1) (x2,y2) -> (x1+.x2,y1+.y2) ) (0.,0.) l in
        let n = List.length l in
        let bary = (sumx/.(float n) , sumy/.(float n)) in
        let d = ref [] in

        (* recherche coeff m et p de la droite de la forme y=mx+p puis évaluation en un point *)
        let eval1 = fun l point i ->
          let (x,y) = point in
          let (xa,ya) = List.nth l i in
          let (xb,yb) = List.nth l (i+1) in
          if xa=xb then (x-.xa)
          else
            begin
              let m = (yb-.ya)/.(xb-.xa) in
              let p = ya -. m*.xa in
              (m*.x+.p-.y)
            end in
        
        for i=0 to n-1 do
          d := (eval1 l bary i, eval1 l point i)::!d done;
        let p = fun x ->
          let (a,b) = x in
          a *. b <= 0. in
        List.exists p !d
      end;;


(*----Fonctions spécifiques----*)

(*calcul du rayon de virage à partir de l'inclinaison*)
let banktoradius = fun speed g bank ->
  speed*.speed/.(g*.tan (degtorad bank));;

(*fonction qui calcule les positions suivantes possibles*)
let nextpos = fun planepos arrival plane dt ->
  let nextpos_list = ref [] in
  let distance = norme planepos.speedvector *. dt in
  let banks_iter_action = fun bank ->
    if (bank=0.) then
      nextpos_list := (pos_vector planepos.speedvector planepos.currpos distance) :: !nextpos_list
    else
      let radius = banktoradius a_trois_vingt.speed g bank in
      let pos_circle_map = fun circle ->
        pos_circle radius planepos.speedvector planepos.currpos circle distance
      in
      let circle_list = center radius planepos.speedvector planepos.currpos in
      nextpos_list := !nextpos_list @ (List.map pos_circle_map circle_list)
  in
  List.iter banks_iter_action plane.banks;
  !nextpos_list;;

(*fonction qui élimine les positions suivantes possibles si elles sont dans un nuage*)
let not_in_clouds = fun nextpos_list clouds -> (*not_in_geom point geom*)
  let not_in_cloud = fun pos ->
    List.for_all (not_in_geom pos) clouds
  in
  List.filter not_in_cloud nextpos_list;;

(*fonction qui imprime une liste de coordonnées*)
let print_point_list = fun point_list ->
  Printf.printf "[";
  let print_iter = fun (a,b) ->
    Printf.printf "(%f , %f); " a b
  in
  List.iter print_iter point_list;
  Printf.printf "]\n";;


print_point_list (not_in_clouds (nextpos departure arrival a_trois_vingt dt) clouds);;

(*A coder : position suivante pour aller directement à l'arrivée*)
(*A coder : intersection avec nuage-finir primitive*)
(*A coder : génération d'une trajectoire naïve*)
(*A coder : affichage*)
(*A coder : fonction qui calcule le nouveau vecteur vitesse à l'issue d'un virage*)
(*Attention au test d'arrivée : il ne peut pas être exact*)
