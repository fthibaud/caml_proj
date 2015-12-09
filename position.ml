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
  | Polygon of point list;;

(*type nuage à définir*)


(*----Paramètres----*)

(*pas de temps en seconde*)
let dt = 1.

(*on créé un avion de test; angles en degrés et vitesse en m/s (basé sur A320)*)
let a_trois_vingt = {banks = [0.; 20.; 30.]; speed = 230.};;

(*position de départ*)
let departure = {currpos = (0., 0.); precpos = (0., 0.); speedvector = (0., a_trois_vingt.speed)};;

(*position d'arrivée*)
let arrival = {currpos = (12000., 12000.); precpos = (12000., 12000.); speedvector = (0., a_trois_vingt.speed)};;


(*----Fonctions géométriques----*)

(*conversion degrés en radians*)
let degtorad = fun deg ->
  deg*.pi/.180.;;

(*solveur de polynome de degré 2*)
let solve2 = fun a b c ->
  let delta = b**2. -. 4.*.a*.c in
  let x1 = (-.b -. sqrt delta)/.(2.*.a) in
  let x2 = (-.b +. sqrt delta)/.(2.*.a) in
  (x1,x2);;

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
  [(x1,y1),(x2,y2)];;

let pos_circle = fun radius dirvector tgpoint center distance ->
  ;;

let pos_vector = fun dirvector distance ->
  ;;

(*fonction qui prend une traj et qui check intersection avec un nuage*)


(*----Fonctions spécifiques----*)

(*calcul du rayon de virage à partir de l'inclinaison*)
let banktoradius = fun speed g bank ->
  speed*.speed/.(g*.tan (degtorad bank));;

(*fonction qui calcule les positions suivantes possibles*)
let nextpos = fun planepos arrival banks dt ->
  let circle_list = [] in
  let nextpos_list = [(pos_vector planepos.speedvector planepos.speevector *. dt)] in
  match banks with
    [] -> failwith "liste vide"
  | tete::queue -> let banks_without_zero = queue in
  let radius = banktoradius a_trois_vingt.speed g bank in
  let banks_iter_action = fun bank ->
    circle_list @ center radius planepos.speedvector planepos.currpos
  in
  List.iter banks_iter_action banks_without_zero;
  let pos_circle_map = fun circle ->
    pos_circle radius planepos.speedvector planepos.currpos circle (planepos.speevector *. dt)
  in
  nextpos_list @ List.map pos_circle_map circle_list;

(*fonction qui calcule une trajectoire à partir d'une inclinaison*)
