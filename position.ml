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

(*fonction qui renvoie les coordonnées des centres de cercles de rayon donné tangent à une droite donnée en un point donné - il y en a deux*)
let center = fun radius dirvector tgpoint ->
  let (u, v) = dirvector in
  let (x, y) = tgpoint in
  (*on cherche l'équation de la perpendiculaire de la droite portée par dirvector en tgpoint i.e. -vx+uy+w=0 avec w qui vérifie les coordonnées de tgpoint*)
  let w = v*.x-.u*.y in
  (*on cherche les deux intersections entre la droite définie précédement et le cercle de centre tgpoint et de rayon radius*)
  (*cela revient à chercher les solutions de ax² + bx + c*)
  let a = 1. +. v**2. /. (u**2.) in
  let b =  -.2. *. (x +. v*.w /. (u**2.) +. v*.y /. u) in
  let c = x**2. +. y**2. +.  w**2. /. (u**2.) +. 2.*.w*.y/.u -. radius**2. in
  let delta = b**2. -. 4.*.a*.c in
  let racdelta = sqrt delta in
  let xs_un = (-.b -. racdelta)/.(2.*.a) in
  let xs_deux = (-.b +. racdelta)/.(2.*.a) in
  let ys = fun xs ->
    (v*.xs-.w)/.u in
  let ys_un = ys xs_un in
  let ys_deux = ys xs_deux in
  ((xs_un, ys_un),(xs_deux, ys_deux));;
  

(*fonction qui prend une traj et qui check intersection avec un nuage*)


(*----Fonctions spécifiques----*)

(*calcul du rayon de virage à partir de l'inclinaison*)
let banktoradius = fun speed g bank ->
  speed*.speed/.(g*.tan (degtorad bank));;

(*fonction qui calcule les positions suivantes possibles*)
let nextpos = fun planepos arrival banks dt ->


(*fonction qui calcule une trajectoire à partir d'une inclinaison*)
