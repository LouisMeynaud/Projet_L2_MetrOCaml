

type noeud = { 
  id : string;
  nom : string;
  x : float;  
  y : float
}


type arete = { 
  source : string;       
  destination : string; 
  couleur : int;
  categorie : string;
  groupe : string
}

type est_afficher = {
  noeuds : noeud list;
  aretes : arete list;
  positions : (string * (int * int)) list;
  categories : (string * bool) list;
  bornes : float * float * float * float;
}
let largeur = 1200
let hauteur = 800
let marge = 50