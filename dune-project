(lang dune 3.21)

(name metrocaml)
(version 0.1.0)

(formatting
 (enabled_for ocaml reason))

(generate_opam_files true)

(source (uri "https://moule.informatique.u-paris.fr/3lmtb/metrocaml"))

(authors "Baptiste Loriot" "Lorenzo Moscardi" "Léo Denquin" "Louis Meynaud" "Matthys Mona" "Thomas Bruno")
(maintainers "Baptiste Loriot" "Lorenzo Moscardi" "Léo Denquin" "Louis Meynaud" "Matthys Mona" "Thomas Bruno")

(license MIT)

(documentation https://moule.informatique.u-paris.fr/3lmtb/metrocaml)

(package
 (name metrocaml)
 (synopsis "Outil de calcul d'itinéraires générique dans des graphes")
 (description "Un moteur de routage modulaire supportant plusieurs algorithmes et critères sur des réseaux quelconques")
 (depends ocaml dune (alcotest :with-test) bisect_ppx csv ocamlformat)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
