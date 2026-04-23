# Contribuer à MétroCaml

1) Cloner le dépôt Git.
```bash
git clone https://moule.informatique.univ-paris-diderot.fr/3lmtb/metrocaml.git
```
1) Depuis dev, créer sa branche de travail avec un nom explicite selon les conventions GitFlow (`feature/` ou `bugfix/`).
2) Faire ses changements et commits avec des messages explicites.
3) Pousser sa branche sur le dépôt distant.
4) Créer une merge request (MR) de sa branche vers dev.
5) La MR déclenche une pipeline CI/CD GitLab qui exécute les tests automatisés.
6) Si la pipeline passe, demander une revue de code à un mainteneur du projet.
7) Après approbation, la MR peut être fusionnée dans dev par le mainteneur.

### Informations supplémentaires :
Lors de la relecture de votre code, les mainteneurs accorderont une attention particulière à :
- La généricité et la clarté de votre code.
- La qualité de votre documentation (commentaires, README, etc.).
- La couverture de vos tests unitaires et d'intégration.
