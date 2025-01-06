
# Analyseurs syntaxique et sémantique pour un sous-langage de TypeScript

## Compilation :
```
$ make test
$ make test_decoupe
$ make test_parser
```

## usage :
Avec un fichier de test "input.txt"
### test_decoupe :
Ceci est un simple test pour le lexer. Le test est repri du TP n°3 ou l'on ne fait que découper le texte.
```
$ cat input.txt | ./test_decoupe
```

### test_parser :
Ceci est un simple test pour le parser. Le test est aussi repri du TP n°3 ou après avoir découpé le texte avec le lexer, on le parse afin de voir si il y a des erreurs de syntaxe.
```
$ cat input.txt | ./test_parser
```

### test :
Cet exécutable est fait pour lancer tout les tests donnés. Ici, tout les tests concernant la partie parser passent quand il le faut, et prduisent une erreur quand la syntaxe n'est pas bonne.
Les autres tests de type quand à eux ne sont pas encore implémentés, donc ne passent pas.
```
$ ./test
```
