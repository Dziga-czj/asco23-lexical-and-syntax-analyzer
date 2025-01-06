
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
Son comportement est de renvoyer une erreur si il y a quelque chose que le lexer ne peut pas interpréter, et ne s'arrête pas sinon.
```
$ cat input.txt | ./test_decoupe
```

### test_parser :
Ceci est un simple test pour le parser. Le test est aussi repri du TP n°3 ou après avoir découpé le texte avec le lexer, on le parse afin de voir si il y a des erreurs de syntaxe.
```
$ cat input.txt | ./test_parser
```

### test :
Cet exécutable est fait pour lancer tout les tests de syntaxe, de types etc. 
Le programme s'arrête quand un des test qui est sensé passer ne passe pas, et inversement.
```
$ ./test
```

## Debug
Il y a la possibilité d'afficher les morceaux de l'ast qui ont été "parsé" grâce au mode débug. 
On peut activer ou désactiver cet affichage en changeant la valeur de la constante "debug" dans les déclarations en haut du fichier "parser.mly" 
