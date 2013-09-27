# Présentation

Raccourcisseur est un programme LISP basé sur caveman qui sert à récupérer des images venant d'une URL ou d'un fichier local, et qui l'héberge selon un nom très court (juste un numéro qui est incrémenté).


# Dépendances

+ Redis server
+ SBCL
+ Quicklisp

# Configuration 

Il est nécessaire de créer une règle dans le serveur web pour rediriger toutes les requêtes d'images vers le dossier /static/.


# Limitations 

Pour le moment, il n'est possible d'envoyer que les extensions suivantes :

+ jpg
+ JPG
+ jpeg
+ JPEG
+ png
+ PNG
+ bmp
+ BMP


# Évolutions futures

+ Afficher les X derniers uploads
+ Pouvoir saisir une date d'expiration de l'image au bout de X jours / heures
+ Générer une miniature de l'image
+ Ajouter le support du svg
+ Améliorer l'interface
