
## Consignes avant de commencer:


0 Se connecter à la plateforme datalab.sspcloud.fr
- Créer un service RStudio

1 Créer un nouveau projet RStudio intitulé "tp_stats_spatiales" -
Cocher "Create a git repository"

2 Créer un dossier "fonds" dans le dossier du projet - Optionnel:
Uplodez le premier fonds de carte: - Placez-vous dans ce dossier dans
l'explorateur puis faire - upload -\> aller sous U et choisir dans
fonds_de_cartes le dossier zippé nommé commune_francemetro_2021.zip -
Rq: Ceci est optionnel car les données et fonds de carte pourront
également directement être chargées depuis un serveur auquel vous aurez
accès.

3 Commencez le TP

## En fin de TP:

Pour conserver vos programmes, deux solutions:

1- la solution la plus propre (vivement conseillée): utiliser un dépôt git

- Se créer un compte sur github / ou gitlab si besoin
- Pour github: Créer un Access Token (Settings -> deveoper settings -> personal acces token). ATTENTION: conserver ce token qq part car invisible ensuite (possiblité d'en régénérer un autre si perdu: pas de panique)

- créer un nouveau repository vierge sur github ou gitlab
- Dans le terminal de RStudio:

cd tp_stats_sptiales
git add -v .gitignore *.R *.Rproj
git branch -M main

- RQ: seuls les programmes sont placés sur git: pas les données

- Sur github/gitlab récupérer l'adresse https du repo 

git remote add origin + url de votre repo (https://.../ccc.git)
git push -u origin main

- sur github, vous aurez besoin de votre personal access token

2- Solution moins propre (ou complémentaire)

Téléchargez le dossier en local
(Attention cela peut être long si vous téléchargez les données)

