# Initiation au traitement de l'information géographique dans un environnement SIG (QGIS)

**Ronan Ysebaert (UAR RIATE, Université Paris Cité) et Violaine Jurie (Géotéca, Université Paris Cité)**

<p float="left">
<img src="fig/riate.png" width="250" align="middle" hspace="20" vspace="20">
<img src="fig/geoteca.png" width="200" align="middle" hspace="20" vspace="20">
<img src="fig/uparis.jpeg" width="200" align="middle" hspace="20" vspace="20">
</p>

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](http://creativecommons.org/licenses/by-sa/4.0/)

## Objectif pédagogique
Cette formation vise à initier ses participants aux spécificités de l'information géographique et leur manipulation 
avec un SIG libre et gratuit de référence, QGIS. Les fonctionnalités de base de l'outil seront abordées :
import de données, gestion des systèmes de coordonnées de référence, jointures attributaires, sélections, géotraitements,
visualisation de données et mise en page. Cette formation inclut un atelier pour permettre aux participants de mettre en pratique
les éléments théoriques et méthodologiques abordés.

## Organisation du dépôt

- La présentation est accessible [ici](https://rysebaert.github.io/qgis_data_shs)
- Elle est construite à partir d'un document RMarkdown, accessible [ici](https://github.com/rysebaert/qgis_data_shs/blob/master/index.Rmd)
- Les données utiles à l'atelier sont disponibles dans ce [dossier du dépôt](https://github.com/rysebaert/qgis_data_shs/tree/master/data). Elles sont structurées en deux sous dossier : `GEOM` inclut les données géométriques vectorielles ; `STAT` un fichier de données tabulaires (Excel), qui seront utilisées dans l'atelier associé à la formation. 

## Durée de la formation
Une journée

## Public
Pas de connaissance particulière en SIG, mais une appétence à la manipulation de données spatiales. 

## Déroulé 

### Information géographique ? 

- Les composantes de l'information géographique
- Où en trouver ?
- Formats de fichiers

### Systèmes d'Information Géographiques ? 

- Bref historique du développement des SIG
- Principes et fonctionnalités
- Principaux SIG
- Présentation de QGIS
- Import de données vectorielles et gestion des styles

### Systèmes de coordonnées de référence, projections

- Qu'est-ce qu'une projection ?
- Gérer les projections de ses couches géographiques dans QGIS
- Manipulations de base (données vectorielles) (11h30 - 12h45)

### Import de données tabulaires

- Jointure attributaire
- Jointure spatiale
- Sélections attributaires

### Géotraitements et statistiques spatiales

- Présentation et mise en pratique
- Modéliser sa chaîne de traitement

###  Cartographie thématique dans QGIS

- Éléments fondamentaux de sémiologie graphique
- Typologies (variables qualitatives)
- Cartes choroplèthes (variables quantitatives de taux)

### Mettre en page une carte
- Eléments fondamentaux sur l'habillage cartographique
- Créer une mise en page avec QGIS

## Contexte de production
Formation créée à partir d'un [cours de 20h](https://github.com/rysebaert/infogeo) destiné à des M1 non géographes, adapté pour l'occasion de la semaine [DATA-SHS](https://www.progedo.fr/services/plates-formes-universitaires-de-donnees/semaine-data-shs/) initiée par [PROGEDO](https://www.progedo.fr/) et la [Plateforme Universitaire de Données de Lille](https://pudl.meshs.fr/)
