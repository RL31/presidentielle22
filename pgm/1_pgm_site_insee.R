# adaptation du pgm téléchargeable ici : https://www.insee.fr/fr/statistiques/4176290?sommaire=4176305
# il faudrait affiner en équirépartissant les individus des carreaux ; mais moins gênant ici car 200m

# Ceci est un exemple de fonction permettant de calculer des agrégats sur des zones quelconques à
# partir des données carroyées à 200m au format csv.
# 
# Le principe de la fonction consiste à déterminer, pour une zone donnée, l'ensemble des carreaux qui
# la recouvrent puis à calculer les agrégats sur cet ensemble de carreaux. L'ensemble de carreaux 
# étant en général plus large que la zone, les agrégats obtenus seront des estimations qui tendent 
# à surestimer les valeurs réelles.
# 
# 4 paramètres sont à renseigner en entrée de la fonction : 
#   - l'emplacement du fichier de contours de la/les zone(s) d'intérêt (au format Shapefile)
#   - l'emplacement du fichier csv contenant les données carroyées
#   - la liste des codes Insee des communes sur lesquelles sont situées la/les zone(s) en géographie 2019
# (voir https://www.insee.fr/fr/information/3720946#titre-bloc-4)
#   - la liste des indicateurs pour lesquels des agrégats sont souhaités : par défaut, la fonction 
# calcule les agrégats de tous les indicateurs possibles
#
# Le fichier de résultats contient une ligne par zonage avec tous les agrégats souhaités ainsi
# qu'une variable (txPopImp) donnant le pourcentage de population vivant dans des carreaux imputés (voir
# documentation). Cette variable permet d'avoir une idée de la précision des agrégats calculés. Plus le taux 
# est élevé et moins les indicateurs seront fiables (en général).

# Packages utilisés
library(data.table) 
library(dplyr)
library(sf)
library(stringr)

# Liste des 28 indicateurs pour lesquels les agrégats sont calculés par défaut
listeIndicDef <- c("Ind","Men","Men_pauv","Men_1ind","Men_5ind","Men_prop","Men_fmp","Ind_snv",
                   "Men_surf","Men_coll","Men_mais","Log_av45","Log_45_70","Log_70_90","Log_ap90",
                   "Log_inc","Log_soc","Ind_0_3","Ind_4_5","Ind_6_10","Ind_11_17","Ind_18_24",
                   "Ind_25_39","Ind_40_54","Ind_55_64","Ind_65_79","Ind_80p","Ind_inc")


#' Fonction calculant les agrégats sur une zone à partir des carreaux qui la recouvrent
#'
#' @param cheminFichierContoursSHP Chemin des contours de la ou des zones
#' @param cheminFichierCSV Chemin du fichier csv des données carroyées à 200m
#' @param listeCodesCommune Liste des codes Insee des communes
#' @param listeVar Liste des agrégats souhaités
#'
#' @return Une table contenant une ligne pour chaque zone du fichier de contours
#' 
#' @examples 
#' calculAgregatsZones("c:/mesdocs/moncontour.shp", 
#'                     "c:/mesdocs/Filosofi2015_carreaux_200m_metropole.csv", 
#'                     c("01001","01002"), 
#'                     c("Ind_40_54","Ind_55_64"))
calculAgregatsZones <- function(cheminFichierContoursSHP, 
                                cheminFichierCSV, 
                                listeCodesCommune, 
                                listeIndic = listeIndicDef)
{
  
  # ajout des colonnes nécessaires par la suite : identifiant du carreau,
  # code commune du carreau, nombre d'individus du carreau et variable indiquant 
  # si les données du carreau sont des valeurs approchées
  listeIndic <- unique(c(listeIndic,"Idcar_200m","lcog_geo","Ind","I_est_200")) 
  
  # importation de la table des carreaux
  # et filtrage des observations selon le(s) code(s) commune(s)
  carreaux <- data.table::fread(cheminFichierCSV, select = c(listeIndic)) %>%
    dplyr::filter(lcog_geo %in% listeCodesCommune) %>%
    dplyr::mutate(I_est_200 = as.integer(I_est_200))
  
  # liste des identifiants Inspire des carreaux, à partir desquels
  # on récupère leurs coordonnées, leur taille et leur code epsg
  cIdcar_200m <- as.character(carreaux$Idcar_200m)
  
  epsg <- as.integer(str_sub(str_extract(carreaux[1,]$Idcar_200m, "CRS\\d+"), 4))
  
  tailleCarreaux <- unlist(lapply(X = cIdcar_200m, FUN = function(ligne){
    return(as.integer(str_sub(str_extract(ligne, "RES\\d+"), 4)))
  }))
  ordonneesCarreaux <- unlist(lapply(X = cIdcar_200m, FUN = function(ligne){
    return(as.integer(str_sub(str_extract(ligne, "N\\d+"), 2)))
  }))
  abscissesCarreaux <- unlist(lapply(X = cIdcar_200m, FUN = function(ligne){
    return(as.integer(str_sub(str_extract(ligne, "E\\d+"), 2)))
  }))
  
  # ajout de 2 colonnes donnant les coordonnées du coin inférieur gauche du carreau
  carreaux$x <- abscissesCarreaux
  carreaux$y <- ordonneesCarreaux
  
  # création d'une colonne geometry contenant les coordonnées des contours des carreaux
  # puis transformation en objets geométriques a l'aide du package sf
  carreaux$geometry <- sprintf("POLYGON ((%i %i, %i %i, %i %i, %i %i, %i %i))", 
                               carreaux$x, carreaux$y, 
                               carreaux$x + tailleCarreaux, carreaux$y, 
                               carreaux$x + tailleCarreaux, carreaux$y + tailleCarreaux, 
                               carreaux$x, carreaux$y + tailleCarreaux, 
                               carreaux$x, carreaux$y) 
  
  carreauxSf <- sf::st_as_sf(carreaux, wkt = "geometry", crs = epsg)
  
  # contours des zones sur lesquelles on souhaite connaître les agrégats
  zones <- sf::st_read(cheminFichierContoursSHP) %>%
    sf::st_transform(epsg)
  
  # intersection des carreaux avec les zones puis calcul des agrégats
  agregatsZones <- carreauxSf %>%
    sf::st_join(zones, join = st_intersects, left = FALSE) %>% 
    sf::st_set_geometry(NULL) %>%
    dplyr::mutate(popImp = I_est_200*Ind) %>%
    dplyr::select(-Idcar_200m, -lcog_geo, -I_est_200, -x, -y) %>%
    dplyr::group_by_at(setdiff(colnames(zones), "geometry")) %>%
    dplyr::summarise_all(sum) %>%
    dplyr::mutate(txPopImp = round(100*popImp/Ind, digits = 1)) %>%
    dplyr::select(-popImp) %>%
    dplyr::ungroup()
  
  return(agregatsZones)
}


filosofi_par_bdv17 <- calculAgregatsZones(cheminFichierContoursSHP="donnees/elections-2017-decoupage-des-bureaux-de-vote.shp", 
                                 cheminFichierCSV="donnees/Filosofi2017_carreaux_200m_met.csv", 
                                 listeCodesCommune=c("31555"),
                                 listeIndic=listeIndicDef)


filosofi_par_bdv22 <- calculAgregatsZones(cheminFichierContoursSHP="donnees/elec-2021-decoupage-bdv.shp", 
                                          cheminFichierCSV="donnees/Filosofi2017_carreaux_200m_met.csv", 
                                          listeCodesCommune=c("31555"),
                                          listeIndic=listeIndicDef)
