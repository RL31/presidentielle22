
##############################################
# Chargement des données fiscales (insee.fr) #
##############################################
lien_filosofi_shp <- "https://www.insee.fr/fr/statistiques/fichier/6215138/Filosofi2017_carreaux_200m_shp.zip"
download.file(url=lien_filosofi_shp,destfile = "donnees/Filosofi2017_carreaux_200m_shp.zip")
unzip(zipfile = "donnees/Filosofi2017_carreaux_200m_shp.zip",
      exdir = "donnees")
archive::archive_extract(archive = "donnees/Filosofi2017_carreaux_200m_shp.7z",
                         dir = "donnees")

file.remove("donnees/Filosofi2017_carreaux_200m_shp.zip")
file.remove("donnees/Filosofi2017_carreaux_200m_shp.7z")

##############################################################
# Chargement des contours des bureaux de vote (open data TM) #
##############################################################
lien_bureaux <- "https://data.toulouse-metropole.fr/explore/dataset/elections-2017-decoupage-des-bureaux-de-vote/download/?format=shp&timezone=Europe/Berlin&lang=fr"
download.file(url=lien_bureaux,destfile = "donnees/elections-2017-decoupage-bureaux-de-vote.zip")
unzip(zipfile = "donnees/elections-2017-decoupage-des-bureaux-de-vote.zip",
      exdir = "donnees")
file.remove("donnees/elections-2017-decoupage-des-bureaux-de-vote.zip")

###############################################
# Chargement des résultats par bureau de vote #
###############################################
lien_resultats <- "https://data.toulouse-metropole.fr/explore/dataset/election-presidentielle-2017-1er-tour-resultats-ville-de-toulouse/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
download.file(url=lien_resultats,destfile = "donnees/presid_2017_T1.csv")
