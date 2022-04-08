
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
lien_bureaux <- "https://data.toulouse-metropole.fr/explore/dataset/elec-2021-decoupage-bdv/download/?format=shp&timezone=Europe/Berlin&lang=fr"
download.file(url=lien_bureaux,destfile = "donnees/elec-2021-decoupage-bdv.zip")
unzip(zipfile = "donnees/elec-2021-decoupage-bdv.zip",
      exdir = "donnees")
file.remove("donnees/elec-2021-decoupage-bdv.zip")

###############################################
# Chargement des résultats par bureau de vote #
###############################################
lien_resultats <- "https://data.toulouse-metropole.fr/explore/dataset/rg-2021-2t-vt-resultats/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
download.file(url=lien_resultats,destfile = "donnees/rg-2021-2t-vt-resultats.csv")
