library(osmdata)
library(sf)
library(tidyverse)

proxy <- curl::ie_get_proxy_for_url()
httr::set_config(httr::use_proxy(proxy))

liste_villes <- c("Lyon","Bordeaux","Roubaix","Rennes","Nîmes","Colmar","Mulhouse",
                  "Grenoble","Angers","Toulouse","Poitiers", # Gouesnou
                  "Nantes","Le Havre","Orléans","Brest","Strasbourg")

extraction_gendarmeries <- function(VILLE){
  print(VILLE)
  requete_police <- getbb(VILLE) %>%
    opq(timeout = 1000) %>%
    add_osm_feature("amenity","police")
  
  gendarmeries <- osmdata_sf(requete_police)$osm_points %>%
    filter(str_detect(str_to_lower(name),"gendarmerie")) %>%
    st_transform(2154)  
  
  requete_terrmilit <- getbb(VILLE) %>%
    opq(timeout = 1000) %>%
    add_osm_feature("landuse","military")
  
  terrmilit <- osmdata_sf(requete_terrmilit)$osm_polygons %>%
    st_centroid() %>%
    st_transform(2154)  
  
  requete_caserne <- getbb(VILLE) %>%
    opq(timeout = 1000) %>%
    add_osm_feature("military","barracks")
  
  caserne <- osmdata_sf(requete_caserne)$osm_points %>%
    st_transform(2154)  
  
  militaires <- gendarmeries %>%
    bind_rows(terrmilit,caserne) %>%
    select(geometry)
  
  return(militaires)
}

militaires <- map_df(liste_villes,extraction_gendarmeries)

bdv_militaires <- st_intersection(contours,militaires %>% select(geometry)) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  count(Libellé.de.la.commune,bdv) %>%
  mutate(militaires=if_else(n==1,"1","2"))%>%
  select(Libellé.de.la.commune,bdv,militaires)

bdv_militaires %>%
  distinct() %>%
  count(Libellé.de.la.commune)

saveRDS(bdv_militaires,"donnees/bdv_militaires.RDS")



liste_villes <- c("Lyon","Bordeaux","Roubaix","Rennes","Nîmes","Colmar","Mulhouse",
                  "Grenoble","Angers","Toulouse","Poitiers", # Gouesnou
                  "Nantes","Le Havre","Orléans","Brest","Strasbourg")

extraction_religieux <- function(VILLE){
  print(VILLE)
 VILLE="Angers"
  requete_monasteres <- getbb(VILLE) %>%
    opq(timeout = 1000) %>%
    add_osm_feature("amenity","monastery")
  
  monasteres <- osmdata_sf(requete_monasteres)
    
  length(monasteres)
  
  monasteres2 <-  monasteres$osm_points %>%
    bind_rows(monasteres$osm_polygons %>% st_centroid())%>%
    filter(!is.na(community))%>%
    st_transform(2154) %>%
    select(geometry)
  
  return(monasteres2)
}

religieux <- map_df(liste_villes,extraction_religieux)

bdv_religieux <- st_intersection(contours,religieux %>% select(geometry)) %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  select(Libellé.de.la.commune,bdv)