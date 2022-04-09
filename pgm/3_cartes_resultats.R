library(tidyverse)
library(sf)

resultats_T1_17 <- read.csv("donnees/presid_2017_T1.csv",sep=";",encoding = "Latin10")


# abstention
abstention <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  mutate(tx_abst=Nombre.d.abstentions/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_abst),color="white")+
  scale_fill_stepsn(name="Taux d'abstention (%)",colors = c("white","black"))+
  theme_void()+
  theme(legend.position = "top")

# principaux candidats

macron <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  mutate(tx_macron=Nombre.de.voix.du.candidat.2/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_macron),color="white")+
  scale_fill_stepsn(name="Vote Macron\n(% d'inscrits)",colors = c("white","darkorchid4"))+
  theme_void()+
  theme(legend.position = "top")

lepen <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  mutate(tx_lepen=Nombre.de.voix.du.candidat.1/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_lepen),color="white")+
  scale_fill_stepsn(name="Vote Le Pen\n(% d'inscrits)",colors = c("white","peru"))+
  labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2017\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))

melenchon <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  mutate(tx_melench=Nombre.de.voix.du.candidat.8/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_melench),color="white")+
  scale_fill_stepsn(name="Vote Mélenchon\n(% d'inscrits)",colors = c("white","firebrick2"))+
  theme_void()+
  theme(legend.position = "top")

melenchon+macron+lepen

# gauche droite
extgauche <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  mutate(tx_extgauche=(Nombre.de.voix.du.candidat.4+Nombre.de.voix.du.candidat.5)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extgauche),color="white")+
  scale_fill_stepsn(name="Vote extrême gauche:\nN.A., P.P.\n(% d'inscrits)",colors = c("white","red4"))+
  theme_void()+
  theme(legend.position = "top")
gauche <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  mutate(tx_gauche=(Nombre.de.voix.du.candidat.3+Nombre.de.voix.du.candidat.8)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_gauche),color="white")+
  scale_fill_stepsn(name="Vote gauche:\nJ.-L.M., B.H.\n(% d'inscrits)",colors = c("white","red2"))+
  theme_void()+
  theme(legend.position = "top")
droite <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  mutate(tx_droite=(Nombre.de.voix.du.candidat.2+Nombre.de.voix.du.candidat.6+Nombre.de.voix.du.candidat.7+Nombre.de.voix.du.candidat.9+Nombre.de.voix.du.candidat.10)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_droite),color="white")+
  scale_fill_stepsn(name="Vote droite:\nE.M., F.F., F.A.,J. C., J.L.\n(% d'inscrits)",colors = c("white","royalblue4"))+
  theme_void()+
  theme(legend.position = "top")
extdroite <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  mutate(tx_extdroite=(Nombre.de.voix.du.candidat+Nombre.de.voix.du.candidat.1)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extdroite),color="white")+
  scale_fill_stepsn(name="Vote extrême droite:\nN.D.-A., M.LP\n(% d'inscrits)",colors = c("white","peru"))+
  labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2017\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))

abstention+(extgauche+gauche+droite+extdroite)
