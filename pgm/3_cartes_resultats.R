library(tidyverse)
library(sf)
library(patchwork)

donnees17 <- st_read("donnees/elections-2017-decoupage-des-bureaux-de-vote.shp") %>%
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(read.csv("donnees/presid_2017_T1.csv",sep=";",encoding = "Latin10"),
            by="Numéro.du.bureau")

donnees22 <- st_read("donnees/elec-2021-decoupage-bdv.shp") %>%
  mutate(Numéro.du.bureau=str_pad(uniq_bdv,4,"left","0") ) %>% 
  st_transform(2154) %>% 
  left_join(read.csv("donnees/presid_2022_T1.csv",sep=";",encoding = "Latin10"),
            by="Numéro.du.bureau")

# abstention
donnees17 %>% 
  summarise(abst=sum(Nombre.d.abstentions)/sum(Nombre.d.inscrits)*100)

donnees22 %>% 
  summarise(abst=sum(Nombre.d.abstentions)/sum(Nombre.d.inscrits)*100)

comparaison_abstention <- donnees22 %>% 
  bind_rows(donnees17) %>% 
  mutate(tx_abst=Nombre.d.abstentions/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_abst),color="white")+
  scale_fill_steps2(name="Taux d'abstention (%)",low="white",high="black",nice.breaks=FALSE,breaks=c(20,30,40,50))+
  facet_wrap(~Année)+
  labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats des 1ers tours de l'élection présidentielle 2017 et 2022\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        strip.text = element_text(face="bold",size=15),
        plot.caption = element_text(size=8))

# principaux candidats
donnees22 %>% 
  mutate(pct=melenchon/Nombre.d.inscrits*100) %>% view()

comparaison_candidat <- function(CAND17="macron",CAND22="macron",COUL="darkorchid4",TITRE="Macron"){
  
  donnees <- donnees22 %>%
    select(Année,.data[[CAND22]],Nombre.d.inscrits) %>% 
    rename(candidat=.data[[CAND22]]) %>% 
    bind_rows(donnees17 %>% 
                select(Année,.data[[CAND17]],Nombre.d.inscrits) %>% 
                rename(candidat=.data[[CAND17]]))
  
  comparaison <-donnees %>%
    mutate(tx=candidat/Nombre.d.inscrits*100) %>% 
    ggplot()+
    geom_sf(aes(fill=tx),color="white")+
    geom_text(data=. %>% 
                st_drop_geometry() %>% 
                as.data.frame() %>% 
                group_by(Année) %>% 
                summarise(tx=round(sum(candidat)/sum(Nombre.d.inscrits)*100,0),
                          n=sum(candidat)),
              aes(label=paste0(tx," %")),x=569000,y=6285000,color=COUL, fontface="bold",size=10)+
    scale_fill_steps2(name=paste0("Vote ",str_to_title(TITRE),"\n(% d'inscrit.e.s)"),low="white",high=COUL)+
    facet_wrap(~Année)+
    labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats des 1ers tours de l'élection présidentielle 2017 et 2022\nTraitements et erreurs : @Re_Mi_La")+
    theme_void()+
    theme(legend.position = "top",
          strip.text = element_text(face="bold",size=15),
          plot.caption = element_text(size=8),
          plot.background = element_rect(fill="white",color="white"))  
  
  ggsave(paste0("sorties/comparaison_17_22_",TITRE,".jpg"),width=14,height = 7,units = "in",dpi=100)
  
}

comparaison_candidat(CAND17 = "macron",CAND22="macron",COUL = "darkorchid4",TITRE="Macron")
comparaison_candidat(CAND17 = "fillon",CAND22="pecresse",COUL = "blue",TITRE = "Fillon-Pécresse")
comparaison_candidat(CAND17 = "lepen",CAND22="lepen",COUL = "peru",TITRE = "Le Pen")
comparaison_candidat(CAND17 = "lepen",CAND22="zemmour",COUL = "brown",TITRE = "Le Pen 2017 - Zemmour 2022")

comparaison_candidat(CAND17 = "arthaud",CAND22="arthaud",COUL = "darkred",TITRE = "Arthaud")
comparaison_candidat(CAND17 = "poutou",CAND22="poutou",COUL = "darkred",TITRE = "Poutou")
comparaison_candidat(CAND17 = "lassalle",CAND22="lassalle",COUL = "blue",TITRE = "Lassalle")
comparaison_candidat(CAND17 = "dupontaignan",CAND22="dupontaignan",COUL = "darkblue",TITRE = "Dupont-Aignan")
comparaison_candidat(CAND17 = "hamon",CAND22="hidalgo",COUL = "deeppink2",TITRE = "Hamon 2017 - Hidalgo 2022")
comparaison_candidat(CAND17 = "hamon",CAND22="jadot",COUL = "chartreuse4",TITRE = "Hamon 2017 - Jadot 2022")


# gauche droite
extgauche17 <- donnees17 %>% 
  mutate(tx_extgauche=(arthaud+poutou)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extgauche),color="white")+
  scale_fill_steps2(name="Vote extrême gauche:\nN.A., P.P.\n(% d'inscrits)",low="white",high="darkred", breaks=c(1,2))+
  labs(title = "2017")+
  theme_void()+
  theme(legend.position = "top")
gauche17 <- donnees17 %>% 
  mutate(tx_gauche=(melenchon+hamon)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_gauche),color="white")+
  scale_fill_steps2(name="Vote gauche:\nJ.-L.M., B.H.\n(% d'inscrits)",low="white",high="red2", breaks=c(10,20,30,40))+
  theme_void()+
  theme(legend.position = "top")
droite17 <-donnees17 %>% 
  mutate(tx_droite=(macron+fillon+asselineau+cheminade+lassalle)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_droite),color="white")+
  scale_fill_steps2(name="Vote droite:\nE.M., F.F., F.A.,J. C., J.L.\n(% d'inscrits)",low="white",high="royalblue4", breaks=c(10,20,30,40))+
  theme_void()+
  theme(legend.position = "top")
extdroite17 <- donnees17 %>% 
  mutate(tx_extdroite=(lepen+dupontaignan)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extdroite),color="white")+
  scale_fill_steps2(name="Vote extrême droite:\nN.D.-A., M.LP\n(% d'inscrits)",low="white",high="peru", breaks=c(5,10,15,20))+
  labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2017\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))

(extgauche17+gauche17+droite17+extdroite17)

extgauche22 <- donnees22 %>% 
  mutate(tx_extgauche=(arthaud+poutou)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extgauche),color="white")+
  scale_fill_steps2(name="Vote extrême gauche:\nN.A., P.P.\n(% d'inscrits)",low="white",high="darkred", breaks=c(1,2))+
  labs(title = "2022")+
  theme_void()+
  theme(legend.position = "top")
gauche22 <- donnees22 %>% 
  mutate(tx_gauche=(melenchon+hidalgo+roussel+jadot)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_gauche),color="white")+
  scale_fill_steps2(name="Vote gauche:\nJ.-L.M., Y.J., A.H., F.R.\n(% d'inscrits)",low="white",high="red2", breaks=c(10,20,30,40))+
  theme_void()+
  theme(legend.position = "top")
droite22 <-donnees22 %>% 
  mutate(tx_droite=(macron+pecresse+lassalle)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_droite),color="white")+
  scale_fill_steps2(name="Vote droite:\nE.M., V.P., J.L.\n(% d'inscrits)",low="white",high="royalblue4", breaks=c(10,20,30,40))+
  theme_void()+
  theme(legend.position = "top")
extdroite22 <- donnees22 %>% 
  mutate(tx_extdroite=(lepen+dupontaignan+zemmour)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extdroite),color="white")+
  scale_fill_steps2(name="Vote extrême droite:\nN.D.-A., E.Z., M.LP\n(% d'inscrits)",low="white",high="peru", breaks=c(5,10,15,20))+
  labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2022\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))

(extgauche17|gauche17|droite17|extdroite17)/(extgauche22|gauche22|droite22|extdroite22)


donnees22 %>% 
  mutate(tx_extdroite=(lepen+dupontaignan+zemmour)/Nombre.d.inscrits*100,
         tx_droite=(macron+pecresse+lassalle)/Nombre.d.inscrits*100,
         tx_gauche=(melenchon+hidalgo+roussel+jadot)/Nombre.d.inscrits*100,
         tx_extgauche=(arthaud+poutou)/Nombre.d.inscrits*100,
         tx_nexp=(Nombre.d.abstentions+Nombre.de.bulletins.blancs+Nombre.de.bulletins.nuls)/Nombre.d.inscrits*100,
         vainqueur = case_when(pmax(tx_extdroite,tx_droite,tx_gauche,tx_extgauche,tx_nexp)==tx_extdroite ~ "exd",
                               pmax(tx_extdroite,tx_droite,tx_gauche,tx_extgauche,tx_nexp)==tx_droite ~ "droite",
                               pmax(tx_extdroite,tx_droite,tx_gauche,tx_extgauche,tx_nexp)==tx_gauche ~ "gauche",
                               pmax(tx_extdroite,tx_droite,tx_gauche,tx_extgauche,tx_nexp)==tx_extgauche ~ "exg",
                               TRUE ~ "nexp")) %>% 
  ggplot()+
  geom_sf(aes(fill=vainqueur),color="white")+
  scale_fill_manual(name="Meilleur score",
                    values = c("nexp"="gray20","exg"="darkred","gauche"="red2","droite"="blue","exd"="peru"),
                    labels =  c("nexp"="Non exprimés","exg"="Extrême gauche","gauche"="Gauche","droite"="Droite","exd"="Extrême droite"))+
  labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2022\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))

donnees22 %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  mutate(tx_extdroite=(lepen+dupontaignan+zemmour)/Nombre.d.inscrits*100,
         tx_droite=(macron+pecresse+lassalle)/Nombre.d.inscrits*100,
         tx_gauche=(melenchon+hidalgo+roussel+jadot)/Nombre.d.inscrits*100,
         tx_extgauche=(arthaud+poutou)/Nombre.d.inscrits*100,
         tx_nexp=(Nombre.d.abstentions+Nombre.de.bulletins.blancs+Nombre.de.bulletins.nuls)/Nombre.d.inscrits*100,
         vainqueur = case_when(pmax(tx_extdroite,tx_droite,tx_gauche,tx_extgauche,tx_nexp)==tx_extdroite ~ "exd",
                               pmax(tx_extdroite,tx_droite,tx_gauche,tx_extgauche,tx_nexp)==tx_droite ~ "droite",
                               pmax(tx_extdroite,tx_droite,tx_gauche,tx_extgauche,tx_nexp)==tx_gauche ~ "gauche",
                               pmax(tx_extdroite,tx_droite,tx_gauche,tx_extgauche,tx_nexp)==tx_extgauche ~ "exg",
                               TRUE ~ "nexp")) %>% 
  filter(vainqueur=="exd") %>% 
  select(vainqueur,lieu_vote)
  count(vainqueur)