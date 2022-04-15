library(tidyverse)
library(sf)
library(patchwork)

#########################################
# Chargement des résultats 2017 et 2022 #
#########################################
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

#######################################
# Quelques résultats sur l'abstention #
#######################################

donnees17 %>% 
  summarise(abst=sum(Nombre.d.abstentions)/sum(Nombre.d.inscrits)*100)

donnees22 %>% 
  summarise(abst=sum(Nombre.d.abstentions)/sum(Nombre.d.inscrits)*100)

donnees22 %>% 
  bind_rows(donnees17) %>% 
  mutate(tx_abst=Nombre.d.abstentions/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_abst),color="white")+
  scale_fill_steps2(name="Taux d'abstention (%)",low="white",high="gray30" ,breaks=c(30,50,70))+
  facet_wrap(~Année)+
  labs(title="L'abstention diminue dans les bureaux des quartiers populaires",
       caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats des 1ers tours de l'élection présidentielle 2017 et 2022\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        strip.text = element_text(face="bold",size=15),
        plot.caption = element_text(size=8))

########################################################
# Comparaison des résultats des candidats 2022 vs 2017 #
########################################################
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
comparaison_candidat(CAND17 = "lepen",CAND22="zemmour",COUL = "tan4",TITRE = "Le Pen 2017 - Zemmour 2022")
comparaison_candidat(CAND17 = "arthaud",CAND22="arthaud",COUL = "darkred",TITRE = "Arthaud")
comparaison_candidat(CAND17 = "poutou",CAND22="poutou",COUL = "darkred",TITRE = "Poutou")
comparaison_candidat(CAND17 = "lassalle",CAND22="lassalle",COUL = "blue",TITRE = "Lassalle")
comparaison_candidat(CAND17 = "dupontaignan",CAND22="dupontaignan",COUL = "darkblue",TITRE = "Dupont-Aignan")
comparaison_candidat(CAND17 = "hamon",CAND22="hidalgo",COUL = "deeppink2",TITRE = "Hamon 2017 - Hidalgo 2022")
comparaison_candidat(CAND17 = "hamon",CAND22="jadot",COUL = "chartreuse4",TITRE = "Hamon 2017 - Jadot 2022")

######################################
# Evolution 17-22 des 4 grands blocs #
######################################
extgauche17 <- donnees17 %>% 
  mutate(tx_extgauche=(arthaud+poutou)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extgauche),color="white")+
  geom_text(data=. %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              group_by(Année) %>% 
              summarise(tx=round(sum(arthaud+poutou)/sum(Nombre.d.inscrits)*100,0),
                        n=sum(arthaud+poutou)),
            aes(label=paste0(tx," %")),x=569000,y=6285000,color="darkred", fontface="bold",size=10)+
  scale_fill_steps2(name="Vote extrême gauche:\nN.A., P.P.\n(% d'inscrits)",low="white",high="darkred", breaks=c(1,2))+
  labs(title = "2017")+
  theme_void()+
  theme(legend.position = "top")
gauche17 <- donnees17 %>% 
  mutate(tx_gauche=(melenchon+hamon)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_gauche),color="white")+
  geom_text(data=. %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              group_by(Année) %>% 
              summarise(tx=round(sum(melenchon+hamon)/sum(Nombre.d.inscrits)*100,0),
                        n=sum(melenchon+hamon)),
            aes(label=paste0(tx," %")),x=569000,y=6285000,color="red2", fontface="bold",size=10)+
  scale_fill_steps2(name="Vote gauche:\nJ.-L.M., B.H.\n(% d'inscrits)",low="white",high="red2", breaks=c(10,20,30,40))+
  theme_void()+
  theme(legend.position = "top")
droite17 <-donnees17 %>% 
  mutate(tx_droite=(macron+fillon+asselineau+cheminade+lassalle)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_droite),color="white")+
  geom_text(data=. %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              group_by(Année) %>% 
              summarise(tx=round(sum(macron+fillon+asselineau+cheminade+lassalle)/sum(Nombre.d.inscrits)*100,0),
                        n=sum(macron+fillon+asselineau+cheminade+lassalle)),
            aes(label=paste0(tx," %")),x=569000,y=6285000,color="royalblue4", fontface="bold",size=10)+
  scale_fill_steps2(name="Vote droite:\nE.M., F.F., F.A.,J. C., J.L.\n(% d'inscrits)",low="white",high="royalblue4", breaks=c(10,20,30,40))+
  theme_void()+
  theme(legend.position = "top")
extdroite17 <- donnees17 %>% 
  mutate(tx_extdroite=(lepen+dupontaignan)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extdroite),color="white")+
  geom_text(data=. %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              group_by(Année) %>% 
              summarise(tx=round(sum(lepen+dupontaignan)/sum(Nombre.d.inscrits)*100,0),
                        n=sum(lepen+dupontaignan)),
            aes(label=paste0(tx," %")),x=569000,y=6285000,color="peru", fontface="bold",size=10)+
  scale_fill_steps2(name="Vote extrême droite:\nN.D.-A., M.LP\n(% d'inscrits)",low="white",high="peru", breaks=c(5,10,15,20))+
  labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2017\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))

extgauche22 <- donnees22 %>% 
  mutate(tx_extgauche=(arthaud+poutou)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extgauche),color="white")+
  geom_text(data=. %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              group_by(Année) %>% 
              summarise(tx=round(sum(arthaud+poutou)/sum(Nombre.d.inscrits)*100,0),
                        n=sum(arthaud+poutou)),
            aes(label=paste0(tx," %")),x=569000,y=6285000,color="darkred", fontface="bold",size=10)+
  scale_fill_steps2(name="Vote extrême gauche:\nN.A., P.P.\n(% d'inscrits)",low="white",high="darkred", breaks=c(1,2))+
  labs(title = "2022")+
  theme_void()+
  theme(legend.position = "top")
gauche22 <- donnees22 %>% 
  mutate(tx_gauche=(melenchon+hidalgo+roussel+jadot)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_gauche),color="white")+
  geom_text(data=. %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              group_by(Année) %>% 
              summarise(tx=round(sum(melenchon+hidalgo+roussel+jadot)/sum(Nombre.d.inscrits)*100,0),
                        n=sum(melenchon+hidalgo+roussel+jadot)),
            aes(label=paste0(tx," %")),x=569000,y=6285000,color="red2", fontface="bold",size=10)+
  scale_fill_steps2(name="Vote gauche:\nJ.-L.M., Y.J., A.H., F.R.\n(% d'inscrits)",low="white",high="red2", breaks=c(10,20,30,40))+
  theme_void()+
  theme(legend.position = "top")
droite22 <-donnees22 %>% 
  mutate(tx_droite=(macron+pecresse+lassalle)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_droite),color="white")+
  geom_text(data=. %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              group_by(Année) %>% 
              summarise(tx=round(sum(macron+pecresse+lassalle)/sum(Nombre.d.inscrits)*100,0),
                        n=sum(macron+pecresse+lassalle)),
            aes(label=paste0(tx," %")),x=569000,y=6285000,color="royalblue4", fontface="bold",size=10)+
  scale_fill_steps2(name="Vote droite:\nE.M., V.P., J.L.\n(% d'inscrits)",low="white",high="royalblue4", breaks=c(10,20,30,40))+
  theme_void()+
  theme(legend.position = "top")
extdroite22 <- donnees22 %>% 
  mutate(tx_extdroite=(lepen+dupontaignan+zemmour)/Nombre.d.inscrits*100) %>% 
  ggplot()+
  geom_sf(aes(fill=tx_extdroite),color="white")+
  geom_text(data=. %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              group_by(Année) %>% 
              summarise(tx=round(sum(lepen+dupontaignan+zemmour)/sum(Nombre.d.inscrits)*100,0),
                        n=sum(lepen+dupontaignan+zemmour)),
            aes(label=paste0(tx," %")),x=569000,y=6285000,color="peru", fontface="bold",size=10)+
  scale_fill_steps2(name="Vote extrême droite:\nN.D.-A., E.Z., M.LP\n(% d'inscrits)",low="white",high="peru", breaks=c(5,10,15,20))+
  labs(caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2022\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))

(extgauche17|gauche17|droite17|extdroite17)/(extgauche22|gauche22|droite22|extdroite22)

######################################
# Qui est en tête dans chaque bureau #
######################################
# carte
donnees22 %>% 
  mutate(tx_lepen=(lepen)/Nombre.d.inscrits*100,
         tx_zemmour=(zemmour)/Nombre.d.inscrits*100,
         tx_macron=(macron)/Nombre.d.inscrits*100,
         tx_melenchon=(melenchon)/Nombre.d.inscrits*100,
         tx_jadot=(jadot)/Nombre.d.inscrits*100,
         tx_nexp=(Nombre.d.abstentions+Nombre.de.bulletins.blancs+Nombre.de.bulletins.nuls)/Nombre.d.inscrits*100,
         vainqueur = case_when(pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_lepen ~ "lepen",
                               pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_zemmour ~ "zemmour",
                               pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_macron ~ "macron",
                               pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_melenchon ~ "melenchon",
                               pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_nexp ~ "nexprimes",
                               TRUE ~ "nexp")) %>% 
  ggplot()+
  geom_sf(aes(fill=vainqueur),color="white")+
  geom_text(data=donnees22 %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              ungroup() %>% 
              summarise(tx_macron=round(sum(macron)/sum(Nombre.d.inscrits)*100,0),
                        tx_melenchon=round(sum(melenchon)/sum(Nombre.d.inscrits)*100,0)) %>% 
              pivot_longer(cols=c("tx_macron","tx_melenchon"), names_to="indic", values_to="tx") %>% 
              mutate(x=if_else(indic=="tx_macron",568000,568000),
                     y=if_else(indic=="tx_macron",6285000,6286000),
                     couleur = if_else(indic=="tx_macron","darkorchid2","tomato2")),
            aes(label=paste0(tx," %"),x=x,y=y,color=couleur), fontface="bold",size=10)+
  scale_color_identity()+
  scale_fill_manual(name="Candidat en tête",
                    values = c("melenchon"="tomato2","macron"="darkorchid2","nexprimes"="gray90"),
                    labels =  c("melenchon"="Mélenchon","macron"="Macron","nexprimes"="Non exprimés"))+
  labs(title = "Avec 28 % des voix des inscrit-e-s, Mélenchon est en tête\ndans 163 bureaux\n ",
    caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2022\nTraitements et erreurs : @Re_Mi_La")+
  theme_void()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))
  guides(fill="none")

# fréquence vainqueur
donnees22 %>% 
  mutate(tx_lepen=(lepen)/Nombre.d.inscrits*100,
         tx_zemmour=(zemmour)/Nombre.d.inscrits*100,
         tx_macron=(macron)/Nombre.d.inscrits*100,
         tx_melenchon=(melenchon)/Nombre.d.inscrits*100,
         tx_jadot=(jadot)/Nombre.d.inscrits*100,
         tx_nexp=(Nombre.d.abstentions+Nombre.de.bulletins.blancs+Nombre.de.bulletins.nuls)/Nombre.d.inscrits*100,
         vainqueur = case_when(pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_lepen ~ "lepen",
                               pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_zemmour ~ "zemmour",
                               pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_macron ~ "macron",
                               pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_melenchon ~ "melenchon",
                               pmax(tx_lepen,tx_zemmour,tx_melenchon,tx_jadot,tx_macron,tx_nexp)==tx_nexp ~ "nexprimes",
                               TRUE ~ "nexp")) %>% 
  count(vainqueur)

donnees22 %>% 
  bind_rows(donnees17) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  ungroup() %>% 
  group_by(Année) %>% 
  summarise(tx_macron=round(sum(macron)/sum(Nombre.d.inscrits)*100,0),
            tx_melenchon=round(sum(melenchon)/sum(Nombre.d.inscrits)*100,0),
            nmac=sum(macron),
            nmel=sum(melenchon))

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

###################################
# A qui profite la mobilisation ? #
###################################
donnees22 %>%
  bind_rows(donnees17 %>% rename(uniq_bdv=bv2017)) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  mutate(tx_abstention= Nombre.d.abstentions/Nombre.d.inscrits*100,
         tx_lepen=(lepen)/Nombre.d.inscrits*100,
         tx_zemmour=(zemmour)/Nombre.d.inscrits*100,
         tx_macron=(macron)/Nombre.d.inscrits*100,
         tx_melenchon=(melenchon)/Nombre.d.inscrits*100 ) %>% 
  pivot_wider(id_cols = "uniq_bdv",names_from="Année",values_from=c(starts_with("tx_"))) %>% 
  mutate(ecart_abstention=tx_abstention_2022-tx_abstention_2017,
         ecart_melenchon=tx_melenchon_2022-tx_melenchon_2017) %>% 
  select(uniq_bdv,ecart_abstention,tx_melenchon_2022,tx_lepen_2022,tx_macron_2022,tx_zemmour_2022) %>% 
  pivot_longer(cols=c(starts_with("tx_")),names_to="candidat",values_to="tx") %>% 
  filter(ecart_abstention>-15) %>% 
  ggplot(aes(x=ecart_abstention,y=tx,color=candidat,fill=candidat))+
  geom_point()+
  geom_smooth()+
  scale_color_manual(name="",
                     values = c("tx_melenchon_2022"="tomato2","tx_macron_2022"="darkorchid2","tx_zemmour_2022"="brown","tx_lepen_2022"="peru"),
                     labels =  c("tx_melenchon_2022"="Mélenchon","tx_macron_2022"="Macron","tx_zemmour_2022"="Zemmour","tx_lepen_2022"="Le Pen"))+
  scale_fill_manual(name="",
                    values = c("tx_melenchon_2022"="tomato2","tx_macron_2022"="darkorchid2","tx_zemmour_2022"="brown","tx_lepen_2022"="peru"),
                    labels =  c("tx_melenchon_2022"="Mélenchon","tx_macron_2022"="Macron","tx_zemmour_2022"="Zemmour","tx_lepen_2022"="Le Pen"))+
  labs(x="Variation de l'abstention entre 2017 et 2022",
       y="Vote en % d'inscrit-e-s",
       title="Mélenchon réussit mieux là où l'abstention diminue",
       caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour de l'élection présidentielle 2022\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))+
  guides(fill="none")



donnees22 %>%
  bind_rows(donnees17 %>% rename(uniq_bdv=bv2017)) %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  pivot_wider(id_cols = "uniq_bdv",names_from="Année",
              values_from=c("macron","melenchon","jadot","lepen","fillon","pecresse","zemmour","hidalgo","dupontaignan","hamon","Nombre.d.abstentions")) %>% 
  mutate(ecartabstention=(Nombre.d.abstentions_2022-Nombre.d.abstentions_2017),#/Nombre.d.abstentions_2017*100,
         ecart_melenchon=(melenchon_2022-melenchon_2017),#/melenchon_2017*100,
         ecart_macron=(macron_2022 - macron_2017),#/macron_2017*100,
         ecart_lr=(pecresse_2022 - fillon_2017),#/fillon_2017*100,
         ecart_extdroite = (lepen_2022+dupontaignan_2022+zemmour_2022-lepen_2017-dupontaignan_2017),#/(lepen_2017+dupontaignan_2017)*100,
         ecart_ecosoc = (jadot_2022+hidalgo_2022-hamon_2017),#/hamon_2017*100 
  ) %>% 
  select(uniq_bdv,starts_with("ecart")) %>% 
  pivot_longer(cols=c(starts_with("ecart_")),names_to="candidat",values_to="ecart") %>% 
  ggplot(aes(x=ecartabstention,y=ecart,color=candidat,fill=candidat))+
  geom_hline(yintercept = 0,color="black",size=1)+
  geom_point()+
  geom_smooth()+
  coord_equal()+
  scale_color_manual(name="",
                     values = c("ecart_melenchon"="tomato2",
                                "ecart_macron"="darkorchid4",
                                "ecart_extdroite"="peru",
                                "ecart_ecosoc"="chartreuse4",
                                "ecart_lr"="blue"))+
  scale_fill_manual(name="",
                    values = c("ecart_melenchon"="tomato2",
                               "ecart_macron"="darkorchid4",
                               "ecart_extdroite"="peru",
                               "ecart_lr"="blue",
                               "ecart_ecosoc"="chartreuse4"))+
  labs(x="Evolution de l'abstention entre 2017 et 2022 (%)",
       y="Evolution du  nombre de voix (%)",
       title="Mélenchon et l'extrême droite améliorent leurs scores là où l'abstention diminue",
       caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour des élections présidentielles 2017 et 2022\nTraitements et erreurs : @Re_Mi_La")+
  facet_wrap(~candidat,labeller = as_labeller(c("ecart_melenchon"="Mélenchon",
                                                "ecart_macron"="Macron",
                                                "ecart_lr"="Fillon, Pécresse",
                                                "ecart_extdroite"="Le Pen, Zemmour, Dupont-Aignan",
                                                "ecart_ecosoc"="Hamon, Jadot, Hidalgo")))+
  theme_minimal()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))+
  guides(fill="none",color="none")


######################################################################################
# Qui récupère le vote Fillon 2017 ? etude des corrélations avec le vote Fillon 2017 #
######################################################################################
donnees17  %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  rename(uniq_bdv=bv2017,nb17=Nombre.d.inscrits,macron17=macron,lepen17=lepen) %>% 
  mutate(tx_lepen17=lepen17/nb17*100,
         tx_macron17=macron17/nb17*100) %>% 
  left_join(donnees22 %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              rename(lepen22=lepen,macron22=macron),by="uniq_bdv") %>%
  mutate(tx_fillon=fillon/nb17*100,
         tx_zemmour=zemmour/Nombre.d.inscrits*100,
         tx_lepen22=lepen22/Nombre.d.inscrits*100,
         tx_macron22=macron22/Nombre.d.inscrits*100,
         tx_pecresse=pecresse/Nombre.d.inscrits*100) %>% 
  select(uniq_bdv,tx_fillon,tx_zemmour,tx_lepen22,tx_macron22,tx_macron17,tx_lepen17,tx_pecresse) %>% 
  pivot_longer(cols=c("tx_zemmour","tx_lepen22","tx_lepen17","tx_macron22","tx_macron17","tx_pecresse"),names_to="candidat",values_to="taux") %>% 
  ggplot(aes(x=tx_fillon,y=taux,color=candidat))+
  geom_abline(slope = 1)+
  geom_point(alpha=.7)+
  coord_equal()+
  scale_color_manual(values = c("tx_zemmour"="tan4","tx_lepen22"="peru","tx_lepen17"="peru","tx_macron22"="darkorchid4",
    "tx_macron17"="darkorchid4","tx_pecresse"="blue"))+
  labs(x="Vote Fillon en 2017 (% des inscrit-e-s)",
       y="Vote (% des inscrit-e-s)",
       title="Corrélation des votes de droite et d'extrême droite avec le vote Fillon 2017",
       caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour des élections présidentielles 2017 et 2022\nTraitements et erreurs : @Re_Mi_La")+
  facet_wrap(facets=~candidat,
             nrow = 1,
             labeller = as_labeller(c("tx_zemmour"="Vote Zemmour corr.= 0,86",
                                                "tx_lepen17"="Vote Le Pen 2017 corr.= -0,34",
                                                "tx_lepen22"="Vote Le Pen 2022 corr.= -0,06",
                                                "tx_macron17"="Vote Macron 2017 corr.= 0,56",
                                                "tx_macron22"="Vote Macron 2022 corr.= 0,87",
                                                "tx_pecresse"="Vote Pécresse corr.= 0,90")))+
  theme_minimal()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))+
  guides(color="none")

# pour les corr
donnees17  %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  rename(uniq_bdv=bv2017,nb17=Nombre.d.inscrits,macron17=macron,lepen17=lepen) %>% 
  mutate(tx_lepen17=lepen17/nb17*100,
         tx_macron17=macron17/nb17*100) %>% 
  left_join(donnees22 %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              rename(lepen22=lepen,macron22=macron),by="uniq_bdv") %>%
  mutate(tx_fillon=fillon/nb17*100,
         tx_zemmour=zemmour/Nombre.d.inscrits*100,
         tx_lepen22=lepen22/Nombre.d.inscrits*100,
         tx_macron22=macron22/Nombre.d.inscrits*100,
         tx_pecresse=pecresse/Nombre.d.inscrits*100) %>% 
  select(uniq_bdv,tx_fillon,tx_zemmour,tx_lepen22,tx_macron22,tx_macron17,tx_lepen17,tx_pecresse) %>% 
  summarise(correlation_mac17fill=cor(tx_macron17,tx_fillon),
            correlation_mac22fill=cor(tx_macron22,tx_fillon),
            correlation_pecfill=cor(tx_pecresse,tx_fillon),
            correlation_lep17fill=cor(tx_lepen17,tx_fillon),
            correlation_lep22fill=cor(tx_lepen22,tx_fillon),
            correlation_zemfill=cor(tx_zemmour,tx_fillon))



# Corrélation des votes de gauche avec Macron 2017
donnees17  %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  rename(uniq_bdv=bv2017,nb17=Nombre.d.inscrits,melenchon17=melenchon,macron17=macron) %>% 
  mutate(tx_melenchon17=melenchon17/nb17*100,
         tx_macron17=macron17/nb17*100) %>% 
  left_join(donnees22 %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              rename(melenchon22=melenchon,macron22=macron),by="uniq_bdv") %>%
  mutate(tx_hamon=hamon/nb17*100,
         tx_jadot=jadot/Nombre.d.inscrits*100,
         tx_melenchon22=melenchon22/Nombre.d.inscrits*100,
         tx_macron22=macron22/Nombre.d.inscrits*100,
         tx_macron17=macron17/Nombre.d.inscrits*100,
         tx_hidalgo=hidalgo/Nombre.d.inscrits*100) %>% 
  select(uniq_bdv,tx_hamon,tx_jadot,tx_melenchon22,tx_macron22,tx_macron17,tx_melenchon17,tx_hidalgo) %>% 
  pivot_longer(cols=c("tx_hidalgo","tx_melenchon22","tx_melenchon17","tx_macron22","tx_jadot","tx_hamon"),names_to="candidat",values_to="taux") %>% 
  ggplot(aes(x=tx_macron17,y=taux,color=candidat))+
  geom_abline(slope = 1)+
  geom_point(alpha=.7)+
  coord_equal()+
  scale_color_manual(values = c("tx_hamon"="deeppink","tx_jadot"="chartreuse4","tx_melenchon22"="red2","tx_macron22"="darkorchid4",
                                "tx_melenchon17"="red2","tx_hidalgo"="deeppink"))+
  labs(x="Vote Macron en 2017 (% des inscrit-e-s)",
       y="Vote (% des inscrit-e-s)",
       title="Corrélation avec le vote Macron 2017",
       caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats du 1er tour des élections présidentielles 2017 et 2022\nTraitements et erreurs : @Re_Mi_La")+
  facet_wrap(facets=~candidat,
             nrow = 1,
              labeller = as_labeller(c("tx_hamon"="Vote Hamon corr.= 0,25",
                                       "tx_jadot"="Vote Jadot corr.= 0,65",
                                       "tx_melenchon22"="Vote Mélenchon 2022 corr.= -0,43",
                                       "tx_macron22"="Vote Macron 2022 corr.= 0,75",
                                       "tx_melenchon17"="Vote Mélenchon 2017 corr.= -0,32",
                                       "tx_hidalgo"="Vote Hidalgo corr.= 0,36"))
             )+
  theme_minimal()+
  theme(legend.position = "top",
        plot.caption = element_text(size=8))+
  guides(color="none")


# pour le corr
donnees17  %>% 
  st_drop_geometry() %>% 
  as.data.frame() %>% 
  rename(uniq_bdv=bv2017,nb17=Nombre.d.inscrits,melenchon17=melenchon,macron17=macron) %>% 
  mutate(tx_melenchon17=melenchon17/nb17*100,
         tx_macron17=macron17/nb17*100) %>% 
  left_join(donnees22 %>% 
              st_drop_geometry() %>% 
              as.data.frame() %>% 
              rename(melenchon22=melenchon,macron22=macron),by="uniq_bdv") %>%
  mutate(tx_hamon=hamon/nb17*100,
         tx_jadot=jadot/Nombre.d.inscrits*100,
         tx_melenchon22=melenchon22/Nombre.d.inscrits*100,
         tx_macron22=macron22/Nombre.d.inscrits*100,
         tx_macron17=macron17/Nombre.d.inscrits*100,
         tx_hidalgo=hidalgo/Nombre.d.inscrits*100) %>% 
  select(uniq_bdv,tx_hamon,tx_jadot,tx_melenchon22,tx_macron22,tx_macron17,tx_melenchon17,tx_hidalgo) %>% 
  summarise(correlation_hamfill=cor(tx_hamon,tx_macron17),
            correlation_jadfill=cor(tx_jadot,tx_macron17),
            correlation_mel22fill=cor(tx_melenchon22,tx_macron17),
            correlation_mac22fill=cor(tx_macron22,tx_macron17),
            correlation_mel17fill=cor(tx_melenchon17,tx_macron17),
            correlation_hidfill=cor(tx_hidalgo,tx_macron17))
