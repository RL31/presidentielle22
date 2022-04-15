library(tidyverse)

# ABSTENTION
abst_ndv <- filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(donnees17,by="Numéro.du.bureau") %>% 
  bind_rows(filosofi_par_bdv22 %>% 
              mutate(Numéro.du.bureau=str_pad(uniq_bdv,4,"left","0") ) %>% 
              left_join(donnees22,by="Numéro.du.bureau")) %>% 
  ggplot()+
  geom_point(aes(x = Ind_snv/Ind,y=Nombre.d.abstentions/Nombre.d.inscrits*100))+
  facet_wrap(~Année)+
  labs(x="Niveau de vie moyen (€)",
       y="Taux d'abstention",
       caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats des 1ers tours de l'élection présidentielle 2017 et 2022\nInsee, Filosofi 2017\nTraitements et erreurs : @Re_Mi_La")+
  
  theme_minimal()+
  theme(legend.position = "top",
        strip.text = element_text(face="bold",size=15),
        plot.caption = element_text(size=8))

filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(donnees17 %>%
              rename(abst17=Nombre.d.abstentions) %>% 
              mutate( bloc_gauche17=melenchon+hamon,
            bloc_droite17=macron+fillon+asselineau+cheminade+lassalle,
            bloc_extdroite17=lepen+dupontaignan),
            by="Numéro.du.bureau") %>% 
 inner_join(donnees22 %>% 
              rename(abst22=Nombre.d.abstentions,inscrits22=Nombre.d.inscrits,melenchon22=melenchon,macron22=macron) %>% 
            mutate( bloc_gauche22=melenchon22+jadot+roussel+hidalgo,
                    bloc_droite22=macron22+pecresse+lassalle,
                    bloc_extdroite22=lepen+dupontaignan+zemmour)  ,
            by="Numéro.du.bureau") %>%
  mutate(progression=case_when(pmax((bloc_gauche22-bloc_gauche17),(bloc_droite22-bloc_droite17),(bloc_extdroite22-bloc_extdroite17))== (bloc_gauche22-bloc_gauche17) ~ "gauche" ,
                               pmax((bloc_gauche22-bloc_gauche17),(bloc_droite22-bloc_droite17),(bloc_extdroite22-bloc_extdroite17))==(bloc_droite22-bloc_droite17) ~ "droite" ,
                               pmax((bloc_gauche22-bloc_gauche17),(bloc_droite22-bloc_droite17),(bloc_extdroite22-bloc_extdroite17))== (bloc_extdroite22-bloc_extdroite17) ~ "extdroite" ,
                               TRUE ~ "pb")) %>% 
  ggplot(aes(x = Ind_snv/Ind,y=abst22-abst17))+
  geom_hline(yintercept = 0)+
  geom_point(aes(color=progression),alpha=0.8,size=4)+
  scale_color_manual(name="Plus forte progression\nen nombre de voix",
                     values = c("gauche"="tomato2","extdroite"="peru"),
                     labels =  c("gauche"="Gauche","extdroite"="Extrême droite"))+
  labs(x="Niveau de vie moyen (€)",
       title="Evolution de l'abstention entre 2017 et 2022 et candidat qui progresse le plus",
       y="Evolution de l'abstention\nentre 2017 et 2022 (nombre de voix)",
       caption = "Source : Mairie de Toulouse, Découpage des bureaux de vote,\nRésultats des 1ers tours de l'élection présidentielle 2017 et 2022\nInsee, Filosofi 2017\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(legend.position = "top",
        strip.text = element_text(face="bold",size=15),
        plot.caption = element_text(size=8))

abst_chom <- bv_infosRP %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  ggplot()+
  geom_point(aes(x = tx_chom*100,y=Nombre.d.abstentions/Nombre.d.inscrits*100))+
  labs(x="Taux de chômage (%)",
       y="Taux d'abstention")+
  theme_minimal()

abst_pauv <- filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  ggplot()+
  geom_point(aes(y = Men_pauv/Men*100,x=Nombre.d.abstentions/Nombre.d.inscrits*100))+
  labs(y="Taux de pauvreté (%)",
       x="Taux d'abstention")+
  theme_minimal()

###############################################
# Pour les catégories sociales à partir du RP #
###############################################
graphiques_catsoc <- function(CAND="Macron",VAR="Nombre.de.voix.du.candidat.2",COUL="darkorchid4"){
  
  graphique <- bv22_infosRP %>% 
    mutate(Numéro.du.bureau=str_pad(uniq_bdv,4,"left","0") ) %>% 
    left_join(donnees22 %>% st_drop_geometry() %>% as.data.frame(),by="uniq_bdv") %>% 
    left_join(filosofi_par_bdv22,by="uniq_bdv") %>% 
    mutate(ndv=Ind_snv/Ind/1000) %>% 
    pivot_longer(cols=c("tx_chom","tx_artcom","tx_cad","tx_pint",
                        "tx_emp","tx_ouv","tx_nsal","tx_retr",
                        "ndv"),
                 names_to="indicateur",
                 values_to="taux") %>% 
    ggplot(aes(x = taux,
               y = .data[[VAR]]/Nombre.d.inscrits*100))+
    geom_point(color=COUL)+
    geom_smooth(color=COUL,fill=COUL)+
    labs(x="",
         y=paste0("Vote ",CAND,"\n(% d'inscrits)"),
         title=paste0("Le vote ",CAND," selon les caractéristiques socio-économiques de la population"),
         subtitle="à Toulouse, par bureau de vote",
         caption="Source : Mairie de Toulouse, Découpage des bureaux de vote\n et résultats du 1er tour de l'élection présidentielle 2022\nInsee, Recensement de la population 2018\net Filosofi 2017\nTraitements et erreurs : @Re_Mi_La")+
    facet_wrap(~indicateur,scales = "free",
               labeller = as_labeller(c(#"tx_agri"="Agriculteurs (%)",
                                        "tx_artcom"="Artisans, commerçants,\nchefs d'entreprise (%)",
                                        "tx_cad"="Cadres (%)",
                                        "tx_chom"="Chômeurs (%)",
                                        "tx_emp"="Employés (%)",
                                        "tx_nsal"="Non salariés (%)",
                                        "tx_ouv"="Ouvriers (%)",
                                        "tx_retr"="Retraités (%)",
                                        "tx_pint"="Professions intermédiaires (%)",
                                        #"tx_etud"="Etudiants (%)",
                                        "ndv"="Niveau de vie (milliers d'€)"#,
                                        #"tx_velo"="Usage D-T du vélo (%)"
                                        )))+
    theme_minimal()+
    theme(     plot.caption = element_text(size=8),
               plot.background = element_rect(fill="white",color="white"))
  ggsave(paste0("sorties/bv22_vote_",CAND,".jpg"),width=9,height = 9,units = "in",dpi=100)
  
}

graphiques_catsoc(CAND="Macron",VAR="macron",COUL="darkorchid4")
graphiques_catsoc(CAND="abst",VAR="Nombre.d.abstentions",COUL="black")
graphiques_catsoc(CAND="Jadot",VAR="jadot",COUL="chartreuse4")
graphiques_catsoc(CAND="Le Pen",VAR="lepen",COUL="peru")
graphiques_catsoc(CAND="Mélenchon",VAR="melenchon",COUL="red2")
graphiques_catsoc(CAND="Hidalgo",VAR="hidalgo",COUL="deeppink2")
graphiques_catsoc(CAND="Roussel",VAR="roussel",COUL="red3")
graphiques_catsoc(CAND="Lassalle",VAR="lassalle",COUL="cadetblue")
graphiques_catsoc(CAND="Pécresse",VAR="pecresse",COUL="blue")
graphiques_catsoc(CAND="Zemmour",VAR="zemmour",COUL="tan4")

# graphique sur le vélotaf

bv22_infosRP %>% 
  mutate(Numéro.du.bureau=str_pad(uniq_bdv,4,"left","0") ) %>% 
  left_join(donnees22 %>% st_drop_geometry() %>% as.data.frame(),by="uniq_bdv") %>% 
  left_join(filosofi_par_bdv22,by="uniq_bdv") %>% 
  mutate(ndv=Ind_snv/Ind/1000) %>% 
  pivot_longer(cols=c("tx_velo"),
               names_to="indicateur",
               values_to="taux") %>% 
  pivot_longer(cols=c("melenchon","roussel","jadot","hidalgo","macron","lepen"),names_to="candidat",values_to="nb_voix") %>% 
  
  ggplot(aes(x = taux,
             y = nb_voix/Nombre.d.inscrits*100))+
  geom_point(color="chartreuse4")+
  geom_smooth(color="chartreuse4",fill="chartreuse4")+
  labs(x="",
       y=paste0("Vote en % d'inscrits"),
       title="Voter avec ses pieds, version vélotaf",
       subtitle="à Toulouse, par bureau de vote",
       caption="Source : Mairie de Toulouse, Découpage des bureaux de vote\n et résultats du 1er tour de l'élection présidentielle 2022\nInsee, Recensement de la population 2018 à l'Iris équiréparti dans les bureaux de vote\nTraitements et erreurs : @Re_Mi_La")+
  facet_wrap(~candidat,scales = "free",
             labeller = as_labeller(c("melenchon"="Mélenchon",
                                      "roussel"="Roussel",
                                      "jadot"="Jadot",
                                      "hidalgo"="Hidalgo",
                                      "macron"="Macron",
                                      "lepen"="Le pen")))+
  theme_minimal()+
  theme(     plot.caption = element_text(size=8),
             plot.background = element_rect(fill="white",color="white"))
