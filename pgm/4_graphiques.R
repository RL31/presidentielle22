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

abst_ndv2 <- filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(donnees17 %>% mutate(abst17=Nombre.d.abstentions/Nombre.d.inscrits*100),
            by="Numéro.du.bureau") %>% 
 inner_join(donnees22 %>% mutate(abst22=Nombre.d.abstentions/Nombre.d.inscrits*100) %>% 
              rename(inscrits22=Nombre.d.inscrits),
            by="Numéro.du.bureau") %>% 
  ggplot(aes(x = Ind_snv/Ind,y=abst22-abst17))+
  geom_point(aes(size=(inscrits22)),alpha=0.5)+
  # ggrepel::geom_text_repel(aes(label=lieu_vote))+
  labs(x="Niveau de vie moyen (€)",
       y="Ecart des taux d'abstention\n2017 et 2022 (en points)",
       size="Nombre d'inscrits en 2022",
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
    pivot_longer(cols=c("tx_chom","tx_agri","tx_artcom","tx_cad","tx_pint","tx_emp","tx_ouv","tx_nsal","tx_retr","tx_etud","ndv"),
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
         caption="Source : Mairie de Toulouse, Découpage des bureaux de vote\n et résultats du 1er tour de l'élection présidentielle 2017\nInsee, Recensement de la population 2018\net Filosofi 2017\nTraitements et erreurs : @Re_Mi_La")+
    facet_wrap(~indicateur,scales = "free",
               labeller = as_labeller(c("tx_agri"="Agriculteurs (%)",
                                        "tx_artcom"="Artisans, commerçants,\nchefs d'entreprise (%)",
                                        "tx_cad"="Cadres (%)",
                                        "tx_chom"="Chômeurs (%)",
                                        "tx_emp"="Employés (%)",
                                        "tx_nsal"="Non salariés (%)",
                                        "tx_ouv"="Ouvriers (%)",
                                        "tx_retr"="Retraités (%)",
                                        "tx_pint"="Professions intermédiaires (%)",
                                        "tx_etud"="Etudiants (%)",
                                        "ndv"="Niveau de vie (milliers d'€)")))+
    theme_minimal()+
    theme(     plot.caption = element_text(size=8),
               plot.background = element_rect(fill="white",color="white"))
  ggsave(paste0("sorties/bv22_vote_",CAND,".jpg"),width=9,height = 9,units = "in",dpi=100)
  
}


# graphiques_catsoc(CAND="Macron",VAR="Nombre.de.voix.du.candidat.2",COUL="darkorchid4")
graphiques_catsoc(CAND="Macron",VAR="macron",COUL="darkorchid4")
graphiques_catsoc(CAND="abst",VAR="Nombre.d.abstentions",COUL="black")
graphiques_catsoc(CAND="Jadot",VAR="jadot",COUL="chartreuse4")
graphiques_catsoc(CAND="Le Pen",VAR="lepen",COUL="peru")
graphiques_catsoc(CAND="Mélenchon",VAR="melenchon",COUL="red2")
graphiques_catsoc(CAND="Hidalgo",VAR="hidalgo",COUL="deeppink2")
graphiques_catsoc(CAND="Roussel",VAR="roussel",COUL="red3")
graphiques_catsoc(CAND="Lassalle",VAR="lassalle",COUL="cadetblue")
graphiques_catsoc(CAND="Pécresse",VAR="pecresse",COUL="blue")
graphiques_catsoc(CAND="Zemmour",VAR="zemmour",COUL="brown")
