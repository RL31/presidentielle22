library(tidyverse)

# ABSTENTION
abst_ndv <- filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  ggplot()+
  geom_point(aes(y = Ind_snv/Ind,x=Nombre.d.abstentions/Nombre.d.inscrits*100))+
  labs(y="Niveau de vie moyen (€)",
       x="Taux d'abstention")+
  theme_minimal()

abst_pauv <- filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  ggplot()+
  geom_point(aes(y = Men_pauv/Men*100,x=Nombre.d.abstentions/Nombre.d.inscrits*100))+
  labs(y="Taux de pauvreté (%)",
       x="Taux d'abstention")+
  theme_minimal()

# MACRON
macron_ndv <- filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  ggplot()+
  geom_point(aes(y = Ind_snv/Ind,
                 x = Nombre.de.voix.du.candidat.2/Nombre.d.inscrits*100),
             color="darkorchid4")+
  labs(y="Niveau de vie moyen (€)",
       x="Vote Macron\n(% d'inscrits)")+
  theme_minimal()
# LEPEN
lepen_ndv <- filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  ggplot()+
  geom_point(aes(y = Ind_snv/Ind,
                 x = Nombre.de.voix.du.candidat.1/Nombre.d.inscrits*100),
             color="peru")+
  labs(y="Niveau de vie moyen (€)",
       x="Vote Le Pen\n(% d'inscrits)",
      caption = "Source : Insee, Zonage à façon sur les données carroyées à 200m de Filosofi 2017\nMairie de Toulouse, Découpage des bureaux de vote\n et résultats du 1er tour de l'élection présidentielle 2017\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(      plot.caption = element_text(size=8))

# MELENCHON
melenchon_ndv <- filosofi_par_bdv17 %>% 
  mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
  left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
  ggplot()+
  geom_point(aes(y = Ind_snv/Ind,
                 x = Nombre.de.voix.du.candidat.8/Nombre.d.inscrits*100),
             color="red2")+
  labs(y="Niveau de vie moyen (€)",
       x="Vote Mélenchon\n(% d'inscrits)")+
  theme_minimal()

melenchon_ndv+macron_ndv+lepen_ndv


# 
# abst_vieux <- filosofi_par_bdv17 %>% 
#   mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
#   left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
#   ggplot()+
#   geom_point(aes(y = (Ind_65_79+Ind_80p)/Ind*100,x=Nombre.d.abstentions/Nombre.d.inscrits*100))+
#   labs(y="Part des plus\nde 64 ans (%)",
#        x="Taux d'abstention")+
#   theme_minimal()
# 
# abst_jeunes <- filosofi_par_bdv17 %>% 
#   mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
#   left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
#   ggplot()+
#   geom_point(aes(y = (Ind_18_24)/Ind*100,x=Nombre.d.abstentions/Nombre.d.inscrits*100))+
#   labs(y="Part des 18-24 ans (%)",
#        x="Taux d'abstention")+
#   theme_minimal()



# abst_enfants <- filosofi_par_bdv17 %>% 
#   mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
#   left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
#   ggplot()+
#   geom_point(aes(y = (Ind_0_3+Ind_4_5+Ind_6_10)/Ind*100,x=Nombre.d.abstentions/Nombre.d.inscrits*100))+
#   labs(y="Part des enfants\nde moins de 11 ans\n dans la population(%)",
#        x="Taux d'abstention")+
#   theme_minimal()



# 
# abst_proprio <- filosofi_par_bdv17 %>% 
#   mutate(Numéro.du.bureau=str_pad(bv2017,4,"left","0") ) %>% 
#   left_join(resultats_T1_17,by="Numéro.du.bureau") %>% 
#   ggplot()+
#   geom_point(aes(y = Men_prop/Men*100,x=Nombre.d.abstentions/Nombre.d.inscrits*100))+
#   labs(y="Part de ménages propriétaires (%)",
#        x="Taux d'abstention")+
#   theme_minimal()
#   
#   
