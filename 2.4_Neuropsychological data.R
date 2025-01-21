


a <- vasc_gold %>% 
  select(NUM_ID, VISITNUM,GOLDSCORE1, GOLDSCORE2) %>% 
  mutate(GOLDSCORE = GOLDSCORE1 + GOLDSCORE2,
         GOLD_ANX = case_when(GOLDSCORE1 < 2 ~ 0,
                              GOLDSCORE1 >= 2 & GOLDSCORE <= 5 ~ 0,
                              GOLDSCORE1 >= 2 & GOLDSCORE > 5 ~ 1),
         GOLD_ANX = factor(GOLD_ANX))

b <- vasc_moca %>% 
     select(NUM_ID, VISITNUM, MOCATPS:MOCATOTAL) 

c <- vasc_wes %>% 
     select(NUM_ID, VISITNUM, WESSCORE,WESERROR) 

d <- vasc_stroop %>% 
  select(NUM_ID, VISITNUM, STROOPCAT, STROOPTEST, STROOPORRES) %>% 
  mutate(item = case_when(STROOPTEST == "Temps" ~ "Stroop_Tps",
                          str_detect(STROOPTEST,"totales") ~ "Stroop_Tot",
                          str_detect(STROOPTEST,"non-c") ~ "Stroop_Non_corr",
                          str_detect(STROOPTEST,"erreurs corrigée") ~ "Stroop_Corr"),
         planche = str_sub(STROOPCAT,9,9),
         cate = paste0(item,"_",planche)) %>% 
  pivot_wider(id_cols=c(NUM_ID, VISITNUM), values_from=STROOPORRES, names_from = cate) %>% 
  filter(!if_all(c(Stroop_Tps_1:Stroop_Tot_3), is.na))


neuropsy <- patsum_vasc %>% 
  select(NUM_ID) %>% 
  left_join(vasc_stvis %>% 
              filter(STVSTATVIS==1) %>% 
              select(NUM_ID, VISITNUM,STVISDAT_D), by="NUM_ID") %>% 
  left_join(a, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(b, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(c, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(d, by=c("NUM_ID", "VISITNUM")) %>% 
  relocate(NUM_ID, VISITNUM) %>% 
  select(-c(STVISDAT_D)) %>% 
  filter(!if_all(c(GOLDSCORE1:Stroop_Tot_3), is.na))














