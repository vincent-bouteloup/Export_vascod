
### On met en forme les données de créat
z <- expand.grid(NUM_ID = vasc_ID,
                 VISITNUM = c(1,3,5,7,9,11)) %>% 
     left_join(lab_transp %>% 
                 select(NUM_ID, VISITNUM, Creat, LBDAT_D),
               by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(patsum_vasc %>% select(NUM_ID, VISDAT, VISITNUM) %>% rename(VASC_VIS = VISITNUM),
            by="NUM_ID") %>% 
  filter(VISITNUM >= VASC_VIS) %>% 
  mutate(VISITNUM = (VISITNUM - VASC_VIS)/2) %>% 
  arrange(NUM_ID, VISITNUM) %>% 
  filter(!is.na(Creat))

  
uri <- patsum_vasc %>% 
  select(NUM_ID, BRTHDAT_D) %>% 
  left_join(patsum %>% select(NUM_ID, SEX), by="NUM_ID") %>% 
  left_join(vasc_stvis %>% 
              filter(STVSTATVIS==1) %>% 
              select(NUM_ID, VISITNUM, STVTYPSUVI, STVISDAT_D), by="NUM_ID" ) %>% 
  left_join(vasc_uri %>%
              select(NUM_ID, VISITNUM, URIRALBCREA,URIALBRES, URIRECTIM_T),
            by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(z %>% select(NUM_ID, VISITNUM, Creat), 
            by=c("NUM_ID", "VISITNUM")) %>% 
  rowwise() %>% 
  mutate(AGE_VIS=as.numeric((STVISDAT_D-BRTHDAT_D)/365.25),
         Creat = ifelse(Creat > 200 | Creat < 1, NA, Creat),
         Creat = Creat / 88 , # On converti en mg/dL, comme c'est le cas dans le papier
         DFG = case_when(SEX == "Masculin" & Creat <= 0.90 ~ 142 * ((Creat/0.9)^(-0.302)) * (0.9938)^AGE_VIS,
                         SEX == "Masculin" & Creat > 0.9 ~ 142 * ((Creat/0.9)^(-1.2)) * (0.9938)^AGE_VIS,
                         SEX == "Féminin" & Creat <= 0.70 ~ 142 * ((Creat/0.7)^(-0.241)) * ((0.9938)^AGE_VIS) * 1.012 ,
                         SEX == "Féminin" & Creat > 0.70 ~ 142 * ((Creat/0.7)^(-1.2)) * ((0.9938)^AGE_VIS) * 1.012 ),
         DFG_CLA = case_when(DFG < 60 ~ "<60",
                             DFG >= 60 & DFG < 90 ~ "60-90",
                             DFG >= 90 ~ ">=90"),
         DFG_CLA=factor(DFG_CLA, levels=c("<60","60-90",">=90"))) %>% 
  relocate(NUM_ID, SEX, VISITNUM, AGE_VIS) %>% 
  select(-c(BRTHDAT_D,STVISDAT_D, STVTYPSUVI, AGE_VIS, SEX)) %>% 
  filter(!if_all(c(URIRALBCREA:DFG_CLA), is.na))

var_label(uri) <- list(URIRECTIM_T = "Heure des urines",
                       Creat = "Creatinine µmol/L")

