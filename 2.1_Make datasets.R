


####################################################################################################################
### Individuals included in VASCOD ----
####################################################################################################################

vasc_ID <- vasc_incl %>% 
            filter(VASINCL==1) %>% 
            pull(NUM_ID) %>% 
            unique()

####################################################################################################################
### Cooking...  ----
####################################################################################################################

patsum_vasc <- vasc_incl %>% # inclusion
  select(NUM_ID, SITEID, SEX, BRTHDAT_D, VASINCLDAT_D) %>%
  filter(NUM_ID %in% vasc_ID) %>%
  mutate(age_vascod_incl = as.numeric(VASINCLDAT_D - BRTHDAT_D)/365.25)


# Looking for the closest annual FU in MEMENTO 
z <- followsum %>% 
  select(NUM_ID, VISITNUM, AGE_VIS,VISDAT) %>% 
  filter(!is.na(VISDAT) & VISITNUM %in% c(1,3,5,7,9,11))

patsum_vasc <- patsum_vasc %>% 
               left_join(z, by="NUM_ID") %>% 
               mutate(delta=abs(AGE_VIS - age_vascod_incl)) %>% 
               group_by(NUM_ID) %>% 
               slice_min(delta, with_ties = F) %>% 
               ungroup()
  
### Baseline data
patsum_vasc_incl <- patsum %>% 
  mutate(VASC=ifelse(NUM_ID %in% vasc_ID, 1, 0)) %>% 
  left_join(thick_m0_m24_lo %>%
              mutate(THICK_M0 = (lh_MeanThickness_M0 + rh_MeanThickness_M0)/2,
                     THICK_M24 = (lh_MeanThickness_M24 + rh_MeanThickness_M24)/2) %>% 
              select(NUM_ID, THICK_M0, THICK_M24),
            by = "NUM_ID") %>%
  left_join(hsb_m0_m24_tr %>% select(NUM_ID, HSB_VOL_M0, HSB_VOL_M24),by = "NUM_ID") %>% 
  left_join(spmvol_m0_m24_tr %>% select(NUM_ID, SPM_BPF_M0, SPM_BPF_M24),by = "NUM_ID") %>% 
  left_join(vol_hipp_amyg_tr_lo %>% select(NUM_ID, FREE_HIPP_VOL_M0, FREE_HIPP_VOL_M24),by = "NUM_ID") %>% 
  left_join(csf %>% 
              filter(CSF_FIRST=="Oui") %>% 
              select(NUM_ID, PTAU, AB4240, TTAU, AB4240_patho, PTAU_patho, TTAU_patho),by = "NUM_ID") %>% 
  left_join(lacunes %>% select(NUM_ID, Nb_Lacunes), by="NUM_ID") %>% 
  left_join(microbleeds %>%  select(NUM_ID,total_CMB), by="NUM_ID") %>% 
  left_join(amyloide_t1_t2_tr %>% select(NUM_ID, SUVR_POS_1), by="NUM_ID") %>% 
  left_join(hv %>% filter(VISITNUM==1) %>% 
              select(NUM_ID, HVTAB, HVALC), by="NUM_ID") %>% 
  mutate(AMYL_POS=case_when(is.na(AB4240_patho) & is.na(SUVR_POS_1) ~ NA,
                            AB4240_patho=="Oui" | SUVR_POS_1 == 1 ~ 1,
                            AB4240_patho=="Non" & SUVR_POS_1 == 0 ~ 0,
                            AB4240_patho=="Non" & is.na(SUVR_POS_1)  ~ 0,
                            is.na(AB4240_patho) & SUVR_POS_1 == 0 ~ 0),
         
         NIVETUD = factor(NIVETUD, levels=c("<= BEPC","BEP, CAP, BAC",">BAC")),
         HVTAB = factor(HVTAB, levels=c("Non fumeur", "Ancien fumeur", "Fumeur actuel")),
         MNAEGNUT = factor(MNAEGNUT, levels=c("Etat nutritionnel normal", "Risque de malnutrition", "Mauvais état nutritionnel")),
         SPPB_TOT_CLA = factor(SPPB_TOT_CLA, levels=c("Robuste", "Fragile", "Très fragile")),
         IPAQ_TOT_CLA = factor(IPAQ_TOT_CLA, levels=c("Faible", "Modéré(e)", "Elevé(e)"))) %>% 
  left_join(overview %>% 
              select(NUM_ID, EVT, EVT_DEM,IRM_NB, TEP_NB, AMYL_NB, LCR, NB_SUIVI, SUIVI_DELAI),
            by="NUM_ID") %>% 
  mutate_at(c("CDRSCR", "APOE_eps4", "AMYL_POS", "IRM_NB", "TEP_NB", "AMYL_NB"), as.factor)


####################################################################################################################
### Looking for the source population, i.e. individuals eligible to VASCOD
####################################################################################################################

w <- followsum %>%
  select(NUM_ID, CEN_ANOM_VIS, AGE_VIS, VISITNUM, SOCHAB, DEMENCE_DAT, VISDAT, VISTEL) %>%
  left_join(ids %>% select(NUM_ID, SITEID), by = "NUM_ID") %>%
  # Adding the starting date of inclusion by center 
  mutate(str_dat_vascod = case_when(
    SITEID == "001" ~ "14/01/2015",
    SITEID == "002" ~ "10/04/2015",
    SITEID == "003" ~ "30/03/2015",
    SITEID == "006" ~ "04/11/2014",
    SITEID == "013" ~ "04/02/2015",
    SITEID == "016" ~ "27/04/2015",
    SITEID == "017" ~ "04/11/2014",
    SITEID == "020" ~ "28/08/2015",
    SITEID == "021" ~ "04/01/2015",
    SITEID == "026" ~ "28/09/2017"),
    str_dat_vascod = as.Date(str_dat_vascod, format = "%d/%m/%Y"),
    # End date
    fin_dat_vascod = as.Date("31/12/2017", format = "%d/%m/%Y")) %>%
  # Keeping only the 10 participanting centers 
  filter(SITEID %in% c("001", "002", "003", "006", "013", "016", "017", "020", "021", "026"),
         # Age >= 50 ans
         AGE_VIS >= 50,
         # Living place
         SOCHAB != "Maison de retraite médicalisée" | is.na(SOCHAB),
         # FU during the center inclusion period, no dementia, no phone visit
         VISDAT < pmin(DEMENCE_DAT, fin_dat_vascod, na.rm = T),
         VISDAT > str_dat_vascod,
         VISTEL != "Oui" | is.na(VISTEL) ) %>%
  # Keeping the first visit 
  group_by(NUM_ID) %>%
  filter(VISDAT == min(VISDAT, na.rm = T)) %>%
  ungroup() %>% 
  # => This is the "theoritical" date of inclusion 
  select(NUM_ID, dat_theorique = VISDAT, VISITNUM) 

### Merge with other datasets 
z1 <- patsum_vasc %>% 
  select(NUM_ID, VISITNUM) %>% 
  left_join(followsum %>% select(NUM_ID, VISITNUM), by=c("NUM_ID", "VISITNUM"))

z2 <- w %>% 
  select(NUM_ID, VISITNUM) %>% 
  left_join(followsum %>% select(NUM_ID, VISITNUM), by=c("NUM_ID", "VISITNUM"))

eligib <- bind_rows(z1,z2) %>% 
  group_by(NUM_ID) %>% 
  slice(1) %>% 
  mutate(VASC=ifelse(NUM_ID %in% vasc_ID, 1, 0)) %>% 
  left_join(followsum %>% select(NUM_ID, VISITNUM, AGE_VIS, MMSSCTOT, CDRSCR, NPI_SCORE_CLIN_D,RISCTOTRL,REYMEM3SC,TMTB_TAUX, FLU_P, FLU_ANIM,   
                                 RESTRICTION_CLA, HTA_140_90_PEC, IMC, HVTAB, HVALC, MNAEGNUT, IMC,SPPB_TOT_CLA,
                                 IPAQ_TOT_CLA, CM_DIAB_EXPOS, CM_DYSLIP_EXPOS, PAS, PAD),
            by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(patsum_vasc_incl %>% select(NUM_ID, SEX, NIVETUD, APOE_eps4,AMYL_POS, ATCD_CV, CM_DIAB, CM_DYSLIP,FREE_HIPP_VOL_M0, THICK_M0, SPM_BPF_M0, HSB_VOL_M0, Nb_Lacunes, total_CMB), by="NUM_ID") %>% 
  mutate(NIVETUD = factor(NIVETUD, levels=c("<= BEPC","BEP, CAP, BAC",">BAC")),
         HVTAB = factor(HVTAB, levels=c("Non fumeur", "Ancien fumeur", "Fumeur actuel")),
         MNAEGNUT = factor(MNAEGNUT, levels=c("Etat nutritionnel normal", "Risque de malnutrition", "Mauvais état nutritionnel")),
         SPPB_TOT_CLA = factor(SPPB_TOT_CLA, levels=c("Robuste", "Fragile", "Très fragile")),
         IPAQ_TOT_CLA = factor(IPAQ_TOT_CLA, levels=c("Faible", "Modéré(e)", "Elevé(e)")))


####################################################################################################################
### Study examinations ----
####################################################################################################################

### MRI
z1 <- vasc_cq_irm %>% 
      mutate(dispo_3DT1 = ifelse(QC_prelim==1,1,0),
             dispo_ASL = ifelse(ASL_param_ok==1,1,0),
             dispo_TOF = ifelse(TOF==1,1,0),
             dispo_MULTI = ifelse(`_3DMULTIGRE`==1,1,0),
             dispo_FLAIR = ifelse(FLAIR==1,1,0),
             dispo_hippo = ifelse(PDhippoHR==1,1,0),
             dispo_DTI = ifelse(DTI==1,1,0)) %>% 
  select(NUM_ID, contains("dispo_")) %>% 
  mutate(VISITNUM=0) 


### Neuropsychological tests
z2 <- vasc_moca %>% 
  select(NUM_ID, VISITNUM, MOCATOTAL) %>% 
  full_join(vasc_wes %>% select(NUM_ID, VISITNUM, WESSCORE), by=c("NUM_ID", "VISITNUM") ) %>% 
  full_join(vasc_stroop %>% filter(NUM_OCC==1) %>% 
              select(NUM_ID, VISITNUM, STROOPORRES), by=c("NUM_ID", "VISITNUM")) %>% 
  full_join(vasc_gold %>% select(NUM_ID, VISITNUM, GOLDSCORE1), by=c("NUM_ID", "VISITNUM")) %>% 
  mutate(MOCA=ifelse(is.na(MOCATOTAL),0,1),
         WES=ifelse(is.na(WESSCORE),0,1),
         STROOP=ifelse(is.na(STROOPORRES),0,1),
         GOLD=ifelse(is.na(GOLDSCORE1),0,1)) %>% 
  select(!c(MOCATOTAL,WESSCORE,STROOPORRES,GOLDSCORE1))

### PW
z3 <- PWA %>% 
      arrange(NUM_ID, VISITNUM, CPT) %>% 
      group_by(NUM_ID, VISITNUM) %>% 
      slice(1) %>% 
      ungroup() %>% 
      mutate(VOP=ifelse(is.na(PROCDAT_D),0,1)) %>% 
      select(NUM_ID, VISITNUM, VOP)


### Final dataset
procs <- vasc_stvis %>% 
  filter(STVSTATVIS==1) %>% 
  select(NUM_ID, VISITNUM, STVTYPSUVI,CEN_ANOM) %>% 
  left_join(z1, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(z2, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(z3, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(vasc_uri %>% select(NUM_ID,VISITNUM,URIALBDOS),by=c("NUM_ID", "VISITNUM") ) %>% 
  left_join(vasc_proc %>% select(NUM_ID, VISITNUM,PROCOPH,PROCDILA,PROCRETI,PROCSDOCT),by=c("NUM_ID", "VISITNUM") ) %>% 
  mutate(STVTYPSUVI = factor(STVTYPSUVI, levels=c("Visite Initiale", "Visite à 1 an", "Visite à 2 ans", "Visite à 3 ans"))) %>% 
  mutate(across(c(contains("dispo_"),URIALBDOS, PROCOPH, PROCDILA, PROCRETI, PROCSDOCT, VOP), ~replace_na(.x,0))) %>% 
  rowwise() %>% 
  mutate(COMPLET_IRM = sum(c_across(dispo_3DT1:dispo_DTI)),
         COMPLET_TNP = sum(c_across(MOCA:GOLD)),
         COMPLET_OPH = sum(c_across(PROCOPH:PROCSDOCT)),
         COMPLET = sum(c_across(dispo_3DT1:PROCSDOCT))) %>% 
  filter(NUM_ID %in% vasc_ID) %>% 
  rowwise()




