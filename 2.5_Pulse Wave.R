

a <- PWV %>% 
     select(NUM_ID, VISITNUM, CPT,PROCDAT_D, `_PWV_Distance`:`_Quality_Control`) %>% 
     group_by(NUM_ID, VISITNUM) %>% 
     filter(`_Quality_Control`==1) %>% 
     select(NUM_ID, VISITNUM, Average_PWV, PWV_Standard_Deviation) %>% 
     group_by(NUM_ID, VISITNUM) %>% 
     summarise(across(Average_PWV:PWV_Standard_Deviation, ~quantile(.x, probs=0.5, na.rm=T))) %>% 
     rename(PWV_Med = Average_PWV, PWV_SD = PWV_Standard_Deviation) %>% 
    ungroup()

b <- PWA %>% 
  select(NUM_ID, VISITNUM, CPT,PROCDAT_D, `_Brachial_Systolic_Pressure`:`_Quality_Control`) %>% 
  filter(`_Quality_Control`==1) %>% 
  group_by(NUM_ID, VISITNUM) %>% 
  summarise(across( `_Brachial_Systolic_Pressure`:`_Central_Augmentation_Index`, ~quantile(.x, probs=0.5, na.rm=T))) %>% 
  rename(PWA_SP_Arm = `_Brachial_Systolic_Pressure`,
         PWA_DP_Arm = `_Brachial_Diastolic_Pressure`,
         PWA_HR = `_Heart_Rate`,
         PWA_SP_Centr = `_Central_Systolic_Pressure`,
         PWA_DP_Centr = `_Central_Diastolic_Pressure`,
         PWA_PP_Centr = `_Central_Pulse_Pressure`,
         PWA_MP_Centr = `_Central_Mean_Pressure`,
         PWA_AP_Centr = `_Central_Augmentation_Pressure`,
         PWA_AI_Centr = `_Central_Augmentation_Index`) %>% 
  ungroup()


PW <- patsum_vasc %>% 
  select(NUM_ID) %>% 
  left_join(vasc_stvis %>% 
              filter(STVSTATVIS==1) %>% 
              select(NUM_ID, VISITNUM,STVISDAT_D), by="NUM_ID") %>% 
  left_join(a, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(b, by=c("NUM_ID", "VISITNUM")) %>% 
  filter(VISITNUM %in% c(0,2)) %>% 
  relocate(NUM_ID, VISITNUM) %>% 
  select(-c(STVISDAT_D)) %>% 
  filter(!if_all(c(PWA_SP_Arm:PWA_AI_Centr), is.na))

var_label(PW) <- list(PWA_SP_Arm = "SP brachial",
                          PWA_DP_Arm = "DP brachial",
                          PWA_HR = "Heart rate",
                          PWA_SP_Centr = "SP Central",
                          PWA_DP_Centr = "DP central",
                          PWA_PP_Centr = "Pulse pressure Central",
                          PWA_MP_Centr = "Mean pressure Central",
                          PWA_AP_Centr = "Augmentation Pressure Central",
                          PWA_AI_Centr = "Augmentation Index Central")



