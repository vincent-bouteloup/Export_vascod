a <- vasc_retin %>% 
  mutate(eye=case_when(RETINCAT=="Oeil Droit" ~ "Droit",
                       RETINCAT=="Oeil Gauche" ~ "Gauche")) %>% 
  select(NUM_ID, VISITNUM,eye,RETINLAXI) 

b1 <- vasc_acu %>% 
     select(NUM_ID, VISITNUM, ACULOIN, ACUPRES, ACUETDRS_OD, ACUSNELLEN_OD, ACUPARIN_OD)
b2 <- vasc_acu %>% 
  select(NUM_ID, VISITNUM, ACULOIN, ACUPRES, ACUETDRS_OG, ACUSNELLEN_OG, ACUPARIN_OG)

b <- bind_rows(b1 %>% rename(ETDRS=ACUETDRS_OD,SNELLEN=ACUSNELLEN_OD,PARIN=ACUPARIN_OD) %>% mutate(eye="Droit"),
               b2 %>% rename(ETDRS=ACUETDRS_OG,SNELLEN=ACUSNELLEN_OG,PARIN=ACUPARIN_OG) %>% mutate(eye="Gauche"))

c <- vasc_octr %>% 
  mutate(eye=case_when(OCTRCAT=="Oeil Droi" ~ "Droit",
                       OCTRCAT=="Oeil Gauch" ~ "Gauche")) %>% 
  select(NUM_ID, VISITNUM, eye, OCTRRETCEN:OCTRRETVOLC, OCTRFIB_G:OCTRCLA_RG) 


FU_eye <- patsum_vasc %>% 
  select(NUM_ID) %>% 
  left_join(vasc_stvis %>% 
              filter(STVSTATVIS==1) %>% 
              select(NUM_ID, VISITNUM), by="NUM_ID") %>% 
  left_join(a, by=c("NUM_ID", "VISITNUM"), relationship = "many-to-many") %>% 
  left_join(b, by=c("NUM_ID", "VISITNUM", "eye")) %>% 
  left_join(c, by=c("NUM_ID", "VISITNUM", "eye")) %>% 
  filter(VISITNUM %in% c(0,2)) %>% 
  relocate(NUM_ID, VISITNUM,eye) %>% 
  filter(!if_all(c(ACULOIN:OCTRCLA_RG), is.na))

var_label(FU_eye) <- list(ACULOIN = "Correction vue de loin",
                          ACUPRES = "Correction vue de près",
                          PARIN = "Parinaud")


