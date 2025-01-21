
####################################################################################################################
### Baseline main data ----
####################################################################################################################

z <- vasc_stvis %>% 
     filter(STVSTATVIS==1) %>% 
     group_by(NUM_ID) %>% 
     slice_max(STVISDAT_D)


VASCOD_BAS <- select(patsum_vasc, NUM_ID, SITEID, VASINCLDAT_D, VISITNUM, AGE_VIS) %>% 
                 left_join(patsum_vasc_incl %>% 
                 select(NUM_ID, IRM_NB, TEP_NB, AMYL_NB, LCR, SEX, NIVETUD, CDRSCR,CDR_SB, APOE_eps4, AMYL_POS,
                        HVTAB, HVALC, NPI_SCORE_CLIN_D, MNAEGNUT, IMC, HTA_140_90_PEC, SPPB_TOT_CLA, IPAQ_TOT_CLA, CM_DIAB, CM_DYSLIP, ATCD_CV, 
                        FREE_HIPP_VOL_M0, FREE_HIPP_VOL_M24, THICK_M0, THICK_M24, SPM_BPF_M0, SPM_BPF_M24, HSB_VOL_M0, HSB_VOL_M24, Nb_Lacunes, total_CMB), by="NUM_ID" ) %>% 
           left_join(overview %>% select(NUM_ID, INCCONSDAT_D, DEMENCE_DAT, DCDAT_D, ETIOL_1, ETIOL_2, DERNOUV_DAT, BRTHDAT_D), by="NUM_ID") %>% 
           left_join(z %>% select(NUM_ID,STVISDAT_D) %>% rename(DERSUIV_VASCOD = STVISDAT_D), by="NUM_ID") %>% 
  relocate(NUM_ID, SITEID,BRTHDAT_D,VASINCLDAT_D,INCCONSDAT_D, AGE_VIS,DERSUIV_VASCOD,DERNOUV_DAT, DEMENCE_DAT,ETIOL_1, ETIOL_2,DCDAT_D     )

var_label(VASCOD_BAS) <- list(VASINCLDAT_D = "Inclusion VASCOD",
                                 INCCONSDAT_D = "Inclusion MEMENTO",
                                        VISITNUM = "Suivi MEMENTO",
                                        AGE_VIS = "Age inclusion VASCOD",
                                        MNAEGNUT = "MNA",
                            DERNOUV_DAT = "Dernier suivi MEMENTO",
                            DERSUIV_VASCOD = "Dernier suivi VASCOD")

####################################################################################################################
### MEMENTO FU data ----
####################################################################################################################

MEMENTO_FU <- select(patsum_vasc, NUM_ID) %>% 
              left_join(overview %>% select(NUM_ID, INCCONSDAT_D), by="NUM_ID") %>% 
              left_join(followsum %>% select(NUM_ID, VISITNUM, VISDAT, DEMENCE_DAT,DCDAT_D, EVT, EVT_DEM_PY, EVT_DC, EVT_DC_PY, SEX, AGE_VIS, NIVETUD, 
                                             CM_HTA_EXPOS:HTA_140_90_PEC, MMSORTMP:EAVORGSEN, NPI_SCORE_CLIN_D:NPI_SCORE_CLIN_G,
                                             EQMOB:MNAEGNUT, MCI_VIS), by="NUM_ID") %>% 
              mutate(COGN = case_when(!is.na(DEMENCE_DAT) & VISDAT >= DEMENCE_DAT ~ "dement",
                                      .default = MCI_VIS),
                     PY=as.numeric((VISDAT - INCCONSDAT_D)/365.25)) %>% 
  select(-MCI_VIS) %>% 
  relocate(NUM_ID, INCCONSDAT_D, VISITNUM,VISDAT,PY, DEMENCE_DAT, EVT, EVT_DEM_PY, EVT_DC, EVT_DC_PY, COGN ) %>% 
  rename(VIS_MEM = VISITNUM)

var_label(MEMENTO_FU) <- list(VIS_MEM = "Suivi MEMENTO", COGN = "Statut cognitif" )


####################################################################################################################
### VASCOD FU data ----
####################################################################################################################

VASCOD_MAIN <- patsum_vasc %>% 
  select(NUM_ID, SITEID, VISITNUM, VASINCLDAT_D, BRTHDAT_D) %>% 
  rename(VIS_MEM = VISITNUM) %>% 
  left_join(vasc_stvis %>% 
              filter(STVSTATVIS==1) %>% 
              select(NUM_ID, VISITNUM,STVISDAT_D), by="NUM_ID") %>% 
  mutate(VIS_MEM = VIS_MEM + 2*VISITNUM,
         PY=as.numeric((STVISDAT_D - VASINCLDAT_D)/365.25),
         AGE_VIS=as.numeric((STVISDAT_D - BRTHDAT_D)/365.25)) %>% 
  left_join(uri, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(neuropsy, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(PW, by=c("NUM_ID", "VISITNUM")) %>% 
  left_join(MEMENTO_FU %>% select(NUM_ID, VIS_MEM, COGN,SEX, NIVETUD:MNAEGNUT), by=c("NUM_ID", "VIS_MEM")) %>% 
  relocate(NUM_ID, SITEID, VISITNUM, VIS_MEM, AGE_VIS, SEX, NIVETUD, COGN) 

var_label(VASCOD_MAIN) <- list(VIS_MEM = "Suivi MEMENTO", VISITNUM = "Suivi VASCOD")




####################################################################################################################
### Exporting data
####################################################################################################################

#library(foreign)

fic_out <- paste0("R:/ETUDES/CIC_EC7/GMA/COHORTE MEMENTO/11_Statistiques/04_Analyses/08_VASCOD/Export_vascod/")

export_sas <- function(tabin, tabin_txt) { 
# 
# write.foreign(df = tabin,
#               datafile = paste0(fic_out,"/",tabin_txt,".txt"),
#               codefile = paste0(fic_out,"/",tabin_txt,".sas"),
#               dataname = tabin_txt,
#               package="SAS")

# write a file with all the labels of the dataset
  
labels <- sapply(tabin, function(x) attr(x, "label"))
sas_labels <- paste(names(labels), '=', paste0('"', labels, '"'), collapse = "\n")
  
writeLines(paste0("LABEL\n",
                  sas_labels, ";\n",
                  "RUN;"), 
                  paste0(tabin_txt,"_labels.sas"))
  }

### Exporting csv files
write.csv(VASCOD_BAS,"VASCOD_BAS.txt", na=".", fileEncoding = "UTF-8", row.names = F)
write.csv(VASCOD_MAIN,"VASCOD_MAIN.txt", na=".", fileEncoding = "UTF-8", row.names = F)
write.csv(MEMENTO_FU,"MEMENTO_FU.txt", na=".", fileEncoding = "UTF-8", row.names = F)








