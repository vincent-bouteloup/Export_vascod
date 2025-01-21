

### Paths ----

path_database <- "R:/ETUDES/CIC_EC7_STATDM/GMA/COHORTE MEMENTO/DATA MANAGEMENT/Base_gelée/Release/Datasets/Internal/2023_Juillet"
path_corresp_id <- "R:/ETUDES/CIC_EC7_STATDM/GMA/COHORTE MEMENTO/DATA MANAGEMENT/Liste de correspondance IDs/id_listes.sas7bdat"
path_vascod_base <- "R:/ETUDES/CIC_EC7_STATDM/GMA/COHORTE MEMENTO/DATA MANAGEMENT/Base_gelée/Analyses/2023_vascod/Base_vascod_20230201"
path_cq_irm <- "R:/ETUDES/CIC_EC7_STATDM/GMA/COHORTE MEMENTO/DATA MANAGEMENT/Base_gelée/Analyses/2023_vascod"
path_lacunes <- "R:/ETUDES/CIC_EC7_STATDM/GMA/COHORTE MEMENTO/DATA MANAGEMENT/Lecture visuelle imagerie/Lacunes E JOUVENT/lacunes.sas7bdat"
path_dir_vop <- "R:/ETUDES/CIC_EC7_STATDM/GMA/COHORTE MEMENTO/DATA MANAGEMENT/Base_gelée/Analyses/2023_vascod/VOP"


####################################################################################################################
# Import VASCOD database ----
####################################################################################################################

# List all SAS tables
list_tables <- function(x){
  
  list_file <- list.files(x, full.names = T) %>% str_subset(".sas7bdat")
  list_name <- str_extract(list_file, regex("(?<=/)[^/]+(?=\\.sas7bdat)"))
  
  return(list(file = list_file, name = list_name))
}

tables <- list_tables(path_vascod_base)

# Import
purrr::walk2(tables$file, tables$name,
             ~assign(.y,
                     read_sas(data_file =  .x, catalog_file = NULL),
                     envir = globalenv()))


####################################################################################################################
# Other datasets ----
####################################################################################################################

# Pulse Wave data
PWA <- read_sas(paste0(path_dir_vop,"/pwa.sas7bdat"))
PWV <- read_sas(paste0(path_dir_vop,"/pwv.sas7bdat"))

# Release 
load(paste0(path_database,'/',"InternalMemento.Rdata"))

# IDs
ids <- read_sas(data_file = path_corresp_id, catalog_file = NULL)

# Lacunes
lacunes <- read_sas(data_file = path_lacunes, catalog_file = NULL)

# CATI QC
vasc_cq_irm <- read_sas(data_file =  paste0(path_cq_irm,'/',"vasc_cq_irm_20210118.sas7bdat"), catalog_file = NULL)



####################################################################################################################
### Merge IDs ----
####################################################################################################################

fun_ID <- function(tab){ 
  tab <- tab %>% 
    left_join(ids %>% select(NUM_ID, USUBJID, CEN_ANOM), by="USUBJID") %>% 
    relocate(NUM_ID) %>% 
    return(tab)
}

df_list <- mget(ls(pattern = "^vasc_"))

df_list_new <- lapply(df_list, fun_ID)
list2env(df_list_new, envir = .GlobalEnv)


PWA <- fun_ID(PWA)
PWV <- fun_ID(PWV)

####################################################################################################################
# Removing useless objects ----
####################################################################################################################

rm(list = c("acc", "ad8", "adl", "ae", "aide", "bref", "cdr", "cesd", "cohen", "coinf", "comed", 
            "dc", "demence", "diffusion", "dm", "dms", "do", "eav", "elp", "emp", "eq5d", "ev", "exc", "flu", "fmri", 
            "formats_sas", "iadl", "incl", "ipaq", "labels_sas", "leip", "loi", "mck", "mmse",
            "mna", "pecp", "phonesum", "prax", "qtel", "res", "rey", "rlri_imd", "rlri_sc", "rso", 
            "schac", "sero", "seuils_mci_vis", "socio", "sppb", "su", "synth", "tmt", "tnp", "zarit", "tables", "df_list", "fun_ID", "df_list_new", "list_tables"))

rm(list=ls(pattern="path"))




