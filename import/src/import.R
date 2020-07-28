# Author: Georgina Jiménez
# Maintainer: GJ, OE
# License: (c) Data Cívica 2020, GPL v2 or newer
#
# -----------------------------------------------
# blog-exceso-muertes-jul20/import/src/import.R
#

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, stringr, foreign, here)

files <- list(inp_solic = here("import/input/solicitudes.csv"),
              inp_covid = here("import/input/200725COVID19MEXICO.csv"),
              inp_sinais = here("import/input/sinais/"),
              out_solic = here("import/output/solicitudes.rds"),
              out_covid = here("import/output/covid-ssa.rds"),
              out_sinais = here("import/output/sinais/")
              )

#### SOLICITUDES ####
print("Working in solicitudes")
solicitudes <- read_csv(files$inp_solic) %>% 
  clean_names() %>% 
  rename(nom_ent = estado) %>% 
  mutate(cve_ent = str_pad(cve_ent, width = 2, side = "left", pad = "0"))

print("Writing solicitudes")
saveRDS(solicitudes, files$out_solic)

#### SSA COVID ####
print("Working in SSA covid")
covid <- read_csv(files$inp_covid) %>% 
  clean_names() %>% 
  filter(!is.na(fecha_def)) %>% 
  rename(cve_ent = entidad_res) %>% 
  mutate(date = substr(as.character(fecha_def), 1, 7)) %>% 
  group_by(cve_ent, date) %>% 
  summarise(def_covid = n())

print("Writing SSA covid")
saveRDS(covid, files$out_covid)

#### SINAIS ####
files_sinais <- dir(files$inp_sinais)

sinais <- data.frame()

pb <- txtProgressBar(min=1, max=length(files_sinais), style=3)
for (i in 1:length(files_sinais)){
  print(paste0("Working in ", files_sinais[i]))
  defun <- read.dbf(paste0(files$inp_sinais, files_sinais[i]), as.is = T) %>%
    clean_names() %>% 
    rename(cve_ent = ent_regis) %>% 
    mutate(mes_regis = str_pad(mes_regis, width = 2, side = "left", pad = "0"),
           date = paste(anio_regis, mes_regis, sep="-")) %>% 
    group_by(cve_ent, date) %>% 
    summarise(def_sinais = n()) %>% 
    ungroup()
  
  print(paste0("Writing ", files_sinais[i]))
  saveRDS(defun, paste0(files$out_sinais, str_replace(tolower(files_sinais[i]), ".dbf", ".rds")))
  
  rm(defun)
  setTxtProgressBar(pb, i)
}
close(pb)
rm(pb, i)

# done.

