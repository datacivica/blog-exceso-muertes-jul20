#
# Author: OE
# Maintainers: OE
# Copyright:  (c) Data Cívica 2020, GPL v2 or newer
# -------------------------------------------------
# actas-defuncion/process-data/src/process-transp.R

if(!require(pacman))install.packages("pacman")

pacman::p_load(tidyverse, lubridate, here)

stub_dirs <- list(input = here("import/output/"),
                  output = here("process-data/output/"))

inp <- "/Volumes/GoogleDrive/Mi unidad/Data Cívica (Drive)/Contenidos/Datos/10_exceso/inp"
out <- "/Volumes/GoogleDrive/Mi unidad/Data Cívica (Drive)/Contenidos/Datos/10_exceso/out"

# --- sinais --- #
monthly_avg_sinais <- function(ent, start_year, end_year){
  # Filter years to average
  years <- seq(start_year, end_year, by=1) %>% formatC(., width = 2, flag = "0", format = "d") %>% paste(., collapse="|")
  files_sinais <- dir(paste0(out, "/sinais"))
  files_sinais <- files_sinais[1:19]
  
  sinais <- data.frame()
  
  # Get average per day
  for (i in 1:length(files_sinais)){
    tempo <- readRDS(paste0(out, "/sinais/", files_sinais[i])) %>% 
      filter(ent_regis == ent & mes_regis!=99) %>% 
      mutate(mes_regis = formatC(mes_regis, width = 2, flag = "0", format = "d")) %>% 
      group_by(ent_regis, mes_regis, anio_regis) %>% 
      summarise(tot_deads = n()) %>% 
      ungroup() %>% 
      select(ent_regis, mes_regis, anio_regis, tot_deads)
    sinais <- bind_rows(sinais, tempo)
  }
  
  start = ifelse(start_year<10, paste0("19", start_year), paste0("20", start_year)) %>% as.integer
  end = ifelse(end_year<10, paste0("19", end_year), paste0("20", end_year)) %>% as.integer
  
  sinais <- sinais %>% 
    filter(anio_regis %in% start:end) %>% 
    group_by(ent_regis, mes_regis, anio_regis) %>% 
    summarise(tot_deads = sum(tot_deads)) %>% 
    ungroup() %>% 
    group_by(ent_regis, mes_regis) %>% 
    summarise(mean_deads = round(mean(tot_deads), 1),
              sd_deads = round(sd(tot_deads), 1),
              min_deads = min(tot_deads),
              max_deads = max(tot_deads)) %>% 
    ungroup()
  
  return(sinais)
}

# === actas por transparencia === #
get_actas <- function(){
  # Get actas 
  actas <- readRDS(paste0(out, "/solicitudes/actas-transp.rds")) %>% 
    rename(mes_regis = mes_defun)
    
  return(actas)
}

# === main === #

last_month = 6

main <- function(){
  print("working on sinais")
  sinais <- bind_rows(monthly_avg_sinais(ent="01", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="06", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="07", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="12", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="20", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="08", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="22", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="23", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="29", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="30", start_year=14, end_year=18),
                      monthly_avg_sinais(ent="32", start_year=14, end_year=18)) %>% 
    filter(as.integer(mes_regis)<=last_month)
  
  print("working on actas")
  actas <- get_actas() %>% 
    filter(as.integer(mes_regis)<=last_month) %>% 
    select(-actas2019)
  
  print("joining data")  
  data <- left_join(sinais, actas)
  
  print("writing data")
  saveRDS(data, paste0(out, "/solicitudes/avg-vs-20-transp.rds"))
}

main()

# done.



