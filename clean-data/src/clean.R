#
# Author: OE
# Maintainers: OE, GJ
# Copyright:  (c) Data Cívica 2020
# -------------------------------------------------
# blog-exceso-muertes-jul20/clean-data/src/clean.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, lubridate, here)

files <- list(covid = here("import/output/covid-ssa.rds"),
              solicitudes = here("import/output/solicitudes.rds"),
              sinais = here("import/output/sinais/"),
              output = here("clean-data/output/exceso-mortalidad.rds")
              )

#### SOLICITUDES ####
solicitudes <- readRDS(files$solicitudes) %>% 
  mutate(year = paste0("solicitud", year),
         month = substr(date, 6, 7)) %>% 
  select(-date) %>% 
  pivot_wider(names_from = "year", values_from = "actas") %>% 
  rename(month_name = mes)

#### SSA COVID ####
ssa <- readRDS(files$covid) %>% 
  mutate(month = substr(date, 6, 7)) %>% 
  select(-date)

#### SINAIS ####
sinais_files <- dir(files$sinais)

sinais <- bind_rows(lapply(paste0(files$sinais, sinais_files), readRDS)) %>% 
  mutate(month = substr(date, 6, 7)) %>% 
  group_by(cve_ent, month) %>% 
  summarise(avg_sinais = (mean(def_sinais)),
            sd_sinais = sd(def_sinais),
            min_sinais = min(def_sinais),
            max_sinais = max(def_sinais))

#### BASE FINAL ####

df <- solicitudes %>% 
  left_join(ssa) %>% 
  left_join(sinais)

saveRDS(df, files$output)

# done.
