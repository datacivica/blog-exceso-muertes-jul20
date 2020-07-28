#
# Author: OE, GJ
# Maintainers: OE, GJ
# Copyright:  (c) Data Cívica 2020
# ---------------------------------------------------------
# blog-exceso-muertes-jul20/descriptives/src/descriptives.R

if(!require(pacman))install.packages("pacman")
pacman::p_load(tidyverse, lubridate, svglite, scales, extrafont, here)
extrafont::loadfonts(quiet=T)

files <- list(input = here("clean-data/output/exceso-mortalidad.rds"),
              plot1_png = here("descriptives/output/tend-actas.png"),
              plot1_svg = here("descriptives/output/tend-actas.svg"),
              plot2_png = here("descriptives/output/diff-actas.png"),
              plot2_svg = here("descriptives/output/diff-actas.svg"),
              plot3_png = here("descriptives/output/diff-covid.png"),
              plot3_svg = here("descriptives/output/diff-covid.svg"),
              plot4_png = here("descriptives/output/actas-avg.png"),
              plot4_svg = here("descriptives/output/actas-avg.svg")
              )

tema <- theme_minimal() +
  theme(plot.title = element_text(size = 16, family = "Barlow Condensed", hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 12, family = "Barlow Condensed", hjust = 0.5),
        plot.caption = element_text(size = 10, family = "Barlow Condensed", hjust = 0, face = "italic"),
        axis.text = element_text(size = 10, family = "Barlow Condensed", face = "bold"),
        axis.title = element_text(size = 12, family = "Barlow Condensed"),
        legend.text = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        legend.title = element_text(size = 10, family = "Barlow Condensed", hjust = 0.5),
        strip.text = element_text(size = 12, face = "bold", family = "Barlow Condensed"),
        text = element_text(family = "Barlow Condensed"))

defs <- readRDS(files$input) %>% 
  mutate(month = factor(month, levels = month, labels = month_name)) %>% 
  filter(month %in% c("Enero", "Febrero", "Marzo",
                      "Abril", "Mayo", "Junio"))


#### Tendencia de actas - 2019 vs 2020 ####
plot1 <- defs %>% 
  select(nom_ent, cve_ent, month, solicitud2019, solicitud2020) %>% 
  pivot_longer(cols = -c(nom_ent, cve_ent, month),
               names_to = "year", values_to = "tot_actas") %>% 
  mutate(year = parse_number(year)) %>% 
  ggplot(aes(x = month, y = tot_actas, color = as.factor(year), group = year)) +
  geom_line(size = 1) +
  facet_wrap(~nom_ent, scales = "free") +
  expand_limits(y = 0) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_manual(values = c("#FF6561", "#6A6AEA")) +
  labs(title="¿Cuántas actas de defunción se han expedido en cada estado?",
       subtitle="2020 vs 2019",
       x="", y = "", color="Año del acta",
       caption = "Fuente: Solicitudes de información realizadas en la Plataforma Nacional de Transparencia") +
  tema +
  theme(legend.position = "top")
  
ggsave(files$plot1_png, plot1, width = 12, height = 8)
ggsave(files$plot1_svg, plot1, width = 12, height = 8)  



ggsave(files$plot1_svg, plot1, width = 12, height = 8)  

#### Diferencia en número de actas - 2019 vs 2020 ####
plot2 <- defs %>% 
  filter(cve_ent!="06") %>% 
  select(nom_ent, cve_ent, month, solicitud2019, solicitud2020) %>% 
  mutate(diff = (solicitud2020-solicitud2019)/solicitud2019) %>% 
  ggplot(aes(x = month, y = diff, fill = ifelse(diff < 0,"#6A6AEA", "#FF6561"))) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#cb181d") +
  facet_wrap(~nom_ent, scales = "free", nrow=2) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_manual(values = c("#6A6AEA", "#FF6561")) +
  labs(title="Cambio porcentual en número de actas emitidas por entidad",
       subtitle="2020 vs 2019",
       x="", y = "", fill="",
       caption = "Fuente: Solicitudes de información realizadas en la Plataforma Nacional de Transparencia") +
  tema +
  theme(legend.position = "none")

ggsave(files$plot2_png, plot2, width = 12, height = 8)  
ggsave(files$plot2_svg, plot2, width = 12, height = 8)  

#### Tendencia cambio vs confirmados COVID ####
plot3 <- defs %>% 
  select(nom_ent, cve_ent, month, solicitud2020, avg_sinais, def_covid) %>% 
  mutate(diff = solicitud2020 - round(avg_sinais)) %>% 
  ggplot(aes(x = month, group = nom_ent)) +
  geom_line(aes(y = diff, color = "Diferencia actas 2020 vs promedio 2014-2018"), size = 1) +
  geom_line(aes(y=def_covid, color = "Muertes confirmadas por COVID-19"), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#cb181d") +
  facet_wrap(~nom_ent, scales = "free", nrow=3) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_manual(name = "", values = c("Diferencia actas 2020 vs promedio 2014-2018" = "#FF6561",
                                           "Muertes confirmadas por COVID-19" = "#6A6AEA")) +
  labs(title="Diferencia en muertes vs muertes confirmadas por COVID-19",
       x="", y = "Total de muertes", fill="",
       caption = "Fuente: Solicitudes de información, datos de la SSA con corte al 25 de julio, y defunciones generales (INEGI).") +
  tema +
  theme(legend.position = "top")

ggsave(files$plot3_png, plot3, width = 12, height = 8)  
ggsave(files$plot3_svg, plot3, width = 12, height = 8)  

#### Tendencia promedio vs actas 2020 ####
plot4 <- defs %>% 
  ggplot(aes(month, group=nom_ent)) +
  geom_line(aes(y = avg_sinais, color="Promedio 2014-2018"), size = 1) +
  geom_ribbon(aes(ymin = avg_sinais-(2*sd_sinais), ymax = avg_sinais+(2*sd_sinais)), alpha = 0.2, fill="#6A6AEA") +
  geom_line(aes(y = solicitud2020, color = "Actas 2020"), size = 1) +
  scale_color_manual(name = "", values = c("Promedio 2014-2018" = "#6A6AEA", "Actas 2020" = "#FF6561")) +
  facet_wrap(~nom_ent, scales = "free", nrow = 4) +
  expand_limits(y = 0) +
  labs(title = "Tendencia mensual de muertes",
       subtitle = "Promedio 2014-2018 vs actas 2020: enero a junio",
       x = "", y = "Total de muertes",
       caption = "Fuente: Defunciones generales (INEGI), y solicitudes de información") +
  tema +
  theme(legend.position = "top")

ggsave(files$plot4_png, plot4, width = 12, height = 8)  
ggsave(files$plot4_svg, plot4, width = 12, height = 8)  

# done.
