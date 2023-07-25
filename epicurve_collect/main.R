library(dplyr)
library(ggplot2)

theme_set(epiplots::theme_clean())
colors <- epiplots::colors


# import -------------------------------------------------------------------------------------------
followup <- rio::import(here::here('epicurve_collect', 'data', 'followup.xlsx')) %>%
  select(zone, week_epi_min, week_epi_max, week_collect, collect_name)

df <- rio::import(here::here('epicurve_collect', 'data', 'cholera_ids.RDS')) %>%
  filter(year == 2023) %>%
  select(zone, week, date, cases, deaths) %>%
  mutate(zone = tinker::str_to_display(zone)) %>%
  filter(zone %in% followup$zone) %>%
  fuzzyjoin::fuzzy_left_join(followup,
                             by = c('zone' = 'zone',
                                    'week' = 'week_epi_min',
                                    'week' = 'week_epi_max'),
                             match_fun = list(`==`,
                                              `>=`,
                                              `<=`)) %>%
  mutate(cases = tidyr::replace_na(cases, 0)) %>%
  select(zone = zone.x, week, date, cases, collect_name, week_collect)


# plot ---------------------------------------------------------------------------------------------
for (z in unique(followup$zone)) {
  writeLines(z)

  tmp <- df %>%
    filter(zone == z)

  collects <- unique(tmp$collect_name)[-1]
  collect_weeks <- followup %>%
    filter(zone == z) %>%
    select(collect_name, week_collect)


  tmp %>%
    mutate(month = lubridate::month(date,
                                    label = TRUE,
                                    locale = 'fr_FR.utf8')) %>%
    ggplot(aes(x = week,
               y = cases,
               fill = collect_name)) +
    geom_col() +
    scale_fill_manual(name = '',
                      breaks = collects,
                      na.value = colors$grey_light_3,
                      values = c(colors$blue,
                                 colors$gold)) +
    scale_x_continuous(breaks = min(tmp$week):max(tmp$week)) +
    facet_grid(.~month,
               scales = 'free',
               switch = 'x',
               space = 'free_x') +
    ylab('Cas Hebdomadaires') +
    xlab('Semaine ISO') +
    labs(title = z) +
    theme(legend.text = ggtext::element_markdown(),
          legend.position = 'right',
          axis.text.x = element_text(size = 11),
          panel.spacing = unit(0, 'cm'),
          strip.placement = 'outside',
          strip.background = element_rect(linewidth = 0,
                                          color = 'grey'),
          strip.text = element_text(size = 17,
                                    face = 'bold'))

  ggsave(here::here('out', paste0(z, '.png')),
         width = 9.34,
         height = 4.17)
}
