# Load plot formatting 
source("0-plot-formatting.R")

# Load weekly fpi data
fpi <- readr::read_csv("./data/weekly-fpi-2020-2022.csv")

# Any NAs?
View(fpi)

sum(is.na(fpi$fpi))
sum(is.na(fpi$off))
sum(is.na(fpi$def))
sum(is.na(fpi$st))

# Unconditional distributions ----
summary(fpi$fpi)
summary(fpi$off)
summary(fpi$def)
summary(fpi$st)

boxplot(fpi$fpi)
boxplot(fpi$off)
boxplot(fpi$def)
boxplot(fpi$st)



# Unconditional boxplot for each series ----
series_labels <- data.frame(series = c("fpi", "off", "def", "st"),
                            label  = c("Overall", "Offense", "Defense", "Special Teams")
                            )

fpi_panel <- dplyr::select(fpi, season, team, week, fpi, off, def, st) %>%
             tidyr::gather(key = series, value = value, -season, -week, -team) %>%
             dplyr::left_join(y = series_labels, by = "series") %>%
             dplyr::mutate(label = factor(label, levels = c("Overall", "Offense", "Defense", "Special Teams")))

fpi_panel_summary <- dplyr::group_by(fpi_panel, series, label) %>%
                     dplyr::summarize(min = min(value), 
                                      max = max(value), 
                                      p25 = quantile(value, .25), 
                                      p50 = quantile(value, .5), 
                                      p75 = quantile(value, .75)
                                      )

ggplot(data = fpi_panel, aes(y = value)) +
geom_boxplot() +
facet_wrap(~label, ncol = 4)


p_distributions <- ggplot(data = fpi_panel_summary) +
geom_errorbar(aes(x = 1, ymin = min, ymax = max)) +
geom_rect(aes(xmin = .6, xmax = 1.4, ymin = p25, ymax = p75, fill = label), color = "black") +
geom_segment(aes(y = p50, yend = p50, x = .6, xend = 1.4)) +
facet_wrap(~label, ncol = 4) +
labs(title = "Distributions of Weekly FPI Values", 
     subtitle = "2020-2022 Seasons",
     y = "FPI Value",
     x = ""
     ) +
scale_y_continuous(limits = c(-14, 14), breaks = -14:14) +
scale_fill_manual(values = fpi_colors) +
theme_bw() +
theme(axis.text.x.bottom = element_blank(),
      axis.ticks.x.bottom = element_blank(),
      legend.position = "none",
      plot.title = element_text(hjust = .5),
      plot.subtitle = element_text(hjust = .5)
      )
  
ggsave(filename = "./analysis/plots/unconditional-fpi-distribution-boxplots.png", plot = p_distributions)


# Variation across time ----



fpi_panel_weekly_summary <- dplyr::group_by(fpi_panel, series, label, season, week) %>%
  dplyr::summarize(min = min(value), 
                   max = max(value), 
                   p25 = quantile(value, .25), 
                   p50 = quantile(value, .5), 
                   p75 = quantile(value, .75),
                   avg = mean(value)
  ) %>%
  dplyr::arrange(label,season, week) %>%
  dplyr::group_by(label) %>%
  dplyr::mutate(index = 1:n())

# Data frames for facet labels
label_factor <- factor(c("Overall", "Offense", "Defense", "Special Teams"), levels = c("Overall", "Offense", "Defense", "Special Teams"))

year_labels <- data.frame(label = sort(rep(label_factor, 3)),
                          year = c(2020:2022, rep("", 9)),
                          x = c(2.5, 19, 36, rep(NA, 9)),
                          y = c(rep(14, 3), rep(NA, 9))
)
                            
series_labels <- data.frame(label = sort(rep(label_factor, 2)),
                            series = c("Average", "Range", rep("", 6)),
                            x = c(3, 8, rep(NA ,6)),
                            y = c(-13, -13, rep(NA, 6))
)
                            
series_arrows <- data.frame(label = sort(rep(label_factor, 2)),
                            x = c(3, 8, rep(NA ,6)),
                            y = c(-10, -10, rep(NA, 6)),
                            xend = c(3, 8, rep(NA ,6)),
                            yend = c(-.5, -5, rep(NA ,6))
)


  

p_ts <- ggplot(data = fpi_panel_weekly_summary) +
        geom_ribbon(aes(x = index, ymin =  min, ymax = max, fill = label), group = 1, color = "black") +
        geom_line(aes(x = index, y = avg), group = 1) +
        geom_vline(xintercept = c(17.5, 34.5), linetype = "dotted") +
        
        # Season labels
        geom_label(data = year_labels, aes(label = year, x = x, y = y), label.size = NA) +
        
        # Series labels
        geom_label(data = series_labels, aes(label = series, x = x, y = y), label.size = NA) +
        geom_segment(data = series_arrows, aes(x = x, y = y, xend = xend, yend = yend), size = .4, arrow = arrow(length = unit(0.1, "cm"))) +  
        
          
        facet_wrap(~label, nrow = 4) +
          labs(title = "Distributions of Weekly FPI Values Across Time", 
               subtitle = "2020-2022 Regular Seasons",
               y = "FPI Value",
               x = "Week of Regular Season"
          ) +
          scale_y_continuous(limits = c(-16, 16), breaks = seq(-15,15,5)) + #c(seq(-14, -2, 4), 0, seq(2, 14, 4))) +
          scale_x_continuous(breaks= 1:208, labels = rep(c(1:17, 1:17, 1:18),4), expand = c(0,0)) +
          scale_fill_manual(values = fpi_colors) +
          theme_substack() +
          theme(legend.position = "none")

ggsave(plot = p_ts, filename = "./analysis/plots/fpi-weekly-distributions.png", width = 13.5, height = 11)






# QQ Plots ----

# Calculate empirical quantiles
eq <- dplyr::group_by(fpi_panel, series, label) %>%
dplyr::summarize(eq = quantile(value, p = seq(.01, .99, .01))) %>%
dplyr::mutate(quantile = seq(.01, .99, .01))

# Calculate theoretical quantiles
fpi_moments <- dplyr::group_by(fpi_panel,series, label) %>% 
               dplyr::summarize(mean = mean(value), sigma2 = var(value), sigma = sd(value), n = n()) %>%
               ungroup() %>%
               arrange(label) %>%
               dplyr::mutate(mean_label_y = c(70, 75, 130, 750),
                             sd_label_y = c(50, 50, 90, 500),
                             mean_label_x = -14,
                             sd_label_x = -13.2
                             )

nq <- data.frame(def = qnorm(mean = fpi_moments$mean[1], sd = fpi_moments$sigma[1], p = seq(0.01, .99, .01)),
                 fpi = qnorm(mean = fpi_moments$mean[2], sd = fpi_moments$sigma[2], p = seq(0.01, .99, .01)),
                 off = qnorm(mean = fpi_moments$mean[3], sd = fpi_moments$sigma[3], p = seq(0.01, .99, .01)),
                 st = qnorm(mean = fpi_moments$mean[4], sd = fpi_moments$sigma[4], p = seq(0.01, .99, .01)),
                 quantile = seq(0.01, .99, .01)) %>%
tidyr::gather(key = series, value = nq, -quantile)
                 
                 
# Combine empirical and theotretical quantiles
qq_data <- dplyr::left_join(x = eq, y = nq, by = c("series", "quantile"))


p_qq <- ggplot(data = qq_data, aes(x = nq, y = eq)) + 
        geom_abline(slope = 1, intercept = 0) +
          geom_point(aes(color = label)) +
          facet_wrap(~label, ncol = 4) +
          labs(title = "Quantile-Quantile Plots for Weekly FPI Values", 
               subtitle = "2020-2022 Regular Seasons",
               x = "Theoretical Normal Quantiles", 
               y = "Empirical Quantiles",
               caption = "Note: the sample size is 1,664 observations for each FPI distribution."
          ) +
          scale_color_manual(values = fpi_colors) +
          theme_bw() +
          theme_substack() +
          theme(legend.position = "none")

ggsave(plot = p_qq, filename = "./analysis/plots/fpi-qq.png", width = 13.5, height = 11)



# Histograms ----


# For density instead of count on y-axis, use argument y = ..density.. in aes() statement
p_histograms <- ggplot(data = fpi_panel, aes(x = value, fill = label)) +
  geom_histogram(color = "black", binwidth = .5) +
  facet_wrap(~label, nrow = 4, scales = "free_y") +
  labs(title = "The Distributions of Weekly FPI Values Are Approximately Normally Distributed", 
       subtitle = "2020-2022 Regular Seasons",
       y = "Count", 
       x = "FPI Value",
       caption = "Note: one histogram bin represents 0.5 FPI points."
  ) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  
  # Labels for mean and standard deviation
  geom_label(data = fpi_moments, fill = "white", aes(x = -14, y = mean_label_y, label = paste("Average =", round(mean, 2)))) +
  geom_label(data = fpi_moments, fill = "white", aes(x = -13.2, y = sd_label_y, label = paste("Standard Deviation =", round(sigma, 2)))) +
  
  
  scale_x_continuous(breaks = seq(-15,15,3), limits = c(-15,15)) +
  scale_y_continuous(expand = expansion(mult = c(0,.1))) +
  scale_fill_manual(values = fpi_colors) +
  
  theme_bw() +
  theme_substack() +
  theme(legend.position = "none",
        # plot.title = element_text(hjust = .5),
        # plot.subtitle = element_text(hjust = .5), strip.text = element_text(size = 10)
        
  )

ggsave(plot = p_histograms, filename = "./analysis/plots/fpi-unconditional-histograms.png", width = 13.5, height = 11)



# Average and ranges for all teams over sample ----

fpi_team_moments <- dplyr::group_by(fpi_panel, team, series, label) %>%
                    dplyr::summarize(mean = mean(value), 
                                     min = min(value),
                                     max = max(value),
                                     sigma2 = var(value),
                                     sigma = sd(value),
                                     p50 = quantile(value, .5)
                    ) %>%
  arrange(label, mean) #%>%
  # dplyr::mutate(team = factor(team, levels = team))

# Save
readr::write_csv(x = fpi_team_moments, file = "./analysis/0-fpi-team-moments.csv")

# Order teams by mean overall FPI
team_order <- dplyr::filter(fpi_team_moments, series == "fpi")$team
fpi_team_moments %<>% dplyr::mutate(team = factor(team, levels = team_order))

  #%>%
  # tidyr::gather(key = statistic, value = value, -team, -series)

p_dot_bar <- ggplot(data = fpi_team_moments) +
            geom_point(aes(x = team, y = mean, color = label), alpha = 2) +
            geom_errorbar(aes(x = team, ymin = min, ymax = max, color = label)) +
  geom_hline(yintercept = 0, linetype = "dotted") + #, color = "gray") +
            scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,5)) +
            scale_color_manual(values = colorspace::darken(fpi_colors, .35)) +
            coord_flip() +
            facet_wrap(~label, ncol = 4) +
            labs(title = "A Snapshot of NFL Team Strength: FPI Averages and Ranges", 
                 subtitle = "2020-2022 Regular Seasons",
                 x = "",
                 y = "FPI Value",
                 caption = "Note: solid dots represent averages, horizontal bars represent ranges."
                 
                 ) +
            theme_substack() +
            theme(legend.position = "none")

ggsave(plot = p_dot_bar, filename = "./analysis/plots/fpi-avg-range-all-teams.png", height = 11, width = 11)

# Above/Below Average ----

fpi %<>% dplyr::mutate(aa_fpi = fpi > 0, aa_off = off > 0, aa_def = def > 0, aa_st = st > 0) 
  
fpi_wks_aa <- dplyr::group_by(fpi, team) %>%
        dplyr::summarize(wks_aa_fpi = sum(aa_fpi),
                         wks_aa_off = sum(aa_off),
                         wks_aa_def = sum(aa_def),
                         wks_aa_st = sum(aa_st),
                         total_weeks = n()
                         ) %>%
        dplyr::mutate(wks_ba_fpi = total_weeks - wks_aa_fpi) %>%
        dplyr::arrange(desc(wks_aa_fpi))

fpi_wks_aa$team <- factor(fpi_wks_aa$team, levels = fpi_wks_aa$team)

p_wks <- ggplot(data = fpi_wks_aa) +
geom_bar(aes(x = team, y = wks_aa_fpi), stat = "identity", fill = "#5C7339", color = "black") +
geom_bar(aes(x = team, y = -wks_ba_fpi), stat = "identity", fill = "#BF9180", color = "black") +
labs(x = "",
     y = "Weeks Below Average FPI     Weeks Above Average FPI",
     title = "Weeks of Play Above or Below Average Overall FPI",
     subtitle = "2020-2022 Regular Seasons (52 Total Weeks of Play)"
     ) +
geom_hline(yintercept = 0) +
# scale_x_discrete(limits = c(-53,53)) +
scale_y_continuous(limits = c(-52, 52), breaks = seq(-52, 52, 4), labels = c(seq(52, 0, -4), seq(4, 52, 4))) +
theme_substack() +
theme(axis.text.x.bottom = element_text(angle = 55, hjust = 1, color = "black", size = 12))

ggsave(plot = p_wks, filename = "./analysis/plots/fpi-weeks-above-below-average.png", width = 13.5, height = 11)


# Time series for a few interesting cases ----

ebr <- dplyr::filter(fpi, team %in% c("Philadelphia Eagles", "Cincinnati Bengals", "Los Angeles Rams")) %>%
  dplyr::group_by(team) %>%
  dplyr::arrange(season, week) %>% 
  dplyr::mutate(index = 1:n(), season_week = paste(season, week, sep = "_")) 

p_ts_teams <- ggplot(data = ebr, aes(x = index, y = fpi)) +
  # geom_ribbon(aes(x = season_week, ymin = -15, ymax = 15, fill = aa_fpi)) +
  
geom_point() +
  
  
geom_line(group = 1) +
geom_hline(yintercept = 0, linetype = "dotted") +
  
  scale_y_continuous(limits = c(-16, 16), breaks = seq(-15,15,5)) + #c(seq(-14, -2, 4), 0, seq(2, 14, 4))) +
  scale_x_continuous(breaks= 1:208, labels = rep(c(1:17, 1:17, 1:18),4), expand = expansion(c(0,0.01))) +
  
  geom_vline(xintercept = c(17.5, 34.5), linetype = "dotted") +
  
  # Season labels
  geom_label(data = year_labels, aes(label = year, x = x, y = y), label.size = NA) +
  
  
  facet_wrap(~team, nrow = 3) +
  
  labs(title = "Overall FPI by Week of Play", 
       subtitle = "2020-2022 Regular Seasons", 
       x = "Week of Play", 
       y = "Overall FPI") +

  
  theme_bw() +
  theme_substack() +
  theme(legend.position = "none",
        # plot.title = element_text(hjust = .5),
        # plot.subtitle = element_text(hjust = .5)
  )

  
ggsave(plot = p_ts_teams, filename = "./analysis/plots/fpi-bengals-rams-eagles.png", width = 13.5, height = 11)



# Correlations ----
dplyr::select(fpi, fpi, off, def, st) %>%
rename("Overall" = fpi, "Offense" = off, "Defense" = def, "Special Teams" = st) %>%
pairs()

dplyr::select(fpi, fpi, off, def, st) %>%
cor()

p_pairs <- dplyr::select(fpi, fpi, off, def, st) %>%
  rename("Overall" = fpi, "Offensive" = off, "Defensive" = def, "Special Teams" = st) %>%
GGally::ggpairs(columns = c("Overall", "Offensive", "Defensive", "Special Teams"),
                upper = list(continuous = GGally::wrap(GGally::ggally_cor, stars = F, size =8, color =  "black")),
                diag = list(continuous = "blankDiag")
                ) +
labs(title = "Overall FPI is Most Strongly Correlated with Offensive FPI",
     subtitle = "2020-2022 Regular Seasons",
y = "FPI Value", 
     x = "FPI Value") +
scale_x_continuous(limits = c(-15,15), breaks = seq(-15, 15, 5)) +
scale_y_continuous(limits = c(-15,15), breaks = seq(-15, 15, 5)) +
theme_substack()

ggsave(p_pairs, filename = "./analysis/plots/fpi-pairwise-correlations.png", width = 13.5, height = 11)

# Miscellaneous ----

# Look at distributions by week
fpi %<>% dplyr::mutate(season_week = paste(season, week, sep = "-"))

# Overall FPI
box_fpi <- ggplot(data = fpi, aes(x = season_week, y = fpi)) +
geom_boxplot()

# Offense
box_off <- ggplot(data = fpi, aes(x = season_week, y = off)) +
  geom_boxplot(color= "blue")

# Defense
box_def <- ggplot(data = fpi, aes(x = season_week, y = def)) +
  geom_boxplot(color= "red")

# Special Teams
box_st <- ggplot(data = fpi, aes(x = season_week, y = st)) +
  geom_boxplot(color= "purple")




# Look over time for each team ----
ts_fpi <- ggplot(data = fpi, aes(x = season_week, y = fpi)) +
geom_line(group = 1) +
geom_point() +
facet_wrap(~team)

ts_off <- ggplot(data = fpi, aes(x = season_week, y = off)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "blue") +
  facet_wrap(~team)

ts_def <- ggplot(data = fpi, aes(x = season_week, y = def)) +
  geom_line(group = 1, color = "red") +
  geom_point(color = "red") +
  facet_wrap(~team)

ts_st <- ggplot(data = fpi, aes(x = season_week, y = st)) +
  geom_line(group = 1, color = "purple") +
  geom_point(color = "purple") +
  facet_wrap(~team)






