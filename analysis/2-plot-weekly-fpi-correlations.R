# Load plot formatting
source("./analysis/0-plot-formatting.R")

# Load data
fpi <- readr::read_csv("./data/weekly-fpi-2020-2022.csv")
correlations <- readr::read_csv("./analysis/1-weekly-fpi-correlations.csv")

# Nicer variable labels
labels <- data.frame(variable = c("fpi", "off", "def", "st"),
                     label = c("Overall FPI", "Offensive FPI", "Defensive FPI", "Special Teams FPI")
                     )

correlations %<>% dplyr::left_join(y = labels, by = "variable") %>%
  dplyr::mutate(label = factor(label, levels = c("Overall FPI", "Offensive FPI", "Defensive FPI", "Special Teams FPI")))

# All on one set of axes
rho_by_week <- ggplot(data = correlations, aes(x = week, y = value, color = label)) +
geom_point() + 
geom_line() +
scale_color_manual(values = colorspace::darken(fpi_colors, .35)) +
scale_y_continuous(limits= c(-1,1), breaks = seq(-1, 1, .1)) +
geom_hline(yintercept = 0, linetype = "dotted") +
scale_x_continuous(limits = c(1,18), breaks = 1:18) + 
labs(x = "Week of Season", 
     y = "Correlation with Week 1 FPI",
     # title = "Week 1 NFL Football Power Index (FPI) Values Are Meaningful After All",
     # title = "ESPN NFL Football Power Index (FPI) Values Are Strongly Correlated Throughout the Season",
     title = "Correlation of Week 1 FPI with All Subsequent Weeks (2020-2022 Seasons)",
     color = "FPI Series"
     ) +
theme_substack() +
theme(legend.position = c(.15,.35), 
      legend.title = element_blank(),
      legend.text = element_text(size = 14)
)


ggsave(filename = "./analysis/plots/fpi-correlations-by-week.png", plot = rho_by_week, width = 13.5, height = 11)


# Facet version ----
rho_by_week_facet <- ggplot(data = correlations, aes(x = week, y = value, color = label)) +
  geom_point(size = 4) + 
  geom_line(size = 3) +
  facet_wrap(~label, nrow = 4) +
  scale_color_manual(values = colorspace::darken(fpi_colors, .35)) +
  scale_y_continuous(limits= c(-1,1), breaks = seq(-1, 1, .25)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_x_continuous(limits = c(1,18), breaks = 1:18) + 
  labs(x = "Week of Regular Season", 
       y = "Correlation with Week 1 FPI",
       # title = "Week 1 NFL Football Power Index (FPI) Values Are Meaningful After All",
       # title = "ESPN NFL Football Power Index (FPI) Values Are Strongly Correlated Throughout the Season",
       title = "Correlation of Week 1 FPI with All Subsequent Weeks",
       subtitle = "(2020-2022 Seasons)",
       caption = "Note: Week 18 correlations are based on the 2022 regular season only.",
       color = "FPI Series"
  ) +
  theme_substack() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        legend.text = element_text(size = 14)
  )

ggsave(filename = "./analysis/plots/fpi-correlations-by-week-facet.png", plot = rho_by_week_facet, width = 13.5, height = 11)

# Week 1 vs. Week 17 overall FPI ----
fpi_by_week <- dplyr::group_split(fpi, week) %>% lapply(FUN = function(x) arrange(x, team))

week1_vs_week17 <- dplyr::left_join(x = fpi_by_week[[1]], y = fpi_by_week[[17]], by = c("season", "team"))

scatter_inset <- ggplot(data = week1_vs_week17, aes(x = fpi.x, y = fpi.y)) +
geom_point() +
geom_smooth(se = F, method = "lm") +
labs(x = "Overall FPI: Week 1", y = "Overall FPI: Week 17",
     # title = "ESPN NFL Overall FPI for Weeks 1 and 17 Are Highly Correlated",
     # subtitle = "2020-2022 NFL Seasons"
     title = "Overall FPI: Week 17 vs. Week 1"
     ) +
theme_bw() +
theme(title = element_text(size = 8))


# Try inset
rho_by_week +
  geom_segment(aes(xend = 17, yend = .74, x = 14, y = -.1), size = .4,
               arrow = arrow(length = unit(0.1, "cm")), color = "black") +

patchwork::inset_element(p = scatter_inset, left = .45, right = .95, top = .45, bottom = 0.05) 
  

  

