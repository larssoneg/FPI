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
scale_y_continuous(limits= c(-1,1), breaks = seq(-1, 1, .1)) +
geom_hline(yintercept = 0) +
scale_x_continuous(limits = c(1,18), breaks = 1:18) + 
labs(x = "Week of Season", 
     y = "Correlation with Week 1 FPI",
     title = "Week 1 NFL Football Power Index (FPI) Values Are Meaningful After All",
     # title = "ESPN NFL Football Power Index (FPI) Values Are Strongly Correlated Throughout the Season",
     subtitle = "Correlation of Week 1 FPI with All Subsequent Weeks (2020-2022 Seasons)",
     color = "FPI Series"
     ) +
theme_bw() +
theme(legend.position = c(.15,.65), legend.title = element_blank())


# Week 1 vs. Week 17 overall FPI
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
  

  

