# Load FPI data
fpi <- readr::read_csv("./data/weekly-fpi-2020-2022.csv")

# Create model matrix ----

# Define column to indicate last week of season
fpi %<>% dplyr::group_by(season) %>%
         dplyr::summarize(last_week = max(week)) %>%
         dplyr::left_join(x = fpi, by = "season")

# Define response
response <- dplyr::filter(fpi, week == last_week) %>%
     dplyr::select(season, team, fpi_end = fpi)

# Construct design matrix
predictors <- dplyr::filter(fpi, week == 1) %>%
     dplyr::select(season, team, fpi_start = fpi) %>%
     # tidyr::separate(col = "team", into = c("city", "name"), remove = F, sep = " ")

     dplyr::mutate(intercept = 1,
                   time      = as.numeric(season == 2020),
                   team_2 = as.numeric(ifelse(team == "Atlanta Falcons", T, F))
                   
                   )

# Loop for remaining team indicator variables
for (i in 3:32) {
  new_col <- paste("team", i, sep = "_")
  old_col <- paste("team", i-1, sep = "_")
  
  predictors %<>% dplyr::mutate(!!sym(new_col) := dplyr::lag(!!sym(old_col), 1) %>% tidyr::replace_na(replace = 0))
}


# Combine predictors and response by season and team
model_matrix <- dplyr::left_join(y = response, x = predictors, by = c("season", "team")) %>%
                dplyr:: select(season, team, fpi_end, fpi_start, intercept, dplyr::everything()) %>%
                dplyr::filter(season %in% 2020:2021) # estimate model on 2020 and 2021 data only


# Checks ----

# All team indicator columns should sum to 2, all team indicators should sum to 62
dplyr::select(model_matrix, dplyr::starts_with("team_")) %>% colSums %>% sum

# Time indicator should sum to 32
dplyr::select(model_matrix, time) %>% sum

# Correlation between response and predictor
# Linearity assumption satistifed
plot(x = model_matrix$fpi_start, y = model_matrix$fpi_end)
cor(x = model_matrix$fpi_start, y = model_matrix$fpi_end)

     
# Estimate ----
Y <- model_matrix$fpi_end
X <- dplyr::select(model_matrix, intercept, fpi_start, time, dplyr::starts_with("team_")) %>% as.matrix()

theta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y

# Fitted values ----
y_hat <- X %*% theta_hat


# Residuals ----
residuals <- Y - y_hat


# Residuals vs. fitted values
# Non-consant variance, randomness satisfied
plot(y = residuals, x = y_hat)
cor(y = residuals, x = y_hat)
mean(residuals)
var(residuals)



# QQ-plot and histogram
# Normality of residuals satisfied
eq <- quantile(residuals, probs = seq(.01, .99, .01))
nq <- qnorm(mean = mean(residuals), sd = sd(residuals), p = seq(.01, .99,.01))
plot(y = eq, x = nq)

hist(residuals)


# Check against lm() ----
# Looks good
lm_check <- dplyr::select(model_matrix, -season, -team, -intercept) %>% 
lm(formula = fpi_end ~ .)

summary(lm_check)

sum(lm_check$coefficients - theta_hat)







# Predictions for 2022 ----
# Goal: use theta_hat coefficients estimated on 2020, 2021 data to predict end of season FPI for 2022
# Then, compare against actual values and compute RMSE and other forecast diagnostics


predictors_2022 <- dplyr::filter(predictors, season == 2022)

X_2022 <- dplyr::select(predictors_2022, -season, -team) %>%
          dplyr::select(intercept, fpi_start, time, dplyr::starts_with("team_")) %>%
          as.matrix()

# Forecasted values for 2022 end-of-season FPI ----
forecast_2022 <- tibble(season = 2022, 
                            team = predictors_2022$team,
                            fpi_forecast = as.vector(X_2022  %*% theta_hat)) %>%
                 dplyr::left_join(y = response, by = c("season", "team")) %>%
                 dplyr::left_join(y = dplyr::select(predictors, season, team, fpi_start), by = c("season", "team")) %>%
                 dplyr::mutate(forecast_error = fpi_end - fpi_forecast) %>%
                 dplyr::select(season, team, fpi_start, fpi_end, fpi_forecast, forecast_error)

# Forecast errors ----
forecast_diagnostics <- dplyr::group_by(forecast_2022, season) %>%
                        dplyr::summarize(mae = mean(abs(forecast_error)),
                                        mse = mean(forecast_error^2),
                                        rmse = sqrt(mse),
                                        max = max(forecast_error),
                                        min = min(forecast_error),
                                        label = "Overall"
                                        )

forecast_diagnostics

hist(forecast_2022$forecast_error, breaks = seq(-16,16,4))

# Compare forecast errors to win percentages ----
win_pct <- readr::read_csv(file = "./data/1-team-win-pct.csv") %>%
           dplyr::rename(team_abb = team)

win_pct_var <- dplyr::group_by(win_pct, team_abb) %>%
               dplyr::summarize(win_pct_var = var(win_pct)) %>% 
               dplyr::arrange(desc(win_pct_var))

# Team abbreviation column
forecast_2022 %<>% dplyr::arrange(team) %>%
dplyr::mutate(team_abb = c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", 
                           "DEN", "DET", "GB",  "HOU", "IND", "JAX", "KC", "LV",  "LAC",  
                           "LA", "MIA", "MIN", "NE", "NO", "NYG", "NYJ", "PHI", 
                           "PIT", "SEA", "SF", "TB", "TEN", "WAS")
) %>%
dplyr::left_join(y = win_pct, by = c("team_abb", "season")) %>%
dplyr::left_join(y = dplyr::filter(win_pct, season == 2021) %>% dplyr::select(season, team_abb, win_pct_2021 = win_pct), by = "team_abb")

sort(forecast_2022$team)

plot(y = forecast_2022$forecast_error, x = forecast_2022$win_pct)

# Plot forecast errors against win percentage
ggplot(data = forecast_2022, aes(x = win_pct, y = forecast_error)) +
geom_point() +
# geom_label(aes(label = team_abb)) +
ggrepel::geom_label_repel(size = 3.5, aes(label = team_abb)) +
geom_hline(yintercept = 0, linetype = "dotted")

cor(y = forecast_2022$forecast_error, x = forecast_2022$win_pct)

  
# Plot forecast errors against change in win percentage from last year
p_err_vs_winpct <-   ggplot(data = forecast_2022, aes(x = win_pct - win_pct_2021, y = forecast_error)) +
                   geom_point() +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    
    
    ggrepel::geom_label_repel(size = 5, aes(label = team_abb)) +
    geom_label(aes(x = -.3, y = 10), label = "Correlation = .83", fill = "yellow", size = 5) +
    
    labs(title = "Model Prediction Errors for 2022 vs. Change in Regular Season Win Percentage",
         subtitle = "Overall FPI",
         # subtitle = "",
         y = "Model Prediction Error for 2022",
         x = "Change in Regular Season Win Percentage (2022-2021)"
    ) +
    scale_x_continuous(limits = c(-.5,.5), breaks = seq(-.5,.5,.1)) +
    scale_y_continuous(limits = c(-15,15), breaks = seq(-15, 15, 3)) +
    theme_substack()
  
  cor(y = forecast_2022$forecast_error, x = forecast_2022$win_pct - forecast_2022$win_pct_2021)

  ggsave(plot = p_err_vs_winpct, filename = "./analysis/plots/fpi-prediction-errors-2022-vs-win-pct-change.png", width = 13.5, height = 11)
  
  
# Plot forecast errors against variance in win pct over sample
ggplot(data = forecast_2022, aes(x = win_pct_var, y = forecast_error)) +
  geom_point() +
  # geom_label(aes(label = team_abb)) +
  ggrepel::geom_label_repel(size = 3.5, aes(label = team_abb, fill = win_pct)) +
  geom_hline(yintercept = c(0, 6, -6), linetype = "dotted")

cor(y = forecast_2022$forecast_error, x = forecast_2022$win_pct_var)

ggplot(data = forecast_2022, aes(x = win_pct_var, y = abs(forecast_error))) +
  geom_point() +
  # geom_label(aes(label = team_abb)) +
  ggrepel::geom_label_repel(size = 3.5, aes(label = team_abb, fill = win_pct)) +
  geom_hline(yintercept = c(0, 6, -6), linetype = "dotted")

cor(y = abs(forecast_2022$forecast_error), x = forecast_2022$win_pct_var)

# Does a simple linear model do better? ----
# Not really, see diagnostics below
lm_simple <- lm(data = model_matrix, formula = fpi_end ~ fpi_start)
summary(lm_simple)

predict(object = lm_simple, newdata = forecast_2022)

forecast_2022_simple <- tibble(season = 2022, 
                        team = predictors_2022$team,
                        fpi_forecast = as.vector(predict(object = lm_simple, newdata = forecast_2022))
                        ) %>%
  dplyr::left_join(y = response, by = c("season", "team")) %>%
  dplyr::left_join(y = dplyr::select(predictors, season, team, fpi_start), by = c("season", "team")) %>%
  dplyr::mutate(forecast_error = fpi_end - fpi_forecast) %>%
  dplyr::select(season, team, fpi_start, fpi_end, fpi_forecast, forecast_error)


forecast_diagnostics_simple <- dplyr::group_by(forecast_2022_simple, season) %>%
  dplyr::summarize(mae = mean(abs(forecast_error)),
                   mse = mean(forecast_error^2),
                   rmse = sqrt(mse),
                   max = max(forecast_error),
                   min = min(forecast_error)
  )


forecast_diagnostics_simple
forecast_diagnostics

sum(lm_check$coefficients - theta_hat)

# Team random effects ----
# Good teams have a positive effect, bad teams have a negative effect
team_coef <- data.frame(id = names(lm_check$coefficients), effect = lm_check$coefficients)

team_effects <- dplyr::select(fpi, team) %>% unique() %>%
dplyr::mutate(index = 1:n(),
              id = paste("team", index, sep = "_")
              ) %>%
left_join(y = team_coef, by = "id") %>%
arrange(effect)

View(team_effects)





# Repeat for offensive FPI ----

predictors_off <- dplyr::select(predictors, -fpi_start, -intercept) %>%
                  dplyr::left_join(dplyr::filter(fpi, week == 1) %>% dplyr::select(week, season, team, off_start = off), by = c("season", "team")) %>%
                  dplyr::left_join(dplyr::filter(fpi, week == last_week) %>% dplyr::select(season, team, off_end = off),  by = c("season", "team")) %>%
                  dplyr::select(off_end, off_start, dplyr::everything())

# Estimate
lm_off <- dplyr::filter(predictors_off, season %in% 2020:2021) %>%
          dplyr::select(-team, -season, -week) %>%
          lm(formula = off_end ~.)

summary(lm_off)

# Predict
y_hat_off <- dplyr::filter(predictors_off, season == 2022) %>%
             dplyr::select(-team, -season, -week) %>%
             predict(object = lm_off)

# Forecast errors

forecast_2022_off <- tibble(season = 2022, 
                        team = unique(predictors_off$team),
                        fpi_forecast_off = y_hat_off
                        ) %>%
  dplyr::left_join(y = dplyr::select(predictors_off, season, team, off_start, off_end), by = c("season", "team")) %>%
  dplyr::mutate(forecast_error = off_end - fpi_forecast_off) %>%
  dplyr::select(season, team, off_start, off_end, fpi_forecast_off, forecast_error)

forecast_diagnostics_off <- dplyr::group_by(forecast_2022_off, season) %>%
  dplyr::summarize(mae = mean(abs(forecast_error)),
                   mse = mean(forecast_error^2),
                   rmse = sqrt(mse),
                   max = max(forecast_error),
                   min = min(forecast_error),
                   label = "Offensive"
  )


# Repeat for defensive FPI ----

predictors_def <- dplyr::select(predictors, -fpi_start, -intercept) %>%
  dplyr::left_join(dplyr::filter(fpi, week == 1) %>% dplyr::select(week, season, team, def_start = def), by = c("season", "team")) %>%
  dplyr::left_join(dplyr::filter(fpi, week == last_week) %>% dplyr::select(season, team, def_end = def),  by = c("season", "team")) %>%
  dplyr::select(def_end, def_start, dplyr::everything())

# Estimate
lm_def <- dplyr::filter(predictors_def, season %in% 2020:2021) %>%
  dplyr::select(-team, -season, -week) %>%
  lm(formula = def_end ~.)

summary(lm_def)

# Predict
y_hat_def <- dplyr::filter(predictors_def, season == 2022) %>%
  dplyr::select(-team, -season, -week) %>%
  predict(object = lm_def)

# Forecast errors

forecast_2022_def <- tibble(season = 2022, 
                            team = unique(predictors_def$team),
                            fpi_forecast_def = y_hat_def
) %>%
  dplyr::left_join(y = dplyr::select(predictors_def, season, team, def_start, def_end), by = c("season", "team")) %>%
  dplyr::mutate(forecast_error = def_end - fpi_forecast_def) %>%
  dplyr::select(season, team, def_start, def_end, fpi_forecast_def, forecast_error)

forecast_diagnostics_def <- dplyr::group_by(forecast_2022_def, season) %>%
  dplyr::summarize(mae = mean(abs(forecast_error)),
                   mse = mean(forecast_error^2),
                   rmse = sqrt(mse),
                   max = max(forecast_error),
                   min = min(forecast_error),
                   label = "Defensive"
  )


# Repeat for special teams FPI ----
predictors_st <- dplyr::select(predictors, -fpi_start, -intercept) %>%
  dplyr::left_join(dplyr::filter(fpi, week == 1) %>% dplyr::select(week, season, team, st_start = st), by = c("season", "team")) %>%
  dplyr::left_join(dplyr::filter(fpi, week == last_week) %>% dplyr::select(season, team, st_end = st),  by = c("season", "team")) %>%
  dplyr::select(st_end, st_start, dplyr::everything())

# Estimate
lm_st <- dplyr::filter(predictors_st, season %in% 2020:2021) %>%
  dplyr::select(-team, -season, -week) %>%
  lm(formula = st_end ~.)

summary(lm_st)

# Predict
y_hat_st <- dplyr::filter(predictors_st, season == 2022) %>%
  dplyr::select(-team, -season, -week) %>%
  predict(object = lm_st)

# Forecast errors

forecast_2022_st <- tibble(season = 2022, 
                            team = unique(predictors_st$team),
                            fpi_forecast_st = y_hat_st
) %>%
  dplyr::left_join(y = dplyr::select(predictors_st, season, team, st_start, st_end), by = c("season", "team")) %>%
  dplyr::mutate(forecast_error = st_end - fpi_forecast_st) %>%
  dplyr::select(season, team, st_start, st_end, fpi_forecast_st, forecast_error)

forecast_diagnostics_st <- dplyr::group_by(forecast_2022_st, season) %>%
  dplyr::summarize(mae = mean(abs(forecast_error)),
                   mse = mean(forecast_error^2),
                   rmse = sqrt(mse),
                   max = max(forecast_error),
                   min = min(forecast_error),
                   label = "Special Teams"
  )

# Look at all FPI series forecast diagnostics together ----

all_diagnostics <- dplyr::bind_rows(forecast_diagnostics, forecast_diagnostics_off, forecast_diagnostics_def, forecast_diagnostics_st) %>%
                   dplyr::select(label, rmse, mse, mae, min, max)


# Plot for forecast errors ----
forecast_2022 %<>% dplyr::mutate(absolute_error = abs(forecast_error)) %>%
                   dplyr::arrange(absolute_error)

forecast_2022$team <- factor(forecast_2022$team, levels = forecast_2022$team)

# Forecast error bars
p_forecast_errors <- ggplot(data = forecast_2022, aes(x = forecast_error, y = team)) +
geom_bar(stat = "identity", fill = "red", color = "black") +
geom_vline(xintercept = 0) +
labs(title = "Model Prediction Errors for Overall FPI",
     subtitle = "2022 Regular Season",
     y = "",
     x = "Prediction Error"
     ) +
scale_x_continuous(limits = c(-16,16), breaks = seq(-16, 16, 4)) +
theme_substack()


ggsave(plot = p_forecast_errors, filename = "./analysis/plots/fpi-prediction-errors-2022.png", width = 13.5, height = 11)


# FPI ranges for 2020 and 2021
series_labels <- data.frame(series = c("fpi", "off", "def", "st"),
                            label  = c("Overall", "Offense", "Defense", "Special Teams")
)

fpi_panel <- dplyr::select(fpi, season, team, week, fpi, off, def, st) %>%
  tidyr::gather(key = series, value = value, -season, -week, -team) %>%
  dplyr::left_join(y = series_labels, by = "series") %>%
  dplyr::mutate(label = factor(label, levels = c("Overall", "Offense", "Defense", "Special Teams")))

# Summary stats
fpi_team_moments <- dplyr::filter(fpi_panel, season %in% 2020:2021) %>%
  dplyr::group_by(team, series, label) %>%
  dplyr::summarize(mean = mean(value), 
                   min = min(value),
                   max = max(value),
                   sigma2 = var(value),
                   sigma = sd(value),
                   p50 = quantile(value, .5)
  ) %>%
  arrange(label, mean)

# Same order as forecast errors
fpi_team_moments %<>% dplyr::filter(series == "fpi")
fpi_team_moments$team <- factor(fpi_team_moments$team, levels = forecast_2022$team)
fpi_team_moments %<>% arrange(team)


p_ranges <-  ggplot(data = fpi_team_moments) +
  geom_point(aes(x = team, y = mean, color = label), alpha = 2) +
  geom_errorbar(aes(x = team, ymin = min, ymax = max, color = label)) +
  geom_hline(yintercept = 0, linetype = "dotted") + #, color = "gray") +
  scale_y_continuous(limits = c(-15,15), breaks = seq(-15,15,5)) +
  scale_color_manual(values = colorspace::darken(fpi_colors, .35)) +
  coord_flip() +
  labs(title = "",
       subtitle = "",
       x = "",
       y = "FPI Value",
       caption = "Note: solid dots represent averages, horizontal bars represent ranges."

  ) +
  theme_substack() +
  theme(legend.position = "none"#, 
        # axis.text.y.left = element_blank()
        )

# Combine
library(patchwork)

p_forecast_errors + p_ranges

# Save
        



