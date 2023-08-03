# Load weekly fpi data
fpi <- readr::read_csv("./data/weekly-fpi-2020-2022.csv")

# Split by week and arrange by team name to preserve order
fpi_by_week <- dplyr::group_split(fpi, week) %>%
lapply(FUN = function(x) arrange(x, team))

# Correlations
rho_fpi <- array(dim = 18)
rho_off <- array(dim = 18)
rho_def <- array(dim = 18)
rho_st  <- array(dim = 18)

for (w in 1:18) {
  rho_fpi[w] <- cor(fpi_by_week[[1]]$fpi, fpi_by_week[[w]]$fpi)
  rho_off[w] <- cor(fpi_by_week[[1]]$off, fpi_by_week[[w]]$off)
  rho_def[w] <- cor(fpi_by_week[[1]]$def, fpi_by_week[[w]]$def)
  rho_st[w] <- cor(fpi_by_week[[1]]$st, fpi_by_week[[w]]$st)
  
}

# Compile into data frame and save
correlations <- data.frame(week = 1:18, fpi = rho_fpi, off = rho_off, def = rho_def, st = rho_st) %>%
tidyr::gather(key = variable, value = value, -week)

readr::write_csv(x = correlations, file = "./analysis/1-weekly-fpi-correlations.csv")



