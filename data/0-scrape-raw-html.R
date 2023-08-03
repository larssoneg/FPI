# Scrape FPI tables from archived ESPN FPI sites
scrape_raw_fpi <- function(html_file) {

  # Time stamp
  time_stamp <- stringr::str_extract(string = html_file, pattern = "\\d{14}")
  
  html_table <- rvest::read_html(html_file)
schedule <- rvest::html_table(html_table, header = T)

teams <- schedule[[1]]
teams <- teams[2:nrow(teams),]
names(teams) <- "team"

fpi <- schedule[[2]]
fpi <- fpi[2:nrow(fpi),]
names(fpi) <- c("record", "fpi", "rank", "trend", "off", "def", "st", "sos", "rem_sos", "avgwp")

fpi <- bind_cols(teams, fpi) %>%
       dplyr::mutate_at(c("fpi", "rank", "trend", "off", "def", "st", "sos", "rem_sos", "avgwp"), as.numeric) %>%
       dplyr::mutate(time_stamp = as.POSIXct(time_stamp, format = "%Y%m%d%H%M%S"))

return(fpi)
}


# All files
file_list <- list.files("./data/raw", pattern = "*.html", full.names = T)
fpi <- lapply(X = file_list, FUN = function(x) scrape_raw_fpi(x) %>% dplyr::arrange(team)) %>%
  dplyr::bind_rows()

# Organize
fpi %<>% dplyr::mutate(year = lubridate::year(time_stamp),
                      month = lubridate::month(time_stamp),
                      day = lubridate::day(time_stamp),
                      season = ifelse(month == 1, year - 1, year),
                      ones = 1
                      ) %>%
group_by(season, team) %>%
mutate(week = cumsum(ones)) %>%
select(season, team, week, record, fpi, rank, trend, off, def, st, time_stamp)

# Checks
group_by(fpi, season) %>% summarize(min = min(week), max = max(week))
unique(fpi$team)


# Name fix for Washington
fpi %<>% dplyr::mutate(team = stringr::str_replace_all(string = team, pattern = "Washington Commanders", replacement = "Washington"))
unique(fpi$team)
length(unique(fpi$team))


# Save
readr::write_csv(x = fpi, file = "./data/weekly-fpi-2020-2022.csv")


# tidyr::gather(key = variable, value = value, -team, )



# Correlations
rhos <- array(dim = 16)
for (i in 1:16) {
  rhos[i] <- cor(season_2020[[1]]$fpi, season_2020[[i]]$fpi)
}

plot(y = rhos, x = 2:17, ylim = c(-1,1), ylab = "Correlation with Week 1 FPI", xlab = "Week")
abline(v = 8)


