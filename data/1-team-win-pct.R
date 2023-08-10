# Use nflfastR to calculate regular season win percentages
# Load data
final_scores <- nflfastR::load_pbp(2020:2022) %>%
  dplyr::filter(season_type == "REG") %>%
  group_by(season, game_id, home_team, away_team) %>%
  summarize(final_home_score = max(home_score),
            final_away_score = max(away_score)
  ) %>%
  mutate(home_win = final_home_score > final_away_score,
         away_win = final_home_score < final_away_score,
         tie = final_home_score == final_away_score
  ) %>%
  ungroup()

final_scores_away <- final_scores %>% 
  rename(team = away_team,
         win  = away_win,
         loss = home_win,
         tie  = tie
  ) %>% 
  select(-home_team, -final_home_score, -final_away_score)


final_scores_home <- final_scores %>% 
  rename(team = home_team,
         win  = home_win,
         loss = away_win,
         tie  = tie
  ) %>% 
  select(-away_team, -final_home_score, -final_away_score)


wlt <- dplyr::bind_rows(list(final_scores_home, final_scores_away))

wlt %<>% group_by(season, team) %>% 
  summarize(wins = sum(win), losses = sum(loss), ties = sum(tie)) %>%
  mutate(record = paste(wins, losses, ties, sep = "-"),
         win_pct = wins / (wins + losses + ties)
  )

# Save
readr::write_csv(x = wlt, file = "./data/1-team-win-pct.csv")



