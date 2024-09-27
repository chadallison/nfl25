
### Contents

- [Offensive and Defensive PPG](#offensive-and-defensive-ppg)
- [Offensive and Defensive YPG](#offensive-and-defensive-ypg)
- [Offensive and Defensive EPA](#offensive-and-defensive-epa)

------------------------------------------------------------------------

### Offensive and Defensive PPG

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

------------------------------------------------------------------------

### Offensive and Defensive YPG

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

------------------------------------------------------------------------

### Offensive and Defensive EPA

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
team_epa_classes = team_epa |>
  transmute(team,
            class = case_when(avg_off_epa >= 0 & avg_def_epa < 0 ~ "Good Offense, Good Defense",
                           avg_off_epa >= 0 & avg_def_epa >= 0 ~ "Good Offense, Bad Defense",
                           avg_off_epa < 0 & avg_def_epa >= 0 ~ "Bad Offense, Bad Defense",
                           avg_off_epa < 0 & avg_def_epa < 0 ~ "Bad Offense, Good Defense"))

descs = nflfastR::fast_scraper_schedules(season = 2024) |>
  filter(week > max(end_games$week)) |>
  slice_min(week, n = 1, with_ties = T) |>
  inner_join(team_epa_classes, by = c("home_team" = "team")) |>
  rename(home_class = class) |>
  inner_join(team_epa_classes, by = c("away_team" = "team")) |>
  rename(away_class = class) |>
  mutate(desc = as.character(glue::glue("{home_team} vs. {away_team}: {home_class} vs. {away_class}"))) |>
  pull(desc)

for (desc in descs) {
  print(desc)
}
```

    ## [1] "NYG vs. DAL: Bad Offense, Good Defense vs. Bad Offense, Bad Defense"
    ## [1] "ATL vs. NO: Bad Offense, Bad Defense vs. Good Offense, Good Defense"
    ## [1] "CAR vs. CIN: Bad Offense, Bad Defense vs. Good Offense, Bad Defense"
    ## [1] "CHI vs. LA: Bad Offense, Good Defense vs. Good Offense, Bad Defense"
    ## [1] "GB vs. MIN: Good Offense, Good Defense vs. Good Offense, Good Defense"
    ## [1] "HOU vs. JAX: Bad Offense, Bad Defense vs. Bad Offense, Bad Defense"
    ## [1] "IND vs. PIT: Bad Offense, Good Defense vs. Bad Offense, Good Defense"
    ## [1] "NYJ vs. DEN: Good Offense, Good Defense vs. Bad Offense, Good Defense"
    ## [1] "TB vs. PHI: Good Offense, Bad Defense vs. Good Offense, Bad Defense"
    ## [1] "ARI vs. WAS: Good Offense, Bad Defense vs. Good Offense, Bad Defense"
    ## [1] "SF vs. NE: Good Offense, Bad Defense vs. Bad Offense, Bad Defense"
    ## [1] "LAC vs. KC: Bad Offense, Good Defense vs. Good Offense, Bad Defense"
    ## [1] "LV vs. CLE: Bad Offense, Bad Defense vs. Bad Offense, Good Defense"
    ## [1] "BAL vs. BUF: Good Offense, Bad Defense vs. Good Offense, Good Defense"
    ## [1] "MIA vs. TEN: Bad Offense, Good Defense vs. Bad Offense, Good Defense"
    ## [1] "DET vs. SEA: Bad Offense, Good Defense vs. Bad Offense, Good Defense"

``` r
link = "https://ftnfantasy.com/dvoa/nfl/team-total-dvoa"

dvoa = read_csv("data/team_total_dvoa.csv", show_col_types = F) |>
  clean_names() |>
  distinct(team, off_dvoa, def_dvoa)

dvoa |>
  ggplot(aes(off_dvoa, def_dvoa)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  geom_vline(xintercept = mean(dvoa$off_dvoa), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = mean(dvoa$def_dvoa), linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_hex) +
  ggrepel::geom_text_repel(aes(label = team), size = 3) +
  scale_x_continuous(breaks = seq(-50, 50, by = 5)) +
  scale_y_continuous(breaks = seq(-50, 50, by = 5)) +
  labs(x = "Offensive DVOA",
       y = "Defensive DVOA",
       title = "Scatterplot of offensive and defensive DVOA")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
off_pass_rush = season_pbp |>
  filter(!is.na(posteam) & play_type %in% c("pass", "run")) |>
  group_by(posteam, play_type) |>
  summarise(avg_epa = mean(epa, na.rm = T),
            .groups = "drop") |>
  pivot_wider(id_cols = "posteam", names_from = "play_type", values_from = "avg_epa") |>
  setNames(c("team", "avg_off_pass_epa", "avg_off_run_epa"))

def_pass_rush = season_pbp |>
  filter(!is.na(defteam) & play_type %in% c("pass", "run")) |>
  group_by(defteam, play_type) |>
  summarise(avg_epa = mean(epa, na.rm = T),
            .groups = "drop") |>
  pivot_wider(id_cols = "defteam", names_from = "play_type", values_from = "avg_epa") |>
  setNames(c("team", "avg_def_pass_epa", "avg_def_run_epa"))

team_off_def_pass_rush_epa = inner_join(off_pass_rush, def_pass_rush, by = "team")

total_with_epa = end_games |>
  transmute(home_team, away_team, total_score = home_score + away_score) |>
  inner_join(team_off_def_pass_rush_epa, by = c("home_team" = "team")) |>
  setNames(c("home_team", "away_team", "total_score",
             "home_avg_off_pass_epa", "home_avg_off_run_epa", "home_avg_def_pass_epa", "home_avg_def_run_epa")) |>
  inner_join(team_off_def_pass_rush_epa, by = c("away_team" = "team")) |>
  setNames(c("home_team", "away_team", "total_score",
             "home_avg_off_pass_epa", "home_avg_off_run_epa", "home_avg_def_pass_epa", "home_avg_def_run_epa",
             "away_avg_off_pass_epa", "away_avg_off_run_epa", "away_avg_def_pass_epa", "away_avg_def_run_epa")) |>
  select(-c(home_team, away_team))

total_mod = lm(total_score ~ ., data = total_with_epa)
```

``` r
off_week_epa = season_pbp |>
  filter(!is.na(posteam)) |>
  group_by(team = posteam, week) |>
  summarise(off_epa = mean(epa, na.rm = T),
            .groups = "drop")

def_week_epa = season_pbp |>
  filter(!is.na(defteam)) |>
  group_by(team = defteam, week) |>
  summarise(def_epa = mean(epa, na.rm = T),
            .groups = "drop")

team_weekly_epa = inner_join(off_week_epa, def_week_epa, by = c("team", "week"))

team_epa
```

    ## # A tibble: 32 × 3
    ##    team  avg_off_epa avg_def_epa
    ##    <chr>       <dbl>       <dbl>
    ##  1 BUF        0.225      -0.0973
    ##  2 WAS        0.200       0.227 
    ##  3 NO         0.134      -0.177 
    ##  4 ARI        0.117       0.0224
    ##  5 CIN        0.115       0.161 
    ##  6 NYJ        0.074      -0.0168
    ##  7 KC         0.0667      0.0236
    ##  8 SF         0.0606      0.0387
    ##  9 MIN        0.0568     -0.210 
    ## 10 GB         0.0519     -0.0593
    ## # ℹ 22 more rows

``` r
end_npr = end_games |>
  inner_join(team_ppg, by = c("home_team" = "team")) |>
  rename(home_off_ppg = off_ppg, home_def_ppg = def_ppg) |>
  inner_join(team_ppg, by = c("away_team" = "team")) |>
  rename(away_off_ppg = off_ppg, away_def_ppg = def_ppg) |>
  mutate(home_exp = (home_off_ppg + away_def_ppg) / 2,
         away_exp = (away_off_ppg + home_def_ppg) / 2,
         home_off_npr = home_score - home_exp,
         home_def_npr = away_exp - away_score,
         away_off_npr = away_score - away_exp,
         away_def_npr = home_exp - home_score)

get_team_off_npr = function(tm) {
  home = end_npr |> filter(home_team == tm) |> pull(home_off_npr)
  away = end_npr |> filter(away_team == tm) |> pull(away_off_npr)
  return(round(mean(c(home, away)), 3))
}

get_team_def_npr = function(tm) {
  home = end_npr |> filter(home_team == tm) |> pull(home_def_npr)
  away = end_npr |> filter(away_team == tm) |> pull(away_def_npr)
  return(round(mean(c(home, away)), 3))
}

team_npr = data.frame(team = all_teams) |>
  mutate(off_npr = sapply(team, get_team_off_npr),
         def_npr = sapply(team, get_team_def_npr),
         npr = off_npr + def_npr,
         npr_rank = rank(-npr),
         team_npr = as.character(glue::glue("{team} ({npr_rank})")))

team_npr |>
  mutate(pos_lab = ifelse(npr >= 0, round(npr, 2), ""),
         neg_lab = ifelse(npr < 0, round(npr, 2), "")) |>
  ggplot(aes(reorder(team_npr, npr), npr)) +
  geom_col(aes(fill = team), show.legend = F) +
  geom_text(aes(label = pos_lab), size = 3, hjust = -0.25) +
  geom_text(aes(label = neg_lab), size = 3, hjust = 1.25) +
  scale_fill_manual(values = team_hex) +
  coord_flip() +
  labs(x = NULL,
       y = "Total NPR",
       title = "2024 NPR Standings") +
  scale_y_continuous(breaks = seq(-10, 10, by = 1))
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
team_npr |>
  ggplot(aes(off_npr, def_npr)) +
  geom_point(aes(col = team), shape = "square", size = 4, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = team), size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_hex) +
  scale_x_continuous(breaks = seq(-10, 10, by = 1)) +
  scale_y_continuous(breaks = seq(-10, 10, by = 1)) +
  labs(x = "Offensive NPR", y = "Defensive NPR",
       title = "Scatterplot of Offensive/Defensive NPR")
```

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
