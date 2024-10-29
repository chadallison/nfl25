
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

------------------------------------------------------------------------

### Offense and Defense EPA Comparisons

    ## [1] "NYJ vs. HOU: Bad Offense, Good Defense vs. Good Offense, Good Defense"
    ## [1] "ATL vs. DAL: Good Offense, Bad Defense vs. Bad Offense, Bad Defense"
    ## [1] "BAL vs. DEN: Good Offense, Bad Defense vs. Bad Offense, Good Defense"
    ## [1] "BUF vs. MIA: Good Offense, Good Defense vs. Bad Offense, Good Defense"
    ## [1] "CAR vs. NO: Bad Offense, Bad Defense vs. Bad Offense, Good Defense"
    ## [1] "CIN vs. LV: Good Offense, Bad Defense vs. Bad Offense, Good Defense"
    ## [1] "CLE vs. LAC: Bad Offense, Good Defense vs. Bad Offense, Good Defense"
    ## [1] "NYG vs. WAS: Bad Offense, Bad Defense vs. Good Offense, Bad Defense"
    ## [1] "TEN vs. NE: Bad Offense, Bad Defense vs. Bad Offense, Bad Defense"
    ## [1] "ARI vs. CHI: Good Offense, Bad Defense vs. Bad Offense, Good Defense"
    ## [1] "PHI vs. JAX: Good Offense, Good Defense vs. Good Offense, Bad Defense"
    ## [1] "GB vs. DET: Good Offense, Good Defense vs. Good Offense, Good Defense"
    ## [1] "SEA vs. LA: Bad Offense, Bad Defense vs. Bad Offense, Bad Defense"
    ## [1] "MIN vs. IND: Good Offense, Good Defense vs. Bad Offense, Good Defense"
    ## [1] "KC vs. TB: Good Offense, Good Defense vs. Good Offense, Bad Defense"

------------------------------------------------------------------------

### DVOA

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

------------------------------------------------------------------------

------------------------------------------------------------------------

### NPR Standings

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

------------------------------------------------------------------------

### Scatterplot of NPR

![](README_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

------------------------------------------------------------------------

### Week Predictions

    ## [1] "BUF def. MIA (98.2%)"
    ## [1] "NO def. CAR (97.3%)"
    ## [1] "PHI def. JAX (92.6%)"
    ## [1] "LAC def. CLE (86.1%)"
    ## [1] "MIN def. IND (82.4%)"
    ## [1] "WAS def. NYG (79.5%)"
    ## [1] "CIN def. LV (72.6%)"
    ## [1] "HOU def. NYJ (72.3%)"
    ## [1] "LA def. SEA (68.8%)"
    ## [1] "ATL def. DAL (68.5%)"
    ## [1] "DET def. GB (67.6%)"
    ## [1] "BAL def. DEN (61.1%)"
    ## [1] "NE def. TEN (60.3%)"
    ## [1] "ARI def. CHI (59.2%)"
    ## [1] "KC def. TB (53.3%)"

``` r
season_pbp |>
  filter(play_type == "pass") |>
  group_by(passer, week) |>
  summarise(n = n(),
            avg_epa = mean(epa, na.rm = T),
            .groups = "drop") |>
  filter(week == max(end_games$week) & n >= 5) |>
  arrange(desc(avg_epa)) |>
  mutate(avg_epa = round(avg_epa, 3),
         passer = as.character(glue::glue("{passer} ({round(avg_epa, 2)})"))) |>
  ggplot(aes(reorder(passer, avg_epa), avg_epa)) +
  geom_col(aes(fill = avg_epa), show.legend = F) +
  coord_flip() +
  scale_fill_gradient(low = "indianred3", high = "springgreen4") +
  labs(x = NULL, y = "Avg. EPA",
       title = "Avg. EPA by QBs this past week") +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1))
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
team_pass_run_share = season_pbp |>
  filter(play_type %in% c("pass", "run")) |>
  group_by(team = posteam) |>
  count(play_type) |>
  mutate(pct = round(n / sum(n), 3)) |>
  ungroup()

run_heavy = team_pass_run_share |>
  filter(play_type == "run") |>
  arrange(desc(pct)) |>
  pull(team)

team_pass_run_share |>
  mutate(team = factor(team, levels = rev(run_heavy)),
         play_type = ifelse(play_type == "pass", "Pass", "Run")) |>
  ggplot(aes(team, pct)) +
  geom_col(aes(fill = play_type)) +
  coord_flip() +
  labs(x = NULL, y = "Rushing Share", fill = NULL,
       title = "Run the damn ball!") +
  scale_fill_manual(values = c("lightsteelblue3", "springgreen4")) +
  theme(legend.position = "right") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent)
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
team_run_shares_vec = team_pass_run_share |>
  filter(play_type == "run") |>
  arrange(team) |>
  pull(pct)

team_off_ypg_vec = team_ypg |>
  arrange(team) |>
  pull(off_ypg)

cor_coef = round(cor(team_run_shares_vec, team_off_ypg_vec), 3)

team_pass_run_share |>
  filter(play_type == "run") |>
  select(team, run_pct = pct) |>
  inner_join(team_ypg |>
  select(team, off_ypg), by = "team") |>
  ggplot(aes(run_pct, off_ypg)) +
  geom_point(aes(col = team), size = 4, shape = "square", show.legend = F) +
  geom_line(stat = "smooth", formula = y ~ x, method = "lm", linetype = "dashed", alpha = 0.5) +
  ggrepel::geom_text_repel(aes(label = team), size = 3) +
  scale_color_manual(values = team_hex) +
  labs(x = "Rushing share", y = "Offensive YPG",
       title = "Does running the damn ball work?",
       subtitle = glue("Correlation: {cor_coef}")) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent) +
  scale_y_continuous(breaks = seq(200, 500, by = 25))
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
season_pbp |>
  filter(play_type %in% c("pass", "run")) |>
  group_by(team = posteam, play_type) |>
  summarise(epa = mean(epa, na.rm = T),
            .groups = "drop") |>
  pivot_wider(id_cols = team, names_from = "play_type", values_from = "epa") |>
  ggplot(aes(run, pass)) +
  geom_point(aes(col = team), size = 4, shape = "square", show.legend = F) +
  scale_color_manual(values = team_hex) +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 32) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
season_pbp |>
  filter(play_type %in% c("pass", "run")) |>
  group_by(team = defteam, play_type) |>
  summarise(epa = mean(epa, na.rm = T),
            .groups = "drop") |>
  pivot_wider(id_cols = team, names_from = "play_type", values_from = "epa") |>
  ggplot(aes(run, pass)) +
  geom_point(aes(col = team), size = 4, shape = "square", show.legend = F) +
  scale_color_manual(values = team_hex) +
  ggrepel::geom_text_repel(aes(label = team), size = 3, max.overlaps = 32) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
```

![](README_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
season_pbp |>
  filter(play_type %in% c("pass", "run")) |>
  group_by(team = posteam) |>
  summarise(off_epa = mean(epa, na.rm = T)) |>
  inner_join(season_pbp |>
  filter(play_type %in% c("pass", "run")) |>
  group_by(team = defteam) |>
  summarise(def_epa = mean(epa, na.rm = T)), by = "team") |>
  mutate(ovr_epa = off_epa + def_epa) |>
  arrange(desc(ovr_epa))
```

    ## # A tibble: 32 × 4
    ##    team  off_epa  def_epa ovr_epa
    ##    <chr>   <dbl>    <dbl>   <dbl>
    ##  1 BAL    0.214   0.0693   0.283 
    ##  2 WAS    0.214   0.00718  0.221 
    ##  3 JAX    0.0148  0.170    0.185 
    ##  4 ARI    0.0586  0.125    0.183 
    ##  5 CIN    0.0803  0.0823   0.163 
    ##  6 TB     0.0907  0.0681   0.159 
    ##  7 BUF    0.197  -0.0690   0.128 
    ##  8 ATL    0.0662  0.0541   0.120 
    ##  9 PHI    0.113  -0.00162  0.111 
    ## 10 DAL   -0.0786  0.155    0.0765
    ## # ℹ 22 more rows

``` r
season_pbp |>
  filter(week >= 4) |>
  filter(play_type == "pass") |>
  group_by(passer) |>
  summarise(n = n(),
            epa_per_db = mean(epa, na.rm = T)) |>
  mutate(n_pct = percent_rank(n)) |>
  filter(n_pct >= 0.5) |>
  ggplot(aes(n, epa_per_db)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = passer), size = 3, max.overlaps = 50) +
  labs(x = "Number of dropbacks",
       y = "EPA per dropback",
       title = "EPA per dropback",
       subtitle = "Weeks 4-6 / Only QBs in 50th percentile of higher in dropbacks") +
  scale_x_continuous(breaks = seq(0, 1000, by = 10)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.1))
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
season_pbp |>
  filter(play_type == "pass" & week >= 4) |>
  group_by(passer) |>
  summarise(n = n(),
            success_rate = round(mean(success, na.rm = T), 3)) |>
  mutate(n_pct = percent_rank(n)) |>
  filter(n_pct >= 0.5) |>
  arrange(desc(success_rate))
```

    ## # A tibble: 33 × 4
    ##    passer         n success_rate n_pct
    ##    <chr>      <int>        <dbl> <dbl>
    ##  1 L.Jackson    153        0.569 0.815
    ##  2 J.Goff        96        0.552 0.554
    ##  3 P.Mahomes    140        0.55  0.754
    ##  4 B.Mayfield   211        0.521 0.969
    ##  5 M.Stafford   139        0.518 0.723
    ##  6 T.Lawrence   161        0.516 0.877
    ##  7 J.Burrow     171        0.503 0.892
    ##  8 K.Murray     151        0.49  0.8  
    ##  9 C.Williams   116        0.483 0.662
    ## 10 J.Daniels    139        0.482 0.723
    ## # ℹ 23 more rows

``` r
team_logos = teams_colors_logos |>
  filter(team_abbr %in% all_teams) |>
  select(team_abbr, team_logo_espn)

# team success rate by down (passing and running plays)
pass_epa_success = season_pbp |>
  filter(play_type == "pass") |>
  group_by(posteam, play_type) |>
  summarise(avg_epa = mean(epa),
            success_rate = mean(success),
            .groups = "drop")

opesp = pass_epa_success |>
  ggplot(aes(avg_epa, success_rate)) +
  geom_point(aes(col = posteam), shape = "square", size = 4, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = posteam), size = 3, max.overlaps = 32) +
  scale_color_manual(values = team_hex) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linetype = "dotted", alpha = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = mean(pass_epa_success$success_rate), linetype = "dashed", alpha = 0.5) +
  labs(x = "Offensive EPA per play, passing",
       y = "Offensive success rate, passing",
       title = "Offensive EPA per play vs. success rate by team (passing)") +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent)

rush_epa_success = season_pbp |>
  filter(play_type == "run") |>
  group_by(posteam, play_type) |>
  summarise(avg_epa = mean(epa),
            success_rate = mean(success),
            .groups = "drop")

oresp = rush_epa_success |>
  ggplot(aes(avg_epa, success_rate)) +
  geom_point(aes(col = posteam), shape = "square", size = 4, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = posteam), size = 3, max.overlaps = 32) +
  scale_color_manual(values = team_hex) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linetype = "dotted", alpha = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = mean(rush_epa_success$success_rate), linetype = "dashed", alpha = 0.5) +
  labs(x = "Offensive EPA per play, rushing",
       y = "Offensive success rate, rushing",
       title = "Offensive EPA per play vs. success rate by team (rushing)") +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent)

pass_epa_success = season_pbp |>
  filter(play_type == "pass") |>
  group_by(defteam, play_type) |>
  summarise(avg_epa = mean(epa),
            success_rate = mean(success),
            .groups = "drop")

dpesp = pass_epa_success |>
  ggplot(aes(avg_epa, success_rate)) +
  geom_point(aes(col = defteam), shape = "square", size = 4, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = defteam), size = 3, max.overlaps = 32) +
  scale_color_manual(values = team_hex) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linetype = "dotted", alpha = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = mean(pass_epa_success$success_rate), linetype = "dashed", alpha = 0.5) +
  labs(x = "Defensive EPA per play, passing",
       y = "Defensive success rate, passing",
       title = "Defensive EPA per play vs. success rate by team (passing)") +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent)

rush_epa_success = season_pbp |>
  filter(play_type == "run") |>
  group_by(defteam, play_type) |>
  summarise(avg_epa = mean(epa),
            success_rate = mean(success),
            .groups = "drop")

dresp = rush_epa_success |>
  ggplot(aes(avg_epa, success_rate)) +
  geom_point(aes(col = defteam), shape = "square", size = 4, show.legend = F) +
  ggrepel::geom_text_repel(aes(label = defteam), size = 3, max.overlaps = 32) +
  scale_color_manual(values = team_hex) +
  geom_line(stat = "smooth", method = "lm", formula = y ~ x, linetype = "dotted", alpha = 0.25) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = mean(rush_epa_success$success_rate), linetype = "dashed", alpha = 0.5) +
  labs(x = "Defensive EPA per play, rushing",
       y = "Defensive success rate, rushing",
       title = "Defensive EPA per play vs. success rate by team (rushing)") +
  scale_x_continuous(breaks = seq(-0.5, 0.5, by = 0.05)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.05), labels = scales::percent)

opesp
```

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
oresp
```

![](README_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
dpesp
```

![](README_files/figure-gfm/unnamed-chunk-19-3.png)<!-- -->

``` r
dresp
```

![](README_files/figure-gfm/unnamed-chunk-19-4.png)<!-- -->

``` r
suppressMessages(library(ggimage))
```

    ## Warning: package 'ggimage' was built under R version 4.3.2

``` r
team_logos = teams_colors_logos |>
  filter(team_abbr %in% all_teams) |>
  select(team = team_abbr, team_logo_wikipedia)

team_epa_with_logos = team_epa |>
  inner_join(team_logos, by = "team")

team_epa_with_logos |>
  ggplot(aes(avg_off_epa, avg_def_epa)) +
  geom_image(aes(image = team_logo_wikipedia), size = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = team_hex) +
  labs(x = "Avg. Offensive EPA",
       y = "Avg. Defensive EPA",
       title = "Offensive and Defensive EPA") +
  scale_x_continuous(breaks = seq(-1, 1, by = 0.05)) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.05))
```

![](README_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
off_qtr_epa_success = season_pbp |>
  filter(play_type %in% c("pass", "run") & qtr %in% 1:4) |>
  group_by(team = posteam, qtr) |>
  summarise(avg_epa = mean(epa, na.rm = T),
            success_rate = mean(success, na.rm = T),
            .groups = "drop")

def_qtr_epa_success = season_pbp |>
  filter(play_type %in% c("pass", "run") & qtr %in% 1:4) |>
  group_by(team = defteam, qtr) |>
  summarise(avg_epa = mean(epa, na.rm = T),
            success_rate = mean(success, na.rm = T),
            .groups = "drop")

off_qtr_epa_success |>
  mutate(qtr = case_when(qtr == 1 ~ "1st Quarter",
                         qtr == 2 ~ "2nd Quarter",
                         qtr == 3 ~ "3rd Quarter",
                         qtr == 4 ~ "4th Quarter")) |>
  group_by(qtr) |>
  slice_max(avg_epa, n = 3, with_ties = F) |>
  ungroup() |>
  ggplot(aes(reorder(team, avg_epa), avg_epa)) +
  geom_col(aes(fill = team), show.legend = F) +
  coord_flip() +
  scale_fill_manual(values = team_hex) +
  facet_wrap(vars(qtr), scales = "free_y") +
  labs(x = NULL, y = "EPA per play")
```

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
off_qtr_epa_success |>
  mutate(qtr = case_when(qtr == 1 ~ "1st Quarter",
                         qtr == 2 ~ "2nd Quarter",
                         qtr == 3 ~ "3rd Quarter",
                         qtr == 4 ~ "4th Quarter")) |>
  group_by(qtr) |>
  slice_max(avg_epa, n = 3, with_ties = F) |>
  ungroup() |>
  ggplot(aes(reorder(team, success_rate), success_rate)) +
  geom_col(aes(fill = team), show.legend = F) +
  coord_flip() +
  scale_fill_manual(values = team_hex) +
  facet_wrap(vars(qtr), scales = "free_y") +
  labs(x = NULL, y = "Offensive success rate")
```

![](README_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

``` r
def_qtr_epa_success |>
  mutate(qtr = case_when(qtr == 1 ~ "1st Quarter",
                         qtr == 2 ~ "2nd Quarter",
                         qtr == 3 ~ "3rd Quarter",
                         qtr == 4 ~ "4th Quarter")) |>
  group_by(qtr) |>
  slice_min(avg_epa, n = 3, with_ties = F) |>
  ungroup() |>
  ggplot(aes(reorder(team, avg_epa), avg_epa)) +
  geom_col(aes(fill = team), show.legend = F) +
  coord_flip() +
  scale_fill_manual(values = team_hex) +
  facet_wrap(vars(qtr), scales = "free_y") +
  labs(x = NULL, y = "EPA per play")
```

![](README_files/figure-gfm/unnamed-chunk-21-3.png)<!-- -->

``` r
def_qtr_epa_success |>
  mutate(qtr = case_when(qtr == 1 ~ "1st Quarter",
                         qtr == 2 ~ "2nd Quarter",
                         qtr == 3 ~ "3rd Quarter",
                         qtr == 4 ~ "4th Quarter")) |>
  group_by(qtr) |>
  slice_min(avg_epa, n = 3, with_ties = F) |>
  ungroup() |>
  ggplot(aes(reorder(team, success_rate), success_rate)) +
  geom_col(aes(fill = team), show.legend = F) +
  coord_flip() +
  scale_fill_manual(values = team_hex) +
  facet_wrap(vars(qtr), scales = "free_y") +
  labs(x = NULL, y = "EPA per play")
```

![](README_files/figure-gfm/unnamed-chunk-21-4.png)<!-- -->
