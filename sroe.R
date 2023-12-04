library(baseballr)
library(dplyr)
library(tidyverse)
library(xgboost)
library(caret)
library(vip)
library(ggplot2)
library(mlbplotR)
library(ggpmisc)
library(gt)

annual_statcast_query <- function(season) {
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = '4 days')
  
  date_grid <- tibble::tibble(start_date = dates, 
                              end_date = dates + 3)
  
  safe_savant <- purrr::safely(scrape_statcast_savant)
  
  payload <- purrr::map(.x = seq_along(date_grid$start_date), 
                        ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                          
                          payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                                 end_date = date_grid$end_date[.x], type = 'pitcher')
                          
                          return(payload)
                        })
  
  payload_df <- purrr::map(payload, 'result')
  
  number_rows <- purrr::map_df(.x = seq_along(payload_df), 
                               ~{number_rows <- tibble::tibble(week = .x, 
                                                               number_rows = length(payload_df[[.x]]$game_date))}) %>%
    dplyr::filter(number_rows > 0) %>%
    dplyr::pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  payload_df_reduced_formatted <- purrr::map(.x = seq_along(payload_df_reduced), 
                                             ~{cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                                                                      "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                                                                      "fielder_8", "fielder_9")
                                             
                                             df <- purrr::pluck(payload_df_reduced, .x) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, as.numeric) %>%
                                               dplyr::mutate_at(.vars = cols_to_transform, function(x) {
                                                 ifelse(is.na(x), 999999999, x)
                                               })
                                             
                                             character_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "character") %>%
                                               dplyr::pull(variable)
                                             
                                             numeric_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "numeric") %>%
                                               dplyr::pull(variable)
                                             
                                             integer_columns <- data_base_column_types %>%
                                               dplyr::filter(class == "integer") %>%
                                               dplyr::pull(variable)
                                             
                                             df <- df %>%
                                               dplyr::mutate_if(names(df) %in% character_columns, as.character) %>%
                                               dplyr::mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
                                               dplyr::mutate_if(names(df) %in% integer_columns, as.integer)
                                             
                                             return(df)
                                             })
  
  combined <- payload_df_reduced_formatted %>%
    dplyr::bind_rows()
  
  combined
}


format_append_statcast <- function(df) {
  
  # function for appending new variables to the data set
  
  additional_info <- function(df) {
    
    # apply additional coding for custom variables
    
    df$hit_type <- with(df, ifelse(type == "X" & events == "single", 1,
                                   ifelse(type == "X" & events == "double", 2,
                                          ifelse(type == "X" & events == "triple", 3, 
                                                 ifelse(type == "X" & events == "home_run", 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & events == "single", 1,
                              ifelse(type == "X" & events == "double", 1,
                                     ifelse(type == "X" & events == "triple", 1, 
                                            ifelse(type == "X" & events == "home_run", 1, NA)))))
    
    df$fielding_team <- with(df, ifelse(inning_topbot == "Bot", away_team, home_team))
    
    df$batting_team <- with(df, ifelse(inning_topbot == "Bot", home_team, away_team))
    
    df <- df %>%
      dplyr::mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      dplyr::mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      dplyr::filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    dplyr::arrange(game_date)
  
  df <- df %>%
    dplyr::filter(!is.na(game_date))
  
  df <- df %>%
    dplyr::ungroup()
  
  df <- df %>%
    dplyr::select(setdiff(names(.), c("error")))
  
  return(df)
}

pitches <- pitches %>%
  mutate(woba_value = ifelse(is.na(woba_value), 0, woba_value))

pitches <- pitches %>%
  group_by(batter, game_year) %>%
  mutate(pitch_count = row_number())

woba_data <- pitches %>%
  select(batter, game_year, pitch_count, woba_value)

woba_data <- woba_data %>%
  group_by(batter, game_year) %>%
  mutate(cumulative_pre_woba = cumsum(woba_value) - woba_value)

pitches <- inner_join(pitches, woba_data, by = c("batter", "game_year", "pitch_count"))

pitch_data <- pitches %>%
  mutate(pitch_type = as.factor(pitch_type), zone = as.factor(zone), game_type = as.factor(game_type), stand = as.factor(stand), p_throws = as.factor(p_throws), balls = as.factor(balls), strikes = as.factor(strikes), outs_when_up = as.factor(outs_when_up), inning = as.factor(inning), inning_topbot = as.factor(inning_topbot), pitch_number = as.factor(pitch_number)) %>%
  mutate(score_diff = fld_score - bat_score, on_1b = ifelse(is.na(on_1b), 0, 1), on_2b = ifelse(is.na(on_2b), 0, 1), on_3b = ifelse(is.na(on_3b), 0, 1), strike = ifelse(type == "S", 1, 0)) %>%
  mutate(on_1b = as.factor(on_1b), on_2b = as.factor(on_2b), on_3b = as.factor(on_3b)) %>%
  select(pitcher, strike, pitch_type, release_speed, release_pos_x, release_pos_z, zone, game_type, stand, p_throws, balls, strikes, pfx_x, pfx_z, plate_x, plate_z, on_1b, on_2b, on_3b, outs_when_up, inning, inning_topbot, vx0, vy0, vz0, ax, ay, az, sz_top, sz_bot, effective_speed, release_spin_rate, release_extension, release_pos_y, pitch_number, score_diff, spin_axis, cumulative_pre_woba)

factor_data <- pitch_data %>%
  ungroup() %>%
  select(-batter, -game_year, -pitcher)

dummy <- dummyVars(" ~ .", data = factor_data)
pitch_factor <- data.frame(predict(dummy, newdata = factor_data))

pitch_data <- pitch_data %>%
  ungroup() %>%
  select(game_year, pitcher)

pitch_data <- cbind(pitch_data, pitch_factor)

xgboost_train <- pitch_data %>%
  filter(game_year != 2023)

xgboost_test <- pitch_data %>%
  filter(game_year == 2023)

labels_train <- as.matrix(xgboost_train[,3])
xgboost_trainfinal <- as.matrix(xgboost_train[, c(4:126)])
xgboost_testfinal <- as.matrix(xgboost_test[, c(4:126)])

soe_model <- xgboost(data = xgboost_trainfinal, label = labels_train, nrounds = 100, objective = "binary:logistic", early_stopping_rounds = 10, max_depth = 6, eta = 0.3)

vip(soe_model)
vi(soe_model)
summary(soe_model)

s_predict <- predict(soe_model, xgboost_testfinal)
s_actual <- as.matrix(xgboost_test[,3])
postResample(s_predict, s_actual)

s_predictions <- as.data.frame(
  matrix(predict(soe_model, as.matrix(pitch_data[,c(4:126)])))
)

all_stats <- cbind(pitch_data, s_predictions) %>%
  select(game_year, pitcher, strike, pred_strike = V1)

all_stats <- all_stats %>%
  group_by(game_year, pitcher) %>%
  summarize(pitches = n(), strikes = sum(strike), pred_strikes = sum(pred_strike), strike_ratio = strikes/pitches, pred_strike_ratio = pred_strikes/pitches, sroe = strike_ratio - pred_strike_ratio)

find_pitchers <- chadwick_player_lu() %>%
  select(key_mlbam, key_bbref)

bbref_pitchers <- bref_daily_pitcher("2023-01-01", "2023-12-31") %>%
  mutate(team_games = 162) %>%
  filter(IP/team_games >= 1.0)

qual_pitchers <- inner_join(bbref_pitchers, find_pitchers, by = c("bbref_id" = "key_bbref")) %>%
  select(name = Name, pitcher = key_mlbam)

stats_2023 <- left_join(qual_pitchers, all_stats, by = c("pitcher")) %>%
  filter(game_year == 2023)

write_csv(stats_2023, "stats_2023.csv")

stats_2023 <- read_csv("stats_2023.csv")

plot <- stats_2023 %>%
  ggplot(aes(x = strike_ratio, y = pred_strike_ratio)) +
  geom_hline(yintercept = mean(stats_2023$pred_strike_ratio), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = mean(stats_2023$strike_ratio), color = "red", linetype = "dashed", alpha = 0.5) +
  geom_mlb_headshots(aes(player_id = pitcher), height = 0.1) +
  geom_smooth(method = "lm") + 
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) + 
  labs(x = "Strike-to-Pitch Ratio",
       y = "Predicted Strike-to-Pitch Ratio",
       title = "Predicting Strike-to-Pitch Ratio and Quantifying SROE",
       caption = "Amrit Vignesh") + 
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20))

ggsave("graph.png", plot, width = 10, height = 6, dpi = "retina")

top10 <- stats_2023 %>%
  arrange(-sroe) %>%
  filter(row_number() <= 10) %>%
  mutate(strike_ratio = round(strike_ratio, 4), pred_strike_ratio = round(pred_strike_ratio, 4), sroe = round(sroe, 4)) %>%
  select(name, strike_ratio, pred_strike_ratio, sroe)

bot10 <- stats_2023 %>%
  arrange(sroe) %>%
  filter(row_number() <= 10) %>%
  mutate(strike_ratio = round(strike_ratio, 4), pred_strike_ratio = round(pred_strike_ratio, 4), sroe = round(sroe, 4)) %>%
  select(name, strike_ratio, pred_strike_ratio, sroe)

top10 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(name, strike_ratio, pred_strike_ratio, sroe)
  ) %>%
  data_color(
    columns = sroe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    name = md("**Pitcher**"),
    strike_ratio = md("**Strike Ratio**"),
    pred_strike_ratio = md("**Pred. Strike Ratio**"),
    sroe = md("**SROE**")
  ) %>%
  tab_header(
    title = md("**2023 MLB Top 10 SROE (Strike-to-Pitch Ratio Over Expected)**"),
    subtitle = "Trained Data From 2021 and 2022 Season, Qualified Pitchers Only"
  ) 

bot10 %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(name, strike_ratio, pred_strike_ratio, sroe)
  ) %>%
  data_color(
    columns = sroe,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL,
      reverse = TRUE
    )
  ) %>%
  cols_label(
    name = md("**Pitcher**"),
    strike_ratio = md("**Strike Ratio**"),
    pred_strike_ratio = md("**Pred. Strike Ratio**"),
    sroe = md("**SROE**")
  ) %>%
  tab_header(
    title = md("**2023 MLB Bottom 10 SROE (Strike-to-Pitch Ratio Over Expected)**"),
    subtitle = "Trained Data From 2021 and 2022 Season, Qualified Pitchers Only"
  ) 
