library(dplyr)
library(tidyverse)
library(DBI)
library(baseballr)
library(ggplot2)

theme_hack3r <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      # add border 1)
      panel.border = element_rect(colour = "darkgreen", fill = NA, linetype = 2),
      # color background 2)
      panel.background = element_rect(fill = "gray15"),
      # modify grid 3)
      panel.grid.major.x = element_line(colour = "darkgreen", 
                                        linetype = 3, 
                                        linewidth = 0.5),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y =  element_line(colour = "darkgreen", 
                                         linetype = 3, 
                                         linewidth = 0.5),
      panel.grid.minor.y = element_blank(),
      
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        color = 'green',
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        color = 'green',
        size = 14),               #font size
      
      plot.caption = element_text(           #caption
        family = font,            #font family
        color = 'green',
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        color = 'darkgreen',
        size = 10),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        color = 'green',
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
      
      #since the legend often requires manual tweaking 
      #based on plot content, don't define it here
    )
}

theme_set(theme_hack3r())

pitch_colors <- c("4-Seam Fastball" = "red",
                  "2-Seam Fastball" = "orange",
                  "Sinker" = "darkorange",
                  "Cutter" = "yellow",
                  "Fastball" = "darkred",
                  "Curveball" = "purple",
                  "Knuckle Curve" = "purple",
                  "Slider" = "green",
                  "Changeup" = "blue",
                  "Split-Finger" = "darkblue",
                  "Knuckleball" = "gray")

# to use it:
# ggplot2::scale_color_manual(values = pitch_color)

annual_statcast_query <- function(season) {
  
  # create weeks of dates for season from mar - nov
  # includes spring training + postseason
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = 'week')
  
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  
  # create 'safe' version of scrape_statcast_savant in case week doesn't process
  safe_savant <- safely(scrape_statcast_savant)
  
  # loop over each row of date_grid, and collect each week in a df
  payload <- map(.x = seq_along(date_grid$start_date), 
                 ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                   
                   payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                          end_date = date_grid$end_date[.x], type = 'pitcher')
                   
                   return(payload)
                 })
  
  payload_df <- map(payload, 'result')
  
  # eliminate results with an empty dataframe
  number_rows <- map_df(.x = seq_along(payload_df), 
                        ~{number_rows <- tibble(week = .x, 
                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
    filter(number_rows > 0) %>%
    pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  combined <- payload_df_reduced %>%
    bind_rows()
  
  return(combined)
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
      mutate(barrel = ifelse(launch_angle <= 50 & launch_speed >= 98 & launch_speed * 1.5 - launch_angle >= 117 & launch_speed + launch_angle >= 124, 1, 0))
    
    df <- df %>%
      mutate(spray_angle = round(
        (atan(
          (hc_x-125.42)/(198.27-hc_y)
        )*180/pi*.75)
        ,1)
      )
    
    df <- df %>%
      filter(!is.na(game_year))
    
    return(df)
  }
  
  df <- df %>%
    additional_info()
  
  df$game_date <- as.character(df$game_date)
  
  df <- df %>%
    arrange(game_date)
  
  df <- df %>%
    filter(!is.na(game_date))
  
  df <- df %>%
    ungroup()
  
  df <- df %>%
    select(setdiff(names(.), c("error")))
  
  cols_to_transform <- c("fielder_2", "pitcher_1", "fielder_2_1", "fielder_3",
                         "fielder_4", "fielder_5", "fielder_6", "fielder_7",
                         "fielder_8", "fielder_9")
  
  df <- df %>%
    mutate_at(.vars = cols_to_transform, as.numeric) %>%
    mutate_at(.vars = cols_to_transform, function(x) {
      ifelse(is.na(x), 999999999, x)
    })
  
  data_base_column_types <- read_csv("https://app.box.com/shared/static/q326nuker938n2nduy81au67s2pf9a3j.csv")
  
  character_columns <- data_base_column_types %>%
    filter(class == "character") %>%
    pull(variable)
  
  numeric_columns <- data_base_column_types %>%
    filter(class == "numeric") %>%
    pull(variable)
  
  integer_columns <- data_base_column_types %>%
    filter(class == "integer") %>%
    pull(variable)
  
  df <- df %>%
    mutate_if(names(df) %in% character_columns, as.character) %>%
    mutate_if(names(df) %in% numeric_columns, as.numeric) %>%
    mutate_if(names(df) %in% integer_columns, as.integer)
  
  return(df)
}

stat24 <- annual_statcast_query(2024)
format_append_statcast(stat24)

