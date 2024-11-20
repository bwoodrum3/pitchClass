library(dplyr)
library(tidyverse)
library(DBI)
library(baseballr)
library(ggplot2)
library(epanetReader)
library(RODBC)
library(rgl)
library(car)
library(extrafont) 
font_import()
loadfonts(device = "win")

theme_hack3r <- function(){ 
  font <- "Franklin Gothic Medium"   #assign font family up front
  
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
      plot.background = element_rect(fill = "gray15"),
      
      
      #since theme_minimal() already strips axis lines, 
      #we don't need to do that again
      
      #text elements
      text = element_text(color = 'green'),
      title = element_text(color = 'green'),
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
pitch_cols <- c("FF" = "red",
                  "TS" = "orange",
                  "SI" = "darkorange",
                  "FC" = "yellow",
                  "FA" = "darkred",
                  "CU" = "purple",
                  "KC" = "purple",
                  "SL" = "green",
                  "ST" = "darkgreen",
                  "SV" = "green",
                  "CH" = "blue",
                  "FS" = "darkblue",
                  "KN" = "gray")
pitch_type_macro <- function(x){
  if_else(x == "FF","FB",
          if_else(x == "TS","FB",
                  if_else(x == "SI","FB",
                          if_else(x == "FC","BB",
                                  if_else(x == "FA","FB",
                                          if_else(x == "CU","BB",
                                                  if_else(x == "KC","BB",
                                                          if_else(x == "SL","BB",
                                                                  if_else(x == "ST","BB",
                                                                          if_else(x == "SV","BB",
                                                                                  if_else(x == "CH","CH",
                                                                                          if_else(x == "FS","CH","OT"))))))))))))
}

ss <- function(x) scale(x,center=min(x)-0.01,scale=diff(range(x))+0.02)

# to use it:
# ggplot2::scale_color_manual(values = pitch_color)

setwd("C:/Users/Bradley/Documents/GitHub/pitchClass")

##### Initial Data Setup
# meatball <- odbcConnect("SQLEXPRESS01")
# dat24 <- sqlQuery(meatball, "
#           select s.pitcherid
#           	, s.batterid
#           	, s.pfx_x*12 [hb]
#           	, s.pfx_z*12 [ivb]
#           	, s.arm_angle
#           	, s.release_spin
#           	, s.release_spin
#           	, s.spin_axis
#           	, s.spin_dir
#           	, s.release_speed
#           	, s.woba_value
#           	, s.woba_denom
#           	, s.pitch_type
#           	, s.event_type
#           	, s.pitch_description
#           	, s.launch_angle
#           	, s.launch_speed
#           	, s.stand
#           	, s.p_throws
#           	, case when s.inning_topbot='Bot' then s.home_team else s.vis_team end [pit_team]
#           	, s.release_extension
#           	, s.gameid
#           	, s.game_date
#           	, s.inning
#           	, s.pa_number
#           	, s.n_thruorder_pitcher
#           from [master].[dbo].[pitStatcast] s
#           where s.game_type='Regular Season'
#                   ")
# close(meatball)
# write.csv(dat24,"dat24.csv", row.names = FALSE)

##### Successive Data Setup
if(nrow(dat24)<1000) {
  dat24 <- read.csv("dat24.csv")
} else {
  print("We good.")
}
colnames(dat24)[20] <- "ballpark"

qualPitchers <- dat24 %>% group_by(pitcherid) %>% summarise(
  n = n()
)
qualPitchers <- subset(qualPitchers, n > 300)

qualDat24 <- inner_join(dat24,qualPitchers,by = "pitcherid")

ggplot(qualDat24, aes(hb, ivb)) +
  geom_point(aes(colour = pitch_type)) +
  facet_grid(cols = vars(p_throws)) +
  ggplot2::scale_color_manual(values = pitch_cols)



pitcherAvgs <- qualDat24 %>% group_by(pitcherid, pitch_type, p_throws) %>%
  summarise(
    hb = mean(hb),
    ivb = mean(ivb),
    velo = mean(release_speed),
    spin = mean(release_spin),
    nPitches = mean(n)
  )
pitcherAvgs$typeMacro <- as.factor(pitch_type_macro(pitcherAvgs$pitch_type))
pitcherAvgs$hbStand <- with(pitcherAvgs,(if_else(p_throws=='L',-1*hb,hb)))

ggplot(pitcherAvgs, aes(hb, ivb)) +
  geom_point(aes(colour = pitch_type)) +
  facet_grid(cols = vars(p_throws)) +
  ggplot2::scale_color_manual(values = pitch_cols)

ggplot(pitcherAvgs, aes(hbStand, ivb)) +
  geom_point(aes(colour = pitch_type)) +
#  facet_grid(cols = vars(p_throws)) +
  ggplot2::scale_color_manual(values = pitch_cols)

mycolors <- c('darkgreen', 'royalblue1', 'darkred','gray')
pitcherAvgs$color <- mycolors[ as.numeric(pitcherAvgs$typeMacro) ]

with(subset(pitcherAvgs, typeMacro != 'OT'), 
     plot3d(
          x = hbStand, 
          y = ivb, 
          z = velo, 
          col = color,
#          type = 's',
#          groups = typeMacro,
#          surface=FALSE, grid = FALSE, ellipsoid = TRUE,
          xlab = "Horizotnal Break",
          ylab = "Vertical Break",
          zlab = "Velo",
          bg.col = "black",
          radius = nPitches)
)