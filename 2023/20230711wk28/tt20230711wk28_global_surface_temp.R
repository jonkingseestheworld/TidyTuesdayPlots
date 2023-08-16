#wk 28: 2023-07-11
#Author: Johnny K Lau
#Subject: Global Surface Temp


library(tidyverse)
library(zoo)

library(glue)
library(ggplot2)
library(ggtext)
library(gganimate)

#library(lubridate)
#library(janitor)
#library(patchwork)
#library(showtext)


# 💾 Load data ---------------------------------------------------------------
global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')


# ✍️ Fonts and palettes ------------------------------------------------------
TXT_COLOR <- "lightgrey"
BG_COLOR <- "black"
BR_PAL <- c('#08306b', '#08519c', '#2171b5', '#4292c6', '#6baed6', '#9ecae1', '#c6dbef', '#deebf7', '#ffffff',
          '#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d')

AXIS_TEXT_SIZE <- 15
          


#font_add_google("Barlow", "bar")



# GLOBAL Trend ##
# 🤼 Wrangle -----------------------------------------------------------------
global_trend <- global_temps |>
  rename( annual_deviation = `J-D`) |>
  ## focusing on records since 1900 and before 2023 (data from 2023 do not constitute a whole year)
  filter( Year < 2023, Year >= 1900) 

## Compute 5-year rolling average & rolling sd
global_rolling <- global_trend |>
  select(yr = Year, annual_deviation) |>
  ## rolling mean
  mutate( roll_avg_5 = zoo::rollmean(annual_deviation, k=5, fill=NA, align="right") ) |>
  ## rolling sd
  mutate( roll_sd_5 = zoo::rollapply(annual_deviation, width=5, FUN=sd, fill=NA, align="right"))



# 📊 Plot ------------------------------------------------------------------------
## Content
DATASET <- "**Data**: GISS Surface Temperature Analysis"
space <- paste0("<span style='color:", BG_COLOR, "'>--</span>")

plot_caption <- paste0("**#TidyTuesday** 2023 Wk 28", space, "|", space, DATASET, space, "|", space, "**Plot**: Johnny K Lau", space, "@jonkingseestheworld (Github)")

title_txt = "RISING GLOBAL TEMPERATURE"
subtitle_txt = "The NASA GISS Surface Tempearture Analysis estimates global surface temperature change.<br>
Animated bar plots illustrate the annual temperature deviations compared to the 1951-1980 average, <br>with a background line showing 5-year rolling means \u00B11 standard deviations in gray. <span style='color:#fb6a4a'>Since early 1980s,</span> <br>the average annual temperature has kept soaring continually."


anim_global_plot <- global_trend |>
  ggplot() +
  ## bar plots
  geom_col(aes(x=Year, y=annual_deviation, fill = annual_deviation), width = 1, alpha=0.95) +
  scale_fill_gradientn(colours = BR_PAL, breaks = c(-0.4, 0, 0.4, 0.6, 0.8, 1) ) + 
  ## line plot: 5-year rolling means
  geom_line(data = global_rolling,
            aes(x = yr, y = roll_avg_5 ),
            color = "gray6")  +
  scale_x_continuous(breaks = c(1900, 1920, 1940, 1960, 1980, 2000, 2020)) +
  scale_y_continuous(labels = paste(c(-0.5, 0, 0.5, 1), "\u00b0C"),  
                     ) +
  ## gibbon shade area
  geom_ribbon(data = global_rolling,
              aes(x = yr,
                  ymax = roll_avg_5 + roll_sd_5,
                  ymin = roll_avg_5 - roll_sd_5),
              fill = "gray", alpha = 0.5
              ) + 
  #coord_cartesian(clip = "off") +
  ## draw a horizontal line
  geom_segment(aes(x = 1900,  y = 0,
                   xend = 2023, yend = 0),
               linetype = "dashed"
               ) +
  labs(title = title_txt,
       subtitle = subtitle_txt,
       fill = "Temp\nDeviation",
       caption = plot_caption
       ) +
  theme_void() +
  theme(
    text = element_text(#family = ft, 
                        colour = TXT_COLOR),
    plot.background = element_rect(fill = BG_COLOR,
                                   color = NA),
    panel.grid.major.y = element_line(color = "grey"),
    axis.text = element_text( #family = FONT_FAMILY,
                              size = AXIS_TEXT_SIZE
                              ),
    plot.margin = margin(c(20,20,10,20)),    #bltr
    plot.title = element_text(#family = FONT_FAMILY,
                              size = 25,
                              face = "bold"),
    plot.subtitle = element_markdown(#family = FONT_FAMILY,
                                     size = 18,
                                     lineheight = 1.2),
    plot.caption = element_textbox_simple(#family = FONT_FAMILY,
                                          halign = 0.5,  size = 15,
                                          margin = margin(b=10,l=0,t=25,r=0)),
    #plot.caption.position = "plot",
    legend.title = element_text( size = AXIS_TEXT_SIZE,
                                 margin = margin(b=12,0, 0, 0)),
    legend.text = element_text( size = AXIS_TEXT_SIZE-1),
    ) +  
  transition_time(Year) +
  shadow_mark()


animate(anim_global_plot,  height = 900, width =1200) 

global_filename = "2023/20230711wk28/tt20230711wk28_global_temp.gif"
anim_save(global_filename, animation = last_animation() )
      
      




# SH / NH Trend ##
# 🤼 Wrangle -----------------------------------------------------------------

df_NHSH <- bind_rows(
  "NH" = nh_temps,
  "SH" = sh_temps,
  .id = "region"
  ) |>
  select(region, Year, annual_deviation = `J-D`) |>
  ## focusing on records since 1900 and before 2023 (data from 2023 do not constitute a whole year)
  filter( Year < 2023, Year >= 1900) 

## Create a dataframe of selected years (for plotting year labels in the graphic later)
df_years <- df_NHSH |>
  filter(
    region == "NH",
    Year %in% seq(1920, 2000, 20)
    )


# 📊 Plot --------------------------------------------------------------------
## animated plot

t_color = "#292e29"

### Anim Plot for Northern Hemisphere
anim_NH_plot <- df_NHSH |>
  filter(region == "NH") |>
  ggplot() +
  ## main bar plots
  geom_col(aes(x=Year, y=1, fill = annual_deviation), width = 1, color=NA) +
  annotate("text", x = 1960, y = 0.65, label = "Northern Hemisphere", 
           size = 15, hjust=0.5,  colour = t_color, fontface = "bold") +
  annotate("text", x = 1960, y = 0.55, label = "Surface Temperature Change", 
           size = 11, hjust=0.5,  colour = t_color, fontface = "bold") +
  geom_text(aes(Year, 0.43, label = Year), df_years,  
            size = 10, colour = t_color ) +
  annotate("segment", x = 1910, xend = 2012, y = 0.47, yend = 0.47, colour = t_color ) +
  geom_point(aes(Year, 0.47), df_years, colour = t_color ) +
  scale_fill_gradientn(colours = BR_PAL) +
  labs(caption = plot_caption) +
  theme_void() +
  theme(
    text = element_text( size = 5, colour = t_color ),
    plot.background = element_rect(fill = BG_COLOR, colour = BG_COLOR),
    plot.caption = element_textbox_simple( halign = 0.5,  size = 15,
                                           margin = margin(b=5,l=0,t=0,r=0), 
                                           color="lightgrey"),
    plot.margin = margin(t=5, b=15),
    legend.position = "none"
  ) + transition_states(Year, wrap = FALSE) +
  shadow_mark()

animate(anim_NH_plot, height = 800, width =1000)

NH_filename = "2023/20230711wk28/tt20230711wk28_NorthernHemi_temp.gif"
anim_save(NH_filename, animation = last_animation() )



### Anim Plot for Southern Hemisphere
anim_SH_plot <- df_NHSH |>
  filter(region == "SH") |>
  ggplot() +
  ## main bar plots
  geom_col(aes(x=Year, y=1, fill = annual_deviation), width = 1, color=NA) +
  annotate("text", x = 1960, y = 0.65, label = "Southern Hemisphere", 
           size = 15, hjust=0.5,  colour = t_color, fontface = "bold") +
  annotate("text", x = 1960, y = 0.55, label = "Surface Temperature Change", 
           size = 11, hjust=0.5,  colour = t_color, fontface = "bold") +
  geom_text(aes(Year, 0.43, label = Year), df_years,  
            size = 10, colour = t_color ) +
  annotate("segment", x = 1910, xend = 2012, y = 0.47, yend = 0.47, colour = t_color ) +
  geom_point(aes(Year, 0.47), df_years, colour = t_color ) +
  scale_fill_gradientn(colours = BR_PAL) +
  labs(caption = plot_caption) +
  theme_void() +
  theme(
    text = element_text( size = 5, colour = t_color ),
    plot.background = element_rect(fill = BG_COLOR, colour = BG_COLOR),
    plot.caption = element_textbox_simple( halign = 0.5,  size = 15,
                                           margin = margin(b=5,l=0,t=0,r=0), 
                                           color="lightgrey"),
    plot.margin = margin(t=5, b=15),
    legend.position = "none"
  ) + transition_states(Year, wrap = FALSE) +
  shadow_mark()

animate(anim_SH_plot, height = 800, width =1000)

SH_filename = "2023/20230711wk28/tt20230711wk28_SouthernHemi_temp.gif"
anim_save(SH_filename, animation = last_animation() )

