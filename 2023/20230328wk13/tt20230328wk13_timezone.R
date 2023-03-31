#wk 13: 2023-03-28
#Author: Johnny K Lau
#Data Source:	IANA tz database


## Packages for data transformation
library(tidyverse)
library(ggplot2)
library(ggtext)

library(showtext)

font_add('Rob-reg', 'fonts/Roboto/Roboto-Regular.ttf')
#font_add_google(name = "Oswald", family = "Oswald")



## Import data
transitions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/transitions.csv')
timezones <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezones.csv')
timezone_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezone_countries.csv')
countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/countries.csv')

## Explore
# usa_timezones <- timezones %>%
#  filter(grepl("America", zone)) %>%
#  right_join(timezone_countries %>%
#               filter(country_code == "US"), 
#             by = "zone")




## Data cleaning
### Which countries still practise Daylight Saving
dls2023_transitions <- transitions %>%
  filter( grepl("2023", begin) ) %>%
  filter( dst == T)

dls_countries <- countries %>% 
  left_join(timezone_countries, by = "country_code", multiple="all") %>%
  left_join(dls2023_transitions %>% select(zone, dst), by ="zone") %>%
  ## Label timezones that observe DLS with 1
  mutate(dls_2023 = case_when( is.na(dst) ~0,
                               dst == TRUE ~1) ) %>%
  group_by(place_name) %>%
  # For countries that have multiple timezones, as long as one of them still observe DLS, keep the country
  filter( dls_2023 == max(dls_2023)) %>%
  distinct(place_name, .keep_all = T) %>%
  # the final list returns only countries that still observe DLS
  filter( dls_2023 == 1) %>%
  ungroup


dls_countries <- dls_countries %>%
  mutate( region = case_when( grepl("Vatican", place_name) ~ "Vatican",
                              grepl("United States", place_name) ~ "USA",
                              grepl("UK", place_name) ~ "UK",
                              grepl("Bosnia", place_name) ~ "Bosnia and Herzegovina",
                              grepl("Caicos", place_name) ~ "Turks and Caicos Islands",
                              T ~ place_name))



### Data prep for plotting specific places of interest: create dataframe of specific geo-points
paris_coord <- timezones %>% 
  filter( grepl("Paris", zone) ) %>%
  select(zone, lat = latitude, lon = longitude) %>%
  mutate(country = "France",
         label = "FR",
         color = "#a3d4ff")

moscow_coord <- timezones %>% 
  filter( grepl("Moscow", zone) ) %>%
  select(zone, lat = latitude, lon = longitude) %>%
  mutate(country = "Russia",
         label = "RU",
         color = "#e66565")

most_tz_df <- rbind(paris_coord, moscow_coord)


eucla_coord <- timezones %>% 
  filter( grepl("Eucla", zone) ) %>%
  select(zone, lat = latitude, lon = longitude) %>%
  mutate( color = "#eaff0f")



### World map geo-data
world_df <- map_data("world") %>% 
  filter(region != "Antarctica") %>%
  left_join( dls_countries %>% select( c(region, dls_2023)), by = "region" ) %>%
  mutate( color = ifelse( is.na(dls_2023), "#696168", "#fcb6f9" ))   



## Plotting
bg_color = "#171c19" 
title_color = "white"
text_color = "#efffe0" 

subtitle_txt = "<span style='color:#fcb6f9'><b>Places still observing DLS time</b></span> in 2023 include most countries in Europe and many states in countries in North America. 
                <br> <span style='color:#e66565'><b>Russia</b></span> has the most time zones (<span style='color:#e66565'>11</span>) over the maindland. 
                <br> <span style='color:#a3d4ff'><b>France</b></span>'s mainland has only <span style='color:#a3d4ff'>1</span> time zone but its overseas territories span across <span style='color:#a3d4ff'>11</span> others.
                <br> <span style='color:#eaff0f'><b>Eucla</b></span>, a small town in Western Australia, has a timezone of +8:45 ahead of UTC."


worldmap_plot <- ggplot() +
  geom_polygon(data = world_df,
               aes(x = long, y = lat, group = group, fill = color), 
               colour = bg_color, size=0.05 
               ) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_fixed(1.2) +
  labs( title = "<b>TRIVIA ABOUT TIME</b>",
        subtitle = subtitle_txt,
        caption = "DATA: {IANA tz database} | GRAPHIC: Johnny K Lau (@jonkingseestheworld)") +
  theme_void() +
  theme(panel.background = element_rect(fill = bg_color, colour = bg_color), 
        plot.background = element_rect(fill = bg_color , colour = bg_color),
        plot.title = element_markdown( colour =  title_color, family = "Rob-reg", 
                                   size = 25, face="plain", 
                                   lineheight=1.2, margin = margin(t = 10, l=50)),
        plot.subtitle = element_markdown( colour =  text_color, 
                                   size = 10, face="plain", 
                                   lineheight=1.15, margin = margin(t = 8, l=50, b=50)),
        plot.caption = element_markdown(colour = text_color, size = 8, hjust = 0.95,     #lineheight = 0.5, 
                                        margin = margin(t=10, l=0, b =5))
        )
        

# Final aesthetic editing
fin_plot <- worldmap_plot + 
  ## Labelling FR & RU
  geom_point(data = geopt_df,
             aes(x = lon, y = lat),
             colour = "#fcfff7", alpha = 1, size = 7.5) +
  geom_point(data = geopt_df,
             aes(x = lon, y = lat, colour = color),   #colour = "#9cc437", 
             alpha = 1, size = 6) +
  geom_text(data = geopt_df,
            aes(x = lon, y = lat, label = label),
            colour = "#0c1c12", alpha = 1, size = 3.5) +
  geom_point(data = eucla_coord,
             aes(x = lon, y = lat, color = color), alpha = 1, size = 2) +
  ## Eucla labelling
  annotate(geom = "curve", 
         x = eucla_coord$lon - 35, y = eucla_coord$lat - 15, 
         xend = eucla_coord$lon, 
         yend = eucla_coord$lat, 
         linewidth = 0.3, colour = eucla_coord$color[1],
         curvature = 0.2) +     # arrow = arrow(length = unit(1, "mm"), type = "closed")
  geom_textbox(data = eucla_coord,
               mapping = aes(x = lon-35, y = lat-25,
                             label = "Eucla, AU:<br>UTC +08:45"),
               color = eucla_coord$color[1],
               halign = 0, hjust = 0.5, vjust = 0,
               size = 3.5,  #family = "RobCon",
               width = unit(4.5, "cm"),
               box.color = NA,
               fill = NA,
               alpha = 0.95
  )
        



# Save plot
ggsave("2023/tt20230329wk13_timezones.png", fin_plot, dpi = 300, width = 9, height = 6.5, bg= bg_color )

