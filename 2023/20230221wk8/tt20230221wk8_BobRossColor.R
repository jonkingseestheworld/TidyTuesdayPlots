#wk 8: 2023-02-21
#author: Johnny K Lau

library(tidyverse)
library(ggplot2)
library(treemapify)
library(gganimate) ## for generating animation 

library(showtext)


# Add custom font
font_add_google(name ="Anton", family = "Anton")
font_add_google(name ="Roboto", family = "Roboto")

showtext_auto()


# load data source
bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')



# split the list and get individual hexes
colors_split <- bob_ross %>%
  mutate(color_hex = str_remove_all(color_hex, '\\['),
         color_hex = str_remove_all(color_hex, '\\]'),
         color_hex = str_remove_all(color_hex, "'")) %>%
  mutate(colors = str_remove_all(colors, '\\['),
         colors = str_remove_all(colors, '\\]'),
         colors = str_remove_all(colors, "'")) %>%
  separate_longer_delim(c(colors, color_hex) ,  delim=", ") %>%  ## split the list into multiple rows
  mutate( colors = str_remove_all(colors, '\\\\r\\\\n'))


  
  
  

# get a unique color and hex list across all episodes
unique_color_hex_list <- colors_split %>% 
  select(colors, color_hex) %>%
  distinct() 


# to put different colors into a few color tone groups (for better presentation later)
## Self-defined color groups

## Black Gesso, Liquid Black, Midnight Black
black <- c("#000000")
## Titanium White, Liquid Clear
white <- c("#FFFFFF")
## Alizarin Crimson, Bright Red, Van Dyke Brown, Burnt Umber, Dark Sienna, Indian Red
brownred <- c("#4E1500", "#DB0000", "#221B15", "#8A3324", "#5F2E1F", "#CD5C5C")
## Phthalo Green, Sap Green, Prussian Blue, Phthalo Blue
greenblue <- c("#102E3C", "#0A3410", "#021E44", "#0C0040")
## Cadmium Yellow, Indian Yellow, Yellow Ochre
yellow <- c("#FFEC00", "#FFB800", "#C79B00")


## add the tone group for each color
unique_color_hex_list <- unique_color_hex_list %>%
  mutate( tone = case_when( color_hex %in% black ~ "black",
                            color_hex %in% white ~ "white",
                            color_hex %in% brownred ~ "brownred",
                            color_hex %in% greenblue ~ "greenblue",
                            #color_hex %in% greenblue ~ "greenblue",
                            color_hex %in% yellow ~ "yellow",) )


# also create a color_pal for the color scheme used in the plot later
col_pal <- c("Black Gesso" = "#000000", "Liquid Black" = "#000000", "Midnight Black" = "#000000", 
             "Titanium White"= "#FFFFFF" , "Liquid Clear" = "#FFFFFF",
             "Alizarin Crimson" = "#4E1500", "Bright Red" = "#DB0000", "Van Dyke Brown" = "#221B15", 
             "Burnt Umber" = "#8A3324", "Dark Sienna" = "#5F2E1F", "Indian Red" = "#CD5C5C",
             "Phthalo Green"="#102E3C", "Sap Green"= "#0A3410", "Prussian Blue"="#021E44", "Phthalo Blue" = "#0C0040",
             "Cadmium Yellow"="#FFEC00", "Indian Yellow" = "#FFB800", "Yellow Ochre"="#C79B00"
)





# summarising the frequency of each color used by season
colors_by_season <- colors_split %>%
  #filter( season <=30) %>%
  group_by(colors, season) %>%    
  summarise(nfreq = n()) %>%
  ungroup() %>%
  left_join(unique_color_hex_list, by="colors") ## join with the unique color list 


# animation of plots by season
anim_treemap_plot <- colors_by_season %>% 
  ggplot(aes(area = nfreq,
             label = colors,
             fill = colors,
             subgroup = tone)) +
  geom_treemap(layout = "fixed" )+ 
  scale_fill_manual(values =  col_pal) + 
  # labs(fill = "") +
  # geom_treemap_subgroup_border() +
  geom_treemap_text(colour = "#C0C0C0", 
                    place = "topleft", 
                    layout = "fixed",
                    #reflow = TRUE,
                    grow = TRUE,
                    size = 10) +
  transition_time(season) +
  ease_aes('linear') +
  ## adding title and subtitle
  labs(title = "The Joy of Painting: Bob Ross Painting Colours",
       subtitle = "SEASON: {as.integer(frame_time)}",
       caption = "Graphic by Johnny K Lau | Data from {BobRossColors} package"
       ) +
  ## adjusting the aesthetics
  theme(legend.position = "none",
        plot.background = element_rect( fill = "#C0C0C0"),
        text=element_text(family="Anton"),
        plot.title = element_text( size = 20),
        plot.subtitle = element_text( size = 18 ),
        plot.caption = element_text( size = 10, family="Roboto" ),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Margins (t, r, b, l)


# anim_save(#"2023/20230221wk8/tt20230221wk8_BRcolor_anim.gif", anim_treemap_plot, nframes = max(colors_by_season$season)*3)



# static plot: color used across all seasons
uniqColor_treemap_plot <- colors_by_season %>%
  group_by(colors) %>%
  summarise(nfreq = n()) %>%
  ungroup() %>%
  left_join(unique_color_hex_list, by="colors") %>% ## join with the unique color list 
  ggplot(aes(area = nfreq,
             label = colors,
             fill = colors,
             subgroup = tone)) +
  geom_treemap(layout = "fixed",  size = 3)+ 
  scale_fill_manual(values =  col_pal) + 
  # labs(fill = "") +
  # geom_treemap_subgroup_border() +
  geom_treemap_text(colour = "#899499", 
                    place = "topleft", 
                    layout = "fixed",
                    #reflow = TRUE,
                    #grow = TRUE,
                    size = 40) +
  ## adding title and subtitle
  labs(title = "The Joy of Painting: Bob Ross Painting Colours",
       subtitle = "Only 18 unique colours were used during 31 seasons of the show",
       caption = "Graphic by Johnny K Lau (@jonkingseestheworld)   |   Data from {BobRossColors} package"
       ) +
  ## adjusting the aesthetics
  theme(legend.position = "none",
        plot.background = element_rect( fill = "#E5E4E2"),
        text=element_text(family="Anton"),
        plot.title = element_text( size = 70),
        plot.subtitle = element_text( size = 45, family="Roboto", margin=margin(b=15) ),
        plot.caption = element_text( size = 30, family="Roboto", margin=margin(t=10) ),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))  # Margins (t, r, b, l)


# ggsave("2023/20230221wk8/tt20230221wk8_BRcolor_uniq.png", plot = uniqColor_treemap_plot, width=10, height=9)
