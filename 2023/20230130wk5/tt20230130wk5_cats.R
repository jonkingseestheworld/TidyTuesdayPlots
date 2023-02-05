
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
#library(grid)
#library(gridExtra)
library(patchwork)
library(ggtext)

library(showtext)
library(glue)

font_add_google(name = "Itim", family = "Itim")
font_add_google(name = "Roboto Condensed", family = "RobCon")

showtext_auto()



# import data directly from github ---
cats_uk_mvmt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')


# Looking into the Variability in number of preys caught ---
## Visual check
cats_uk_reference %>%
  group_by(hrs_indoors) %>%
  summarise( count = n())

## Dichotomise the cats into Mostly Indoors and Mostly Outdoors types
cats_uk_reference <- cats_uk_reference %>% 
  mutate( indoors_mostly = ifelse(hrs_indoors >=17.5, "Indoor", "Outdoor") ) 



### Plot: no of preys vs age (for mixed/mostly outdoors cat)
cats_prey_age_outdoors_plot <- cats_uk_reference %>%
  filter( indoors_mostly == "Outdoor") %>%
  ggplot(aes( x = age_years, y=prey_p_month)) +
  geom_density_2d_filled(aes(fill = ..level..), alpha = 0.9, contour_var = "ndensity") +
  geom_jitter(height=0.1,  color = "pink", alpha=0.4) +
  theme_minimal() +
  xlim(-0.25, 17) +
  ylim(-0.25, 18) +
  labs( title="More outdoor cats (spending </= 12.5hrs indoor)", x = "Age (years)", y= "More prey caught") +
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.text=element_blank(),
         legend.position = 'none',
         plot.background=element_rect(fill = "black", colour="black"),
         plot.title = element_text(hjust = 0.1 , vjust= -0.5, family = "RobCon", size=28, colour="#ff5349"), 
         axis.title.y = element_text(hjust=1, family = "RobCon", size=26, colour="white"), 
         axis.title.x = element_text(hjust=1, family = "RobCon", size=26, colour="white"), 
         axis.line.y = element_line(arrow = grid::arrow(length = unit(0.15, "cm")), colour="white" ),
         axis.line.x = element_line(arrow = grid::arrow(length = unit(0.15, "cm")), colour="white" ),
         plot.margin = unit(c(t=0, r=0.75, b=0.5, l=0), "cm") )

### add arrows + customised texts for specific cats
Jago_pers_data <- cats_uk_reference %>%
  filter( animal_id == "Jago") %>%
  select(animal_id, hrs_indoors, prey_p_month, age_years)

Sid_pers_data <- cats_uk_reference %>%
  filter( animal_id == "Sid") %>%
  select(animal_id, hrs_indoors, prey_p_month, age_years)


cats_prey_age_outdoors_plot <- cats_prey_age_outdoors_plot +
  ## Jago: labelling
  annotate(geom = "curve", 
           x = Jago_pers_data$age_years[1]-1, y = Jago_pers_data$prey_p_month[1]-2, 
           xend = Jago_pers_data$age_years[1], 
           yend = Jago_pers_data$prey_p_month[1], 
           linewidth = 0.5, colour = "#ff5349",
           curvature = 0.5,
           arrow = arrow(length = unit(1, "mm"), type = "closed")) +
  geom_textbox(data = Jago_pers_data,
               mapping = aes(x = age_years-2, y = prey_p_month-5.5,
                             label = toupper(animal_id) ),
               color = "#ff5349",
               halign = 0, hjust = 0, vjust = 0,
               size = 8, family = "RobCon",
               box.color = NA,
               fill = NA,
               #fill = cat_palette$light_text,
               alpha = 0.95
               ) +
  ## Sid: labelling
  annotate(geom = "curve", 
           x = Sid_pers_data$age_years[1]+2, y = Sid_pers_data$prey_p_month[1]-2, 
           xend = Sid_pers_data$age_years[1], 
           yend = Sid_pers_data$prey_p_month[1], 
           linewidth = 0.5, colour = "#ff5349",
           curvature = 0.5,
           arrow = arrow(length = unit(1, "mm"), type = "closed")) +
  geom_textbox(data = Sid_pers_data,
               mapping = aes(x = age_years+2, y = prey_p_month-4,
                             label = toupper(animal_id) ),
               color = "#ff5349",
               halign = 0, hjust = 0, vjust = 0,
               size = 8, family = "RobCon",
               box.color = NA,
               fill = NA,
               alpha = 0.95
               )

#stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white", bins=50, , contour_var = "ndensity") +
#geom_hex(bins = 40) +
# facet_grid(~indoors_mostly)




### Plot: no of preys vs age (for mostly indoors cat) 
cats_prey_age_indoors_plot <- cats_uk_reference %>%
  filter( indoors_mostly == "Indoor") %>%
  ggplot(aes( x = age_years, y=prey_p_month)) +
  geom_density_2d_filled(aes(fill = ..level..), alpha = 0.9, contour_var = "ndensity") +
  geom_jitter(height=0.1, color = "pink", alpha=0.4) +
  theme_minimal() +
  xlim(-0.25, 17) +
  ylim(-0.25, 18) +
  labs( title="More indoor cats (spending >12.5hrs indoor)", x="", y="") +
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         axis.text=element_blank(),
         legend.position = 'none',         
         plot.background=element_rect(fill = "black", colour="#808080", linetype="dotted", size=1 ),
         plot.title = element_text(hjust = 0.1, vjust= -0.5, family = "RobCon", size=28, colour="#808080"),
         plot.margin = unit(c(t=0, r=0.75, b=0, l=0), "cm"))



# (cats_prey_age_outdoors_plot)/(cats_prey_age_indoors_plot)





# Looking into the geomotion distribution of cats with highest number of preys ---
## Visual check
cats_uk_mvmt %>%
  group_by(tag_id) %>%
  summarise( count = n() ) %>%
  ungroup() %>%
  arrange( desc(count) )


## Function for plotting GPS coordinates on a map
cat_motion_map_plot <- function(df, pet_tag){
  col2 = "#6497b1"
    
  df_sel <- df %>%
    filter( tag_id %in% c(pet_tag))
  
  med_long <- median(df_sel$location_long)
  med_lat <- median(df_sel$location_lat)
  bbox = c(left = med_long-0.0035, bottom = med_lat-0.002, 
           right = med_long+0.0035, top = med_lat+0.002)
  
  map <- get_stamenmap( bbox = bbox, zoom = 16, maptype = "toner-background", 
                        color="color")  #"watercolor"
  
  stm_map <- ggmap(map) 
  
  motion_plot <- stm_map +  
    geom_point(aes(x = location_long, y = location_lat), 
               data = df_sel, 
               colour = col2,  size = 0.9) + 
    stat_density2d(
      aes(x = location_long, y = location_lat, fill = ..level.., alpha = 0.25),
      size = 0.5, bins = 100, data = df_sel,
      geom = "polygon" ) +
    geom_density2d(data = df_sel, 
                   aes(x = location_long, y = location_lat), size = 0.3) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text=element_blank(),
          legend.position = 'none',
          plot.background=element_rect(fill = "black", colour="black"),
          panel.background=element_rect(fill = "black", colour="black")
          )
  
  return(motion_plot)
}

## Two examples of where the cats were about: geolocation density
Jago_motion_plot <- cat_motion_map_plot(cats_uk_mvmt, "Jago") + 
  labs(title = glue("(Example 1) <span style='color:#ff5349'>JAGO</span> 's movements in neighbourhood") ) +
  theme(plot.title = element_markdown(hjust = 0.5, family = "RobCon", size=28, colour="white"))

Sid_motion_plot <- cat_motion_map_plot(cats_uk_mvmt, "Sid-Tag") +
  labs(title = glue("(Example 2) <span style='color:#ff5349'>SID</span> 's movements in neighbourhood")) +
  theme(plot.title = element_markdown(hjust = 0.5, family = "RobCon", size=28, colour="white"))



## Combined plot (all in one) ---
### subtitle content
subtitle <- glue("Do you know what your pet has been doing?<br><br><span style='color:#41a7f5'>Movebank for Animal Tracking Data</span> 
            includes GPS coordinates, behavioural and physical characteristics for pet cats in the UK. 
            <br>Not so surprisingly, the <span style='color:#ff5349'>more outdoor cats</span> show greater variability in the amount of prey they caught each month (especially 
            <br>when they were younger), compared with the <span style='color:#808080'>more indoor cats</span>.")

### Putting all plots together
cats_fin_plot <- cats_prey_age_outdoors_plot + Jago_motion_plot + cats_prey_age_indoors_plot + Sid_motion_plot +
  plot_layout(nrow = 2, ncol = 2, widths = c(0.9, 1 , 0.9, 1), heights = rep(1, 4)) +
  plot_annotation(  title = "Oh kitty, where paw-sibly could you have been?",
                    subtitle = subtitle,
                    caption = "Graphic: Johnny K Lau | Data : movebank.org | #TidyTuesday",
                    theme = theme(plot.background = element_rect( fill = "black", colour="black"),
                                  plot.margin = unit(c(t=1, r=0.5, b=0.75, l=1.25), "cm"),
                                  plot.title = element_text(hjust = 0.5, colour="#41a7f5", family = "Itim", size=65),
                                  plot.subtitle = element_markdown( family = "RobCon", color = "white",
                                                                     size = 30, hjust = 0, lineheight = 0.3,
                                                                     margin = margin(l = 15, b = 40, t=20)),
                                  plot.caption = element_text(family = "RobCon",
                                                              color = "white", size = 26, hjust = 0.5,
                                                              margin = margin(t=40))) 
                      )



# Save plot ----
 ggsave("2023/20230130wk5/tt20230130wk5_cats.png", cats_fin_plot, dpi = 300, width = 8.5, height = 8, bg="black")






