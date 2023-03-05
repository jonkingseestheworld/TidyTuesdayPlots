#wk 10: 2023-03-07
#author: Johnny K Lau

## for data transformation
library(tidyverse)
library(glue)

## for plotting
library(ggtext)
library(ggplot2)
library(patchwork)

library(showtext)

## add custom font
font_add_google(name ="Roboto", family = "Roboto Slab")
showtext_auto()

## for map plotting
library(sf)



numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

## keep only data from 2010 onwards
numbats_f2010 <- numbats %>% 
  filter(year>=2010) %>% 
  rename( lat = decimalLatitude,
          lon = decimalLongitude) %>%
  # classify instances based on whether they were taken in Dryandra Woodlands or Not
  mutate( sightingLoc = ifelse( dryandra == TRUE, "Sightings (Dryandra Woodlands)", "Sightings (Elsewhere)" ) )

  
# Data exploration
#table(numbats_f2010$year, numbats_f2010$dryandra)

#numbats_f2010 %>% 
#  distinct(lat, lon) %>%
#  nrow()


# ----------------------------------------------------------------------
## Generic colour scheme
main_yellow = "#ffd435"
bg_black = "#1c1c1c"


# Plot1 - Top-left - text segment
text <- glue("<b style='color: {main_yellow}; font-size:90px; padding-bottom: 15px;'>Numbats in Australia</b><br><br>
              <i style='color: {main_yellow}; font-size:40px;'>Numbats were once widespread across southern Australia, 
              but is now restricted to small colonies mainly in Western Australia. It is considered an endangered species 
              and protected by conservation programs.<br><br>
              The data here shows the sightings of numbats since 2010. Over years numbats are seen more often in the warmer months (near the end of year) in Australia.</i>") 

text_seg_plot <- ggplot() +     
  aes(x = 0, y = 0.5, label = text) +
  geom_textbox(
    #color="#ffd435",
    #size=20,
    box.color = NA,
    fill = NA,
    width = unit(9.5, "cm"),
    halign= 0,
    hjust = 0, vjust = 0.3
    ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1.3), expand = c(0, 0)) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto"),
    plot.background = element_rect(fill= bg_black, colour= bg_black),
    panel.background = element_rect(fill= bg_black, colour= bg_black),
    plot.margin = margin(10, 10, 10, 10)
    )


# ----------------------------------------------------------------------
# Plot2 - Top-right - AUS map

## import shapefile of state boundaries
### shapefiles d/l from Australia Bureau of Statistics website:
### https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.001July%202016?OpenDocument
AUS_STATE_shp <- read_sf("exploration/AUS_STE","STE_2016_AUST")

#AUS_SA2_shp <- read_sf("exploration/AUS_SA2","SA2_2016_AUST")

## exclde 'Other Territories'
AUS_STATE_shp_main <- AUS_STATE_shp %>%
  filter( ! STE_NAME16 == "Other Territories" ) 


## instances of sightings to be plotted as points
## exclude incidents without lat & lon data
## convert lat/lon columns to geometry columns with st_as_sf() function.
numbats_geo <- numbats_f2010 %>% 
  filter( !is.na(lat) & !is.na(lon) ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)



## specific colour scheme defined
col_pal <- c("Sightings (Dryandra Woodlands)" = "#ff4e00",
             "Sightings (Elsewhere)" = "#4d4dfe")

## map plot
AUS_map_seg <- ggplot() +
  geom_sf(data= AUS_STATE_shp_main, fill = main_yellow, color= main_yellow) +
  geom_sf(data=numbats_geo, aes( fill = sightingLoc, color= sightingLoc), alpha=0.5, size=1, show.legend="point") +
  scale_fill_manual( name = "", 
                     values = col_pal,
                     aesthetics = c("colour", "fill"),
                     guide = guide_legend(override.aes = list(size = 5, alpha = 0.7)) 
                     ) + 
  #coord_map() +
  theme_void() +
  theme( text = element_text( family = "Roboto", colour="white"),
         plot.background = element_rect( fill = bg_black, colour = bg_black ),
         panel.background = element_rect( fill = bg_black, colour = bg_black ),
         legend.position = "top",
         legend.text = element_text(size=22, color= main_yellow ),
         plot.margin = margin(l=20, t=15, r=0, b=30)  # ltrb
         )


#p_top <- text_seg + AUS_map_seg  +
#  plot_layout(nrow = 1, ncol = 2, widths = rep(1, 2)) 


# ----------------------------------------------------------------------
# Plot3 - Bottom-left - Numbat sighting freq (across AUS)

## get no. of sightings per month from 2010
numbats_pMth_f2010 <- numbats_f2010 %>%
  group_by(year, month) %>%
  summarise( nTotal = n()) %>%
  ungroup() %>%
  complete(year=2010:2022, month,
           fill = list(nTotal=0)) %>%
  mutate( month = factor(month, levels=month.abb)) # reorder the factor levels

## tile plot
tileplot1_all <- ggplot(numbats_pMth_f2010, aes(x = year, y = month)) +
  geom_tile(aes(fill = nTotal), color = bg_black, size=2) +
  coord_fixed(ratio = 1/1.4) +
  scale_x_continuous( limits=c(2009.5, 2021.5),
                      breaks= c(2010, 2012, 2014, 2016, 2018, 2020)) +
  scale_fill_gradient(low = "#1f1d1d",
                      high = "#94ebff",
                      na.value = "#1f1d1d",
                      limits = c(0, 45),
                      guide="legend") +
  theme_minimal() +
  labs(x="", y="", fill = "No. of sightings across Australia") +
  theme(plot.background = element_rect(fill= bg_black, colour=bg_black),
        panel.background = element_rect(fill= bg_black, colour= bg_black),
        text = element_text( color = main_yellow, size=23),
        panel.grid = element_blank(),
        axis.text = element_text( color = main_yellow),
        axis.text.y = element_text(size = 24),
        axis.text.x = element_text(size = 24, margin = margin(t=5, unit="pt")), 
        legend.position = "bottom",
        legend.direction="horizontal",
        # legend.spacing.x = unit(1, 'cm')
        ) +
  guides(fill = guide_colorbar(ticks.colour = NA,
                               barheight = 0.75, barwidth = 10,
                               title.position = "top", title.hjust = 0.5))



# ----------------------------------------------------------------------
# Plot4 - Bottom-right - Numbat sighting freq (in Dryandra)

## get no. of sightings per month from 2010
numbats_pMth_f2010_dryan <- numbats_f2010 %>%
  filter(dryandra == TRUE) %>%    ## include only instances taken in Dryandra
  group_by(year, month) %>%
  summarise( nTotal = n()) %>%
  ungroup() %>%
  complete(year=2010:2022, month,
           fill = list(nTotal=0)) %>%
  mutate( month = factor(month, levels=month.abb)) # reorder the factor levels


## tile plot
tileplot2_dryandra <-ggplot(numbats_pMth_f2010_dryan, aes(x = year, y = month)) +
  geom_tile(aes(fill = nTotal), color = bg_black, size=2) +
  coord_fixed(ratio = 1/1.4) +
  scale_x_continuous( limits=c(2009.5, 2021.5),
                      breaks= c(2010, 2012, 2014, 2016, 2018, 2020)) +
  scale_fill_gradient(low = "#1f1d1d",
                      high = "#ff7400",
                      na.value = "#1f1d1d",
                      limits = c(0, 30),
                      breaks = c(0,15,30),
                      guide="legend") +
  theme_minimal() +
  labs(x="", y="", fill = "No. of sightings in Dryandra Woodlands") +
  theme(plot.background = element_rect(fill= bg_black, colour= bg_black),
        panel.background = element_rect(fill= bg_black, colour= bg_black),
        text = element_text( color = main_yellow, size=23),
        panel.grid = element_blank(),
        axis.text = element_text( color = main_yellow),
        axis.text.y = element_text(size = 24),
        axis.text.x = element_text(size = 24, margin = margin(t=5, unit="pt")), 
        legend.position = "bottom",
        legend.direction="horizontal"
        # legend.spacing.x = unit(1, 'cm')
  ) +
  guides(fill = guide_colorbar(ticks.colour = NA,
                               barheight = 0.75, barwidth = 10,
                               title.position = "top", title.hjust = 0.5))


#p_bottom <- tile1_all + tile2_dryandra +
#  plot_layout(nrow = 1, ncol = 2, widths = rep(1, 2))  #, heights = rep(1, 4)




# ----------------------------------------------------------------------
# Combining all plots into one
p <- text_seg_plot + AUS_map_seg + tileplot1_all + tileplot2_dryandra +
  plot_layout(nrow=2, ncol=2, heights=c(1.5, 1), widths=c(1,1)) +
  plot_annotation(caption = "Graphic by Johnny K Lau | Data from {Atlas of Living Australia} | #TidyTuesday 20230307",
                  theme=theme(plot.caption = element_text(color = main_yellow, size=23, margin=margin(t=30, b=20, unit="pt"), hjust=0.075 )) ) &
  theme(plot.background = element_rect(fill = bg_black, color = bg_black ) ) 
          


ggsave("tt20230307wk10_numbats.png", plot = p, width=9, height=7.5)
