#wk 27: 2023-07-04
#Author: Johnny K Lau
#Theme: Historical Markers in the USA


# Load libraries
library(tidyverse)

library(ggplot2)
library(ggtext)
library(ggrepel)
library(cowplot)
library(patchwork)
library(maps)

library(showtext)

## Loading Google fonts (https://fonts.google.com/)
font_add_google("Tinos", "Tinos")
font_add_google("Prata", "Prata")


### SET-UP ####
# Data input ----------------------------------------------------------
hist_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')
no_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/no_markers.csv')


hist_markers <- hist_markers |>
  ## focusing only on main territory of the USA
  filter( !state_or_prov %in% c("Alaska", "Hawaii", "Puerto Rico"))

## Selecting entries related to freedom, civil and/or rights
freedom_markers <- hist_markers |>
  filter( grepl( "freedom| right|civil", tolower(title), perl=TRUE) | grepl( "civil", tolower(erected_by), perl=TRUE) | 
            grepl( "civil right", tolower(addl_subtitle), perl=TRUE)) |>
  filter( !grepl( "right wing", tolower(title), perl=TRUE) ) 

## Pre-select a few examples of labels for plotting
text_sel_df <- freedom_markers |>
  filter( title %in% c("Rosa Parks", "David Walker", "Civil War Routes", "LGBT Civil Rights Movement") )


### PLOT ####
USA_main <- map_data('state') # map data

bg_color = "#133133"
fill_color =  "#133133" 
line_color = "#999999"
font_color = "#ededed"


## main map plot
mapPlot <- ggplot() + 
  geom_polygon(data= USA_main, aes(x=long, y=lat, group = group),
               fill = fill_color, color = line_color, linewidth=0.3) + 
  theme(axis.title=element_blank(), 
        axis.text=element_blank(), 
        axis.ticks=element_blank()) + 
  coord_fixed(1.3) + theme_void() + 
  ## add marker locations as points
  geom_point(data=hist_markers, aes(x= longitude_minus_w, y= latitude_minus_s ), 
             size = 0.3, color = "#097969", alpha=0.1) +   
  geom_point(data= freedom_markers, aes(x= longitude_minus_w, y= latitude_minus_s ), 
             size = 1.5, color = "#dc8012", alpha=0.5) +  
  geom_label_repel(data = text_sel_df,
                   aes(x = longitude_minus_w,
                       y = latitude_minus_s,
                       label = title),
                   min.segment.length = 0,
                   fill = "white", alpha=0.6,
                   color="black",
                   segment.color = "white",
                   force = 80,
                   fontface = "bold",
                   #family = plotFont,
                   #xlim = c(-127.5, Inf),
                   seed = 2,
                   size = 2.5,
                   max.overlaps = Inf
                   ) +
  ## add titles
  labs(title ="Public History Cast in Metal, \nCarved on Stone, or Embedded in Resin",
        subtitle = "Map of <span style='color:#097969'>historical plaques</span> in the United States
                    \nOn record most plaques have been put up in east coast towns especially <span style='color:#dc8012'>those pertinent to freedom and (civil) rights</span>"
          ) +
  ## adjust plot aesthetics
  theme( plot.background = element_rect(fill = bg_color, color = bg_color),
         panel.background = element_rect(fill = bg_color, 
                                         colour = bg_color)) +
  theme( plot.title = element_text(family = "Didot", face="bold",
                                  size = 28,  colour = font_color, 
                                  margin= margin(t=0, l=5, b=1, r=0, unit="cm") ),   #tlbr
        plot.subtitle = element_markdown(family = "Prata",
                                     size = 13, lineheight=0.6, colour = font_color,
                                     margin= margin(t=0, l=0, b=2, r=0, unit="cm")) ) +
  theme( plot.margin = margin(1.5, 2, 3, 2, "cm"))



## Adding customised footer
footer_txt = ggdraw() + 
  geom_richtext(aes(label="#TidyTuesday Week27 <br>Source: Historical Marker Database (USA Index)", 
                    x = 0, y = -5), 
                color = font_color, fill=NA, 
                label.size=NA, lineheight=0.8,  size = 4, hjust = 0, vjust = 1) +
  geom_richtext(aes(label="Graphic by Johnny K Lau <br>Github: jonkingseestheworld", 
                    x = 9, y = -5), 
                color = font_color,  fill=NA, 
                label.size=NA, lineheight=1, size = 4, hjust = 1, vjust = 1)


## Setting canvas for different layers
design <- c(
  area(1, 1, 90,90),   #t,l,b,r
  area(90, 1, 90, 10)
  )

## Putting multiple layers together
fin_plot <- mapPlot + footer_txt +
  plot_layout(design = design) &
  theme(plot.background = element_rect(fill=bg_color, color=bg_color))



# Save final plot
ggsave("2023/20230704wk27/tt20230704wk27_hist_markers.png", plot = fin_plot,
       width = 30, height = 20, units = "cm") 

