# TidyTuesday | September 27, 2022 | Week 39 Artists in US
# Data source is arts.gov (by way of Data is Plural)


# load libraries ---------------------------------------------------------------
library(tidyverse)
library(circlize)

library(RColorBrewer)
library(showtext)

library(ggplot2)
library(ggimage)
library(cowplot)

# add font ----------------------------------------------------------------
font_add_google(name = "BenchNine", family = "BenchNine")
font_add_google(name = "Noto Sans Mono", family = "Noto")

showtext_auto()


# load data
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')


# data preprocessing
## Splitting artists by whether they are related to the entertainment industry or not
artists_by_ethn <- artists %>%
  group_by(race, type) %>%
  summarise( artists_n = sum(artists_n, na.rm=T)  ) %>%
  ungroup() %>%
  ## split artists into two types
  mutate( 
    category = case_when(type %in% c("Actors", "Announcers", "Dancers And Choreographers" ,
                                     "Entertainers", "Music Directors And Composers",
                                     "Musicians", "Producers And Directors") ~ "Entertainment",
                         T ~ "Others") 
  ) %>%
  ## renaming to shorten names of some of the categories
  mutate(
    type = case_when( 
      #grepl("Announcers", type) ~ "Announ" ,
      grepl("Dancers And Choreographers", type) ~ "Dancers+" ,
      grepl("Entertainers", type) ~ "Entertainers" ,
      grepl("Music Directors And Composers" , type) ~ "Composers+",
      grepl("Producers And Directors" , type) ~ "Producers/Directors",
      grepl("Fine Artists, Art Directors, And Animators" , type) ~ "F.Artists+",
      grepl("Landscape Architects" , type) ~ "Ld.Arch",
      grepl("Writers And Authors" , type) ~ "Writers/Authors",
      T ~ type
    )
  ) %>%
  mutate(
    race = case_when(grepl("African-American", race) ~ "African-Am",
                     T ~ race))




# layout(matrix(1:2, 1, 2)) 


artist_types <- c("Entertainment", "Others")
print_artist_types <- c("Entertatinment", "Non-entertainment")


for(i in 1:length(artist_types)){
  ## transform subset data to wide, matrix format
  entertainer_race_wide <-
    artists_by_ethn %>% 
    filter(category == artist_types[i]) %>%
    pivot_wider( names_from = type, values_from = artists_n ) %>%
    column_to_rownames(var = "race")   %>%
    select(-category) %>%
    as.matrix()
  
  
  ## specify labels, groups, colour palette
  nRace = lengths(dimnames(entertainer_race_wide)[1])
  nArtists = lengths(dimnames(entertainer_race_wide)[2])
  
  labels = unique(unlist(dimnames(entertainer_race_wide)))
  group = setNames( c(rep("race", nRace), rep("artists", nArtists)),
                    labels)

  race_col_pal = c("#4D82BD", "#009a9c", "#17b79c", "#80ced6", "#c5d5c5") 
  artist_col_pal = c("#FFFDD0", "#FEE391", "#FEC44F", "#FE9929",
                     "#EC7014", "#d96459", "#ff6f69")
  
  grid.col = setNames( c( race_col_pal, artist_col_pal),
                       labels )
  
  
  ## chordDiagram plotting
  circos.clear()
  
  png(paste0("wk39_20220927_Artists/", artist_types[i], "_Chord.png"), width = 900, height = 900,
      units = "px") 
  
  par(oma=c(0,0, 1.5, 0))
  par(bg =  "#eeeeee") 
  
  circos.par(start.degree = 265)
  
  chordDiagram(entertainer_race_wide, group = group, grid.col = grid.col,
               annotationTrack = c( "grid"),
               annotationTrackHeight = mm_h(10),
               preAllocateTracks = list(
                 track.height = mm_h(14), ## height of the outer par
                 track.margin = c(mm_h(0.4), 0)
               )
  )
  
  circos.track(track.index = 2, panel.fun = function(x, y) {
    sector.index = get.cell.meta.data("sector.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    circos.text(mean(xlim), mean(ylim), sector.index, cex = 0.85, 
                niceFacing = T, col = "#3d3d3d", font=par("label_fonts"))
  }, bg.border = NA)
  
  
  par(family = "BenchNine")
  graphics::title(paste0(print_artist_types[i], " Artists"), cex.main = 2, col.main="#0A1E34", family="Noto")
  
  highlight.sector(rownames(entertainer_race_wide), track.index = 1, col =  "#164578",   
                   text = "Ethnicity", cex = 2.1, text.col = "white", niceFacing = F)
  highlight.sector(colnames(entertainer_race_wide), track.index = 1, col =  "#BD4045",  
                   text = "Artists", cex = 2.1, text.col = "white", niceFacing = TRUE)
  
  dev.off()
}


# lgd_links = Legend(at = colnames(entertainer_race_wide), legend_gp = gpar(col = 1:5) , 
#                   title_position = "topleft", title = "Artists", direction = "horizontal")
# draw(lgd_list_horizontal, y = unit(1, "npc") - circle_size, just = "top")





# Compile a poster using the two chord diagrams & add titles

df <- data.frame(
  artist_types = c("entertainment", "others"),
  images = c("wk39_20220927_Artists/Entertainment_Chord.png", 
             "wk39_20220927_Artists/Others_Chord.png")
)


p1 <- ggplot(df %>% filter(artist_types == "entertainment"), aes(x=0, y=0 ) ) +
  geom_image(aes(image = images), size = 1) + theme_void() + 
  theme(aspect.ratio = 1,
        plot.margin = margin( t=1, b=0, l=0.2, r=0.2, unit="cm")  )

p2 <- ggplot(df %>% filter(artist_types == "others"), aes(x=0, y=0 ) ) +
  geom_image(aes(image = images), size = 1) + theme_void() + 
  theme(aspect.ratio = 1,
        plot.margin = margin( t=1, b=0, l=0.2, r=0.2, unit="cm")  ) 

comb_p <- plot_grid(p1, p2) 

title <- cowplot::ggdraw() + 
  draw_label("Ethnic Representation of Artists in the USA",
             fontfamily = "Noto", size=55, x=0,  hjust =0,
             fontface = "bold") + 
  draw_label("Designers make up the largest share of artists",
             fontfamily = "Noto", size=40, x=0, hjust =0, vjust=3 ) +
  theme( plot.margin = margin( b=2, t=2, l= 1.05, unit="cm") )

caption <- cowplot::ggdraw() +
  draw_label("Source: arts.gov | Graphic: Johnny Lau",
             fontfamily = "Noto", size = 25, x=0.73, hjust=0) +
  theme( plot.margin = margin( t=-0.6, b=0.2, l=1.05, unit="cm") )

US_artists_plot <- plot_grid(title, comb_p, caption, ncol=1, rel_heights = c(0.1, 1)) +
  theme(plot.background = element_rect(fill = "#eeeeee", colour = NA))


# Save plot ----
ggsave("wk39_20220927_Artists/20220927_USA_artists.png", US_artists_plot, dpi = 320, width = 12, height = 6)



