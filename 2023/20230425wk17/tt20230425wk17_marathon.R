#wk 17: 2023-04-25
#Author: Johnny K Lau
#Data Source:	{LondonMarathon} R data package shared by NRennie


# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(countrycode)

## For Graphic/plotting
library(ggplot2)
library(ggflags)
library(cowplot) ##ggdraw() creates a drawing layer
library(ggtext). ##geom_richtext() allows formatting using html
library(patchwork) ##area() function specifies a plotting area in a layer



# Data input ----------------------------------------------------------
wins <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
ldn_mthn <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')


# Data wrangling -----------------------------------------------------------
## Exploration
table(wins$Nationality, wins$Category)

## Summary by Category & Nationality
wins_by_cat <- wins |>
  group_by(Nationality, Category) |>
  summarise( cat_total = n() ) |>
  group_by( Category) |>
  mutate(rank = rank( -cat_total, ties.method = "min")) |>
  arrange(Category, desc(cat_total) ) |>
  ## only keep top 10 countries for each Cateogory
  filter( rank <=10 ) |>
  ungroup() |>
  mutate( Nationality = case_when(Nationality == "United Kingdom" ~ "UK",
                                  Nationality == "United States" ~ "USA",
                                  Nationality == "Soviet Union" ~ "Russia",
                                  T ~ Nationality )) |>
  ## add ISO2 code for countries/nationality  (for plotting ggflags::geom_flags)
  ## ggflags::geom_flags() function needs iso2c code in lower case
  mutate( code = countrycode( Nationality, "country.name", "iso2c" ) |> tolower())



## For graph aesthetics: Set a number of 'empty rows/bars' to add at the end of each Category (manually)
empty_nrow = 3
to_add <- data.frame( matrix(NA, nrow = empty_nrow*n_distinct(wins_by_cat$Category), ncol = ncol(wins_by_cat)) )
colnames(to_add) <- colnames(wins_by_cat)
to_add$Category=rep(unique(wins_by_cat$Category), each=empty_nrow) ## add Category label to each row

## The modified data frame for the polar barplot (adding empty rows!)
wins_by_cat_space <- wins_by_cat |>
  rbind(to_add) |>
  arrange(Category, desc(cat_total)) |>
  mutate( seq_id = row_number())


## Prepare a data frame for Category headings & top segment lines
### Define element position and angle for each Category
base_data = wins_by_cat_space |>
  group_by( Category ) |>
  summarize(start=min(seq_id), end=max(seq_id)-empty_nrow) |>    ## remember to remove empty rows
  rowwise() |>
  mutate(title = mean(c(start, end)) ) |>  
  mutate( printname = case_when(Category == "Men" ~ "MEN", 
                                Category == "Women" ~ "WOMEN", 
                                Category == "Wheelchair Men" ~ "Wheelchair\nMEN",
                                Category == "Wheelchair Women" ~ "Wheelchair\nWOMEN")) |>
  mutate( angle = 360* (title)/nrow(wins_by_cat_space),
          angle = ifelse(angle < 180, angle-90, angle+90))



## A separate label_data added with 'angle' and 'hjust' values for plotting the country names
label_data = wins_by_cat_space
nBar = nrow(label_data)
angle= 90- 360 * (label_data$seq_id - 0.5) /nBar    ## substract 0.5 because the letter must have the angle of the center of the bars (not the ends of the bar)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)




# Graphic -----------------------------------------------------------
bg_color = "#13293d"
line_color = "#e9f1f2"
font_color = "white"
bar_pal = c("Men" = "#679289", "Wheelchair Men"="#c9cba3",
            "Women" = "#ee2e31", "Wheelchair Women"="#f4c095")

## Main plot  
polar_barplot <- ggplot( wins_by_cat_space ) +
  ## Add basic bar plot element
  geom_bar(
    aes(
      x = seq_id, y = cat_total, 
      fill = Category
      ),
    stat = "identity", position = "dodge",
    show.legend = F,
    alpha = .9,
    color = line_color,
    linewidth = 0.5
    ) + 
  # Make it circular!
  coord_polar() +
  # Scale y axis so bars don't start in the center
  scale_y_continuous(
    limits = c(-8, 20)    
    ) +
  ## Set customised bar colors
  scale_fill_manual(values = bar_pal, guide="none") +
  scale_color_manual(values = bar_pal, guide="none") +
  theme_minimal() +
  labs( x="",  y="",
        ) +
  theme( panel.background = element_rect(fill = bg_color, colour = bg_color), 
         plot.background = element_rect(fill = bg_color , colour = bg_color),
         text = element_text( color = line_color),   #family = "inter", lineheight = 0.4,
         axis.text = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         #plot.title = element_text(size = 15, margin = margin(b = 20), face = "bold"),
         #plot.subtitle = element_markdown(size = 10, margin = margin(b = 25)),
         plot.margin = margin(100, 0, 0, 0)
         )

## more aesthetics for the main plot
fin_polar_barplot <- polar_barplot +
  ## Show value (number of accumulative winners) for top3 countries in each Category
  geom_text(data=label_data, 
            aes(x=seq_id, y=cat_total-1.5, label= ifelse(rank<=3, cat_total, ""), hjust=hjust), 
            color=font_color, fontface="bold",
            alpha=1, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  ## Show Country flags
  ggflags::geom_flag(data=label_data, 
                     aes(x=seq_id, y=cat_total + 1.25, country = code), size=3.5) +
  ## Show Country names
  geom_text(data=label_data, 
            aes(x=seq_id, y=cat_total + 2.5, label=Nationality, hjust=hjust), 
            color=font_color, fontface="bold",
            alpha=0.7, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
  
  ## add top curve line segment and heading text for each category
  geom_segment(data=base_data, 
               aes(x = start+1, y = max(wins_by_cat_space$cat_total, na.rm=T) + 0.5, 
                   xend = end, yend = max(wins_by_cat_space$cat_total, na.rm=T) + 0.5,
                   color = Category), 
               alpha=0.8, size=0.6, inherit.aes = FALSE 
               ) +
  geom_text(data=base_data, 
            aes(x = title+0.5, y = max(wins_by_cat_space$cat_total, na.rm=T) - 1.5, 
                label= printname, angle=angle, colour = Category), 
            hjust=c(1, 0.5, 0.5, 0.5), 
            alpha=1, size=3.25,  fontface="bold", inherit.aes = FALSE) 
  
  
  
  
### Layer2: Central Lgo
cntr_text <- cowplot::ggdraw() +
  geom_richtext(aes(label="<span style='color:#1b98e0'>London</span><br><span style='color:orangered'>Marathon</span>", 
                    x = 0.54, y = 0.5), 
                alpha=0.1, fill=NA, label.size=NA, fontface="bold",
                lineheight=0.8, size = 7, hjust = 0.5, vjust = 1)


### Layer3 & 4: Titles & Caption
title_text = ggdraw() +
  geom_richtext(aes(label="Countries With Most Winners In London Marathon Since 1981", 
                    x = 0.5, y = 6.5), 
                color = "white",
                fill=NA, label.size=NA, lineheight=0.8,  size = 6.25, hjust = 0, vjust = 1) +
  geom_richtext(aes(label="Kenya and the UK have most winners over years across the <span style='color:#679289'>Men</span>, <span style='color:#ee2e31'>Women</span>,<br>
                    <span style='color:#c9cba3'>Wheelchair (Men)</span> & <span style='color:#f4c095'>Wheelchair (Women)</span> categories", 
                    x = 0.5, y = 4.5), 
                color = "white",
                fill=NA, label.size=NA, lineheight=1.4,  size = 4.5, hjust = 0, vjust = 1)

caption_txt = ggdraw() + 
  geom_richtext(aes(label="Data from {LondonMarathon} package by @NRennie", 
                    x = 0.5, y = 0), 
                color = line_color,
                fill=NA, label.size=NA, lineheight=0.8,  size = 3, hjust = 0, vjust = 1) +
  geom_richtext(aes(label="<b>Graphic by Johnny K Lau</b><br>Github: jonkingseestheworld", 
                    x = 9.5, y = 0.5), 
                color = line_color,
                fill=NA, label.size=NA, lineheight=0.8, size = 3.5, hjust = 1, vjust = 1)



## Final grpahic: putting multiple layers together (layers1+2+3)
design <- c(
  area(1, 1, 100,100),   #t,l,b,r
  area(42,45,50,55),
  area(2, 1, 4, 10),
  area(95, 1, 95, 10)
  )


fin_plot <- fin_polar_barplot + cntr_text + title_text + caption_txt +
  plot_layout(design = design) &
  theme(plot.background = element_rect(fill=bg_color, color=bg_color))



# Saving
path <- here::here("exploration", "2023_wk17_ldn_marathon")
ggsave(filename=glue::glue("{path}/marathon.png"), 
       fin_plot, width = 8.5, height = 8.5, units = "in", dpi = 300, scale = 1 )



