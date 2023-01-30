library(tidyverse)
library(packcircles)
library(showtext)
library(gganimate)
library(ggtext)

showtext_auto()

text = "Montserrat"
sysfonts::font_families_google()

#sysfonts::font_add_google(text,text)

sysfonts::font_add_google("Tourney", family="Tourney")

knitr::opts_chunk$set(echo = TRUE, fig.width = 20, fig.height = 24)



inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')

#elements <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/elements.csv.gz')

#themes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')

parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/parts.csv.gz')




total = merge(inventories,inventory_parts,by.x = "id",by.y = "inventory_id")

total1 <- merge(total,colors,by.x="color_id",by.y = "id")

df_final <- merge(total1, sets,by="set_num")



#################################################################
# merge datasets with color coding, year, and number of individual parts

df_colors <- merge(df_final, parts, by="part_num")

df_colors_subset <- df_colors %>% select(name.x, rgb, year,quantity) %>% 
  group_by(year,rgb,name.x) %>% 
  summarise(total = sum(quantity)) 

df_colros_total <- df_colors_subset %>% group_by(year) %>%
  summarise(total_year = sum(total))

df = merge(df_colors_subset,df_colros_total,by="year")

a=df %>% filter(!(rgb=="F3C305")) %>% mutate(percent = total/total_year, rgb = paste0("#",rgb), year = case_when(year<1952~year+1,
                                                                                                                 TRUE~year))




# circle packing from r-graph-gallery.com
#df_pack <- a %>% group_by(year, rgb, name.x) %>% summarise(count = sum(total))
df_pack <- a %>% 
  select(year, rgb, name.x, count=total)  %>%
  mutate( decade = cut(year, breaks = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, Inf),
             labels = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010 onwards"), include.lowest=T, right=F))  %>%
  group_by(decade, rgb, name.x) %>%
  summarise( count = sum(count)) %>%
  ungroup() %>%
  mutate( decade = as.character(decade)) 
#%>%
  #filter(decade %in% c("1950s", "1960s", "1970s", "1980s"))


#df_pack <- a %>% group_by(year, rgb, name.x) %>% summarise(count = sum(total)) %>%
#  filter( year <1954)




lists_packByDecade <- df_pack %>% 
  split(.$decade)  #decade

packing <- lists_packByDecade %>% 
  map(~circleProgressiveLayout(.$count, sizetype="area")) %>%
  bind_rows()


#packing <- circleProgressiveLayout(df_pack$count,sizetype = "area")

df_pack1 <- cbind(df_pack,packing) %>%
  as.data.frame() %>%
  mutate(id = as.numeric(rownames(.)))

gg_layout <- circleLayoutVertices(packing,npoints = 50) %>%
  left_join(df_pack1 %>% select( decade , id), by="id")    #decade
  

df_top5_perDecade <- df_pack1 %>%
  group_by(decade) %>%   #year
  slice_max(order_by=count, n=5) %>%
  #arrange(year, desc(count)) %>%
  mutate( rank = order(count, decreasing =T))
#  mutate( rank = paste("Top", order(count, decreasing =T)))


bg = "gray85"

bubbleplot <- ggplot(data=gg_layout,aes(x,y, group=id, fill=as.factor(id)))+
  geom_polygon()+
  scale_fill_manual(values = df_pack$rgb) +
  theme_void()+
  coord_equal() +
  geom_label(data = df_top5_perDecade, aes(x, y, label=if_else(rank==1, paste0("Top", rank), as.character(rank)), size = 3.0), 
             color=bg, fill=NA, label.size=NA) + 
  labs(title = "The Colour Map of Lego",
       subtitle = "Period: {closest_state}",   #
       #subtitle = "Lego has used 202 unique colors. \nThe size of the circle depicts the frequency of the colour used in all lego sets each year.",
       #caption = "Data: Rebrickable | Graphic: Abhinav Malasi"
       ) +
  theme(legend.position = "none",
        plot.title = element_text(family = "Tourney",
                                      face="bold", size=24),
        plot.subtitle = element_text(size=18, lineheight = 1.5,margin = margin(t=10)),
        #plot.caption = element_text(size=5),
        plot.margin = margin(t=20,b=10, l=20, r=20),
        plot.background = element_rect(color=bg,fill=bg),
        #panel.background = element_rect(color=bg,fill=bg),
        text = element_text(family = text)) 

 # + annotate( "richtext", x=-200, y=450, label="<b>Period: {closest_state}<b>", fill = NA, label.color = NA)
  #geom_text( aes( x=-200, y=550, label="Year {closest_state}"), color="black")


  
bubbleplot_anim <- bubbleplot + 
  transition_states(decade, transition_length = 0.5, state_length = 1.5, wrap = TRUE)


animate(bubbleplot_anim, height = 600, width =610)
anim_save( "Wk36_Lego/bubbleplot_1950to2020.gif")


#ggsave("Wk36_Lego/lego_bubble_1950to80.png", bubbleplot, width = 10,height=10, units = "in")
