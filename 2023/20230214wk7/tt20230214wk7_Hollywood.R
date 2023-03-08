
#wk 7: 2023-02-14
#author: Johnny K Lau

library(tidyverse)
library(glue)

library(reactablefmtr)
library(htmltools)
library(htmlwidgets)
library(lubridate)

library(showtext)

# Add custom font
font_add_google(name ="Ubuntu", family = "Ubuntu")

showtext_auto()



## Data Cleaning
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

#unique(c(age_gaps$character_1_gender, age_gaps$character_2_gender))

## Include only couples of opposite sexes
age_gaps_oppSex <- age_gaps %>%
  filter(! character_1_gender == character_2_gender) %>%
  ## get ages of M / F actors in the couple respectively
  mutate( age_M = ifelse( character_1_gender=="man", actor_1_age, actor_2_age ),
          age_F = ifelse( character_1_gender=="woman", actor_1_age, actor_2_age )) %>%
  mutate( decade = case_when( release_year<=1939 ~ "1930s",
                              release_year>=1940 & release_year<=1949 ~ "1940s",
                              release_year>=1950 & release_year<=1959 ~ "1950s",
                              release_year>=1960 & release_year<=1969 ~ "1960s",
                              release_year>=1970 & release_year<=1979 ~ "1970s",
                              release_year>=1980 & release_year<=1989 ~ "1980s",
                              release_year>=1990 & release_year<=1999 ~ "1990s",
                              release_year>=2000 & release_year<=2009 ~ "2000s",
                              release_year>=2010 & release_year<=2019 ~ "2010s",
                              release_year>=2020 ~ "2020s"))


## Derive summary data: average ages and age difference in each decade
avg_age_df <- age_gaps_oppSex %>%
  group_by(decade ) %>%
  summarise( avg_age_M = round( mean(age_M), 2),
             avg_age_F = round( mean(age_F), 2),
             avg_age_diff = mean(age_difference),
             age_list_M = list( sort(age_M) ),
             age_list_F = list( sort(age_F) ) ) %>%
  ungroup()

## Identify the movie with the largest age_diff between characters in each decade
movie_max_diff_df <- age_gaps_oppSex %>%
  group_by(decade ) %>%
  filter( age_difference == max(age_difference) ) %>%
  mutate( actor_name_M = ifelse( character_1_gender=="man", actor_1_name, actor_2_name ),
          actor_name_F = ifelse( character_2_gender=="man", actor_1_name, actor_2_name )) %>%
  select( movie_name, release_year, director, age_difference, actor_name_M, actor_name_F, 
         age_M, age_F, decade ) %>%
  mutate( color_age_diff = ifelse( age_M > age_F, "#097969", "#E74C3C") ) %>%
  distinct(decade, .keep_all = T) %>%
  ungroup()


## Merge up the two tables into one final table (one decade per row)

df_movie_table <- avg_age_df %>%
  full_join( movie_max_diff_df, by = "decade", multiple="all") %>%
  filter(!decade %in% c("1930s", "1940s")) %>%   ##exclude anything before 1950s
  select( decade, avg_age_M, avg_age_F, age_list_M, age_list_F, 
          movie_name, director, release_year, actor_name_M, age_M, actor_name_F, age_F, age_difference, color_age_diff)


## create and locate the image url
base_url = "https://raw.githubusercontent.com/jonkingseestheworld/TidyTuesdayPlots/main/2023/20230214wk7/img/"
img_url <- paste0(base_url, "hollywood",".png")



#create summary table using reactable library
movie_reactable <- df_movie_table %>% 
  reactable( fullWidth = FALSE,
             height="90%",
             #pagination = FALSE,
             borderless = FALSE,
             defaultPageSize=8,
             defaultColDef = colDef(vAlign="center", align="center", width=120),
             theme = reactableTheme(
               style=list(fontFamily= "Ubuntu")
             ),
             #create grouped columns (equivalent of table spanners in gt)
             columnGroups = list(
               colGroup(name="All Recorded Movies in the Decade", 
                        columns = c("avg_age_M", "avg_age_F", "age_list_M", "age_list_F")),
               colGroup(name="Movie with the Largest Age Gap in the Decade", 
                        columns = c("movie_name", "release_year", "actor_name_M", "actor_name_F", "age_difference")) 
             ),
             #customize individual columns
             columns = list(
               html=TRUE,
               #hide specific columns using 'colDef(show=FALSE)' function
               color_age_diff = colDef(show=FALSE),
               age_M = colDef(show=FALSE), 
               age_F = colDef(show=FALSE), 
               director = colDef(show=FALSE), 
               release_year = colDef(show=FALSE ),
               decade = colDef(name = "Decade",
                               width=100),
               avg_age_M = colDef(name = "Average Age of Male Actors",
                                  style = list(color = "#097969")
                                    ),
               avg_age_F = colDef(name = "Average Age of Female Actors",
                                  style = list(color = "#E74C3C")
               ),
               ## columns with individual sparkbar plots
               age_list_M = colDef(html=T,
                                   name = glue("Ages of Male Actors <br>(---median)"), 
                                   width = 180,
                                   cell = react_sparkbar(data = .,
                                                         height = 70,
                                                         max_value=79,
                                                         statline = 'median',
                                                         fill_color = "#097969",
                                                         tooltip_type = 1,
                                                         margin = reactablefmtr::margin(r = 30)
                                                         )),
               age_list_F = colDef(html=T,
                                   name = "Ages of Female Actors <br>(---median)", 
                                   width = 180,
                                   cell = react_sparkbar(data = .,
                                                         height = 70,
                                                         max_value=79,
                                                         statline = 'median',
                                                         fill_color = "#E74C3C",
                                                         tooltip_type = 1,
                                                         margin = reactablefmtr::margin(r = 30)
                                                         )),
               movie_name = colDef(html=T,
                                   name = "Movie Name <br>(Director, Year)",
                                   width = 250,
                                   #use cell_style to add left sided border
                                   style = cell_style(
                                     border_color = "#f2f2f2",
                                     border_style = "solid",
                                     vertical_align = "center",
                                     border_width = "1px 0px 0px 2px",
                                     ),
                                   #use custom function for incorporating info from multiple cols into one
                                   cell = function(value, index){
                                     director_name = df_movie_table$director[[index]] 
                                     release_year = df_movie_table$release_year[[index]]
                                     
                                     #use htmltools to create divs
                                     htmltools::tagList(
                                       div( #style="display: inline-block;vertical-align:middle;",
                                           span(value),
                                           br(),
                                           span("(",director_name,", ",  release_year, ")", style = "color:#808080")
                                       )
                                     )
                                   }),
               actor_name_M = colDef(html=T,
                                     name="Male Actor, <br>Age",
                                     # use custom function for incorporating info from multiple cols into one
                                     cell = function(value, index){
                                       age_M = df_movie_table$age_M[[index]] 
                                       tagList(
                                         span(value, br(), age_M )
                                       )
                                       },
                                     style = list(color = "#097969") 
                                     ),
               actor_name_F = colDef(html=T,
                                     name="Female Actor, <br>Age",
                                     # use custom function for incorporating info from multiple cols into one
                                     cell = function(value, index){
                                       age_F = df_movie_table$age_F[[index]] 
                                       tagList(
                                         span(value, br(), age_F)
                                       )
                                     },
                                     style = list(color = "#E74C3C") 
                                     ),
               age_difference = colDef(name="Age Gap",
                                       # use custom function for incorporating info from multiple cols into one
                                       cell = function(value, index){
                                         color = df_movie_table$color_age_diff[[index]]
                                         age_comp = ifelse(df_movie_table$color_age_diff[[index]]=="#097969", 
                                                           paste0("M > F"), paste0("F > M") )
                                         tagList(
                                           span(age_comp, br(), value,  style=glue("text-align:center; display:inline-block;width:80px;color:{color};"))
                                         )
                                         })            
               ) 
             ) 



## final table editting
movie_reactable %>%
  #use htmlwidgets to add title and subtitle with prependContent
  htmlwidgets::prependContent(
    tagList(
      div(
        style = "display: inline-block;vertical-align:middle; padding-bottom:5px",    
        img(src=img_url, style = "width: 280px; height: 120px; float: left; margin-right:35px; margin-top:5px"),
        div(
          style= 'margin-top: 0px; padding-top:0px; margin-bottom:10px; width: 1200px',
          h1("Age Gap In Movies", style=glue('font-family:Ubuntu;margin-bottom:10px;font-size:35px;')),
          div("Age difference of actors playing a love interest in Hollywood movies over decades since the 1950s. A sample of 1132 opposite sex couples are included.", 
              style=glue('font-family:Ubuntu;padding-bottom:10px; margin-bottom: 10px; color:#A2A2A2;font-size:17px;'))
          )
        )
     )
  ) %>%
  #use htmlwidgets to add table footer with info on Source + personal social icons
  htmlwidgets::appendContent(
    tagList(
      div(
        div("W7 2023 #TidyTuesday | Source: Data is Plural", style=glue('display:inline-block;align:right;text-align:left;vertical-align:middle;font-size:15px;margin-top:0px;padding-bottom:3px;color:	#565656;width:500px;')),
        div(span("Graphic by Johnny K Lau",style=glue('font-size:15px;padding-right:15px;')),
            span(shiny::icon("github",style="color:#232b2b;")),
            span("jonkingseestheworld", style=glue('font-size:15px;')),
            style=glue('display:inline-block;align:right;text-align:right;vertical-align:middle;font-size:15px;margin-top:5px;padding-bottom:3px;color:	#565656; width:785px;'))
        , style='width:1315px;border-top-style:solid;border-color:#f2f2f2;padding-top:5px;')
    )
  )
      



# reactablefmtr::save_reactable_test(input=movie_reactable, output="HollywoodAge.png")


