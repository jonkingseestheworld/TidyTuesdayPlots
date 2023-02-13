library(ggbump)
library(tidyverse)
library(cowplot)
library(wesanderson)
library(ggtext)
library(showtext)

# Add custom font
font_add_google(name = "Inter", family = "inter")
showtext_auto()

## Load data
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


df_stock_year_end <- big_tech_stock_prices %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(stock_symbol, year) %>%
  filter( date == max(date) ) %>%
  select( stock_symbol, year, close ) %>%
  group_by(year) %>%
  mutate( rank = rank( -close, ties.method="min")) %>%
  ungroup() %>%
  arrange(year, rank) %>%
  filter( year <= 2022) 


pal <- c(AAPL = "#a2aaad", ADBE = "#ff0000", AMZN = "#FFA500", CRM = "#009edb", 
         CSCO = "#1fc0c1", GOOGL = "#f34f1c", IBM = "#0668e1",
         INTC = "#127cc1", META = "#1798c1", MSFT = "#34a853", NFLX = "#000000", 
         NVDA = "#76b900", ORCL = "#a45729", TSLA = "#e31937")

df_pal <- data.frame(pal) %>%
  mutate(stock_symbol = row.names(.)) %>%
  left_join( df_stock_year_end %>%  filter( year==2010 & rank<=3 | year == 2022 & rank <=3) %>% 
               distinct(stock_symbol) %>%
               mutate(rankFirst3 = 1),
             by = "stock_symbol") %>%
  mutate( pal = ifelse(!is.na(rankFirst3), pal, "#a2aaad") ) %>%
  select(-rankFirst3)

df_stock_year_end <- df_stock_year_end %>%
  left_join(df_pal, by="stock_symbol")


# Get company max closing value and min date
start_df_logo <- df_stock_year_end %>%
  filter( year == 2010 ) %>%    #| year==2022
  select(stock_symbol, year, rank) %>%
  mutate( logo_path = paste0(here::here(), "/2023/20230207wk6/logos/", stock_symbol, ".png") )

end_df_logo <- df_stock_year_end %>%
  filter( year == 2022) %>%    #| year==2022
  select(stock_symbol, year, rank) %>%
  mutate( logo_path = paste0(here::here(), "/2023/20230207wk6/logos/", stock_symbol, ".png") )




## plot
big_tech_stock_rank_plot <- ggplot(df_stock_year_end, aes(year, rank, color = stock_symbol)) +
  geom_point(size = 4) +
  geom_bump(size = 1.5, smooth = 6) +
  geom_richtext(
    data = start_df_logo, 
    aes(x = year-2 , y = rank, 
        label = glue::glue("<img src='{logo_path}' width='30'/>") ),
    hjust = 0,  fill = NA,
    label.padding = unit(.125, "lines"),
    label.r = unit(0, "lines"), label.size = unit(0, "lines") 
    ) +
  geom_richtext(
    data = end_df_logo, 
    aes(x = year+1 , y = rank, 
        label = glue::glue("<img src='{logo_path}' width='30'/>") ),
    hjust = 0,  fill = NA,
    label.padding = unit(.125, "lines"),
    label.r = unit(0, "lines"), label.size = unit(0, "lines") 
  ) +
  scale_x_continuous(limits = c(2008, 2024),
                     breaks = seq(2010, 2022, 2)) +
  theme_minimal_grid(font_size = 14, line_size = 0)  +
  labs(y = "RANK",
       x = NULL) +
  scale_y_reverse(breaks = seq(1, 14, 1)) +
  scale_color_manual(values = df_pal$pal) +
  labs( title = "Big Tech Ranking By Stock Price Over Years",
        subtitle = "Rank movement of 14 big tech companies by year-end closing stock price between 2010 and 2022.
        <br>Most companies had experienced a roller-coster ride. In 2010, the top 3 were IBM, SalesForce and Oracle; 
        <br>in 2022, they were Adobe, Neflix and Microsoft.",
        caption = "Graphic: Johnny K Lau | Data: Yahoo Finance "
  ) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        text = element_text(family = "inter", lineheight = 0.4, color = "black"),
        plot.title = element_text(size = 70, margin = margin(t=20, b = 18), face = "bold"),
        plot.subtitle = element_markdown(size = 35, margin = margin( b = 25)),
        plot.caption = element_markdown(size = 28, margin = margin(t=30, b = 15), hjust=0.9),
        axis.title.y = element_text(face = 2, size = 35, color = "#a2aaad", hjust=0.95),
        axis.text.x = element_text(face = 2, size = 30, color = "#a2aaad"),
        axis.text.y = element_text(face = 2, size = 30, color = "#a2aaad"),
        panel.background = element_rect(fill = "#e5fbe5"),
        plot.background = element_rect(fill = "#e5fbe5"),
        plot.margin = margin(20, 20, 20, 30) ) 

ggsave("/2023/20230207wk6/20230207wk6_techrank.png", big_tech_stock_rank_plot, width = 10, height = 9)


