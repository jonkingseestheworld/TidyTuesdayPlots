library(tidyverse)
library(showtext)
library(janitor)
library(ggtext)
library(ggimage)
library(glue)

# Load data
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')


# Add custom font
font_add_google(name = "Inter", family = "inter")
showtext_auto()

# Add color palette
#pal <- c(AAPL = "#a2aaad", ADBE = "#ff0000", AMZN = "#ff9900", CRM = "#009edb", CSCO = "#1798c1", GOOGL = "#34a853", IBM = "#1fc0c1",
#         INTC = "#127cc1", META = "#0668e1", MSFT = "#f34f1c", NFLX = "#d81f26", NVDA = "#76b900", ORCL = "#c74634", TSLA = "#e31937")

pal <- c(AAPL = "#a2aaad", ADBE = "#ff0000", AMZN = "#FFA500", CRM = "#009edb", 
         CSCO = "#1fc0c1", GOOGL = "#f34f1c", IBM = "#0668e1",
         INTC = "#127cc1", META = "#1798c1", MSFT = "#34a853", NFLX = "#000000", 
         NVDA = "#76b900", ORCL = "#a45729", TSLA = "#e31937")


# Clean company data
big_tech_companies$company <- gsub(',', '', big_tech_companies$company)
big_tech_companies$company <- gsub('Inc.', '', big_tech_companies$company)
big_tech_companies$company <- gsub('Corporation', '', big_tech_companies$company)


# Get company max closing value and min date
companies_logos_df <- big_tech_stock_prices %>%
  mutate(
    first_date = min(date),
    max_close = max(close)
  ) %>%
  distinct(stock_symbol, first_date, max_close) %>%
  mutate( logo_path = paste0(here::here(), "/2023/20230207wk6/logos/", stock_symbol, ".png") )



# Join datasets 
df_joined <- big_tech_stock_prices %>%
  left_join( big_tech_companies, by="stock_symbol") %>%
  select(-stock_symbol)



# Create visualization
stockprice_plot <- ggplot(data= df_joined) +
  geom_line(aes(date, close, group = company), alpha = 0.5, size = 0.1, color = "#a2aaad") +
  geom_area(data = big_tech_stock_prices,
            aes(date, close, color = stock_symbol, fill = stock_symbol), #big_tech_stock_prices, 
            alpha = 0.2, size = 0.5) +
  geom_richtext(
    data = companies_logos_df, 
    aes(x = first_date, y = max_close-100, 
        label = glue::glue("<img src='{logo_path}' width='30'/>")
        ),
    hjust = 0,
    fill = NA,
    label.padding = unit(.125, "lines"),
    label.r = unit(0, "lines"),
    label.size = unit(0, "lines")
  ) +
  facet_wrap(~stock_symbol, ncol = 4) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme_void() +
  
  labs( title = "Big Tech Stock Prices",
        subtitle = "Stock price fluctuation of 14 big tech companies between 2010 and 2022 
        <br>Close price shown only on business days",
        caption = "Graphic: Johnny K Lau | Data: Yahoo Finance "
        ) +
  theme(text = element_text(family = "inter", lineheight = 0.4, color = "black"),
        plot.background = element_rect(fill = "#e5fbe5", color = "#e5fbe5"),
        panel.background = element_rect(fill = "#e5fbe5", color = "#e5fbe5"),
        plot.title = element_text(size = 84, margin = margin(b = 20), face = "bold"),
        plot.subtitle = element_markdown(size = 48, margin = margin(b = 25)),
        plot.caption = element_markdown(margin = margin(b = 10), size = 35, hjust = 0.98),
        plot.margin = margin(20, 50, 20, 50),
        axis.text = element_text(size = 35),
        strip.text = element_blank(),
        legend.position = "none")

# Save plot
ggsave("2023/20230207wk6/20230207wk6_techstockprice.png", stockprice_plot, width = 12, height = 12)
