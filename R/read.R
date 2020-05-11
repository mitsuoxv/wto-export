library(tidyverse)

# downloaded from https://timeseries.wto.org/
data_raw <- read_csv("data/WtoData_20200510015454.csv")

wto_export <- data_raw %>% 
  select(`Reporting Economy`, `Product/Sector`, Year, Value)

names(wto_export) <- c("exporter", "product", "year", "value")

# wto_export$exporter %>% unique()

# check sum
exports <- wto_export %>% 
  filter(exporter != "World") %>% 
  # remove double count
  filter(!(exporter %in% 
             c(
               "German Democratic Republic",
               "Germany, Federal Republic of",
               "Ethiopia (+ Eritrea)"
             )
  )) %>% 
  # Hong Kong returned to China in 1997
  filter(!(exporter == "Hong Kong, China" & year >= 2000))

world <- wto_export %>% 
  filter(exporter == "World") %>% 
  select(-exporter)

sum(is.na(world$value))

total <- exports %>% 
  group_by(product, year) %>% 
  summarize(total = sum(value))

# In some products, total of each area differs from world
# by more than 5 percent, but dicide to ignore
large_diff <- total %>% 
  left_join(world) %>% 
  mutate(
    diff = value - total,
    diff_percent = diff / value * 100
    ) %>% 
  filter(abs(diff_percent) > 5)

View(large_diff)

# add share column
exports2 <- exports %>% 
  left_join(total) %>% 
  mutate(
    share = value / total * 100,
    share_sq = share^2)

export_hhi <- exports2 %>% 
  group_by(product, year) %>% 
  summarize(
    hhi = sum(share_sq)
  ) %>% 
  left_join(world) %>% 
  drop_na(value) %>% 
  select(-value)

# chart
export_hhi %>% 
  ggplot(aes(year, hhi, color = fct_reorder2(product, year, hhi))) +
  geom_line()

# save
ggsave(filename = "output/hhi.pdf",
       width = 8, height = 6, units = "in", dpi = 300)

export_hhi %>% 
  spread(year, hhi) %>% 
  write.csv("output/export_hhi.csv", row.names = FALSE)

exports2$exporter %>% unique()

exports2 %>% 
  select(-value, -total, -share_sq) %>% 
  filter(product == "Manufactures") %>% 
  filter(exporter %in% c("United States of America", "China",
                         "Germany", 
                         "Japan",
                         "Chinese Taipei", "Hong Kong, China",
                         "Korea, Republic of", "Singapore",
                         "Indonesia", "Malaysia", "Philippines", "Thailand")) %>% 
  select(-product) %>% 
  spread(year, share) %>% 
  write.csv("output/export_mfg_share.csv", row.names = FALSE)


