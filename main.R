library(tidyverse)
library(lubridate)
table_count_satisfaction <- list.files(path = "data-raw/", pattern = "export\\_alimconfiance\\_.*.csv") %>%
  map(
    .x = ., 
    .f = safely(
      .f = function(x) {
        read_csv2(file = paste0("data-raw/", x)) %>% 
          count(Synthese_eval_sanit) %>%
          mutate(date = sub(
            pattern = "export\\_alimconfiance\\_([[:digit:]]{4}\\-[[:digit:]]{2}\\-[[:digit:]]{2})\\.csv", 
            replacement = "\\1", 
            x = x)
            )
      }, quiet = TRUE)
    ) %>%
  map_df("result")
write_csv(x = table_count_satisfaction, "data-raw/table_count_satisfaction.csv")

table_count_satisfaction %>% 
  group_by(Synthese_eval_sanit) %>%
  summarise(n = sum(n)) %>%
  mutate(share = 100 * n / sum(n))

    






