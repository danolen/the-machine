library(tidyverse)
library(baseballr)

pks2019 <- read.csv('Baseball Machine/Daily Files/2019/game_pks_2019.csv')
pks2020 <- read.csv('Baseball Machine/Daily Files/2020/game_pks_2020.csv')
pks2021 <- read.csv('Baseball Machine/Daily Files/2021/game_pks_2021.csv')
pks2022 <- read.csv('Baseball Machine/Daily Files/2022/game_pks_2022.csv')

allpks <- pks2019 %>% 
  bind_rows(pks2020, pks2021, pks2022)

tbl <- data.frame()
i = 1
for (i in 1:length(allpks$game_pk)) {
  probs = mlb_probables(allpks$game_pk[i])
  tbl <- tbl %>% 
    bind_rows(probs)
}

write.csv(tbl, 'Baseball Machine/Daily Files/probables_2019_2022.csv')
