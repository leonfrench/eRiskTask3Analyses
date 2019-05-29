library(ggplot2)
library(reshape2)
library(readr)
library(dplyr)
library(here)
library(magrittr)
library(readxl)

eval_results <- read_excel(here("/data/t3_evaluation_report.pdf.Table form results.xls"))
random <- read_csv(here("data/random_answers_results.csv"))
random %<>% mutate(ID = "random")
random %<>% mutate_if(is.numeric,  funs(./100))
random <- as_tibble(melt(random))
eval_results <- as_tibble(melt(eval_results))

joined <- bind_rows(random, eval_results)
unique(joined$ID)
joined %<>% mutate(Team = if_else(grepl("CAMH", ID), "CAMH", "Other team"))
#joined %<>% filter(variable == "AHR")

ggplot(data=joined, aes(x=value)) +  geom_blank() + theme_bw() +
  geom_histogram(data = joined %>% filter(ID=="random"), fill="grey") +
  #geom_histogram(data = joined %>% filter(ID!="random"), color="red", fill="red") +
  geom_vline(data = joined %>% filter(ID!="random"), aes(xintercept = value, color = Team)) +
  facet_wrap(. ~ variable, nrow = 2, scales = "free") + xlab("") + ylab("")


#pick some random runs, get max score and test if it beats the CAMH score
random_runs <- 18 #or 5 can be used
better_score = 0
for (i in 1:10000) {
  cat(".")
  if ((random %>% filter(variable == "ADODL") %>% sample_n(random_runs) %>% arrange(-value) %>% head(1) %>% .$value) >= 0.810) {
    better_score <- better_score + 1
  }
}
print(better_score)
