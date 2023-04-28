library(tidyverse)
library(magrittr)
library(feather)
library(ggplot2)
source("./scripts/0_scripts.R")

# Read files -------------------------------------------------------------------
## get the files
files <- list.files("./data/Rgenerated/", 
                    pattern = "\\.feather$", 
                    full.names = T)

## form object names
object_names <- files %>% basename(.) %>% 
  tools::file_path_sans_ext(.) %>% 
  gsub("data_", '', .)

## read files and read them to environment
lapply(files, 
       read_feather) %>% 
  setNames(object_names) %>%
  list2env(envir=globalenv())

# Analysis: question responses -------------------------------------------------
questions %<>% mutate(response_yes = case_when(answer == "2ndOne" ~ F, TRUE ~ T))

## Averages
q_avgs <- se_cousineau(questions, n_condition=4,
                       subject, response_yes,
                       group = "condition", is_proportion = T) %>% 
  as.data.frame() %>% 
  mutate(M = round(100*M, digits = 0),
         Var = round(100*Var, digits = 0),
         SE = round(100*SE, digits = 0))

p_q_avgs <- q_avgs %>% ggplot(aes(condition, M)) + geom_point() +
  geom_errorbar(aes(ymin = M - SE,
                    ymax = M + SE),
                width = 0.4, linewidth=0.5) + 
  xlab("Condition") + ylab("Percentage 'yes'") + 
  labs(title = "Mean percentage of 'yes' responses per condition")

## bySubject means
q_bySubj <- questions %>% group_by(subject, condition) %>% 
  summarize(M = round(100*mean(response_yes), digits = 0), 
            SE = round(100*se.bin(response_yes), digits = 0),
            CI = 1.96*SE)
p_q_bySubject <- ggplot(q_bySubj, aes(x = subject, y = M )) + 
  geom_point() +
  geom_errorbar(aes(ymin = M - SE,
                    ymax = M + SE)) + 
  facet_wrap(~condition) + 
  scale_x_discrete(name="Participants", levels(q_bySubj$subject)[c(T, rep(F,4))]) +
  ylab("Accuracy (%)") + labs(
  title = "Mean proportion of 'yes' responses per participant",
  subtitle = "for each condition")

p_q_bySubject_d <- ggplot(filter(q_bySubj, condition == "d"), 
                          aes(x = subject, y = M )) + 
  geom_point() +
  geom_errorbar(aes(ymin = M - SE,
                    ymax = M + SE)) + 
  facet_wrap(~condition) + 
  scale_x_discrete(name="Participants", levels(q_bySubj$subject)[c(T, rep(F,4))]) +
  ylab("Accuracy (%)") + labs(
    title = "Mean proportion of 'yes' responses per participant",
    subtitle = "for condition 'd'")

## byItem means
q_byItem <- questions %>% group_by(item_no, condition) %>% 
  summarize(M = round(100*mean(response_yes), digits = 0), 
            SE = round(100*se.bin(response_yes), digits = 0),
            CI = 1.96*SE)
p_q_byItem <- ggplot(q_byItem, aes(x = item_no, y = M )) + 
  geom_point() +
  geom_errorbar(aes(ymin = M - SE,
                    ymax = M + SE)) + 
  facet_wrap(~condition) + 
  #scale_x_discrete(name="Items", levels(q_byItem$item_no)[c(T, rep(F,4))]) +
  ylab("Percentage 'yes'") + labs(
    title = "Mean percentage of 'yes' responses per item",
    subtitle = "for each condition")
# Analysis: context reading times ----------------------------------------------

# Analysis: spr reading times --------------------------------------------------

 