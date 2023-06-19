# Setup ------------------------------------------------------------------------
library(gdata)
library(hypr)
library(tidyverse)
library(magrittr)
library(feather)
library(ggplot2)
library(brms)
library(cmdstanr)
library(tidybayes)
library(marginaleffects)
source("./scripts/0_scripts.R")

fname_spr <- "data/Rgenerated/data_spr.feather"
fname_sprf <- "data/Rgenerated/data_spr_filler.feather"
fname_model <- "data/fits/spr"
fname_plot <- "fig/spr"

# Read files -------------------------------------------------------------------
spr <- read_feather(fname_spr)
spr_filler <- read_feather(fname_sprf)

# Delete conditions that have more than 2000 ms in it.
long_RTs <- spr %>% filter(RT > 2999) %>% select(subject, item_no, RT) %>% 
  group_by(subject, item_no) %>% slice(1)
spr %<>% anti_join(., long_RTs, by = c("subject", "item_no"))

# Short RTs
short_RTs <- spr %>% filter(RT < 100) %>% select(subject, item_no, RT) %>% 
  group_by(subject, item_no) %>% slice(1)
spr %<>% anti_join(., short_RTs, by = c("subject", "item_no"))

# write.csv(spr, "./data/Rgenerated/spr.csv")
# write.csv(spr_fillers, "./data/Rgenerated/spr_fillers.csv")
spr$word_labels %<>% reorder.factor(new.order = c(
  "det-2", "det-1", "det", "noun", "noun+1"
))

spr_critical <- spr %>% filter(!is.na(word_labels))
c <- spr_critical %>% 
  group_by( condition, word_labels) %>% 
  summarise(M = mean(RT),
            SE = se(RT), 
            CI = 1.96*SE)

avgs_spr %>%
  ggplot(aes(word_labels, M, group = condition, color = condition)) +
  theme(text=element_text(size=12),
        legend.position = "bottom", legend.title = element_blank(),
        axis.text.x = element_text(face = "bold.italic", angle = 30, hjust = 1)) +
  geom_point(aes(shape = condition), size= 2.4, 
             position = position_dodge(width = 0.1)) + 
  geom_line(linetype = "dashed") +
  geom_errorbar(aes(ymin = M-CI, ymax = M+CI, color=condition),
                width = .15, position = position_dodge(width = 0.1)) +
  ylab("average RT (ms)") 

# Models
## DET
m_det_rt <- brm(
  RT ~1+ condition + (1+condition|item_no) + (1+condition|subject),
  data = filter(spr_critical, word_labels == "det"),
  family = lognormal(),
  chains = 4, cores = 4,
  iter = 4000,warmup = 2000,
  control = list(adapt_delta = 0.9),
  backend="cmdstanr", 
  stan_model_args=list(stanc_options = list("O1")), 
  file = paste0(fname_model, "_det_rt")
)
c1 <- as.numeric((hypothesis(m_det_rt, "conditiond < 0")$hypothesis["Post.Prob"])*100)
c2 <- as.numeric((hypothesis(m_det_rt, "conditionnr < 0")$hypothesis["Post.Prob"])*100)
c3 <- as.numeric((hypothesis(m_det_rt, "conditionwr < 0")$hypothesis["Post.Prob"])*100)
as_draws_df(m_det_rt) %>% 
  select(starts_with("b")) %>% select(-b_Intercept) %>%
  # set_names(c("WR", "NR", "D")) %>% 
  # mutate(D = exp(D) / (1 + exp(D)),
  #        NR = exp(NR) / (1 + exp(NR)),
  #        WR = exp(WR) / (1 + exp(WR))) %>%
  pivot_longer(everything()) %>% 
  
  ggplot(aes(x = value, y = reorder(name, value))) +  # note how we used `reorder()` to arrange the coefficients
  geom_vline(xintercept = 0, color = "black", alpha = 1/10) +
  #geom_hline(yintercept = 0) +
  stat_pointinterval(point_interval = mode_hdi, .width = .95, 
                     size = 4, point_size =4, shape = 21, 
                     point_fill = "white", color = "black") +
  labs(title = "Effects on baseline as response times",
       x = "Estimate", 
       y = NULL) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) +
  expand_limits(x = 0.1) +
  annotate("text", x = 0.05, y = "WR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c1, 1), "%"),
           size = 4.3, color="black") + 
  annotate("text", x = 0.05, y = "NR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c2, 1), "%"),
           size = 4.3, color="black") +
  annotate("text", x = 0.05, y = "D", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c3, 1), "%"),
           size = 4.3, color="black")


## NOUN
m_noun_rt <- brm(
  RT ~1+ condition + (1+condition|item_no) + (1+condition|subject),
  data = filter(spr_critical, word_labels == "noun"),
  family = lognormal(),
  chains = 4, cores = 4,
  iter = 4000,warmup = 2000,
  control = list(adapt_delta = 0.9),
  backend="cmdstanr", 
  stan_model_args=list(stanc_options = list("O1")), 
  file = paste0(fname_model, "_noun_rt")
)

as_draws_df(m_noun_rt) %>% 
  select(starts_with("b")) %>% select(-b_Intercept) %>%
  set_names(c("D", "NR", "WR")) %>% 
  # mutate(D = exp(D) / (1 + exp(D)),
  #        NR = exp(NR) / (1 + exp(NR)),
  #        WR = exp(WR) / (1 + exp(WR))) %>%
  pivot_longer(everything()) %>% 
  
  ggplot(aes(x = value, y = reorder(name, value))) +  # note how we used `reorder()` to arrange the coefficients
  geom_vline(xintercept = 0, color = "black", alpha = 1/10) +
  #geom_hline(yintercept = 0) +
  stat_pointinterval(point_interval = mode_hdi, .width = .95, 
                     size = 4, point_size =4, shape = 21, 
                     point_fill = "white", color = "black") +
  labs(title = "Effects on baseline as response times",
       x = "Estimate", 
       y = NULL) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) +
  expand_limits(x = 0.07) +
  annotate("text", x = 0.05, y = "WR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(as.numeric((hypothesis(m_noun_rt, "conditionwr < 0")$hypothesis["Post.Prob"])*100), 1), "%"),
           size = 4.3, color="black") + 
  annotate("text", x = 0.05, y = "NR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(as.numeric((hypothesis(m_noun_rt, "conditionnr < 0")$hypothesis["Post.Prob"])*100), 1), "%"),
           size = 4.3, color="black") +
  annotate("text", x = 0.05, y = "D", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(as.numeric((hypothesis(m_noun_rt, "conditiond < 0")$hypothesis["Post.Prob"])*100), 1), "%"),
           size = 4.3, color="black")


## SPILLOVER
m_nounp1_rt <- brm(
  RT ~1+ condition + (1+condition|item_no) + (1+condition|subject),
  data = filter(spr_critical, word_labels == "noun+1"),
  family = lognormal(),
  chains = 4, cores = 4,
  iter = 4000,warmup = 2000,
  control = list(adapt_delta = 0.9),
  backend="cmdstanr", 
  stan_model_args=list(stanc_options = list("O1")), 
  file = paste0(fname_model, "_nounp1_rt")
)

fixef(m_det_rt)     %>% round(. , digits = 2)
fixef(m_noun_rt)    %>% round(. , digits = 2)
fixef(m_nounp1_rt)  %>% round(. , digits = 2)


