# TODO: Include question length
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

fname_q <- "data/Rgenerated/data_questions.feather"
fname_qf <- "data/Rgenerated/data_questions_filler.feather"
fname_model <- "data/fits/question"
fname_plot <- "fig/question"

# Read files -------------------------------------------------------------------
questions <- read_feather(fname_q)
questions_filler <- read_feather(fname_qf)

# Descriptive: Answer Analysis -------------------------------------------------


## Averages
q_avgs <- se_cousineau(questions, n_condition=4,
                       subject, is_expected,
                       group = "condition", is_proportion = T) %>% 
  as.data.frame() %>% 
  mutate(M = round(100*M, digits = 0),
         Var = round(100*Var, digits = 0),
         SE = round(100*SE, digits = 0),
         CI = 1.96*SE)

p_q_avgs <- q_avgs %>% ggplot(aes(condition, M)) + geom_point() +
  geom_errorbar(aes(ymin = M - CI,
                    ymax = M + CI),
                width = 0.4, linewidth=1) + 
  xlab("Condition") + ylab("Percentage of 'Expected Readings'") + 
  labs(title = "Mean percentage of 'yes' responses per condition")

## bySubject means
q_bySubj <- questions %>% group_by(subject, condition) %>% 
  summarize(M = round(100*mean(is_expected), digits = 0), 
            SE = round(100*se.bin(is_expected), digits = 0),
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
  summarize(M = round(100*mean(is_expected), digits = 0), 
            SE = round(100*se.bin(is_expected), digits = 0),
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
# Descriptive: Answer Analysis Filler ------------------------------------------

q_f_avgs <- questions_filler %>% 
  summarize(M = mean(is_expected), 
            SE = se.bin(is_expected),
            CI = 1.96*SE) %>% as.data.frame()  %>% 
  mutate(M = round(100*M, digits = 0),
         SE = round(100*SE, digits = 0),
         CI = 1.96*SE)
  

# Bayesian: Answer Models ------------------------------------------------------
questions$item_no %<>% as.factor()
questions$condition %<>% as.factor()

wr = c(.5, 0, 0)
nr = c(0, .5, 0)
d = c(0, 0, .5)
baseline = rep(-.5, 3)
c_H = rbind(baseline, d, nr, wr)
contrasts(questions$condition) = c_H


m <- brm(
  is_expected ~1+ condition + (1+condition|item_no) + (1+condition|subject),
  data = questions,
  family = bernoulli,
  chains = 4, cores = 4,
  iter = 4000,warmup = 2000,
  control = list(adapt_delta = 0.9),
  backend="cmdstanr", 
  stan_model_args=list(stanc_options = list("O1")), 
  file = paste0(fname_model, "_mresp")
)


# Bayesian: Answer Model Plot --------------------------------------------------
c1 <- as.numeric((hypothesis(m, "condition1 < 0")$hypothesis["Post.Prob"])*100)
c2 <- as.numeric((hypothesis(m, "condition2 < 0")$hypothesis["Post.Prob"])*100)
c3 <- as.numeric((hypothesis(m, "condition3 < 0")$hypothesis["Post.Prob"])*100)
p_m <- as_draws_df(m) %>% 
  select(starts_with("b")) %>% select(-b_Intercept) %>%
  set_names(c("WR", "NR", "D")) %>% 
  # mutate(D = exp(D) / (1 + exp(D)),
  #        NR = exp(NR) / (1 + exp(NR)),
  #        WR = exp(WR) / (1 + exp(WR))) %>%
  pivot_longer(everything()) %>% 
  
  ggplot(aes(x = value, y = reorder(name, value))) +  # note how we used `reorder()` to arrange the coefficients
  geom_vline(xintercept = 0.0, color = "black", alpha = 1/10) +
  #geom_hline(yintercept = 0) +
  stat_pointinterval(point_interval = mode_hdi, .width = .95, 
                     size = 4, point_size =4, shape = 21, 
                     point_fill = "white", color = "black") +
  labs(title = "Effects on baseline",
       x = "Estimate (logit)", 
       y = NULL) +
  theme_classic() +
  theme(axis.text.y = element_text(hjust = 0),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank()) +
  expand_limits(x = 1.7) +
  annotate("text", x = 1.1, y = "WR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c1, 1), "%"),
           size = 4.3, color="black") + 
  annotate("text", x = 1.1, y = "D", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c2, 1), "%"),
           size = 4.3, color="black") +
  annotate("text", x = 1.1, y = "NR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c3, 1), "%"),
           size = 4.3, color="black")

# Descriptive Response Time Analysis -------------------------------------------
questions$RT %<>% as.integer()
q_avgs_rt <- se_cousineau(questions, n_condition=4,
                       subject, RT,
                       group = "condition") %>% 
  as.data.frame() %>% 
  mutate(CI = 1.96*SE)

p_q_avgs_rt <- q_avgs_rt %>% ggplot(aes(condition, M)) + geom_point() +
  geom_errorbar(aes(ymin = M - CI,
                    ymax = M + CI),
                width = 0.4, linewidth=1) + 
  xlab("Condition") + ylab("Response Time") + 
  labs(title = "Mean response times per condition")

## bySubject means
q_bySubj_rt <- questions %>% group_by(subject, condition) %>% 
  summarize(M = mean(RT), 
            SE = se(RT),
            CI = 1.96*SE)
p_q_bySubject_rt <- ggplot(q_bySubj_rt, aes(x = subject, y = M )) + 
  geom_point() +
  geom_errorbar(aes(ymin = M - SE,
                    ymax = M + SE)) + 
  facet_wrap(~condition) + 
  scale_x_discrete(name="Participants", levels(q_bySubj$subject)[c(T, rep(F,4))]) +
  ylab("Response Time") + labs(
    title = "Mean response time per participant",
    subtitle = "for each condition")

## byItem means
q_byItem_rt <- questions %>% group_by(item_no, condition) %>% 
  summarize(M = mean(RT), 
            SE = se(RT),
            CI = 1.96*SE)
p_q_byItem_rt <- ggplot(q_byItem_rt, aes(x = item_no, y = M )) + 
  geom_point() +
  geom_errorbar(aes(ymin = M - SE,
                    ymax = M + SE)) + 
  facet_wrap(~condition) + 
  #scale_x_discrete(name="Items", levels(q_byItem$item_no)[c(T, rep(F,4))]) +
  ylab("Percentage 'yes'") + labs(
    title = "Mean percentage of 'yes' responses per item",
    subtitle = "for each condition")

# Descriptive Response Time Analysis Filler ------------------------------------

q_f_avgs_rt <- questions_filler %>% 
  summarize(M = mean(RT), 
            SE = se(RT),
            CI = 1.96*SE) %>% as.data.frame() 

# Bayesian: Response Time Model ------------------------------------------------
m_rt <- brm(
  RT ~1+ condition + (1+condition|item_no) + (1+condition|subject),
  data = questions,
  family = lognormal(),
  chains = 4, cores = 4,
  iter = 4000,warmup = 2000,
  control = list(adapt_delta = 0.9),
  backend="cmdstanr", 
  stan_model_args=list(stanc_options = list("O1")), 
  file = paste0(fname_model, "_rt")
)

# Bayesian: Response Time Model Plot -------------------------------------------
c1 <- as.numeric((hypothesis(m_rt, "condition1 < 0")$hypothesis["Post.Prob"])*100)
c2 <- as.numeric((hypothesis(m_rt, "condition2 < 0")$hypothesis["Post.Prob"])*100)
c3 <- as.numeric((hypothesis(m_rt, "condition3 < 0")$hypothesis["Post.Prob"])*100)
p_m_rt <- as_draws_df(m_rt) %>% 
  select(starts_with("b")) %>% select(-b_Intercept) %>%
  set_names(c("WR", "NR", "D")) %>% 
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
  expand_limits(x = 0.5) +
  annotate("text", x = 0.4, y = "WR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c1, 1), "%"),
           size = 4.3, color="black") + 
  annotate("text", x = 0.4, y = "NR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c2, 1), "%"),
           size = 4.3, color="black") +
  annotate("text", x = 0.4, y = "D", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c3, 1), "%"),
           size = 4.3, color="black")

# RT Correct -------------------------------------------------------------------
q_correct <- questions %>% filter(is_expected == T)
qc_avgs_rt <- se_cousineau(q_correct, n_condition=4,
                          subject, RT,
                          group = "condition") %>% 
  as.data.frame() %>% 
  mutate(CI = 1.96*SE)

p_qc_avgs_rt <- qc_avgs_rt %>% ggplot(aes(condition, M)) + geom_point() +
  geom_errorbar(aes(ymin = M - CI,
                    ymax = M + CI),
                width = 0.4, linewidth=1) + 
  xlab("Condition") + ylab("Response Time") + 
  labs(title = "Mean response times (expected readings) per condition")

## bySubject means
qc_bySubj_rt <- q_correct %>% group_by(subject, condition) %>% 
  summarize(M = mean(RT), 
            SE = se(RT),
            CI = 1.96*SE)
p_qc_bySubject_rt <- ggplot(qc_bySubj_rt, aes(x = subject, y = M )) + 
  geom_point() +
  geom_errorbar(aes(ymin = M - SE,
                    ymax = M + SE)) + 
  facet_wrap(~condition) + 
  scale_x_discrete(name="Participants", levels(q_bySubj$subject)[c(T, rep(F,4))]) +
  ylab("Response Time") + labs(
    title = "Mean response time per participant",
    subtitle = "for each condition")

## byItem means
qc_byItem_rt <- q_correct %>% group_by(item_no, condition) %>% 
  summarize(M = mean(RT), 
            SE = se(RT),
            CI = 1.96*SE)
p_qc_byItem_rt <- ggplot(qc_byItem_rt, aes(x = item_no, y = M )) + 
  geom_point() +
  geom_errorbar(aes(ymin = M - SE,
                    ymax = M + SE)) + 
  facet_wrap(~condition) + 
  #scale_x_discrete(name="Items", levels(q_byItem$item_no)[c(T, rep(F,4))]) +
  ylab("Percentage 'yes'") + labs(
    title = "Mean percentage of 'yes' responses per item",
    subtitle = "for each condition")

# Descriptive Response Time Analysis Filler ------------------------------------

qc_f_avgs_rt <- questions_filler %>% filter(is_expected == T) %>% 
  summarize(M = mean(RT), 
            SE = se(RT),
            CI = 1.96*SE) %>% as.data.frame() 

# Bayesian: Response Time Model ------------------------------------------------
m_correct_rt <- brm(
  RT ~1+ condition + (1+condition|item_no) + (1+condition|subject),
  data = q_correct,
  family = lognormal(),
  chains = 4, cores = 4,
  iter = 4000,warmup = 2000,
  control = list(adapt_delta = 0.9),
  backend="cmdstanr", 
  stan_model_args=list(stanc_options = list("O1")), 
  file = paste0(fname_model, "_correct_rt")
)

# Bayesian: Response Time Model Plot -------------------------------------------
c1 <- as.numeric((hypothesis(m_correct_rt, "condition1 < 0")$hypothesis["Post.Prob"])*100)
c2 <- as.numeric((hypothesis(m_correct_rt, "condition2 < 0")$hypothesis["Post.Prob"])*100)
c3 <- as.numeric((hypothesis(m_correct_rt, "condition3 < 0")$hypothesis["Post.Prob"])*100)
p_m_correct_rt <- as_draws_df(m_correct_rt) %>% 
  select(starts_with("b")) %>% select(-b_Intercept) %>%
  set_names(c("WR", "NR", "D")) %>% 
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
  expand_limits(x = 0.5) +
  annotate("text", x = 0.4, y = "WR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c1, 1), "%"),
           size = 4.3, color="black") + 
  annotate("text", x = 0.4, y = "NR", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c2, 1), "%"),
           size = 4.3, color="black") +
  annotate("text", x = 0.4, y = "D", hjust = -0.08,
           label = paste0("P(\u03b8 < 0): ",
                          round(c3, 1), "%"),
           size = 4.3, color="black")


# Export Plots -----------------------------------------------------------------
purrr::map(c(
  paste0(fname_plot, "_avg.pdf"),
  paste0(fname_plot, "_avg.png")
), ~ ggsave(.x,
  plot = p_q_avgs,
  width = 4.5,
  height = 3.2,
  dpi = 320
)
)

purrr::map(c(
  paste0(fname_plot, "_rt_avg.pdf"),
  paste0(fname_plot, "_rt_avg.png")
), ~ ggsave(.x,
            plot = p_q_avgs_rt,
            width = 4.5,
            height = 3.2,
            dpi = 320
)
)

purrr::map(c(
  paste0(fname_plot, "_model_post.pdf"),
  paste0(fname_plot, "_model_post.png")
), ~ ggsave(.x,
            plot = p_m,
            width = 9,
            height = 3,
            dpi = 320
)
)

purrr::map(c(
  paste0(fname_plot, "_model_rt_post.pdf"),
  paste0(fname_plot, "_model_rt_post.png")
), ~ ggsave(.x,
            plot = p_m_rt,
            width = 7.5,
            height = 3,
            dpi = 320
)
)
