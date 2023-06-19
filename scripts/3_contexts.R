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
source("./scripts/0_scripts.R")

fname_c <- "data/Rgenerated/data_contexts.feather"
fname_cf <- "data/Rgenerated/data_contexts_filler.feather"
fname_model <- "data/fits/context"
fname_plot <- "fig/context"

# Read files -------------------------------------------------------------------
contexts <- read_feather(fname_c)
contexts_filler <- read_feather(fname_cf)

# Analysis: Context RT ---------------------------------------------------------
contexts$RT %<>% as.integer()
c_avgs_rt <- se_cousineau(contexts, n_condition=4,
                          subject, RT,
                          group = "condition") %>% 
  as.data.frame() %>% 
  mutate(CI = 1.96*SE)

c_q_avgs_rt <- c_avgs_rt %>% ggplot(aes(condition, M)) + geom_point() +
  geom_errorbar(aes(ymin = M - CI,
                    ymax = M + CI),
                width = 0.4, linewidth=1) + 
  xlab("Condition") + ylab("Response Time") + 
  labs(title = "Mean context reading times per condition")

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
