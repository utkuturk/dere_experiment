# Setup ------------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(feather)
library(janitor)
source("./scripts/0_scripts.R")

path_questions <- "./data/Rgenerated/data_questions.feather"
path_spr <- "./data/Rgenerated/data_spr.feather"
path_contexts <- "./data/Rgenerated/data_contexts.feather"
path_questions_filler <- "./data/Rgenerated/data_questions_filler.feather"
path_spr_filler <- "./data/Rgenerated/data_spr_filler.feather"
path_contexts_filler <- "./data/Rgenerated/data_contexts_filler.feather"
path_results <- "./data/ibex/raw_results_apr24.csv"
path_items <- "./data/ibex/chunk_includes/experiment.csv"
path_fillers <- "./data/ibex/chunk_includes/fillers.csv"


# Read data --------------------------------------------------------------------
df <- read.pcibex(path_results)
colnames(df) <- c(
  "time", "ip_md5", "controller", "item_id", "element_id", "label", 
  "latin", "penn_type", "penn_name", "parameter", "word", "event_time", 
  "age", "native", "answer_rt", "context_rt", "condition", "item_no", 
  "item_type", "word_rt", "newline", "Ambg_sentence", "comments"
)

df$subject <- with(df, paste(time, ip_md5)) %>% as.factor %>% as.integer %>% 
  sprintf("S[%s]", .) %>% as.factor()
df %<>% dplyr::select(-time, -ip_md5)

# Demo Trials ------------------------------------------------------------------
### whenever we wanted to test the experiment 
### we used a number below 18 or above 90 for the age. 
### So, we are extracting that data here.
df$age %<>% as.integer()
self_trials <- df %>% subset(!is.na(age)) %>% subset(age < 18 | age > 90) %>% 
  .$subject %>% unique() %>% droplevels()
df %<>% subset(!subject %in% self_trials)

# Form -------------------------------------------------------------------------
# extract responses to questions in form
form <- df %>% select(subject, age, native)
form %<>% subset(age != "undefined")
form <- form[!duplicated(form), ]
form$age %<>% as.integer()
form$subject %<>% as.factor()
N_total <- length(form$subject)
## native language
non_native_subjects <- c("S[14]", "S[15]", "S[38]", "S[119]")
N_nonnative <- length(non_native_subjects)


# Practice Accuracy ------------------------------------------------------------
df_no_exclusion = df
# do other exclusions based on practice

# get the answers to practice questions
practice <- filter(df, (penn_name == "answer" & label == "practice"))
correct_practice <- data.frame(
  item_no = c(101, 102, 103, 104, 105),
  correct_answer = c("2ndOne", "2ndOne", "1stOne", "1stOne", "2ndOne")
)

practice %<>% left_join(. , correct_practice, by = "item_no") %>% 
  mutate(is_accurate = ifelse(word == correct_answer, T, F)) %>%
  select(subject,condition,item_id,item_no,word,answer_rt, correct_answer, is_accurate)

low_practice_accuracy <- practice %>% group_by(subject) %>% 
  summarise( mean = mean(is_accurate)) %>% 
  filter( mean < 0.5 ) %>% .$subject 


# Self Paced Reading Times -----------------------------------------------------
spr <- subset(df, penn_type == "Controller-DashedSentence")
spr %<>% subset(label != "practice")
spr %<>% dplyr::rename("word_pos"=parameter, "RT"=word_rt)
spr %<>% dplyr::select(subject, condition, Ambg_sentence,item_id,item_no,word_pos, word, RT)
spr$word_pos %<>% as.integer()
spr_fillers <- spr %>% subset( is.na(condition) ) 
spr %<>% subset( !is.na(condition) ) 

# Questions  -------------------------------------------------------------------
questions <- subset(df, penn_name == "answer")
questions %<>% subset(item_type != "practice")
questions %<>% dplyr::select(subject,condition,item_id,item_no,word,answer_rt)
questions %<>% dplyr::rename( "answer"=word, "RT"=answer_rt)
#head(questions)
question_fillers <- questions %>% subset( is.na(condition) ) 
questions %<>% subset( !is.na(condition) ) 

# Contexts ---------------------------------------------------------------------
contexts <- subset(df, penn_name == "Context")
contexts %<>% subset(item_type != "practice")
contexts %<>% dplyr::select(subject,condition,item_id,item_no,context_rt)
contexts %<>% dplyr::rename("RT"=context_rt)
context_fillers <- contexts %>% subset( is.na(condition) ) 
contexts %<>% subset( !is.na(condition) ) 

# DataFrame Transpose -----------------------------------------------------------
# For some participants the order of words in the df is reversed.
wrong_order_participants = spr %>% filter( (word_pos == 1 & lead(word_pos) == 2) ) %>% .$subject %>% unique()
wrong_order_spr = spr %>% subset(subject %in% wrong_order_participants) 
correct_order_spr = spr %>% subset(!(subject %in% wrong_order_participants)) 

# calculating reverse
rev_wospr <- apply(wrong_order_spr, 2, rev) %>% as.data.frame()
rev_wospr$subject %<>% as.factor()
rev_wospr$item_id %<>% as.integer()
rev_wospr$item_no %<>% as.integer()
rev_wospr$word_pos %<>% as.integer()
rev_wospr$RT %<>% as.integer()
correct_order_spr$RT %<>% as.integer()
# bind them together
spr = dplyr::bind_rows(correct_order_spr, rev_wospr)

# Encode Critical Word ----------------------------------------------------
# keep the following in mind:
# some noun+1s are at the embedded verb
# some are at the initial noun of the complex embedded verb
spr %<>% mutate(
  is_bir = ifelse(word == "bir", "det", NA),
  n_p1 = lead(word),
  n_p2 = lead(word, n = 2),
  n_m1 = lag(word),
  n_m2 = lag(word, n = 2)
) %>% mutate(
  n_p1 = ifelse(n_p1 == "bir", "noun", NA),
  n_p2 = ifelse(n_p2 == "bir", "noun+1", NA),
  n_m1 = ifelse(n_m1 == "bir", "det-1", NA),
  n_m2 = ifelse(n_m2 == "bir", "det-2", NA),
) %>% mutate(
  word_labels = dplyr::coalesce(is_bir, n_p1, n_p2, n_m1, n_m2)
) %>% select(-is_bir, -n_p1, -n_p2, -n_m1, -n_m2)

# Correct/Expected Answers -----------------------------------------------------
# EXP ITEMS
## baseline conditions had correct answers
## other conditions had intended answers

items <- read_csv(path_items)
items %<>% select(ItemNo, Condition, is_Evet) %>%
  rename(item_no = ItemNo, condition = Condition)
items$is_Evet %<>% as.logical()

## Merge this with other files
questions %<>% merge(., items, by = c("item_no", "condition"))
contexts %<>% merge(., items, by = c("item_no", "condition"))
spr %<>% merge(., items, by = c("item_no", "condition"))
# add other details of questions
q_to_merge <- questions %>% select(item_no, subject, condition, answer) 

contexts %<>% merge(., q_to_merge, by = c("subject","item_no", "condition"))
spr %<>% merge(., q_to_merge, by = c("subject", "item_no", "condition"))


# FILLERS
filler_items <- read_csv(path_fillers)
filler_items %<>% select(ItemNo, is_Evet) %>%
  rename(item_no = ItemNo)
filler_items$is_Evet %<>% as.logical()

## Merge this with other files
question_fillers %<>% merge(., filler_items, by = "item_no")
context_fillers %<>% merge(., filler_items, by = "item_no")
spr_fillers %<>% merge(., filler_items, by = "item_no")

qf_to_merge <- question_fillers %>% select(subject, item_no, answer)

context_fillers %<>% merge(., qf_to_merge, by = c("item_no", "subject"))
spr_fillers %<>% merge(., qf_to_merge, by = c("item_no", "subject"))

# Encode expected responses -----------------------------------------------------


questions %<>% encode_expected()
question_fillers %<>% encode_expected()
spr %<>% encode_expected()
spr_fillers %<>% encode_expected()
contexts %<>% encode_expected()
context_fillers %<>% encode_expected()



# Anomalous RT -----------------------------------------------------------------

## SPR RT
ultra_short_spr_rt <- 50
ultra_long_spr_rt <- 2000
anomalous_rts <- spr %>% group_by(subject, item_no) %>% 
            dplyr::summarise( anomalous_spr_rts = sum(RT < ultra_short_spr_rt | RT > ultra_long_spr_rt ) > 2 ) %>%
            dplyr::summarise( perc_anomalous_spr_rts = mean(anomalous_spr_rts) )
hist(anomalous_rts$perc_anomalous_spr_rts)

ultrafast_rts <- anomalous_rts %>% subset(perc_anomalous_spr_rts > .1) %>% .$subject %>% as.character()

# Exclusion --------------------------------------------------------------------

excluded_subjects <- c(as.character(low_practice_accuracy), ultrafast_rts, non_native_subjects) %>% unique()

# exclude participants with low practice accuracy or too many anomalous RTs


questions %<>% excl(. , "subject", excluded_subjects)
question_fillers %<>% excl(. , "subject", excluded_subjects)
spr %<>% excl(. , "subject", excluded_subjects)
spr_fillers %<>% excl(. , "subject", excluded_subjects)
contexts %<>% excl(. , "subject", excluded_subjects)
context_fillers %<>% excl(. , "subject", excluded_subjects)

# Certain problematic items 
# 9 = bir yunan adasi
# 1 = bir avrupa uykesi
# 22 = bir bitki cayi
# 16 = bir muzik ogretmeni
# let's exclude these ones as well.
excluded_items <- c(1, 9, 22, 16)

questions %<>% excl(. , "item_no", excluded_items)
question_fillers %<>% excl(. , "item_no", excluded_items)
spr %<>% excl(. , "item_no", excluded_items)
spr_fillers %<>% excl(. , "item_no", excluded_items)
contexts %<>% excl(. , "item_no", excluded_items)
context_fillers %<>% excl(. , "item_no", excluded_items)

# Exclusion Stats --------------------------------------------------------------


p_subject_excl = 0.08 # output from excl() function

p_item_excl = round(4/24, digits = 2)


# Save -------------------------------------------------------------------------
# save formatted and filtered RTs
feather::write_feather(questions, path = path_questions)
feather::write_feather(question_fillers, path = path_questions_filler)

feather::write_feather(spr, path = path_spr)
feather::write_feather(spr_fillers, path = path_spr_filler)

feather::write_feather(contexts, path = path_contexts)
feather::write_feather(context_fillers, path = path_contexts_filler)
