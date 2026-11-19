library(tidyverse)
library(lme4)

# 1. Load both .rda files and tag them with their stimulus condition
load("data/PairsPI2_dat.rda")
exp4_pairs <- dat %>% mutate(stimulus_type = "Word_Picture")
load("data/SmileyPairs_dat.rda")
exp4_smiley <- dat %>% mutate(stimulus_type = "Smiley_Pairs")

# 2. Stack both datasets together (bind_rows automatically aligns shared columns)
exp4_raw <- bind_rows(exp4_pairs, exp4_smiley)

# 3. Reshape rcat1:rcat8 into long format & remove unused '0' probe slots
exp4_clean <- exp4_raw %>%
  pivot_longer(
    cols = starts_with("rcat"),
    names_to = "probe",
    values_to = "rcat"
  ) %>%
  filter(rcat != 0) %>%
  mutate(
    correct = ifelse(rcat == 1, 1, 0),
    response_type = factor(rcat,
                           levels = 1:4,
                           labels = c("Target", "Same-Trial Lure", "Old Lure", "New Lure"))
  )

# 4. Summary table: accuracy by set size and stimulus type
exp4_summary <- exp4_clean %>%
  group_by(ID, setsize, stimulus_type) %>%
  summarize(subj_acc = mean(correct), .groups = "drop") %>%
  group_by(setsize, stimulus_type) %>%
  summarize(
    n_subjects = n(),
    mean_acc   = round(mean(subj_acc), 3),
    se_acc     = round(sd(subj_acc) / sqrt(n()), 3),
    .groups    = "drop"
  )
print(exp4_summary)

# 5. plot: word-picture vs. smiley pairs aross set sizes
ggplot(exp4_clean, aes(x = factor(setsize), y = correct, color = stimulus_type, group = stimulus_type)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    title = "Experiment 4 Replication: Semantic Support Across Set Sizes",
    subtitle = "Word-Picture pairs benefit from episodic LTM offloading at high loads compared to Smiley pairs",
    x = "Memory Set Size",
    y = "Proportion Correct (Target)",
    color = "Stimulus Type"
  )

# 6. Inferential Mixed-Effects Logistic Regression (same as exp3, test stat. sig.)
exp4_model <- glmer(correct ~ factor(setsize) * stimulus_type + (1 | ID),
                    data = exp4_clean,
                    family = binomial)
summary(exp4_model)