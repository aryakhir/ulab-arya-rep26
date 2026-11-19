library(tidyverse)
library(lme4)

# check the first 2 lines and column count of merged_all.txt to see if it has column names
head(readLines("data/merged_all.txt", n = 2))
# it doesn't, so we check the authors' column names in Analysis_Exp5.R
readLines("scripts/Analysis_Exp5.R", n = 55)

names(exp5_raw) <- c(
  "ID", "session","block", "trial", "setsize","cue", "distractor", "testorder1", "testorder2", "testorder3","oldage1", "oldage2", "oldage3", "rcat1", "rcat2", "rcat3","RT1", "RT2","RT3")
exp5_clean <- exp5_raw %>%
  pivot_longer(
    cols = starts_with("rcat"),
    names_to = "item",
    values_to = "rcat"
  ) %>%
mutate(
  item = as.numeric(str_remove(item, "rcat")),
  correct = ifelse(rcat == 1, 1, 0),
  distractor_label = factor(distractor,
                            levels = c(0,1),
                            labels = c("No Distractor (Immediate)", "Distractor Delay")),
  response_type = factor(rcat,
                         levels = 1:4,
                         labels = c("Target (Correct)", 
                                    "Same-Trial Lure (Binding Error)", 
                                    "Old Lure (PI Intrusion)", 
                                    "New Lure (Item Error)"))

  ) %>%
filter(floor(setsize / 2) >= item, rcat != 0)

# 4. summary table: accuracy by set size and distractor
exp5_summary <- exp5_clean %>%
  group_by(ID, setsize, distractor_label) %>%
  summarize(subj_acc = mean(correct), .groups = "drop") %>%
  group_by(setsize, distractor_label) %>%
  summarize(
    n_subjects = n(),
    mean_acc   = round(mean(subj_acc), 3),
    se_acc     = round(sd(subj_acc) / sqrt(n()), 3),
    .groups    = "drop"
  )
print(exp5_summary)

# 5. plot: distractor delay effect across set sizes
ggplot(exp5_clean, aes(x = factor(setsize), y = correct, color = distractor_label, group = distractor_label)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    title = "Experiment 5 Replication: Distractor Delay Across Set Sizes",
    subtitle = "Distractor delay impairs Set Size 2 (WM) while sparing larger loads (LTM)",
    x = "Memory Set Size",
    y = "Proportion Correct (Target)",
    color = "Retention Condition"
  )

# 6. Inferential Mixed-Effects Logistic Regression (same as 3 and 4, to show stat. sig)
exp5_model <- glmer(correct ~ factor(setsize) * distractor_label + (1 | ID),
                    data = exp5_clean,
                    family = binomial)
summary(exp5_model)
