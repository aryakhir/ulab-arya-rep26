# experiment 2
library(tidyverse)
library(lme4)

# 1. Loading files from data subfolder
dat_noPI <- read.table("data/merged_openPool.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)
dat_noPI$PI <- "No_PI"
dat_PI <- read.table("data/merged_PI.txt", quote = "\"", comment.char = "", stringsAsFactors = FALSE)
dat_PI$PI <- "PI"

# 2. Stack both conditions together
exp2 <- bind_rows(dat_noPI, dat_PI)

# 3. Assign the exact 66 column names from the authors' script
names(exp2) <- c(
  "ID", "block", "trial", "setsize", "cue",
  paste0("testorder", 1:15),
  paste0("oldage", 1:15),
  paste0("rcat", 1:15),
  paste0("RT", 1:15),
  "PI"
)

# 4. Reshape rcat1:rcat15 into long format & filter out unused '0' probe slots to prevent redundancy.
exp2_clean <- exp2 %>%
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

# 5. Summary table: mean accuracy by set size and PI condition
exp2_summary <- exp2_clean %>%
  group_by(ID, setsize, PI) %>%
  summarize(subj_acc = mean(correct), .groups = "drop") %>%
  group_by(setsize, PI) %>%
  summarize(
    n_subjects = n(),
    mean_acc = round(mean(subj_acc), 3),
    se_acc = round(sd(subj_acc) / sqrt(n()), 3),
    .groups = "drop"
  )
print(exp2_summary)

# 6. Plot: Size x Proactive Interference
ggplot(exp2_clean, aes(x = factor(setsize), y = correct, color = PI, group = PI)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "red") +
  theme_classic() +
  labs(
    title = "Experiment 2 Replication: Proactive Interference Across Set Sizes",
    subtitle = "Low loads resist PI; larger loads suffer PI due to episodic LTM offloading",
    x = "Memory Set Size",
    y = "Proportion Correct (Target)",
    color = "Condition"
  )
