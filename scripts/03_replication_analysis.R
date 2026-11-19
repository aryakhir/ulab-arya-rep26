library(tidyverse)
library(lme4)

# 1. Load the Experiment 3 .rda (aka preformatted for R) file from data subfolder
load("data/PairsPI2_dat.rda")

# 2. Reshape rcat1:rcat8 to long format & remove unused '0' probe slots
exp3_clean <- dat %>%
  pivot_longer(
    cols = starts_with("rcat"),
    names_to = "probe",
    values_to = "rcat"ds
  ) %>%
  filter(rcat != 0) %>%
  mutate(
    correct = ifelse(rcat == 1, 1, 0),
    response_type = factor(rcat,
                           levels = 1:4,
                           labels = c("Target (Correct)", 
                                      "Same-Trial Lure (Binding Error)", 
                                      "Old Lure (PI Intrusion)", 
                                      "New Lure (Item Error)"))
  )

# 3. Accuracy & Capacity (K) Summary Table across Set Sizes
exp3_summary <- exp3_clean %>%
  group_by(ID, setsize) %>%
  summarize(subj_acc = mean(correct), .groups = "drop") %>%
  group_by(setsize) %>%
  summarize(
    n_subjects = n(),
    mean_acc   = round(mean(subj_acc), 3),
    se_acc     = round(sd(subj_acc) / sqrt(n()), 3),
    mean_K     = round(mean(setsize * (subj_acc - 0.25) / (1 - 0.25)), 2),
    .groups    = "drop"
  )
print(exp3_summary)

# 4. Error Decomposition Summary Table (Binding vs. Item Errors)
exp3_errors <- exp3_clean %>%
  group_by(setsize, response_type) %>%
  summarize(count = n(), .groups = "drop_last") %>%
  mutate(proportion = round(count / sum(count), 3))
print(exp3_errors)

# 5. Plot Error Decomposition Across Set Sizes
ggplot(exp3_errors, aes(x = factor(setsize), y = proportion, fill = response_type)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(
    title = "Experiment 3 Replication: Error Decomposition Across Set Sizes",
    subtitle = "Binding errors rise steeply with load while item errors remain near zero",
    x = "Memory Set Size",
    y = "Proportion of Responses",
    fill = "Response Category"
  )

# 6. Inferential Mixed-Effects Logistic Regression (for fun, basically tells us if results were statistically significant or not)
exp3_model <- glmer(correct ~ factor(setsize) + (1 | ID),
                    data = exp3_clean,
                    family = binomial)
summary(exp3_model)
