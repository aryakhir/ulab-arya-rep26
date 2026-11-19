# experiment 1
library(tidyverse)
library(lme4) # don't actually need this here but whatever

# load files from data subfolder
exp1 <- read.table("data/Pairs6_full.dat", header = FALSE)
colnames(exp1) <- c("ID", "block", "trial", "setsize","cue", "testorder1", "testorder2", "testorder3","testorder4","oldage1", "oldage2", "oldage3", "oldage4","rcat1", "rcat2", "rcat3", "rcat4","RT1","RT2","RT3","RT4")
glimpse(exp1)

# Total participants and trial count
exp1 %>%
  summarize(
    total_participants = n_distinct(ID),
    total_trials = n()
  )

# Breakdown of trials by set size
table(exp1$setsize)

exp1 %>%
  group_by(setsize) %>%
  summarize(
    n_trials = n(),
    accuracy = mean(rcat1 == 1, na.rm = TRUE)
  )
error_table <- exp1 %>%
  group_by(setsize, rcat1) %>%
  summarize(count = n(), .groups = "drop_last") %>%
  mutate(proportion = round(count/sum(count), 3))
print(error_table)

# Convert response category numbers to meaningful labels
error_plot_data <- exp1 %>%
  mutate(response_label = factor(rcat1, 
                                 levels = 1:4, 
                                 labels = c("Target (Correct)", 
                                            "Same-Trial Lure (Binding Error)", 
                                            "Old Lure (PI Intrusion)", 
                                            "New Lure (Item Error)"))) %>%
  group_by(setsize, response_label) %>%
  summarize(count = n(), .groups = "drop_last") %>%
  mutate(proportion = count / sum(count))

# Plot error decomposition
ggplot(error_plot_data, aes(x = factor(setsize), y = proportion, fill = response_label)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  labs(
    title = "Replication of Experiment 1: Error Decomposition",
    x = "Memory Set Size",
    y = "Proportion of Choices",
    fill = "Response Type"
  )
