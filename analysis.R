# ============================================================================
# Empirical Seminar – Data Cleaning and Analysis
# Author: Christina Knes, 11902902
# ============================================================================

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------
setwd("C:/Users/knesc/OneDrive/Documents/Uni/Master/2. Semester/ES/Allgemeines")

library(readxl)
library(tidyverse)
library(lubridate)
library(rstatix)
library(car)
library(showtext)
library(gt)
library(corrplot)

font_add_google("Press Start 2P", "pressstart")
showtext_auto()

# ----------------------------------------------------------------------------
# LOAD & CLEAN DATA
# ----------------------------------------------------------------------------

merged_data <- read_excel("merged_data.xlsx")

clean_data <- merged_data %>%
  mutate(
    ParsedDate = parse_date_time(`Datum letzte Aktivität.x`, orders = "ymd HMS"),
    Datum = as.Date(ParsedDate)
  ) %>%
  filter(!is.na(Datum),
         Datum > as.Date("2025-04-23"),
         Datum < as.Date("2025-06-05"),
         ID != 11,
         LSGroup %in% c("B1", "B2")) %>%
  distinct(ID, .keep_all = TRUE)

# ----------------------------------------------------------------------------
# SCORE ACCURACY CALCULATION
# ----------------------------------------------------------------------------

accuracy_cols <- grep("L1 T[1-9]?[0-9] accuracy", names(clean_data), value = TRUE)

clean_data[accuracy_cols] <- lapply(clean_data[accuracy_cols], function(x) {
  x <- as.numeric(as.character(x))
  x[x == -1] <- NA
  return(x)
})

clean_data <- clean_data %>%
  rowwise() %>%
  mutate(score_accuracy = mean(c_across(all_of(accuracy_cols)), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    rounds_played = Games,
    time_spent = as.numeric(`Gesamtzeit.x`) / 60
  )

clean_data$LSGroup <- factor(clean_data$LSGroup, levels = c("B1", "B2"))

# ----------------------------------------------------------------------------
# ATTACHMENT SCORE (PAQ)
# ----------------------------------------------------------------------------

paq_cols <- c(
  "Die folgenden Aussagen beziehen sich darauf, wie Sie die Beziehung zu (Haus-)Tieren empfinden. [Die Nähe zu Tieren ist für mich angenehm.]",
  "Die folgenden Aussagen beziehen sich darauf, wie Sie die Beziehung zu (Haus-)Tieren empfinden. [Zeichen der Zuneigung von Tieren stärken mein Selbstwertgefühl.]",
  "Die folgenden Aussagen beziehen sich darauf, wie Sie die Beziehung zu (Haus-)Tieren empfinden. [Ich interessiere mich nicht wirklich für Tiere.]",
  "Die folgenden Aussagen beziehen sich darauf, wie Sie die Beziehung zu (Haus-)Tieren empfinden. [Ich werde traurig, wenn Tiere nicht so nah bei mir sein möchten, wie ich es mir wünsche.]",
  "Die folgenden Aussagen beziehen sich darauf, wie Sie die Beziehung zu (Haus-)Tieren empfinden. [Ich mache mir Sorgen, mit einem Tier allein gelassen zu werden.]",
  "Die folgenden Aussagen beziehen sich darauf, wie Sie die Beziehung zu (Haus-)Tieren empfinden. [Oft sind Tiere eine Plage für mich.]"
)

merged_data[paq_cols] <- lapply(merged_data[paq_cols], function(col) {
  col <- trimws(as.character(col))
  case_when(
    str_detect(col, regex("^Stimme gar nicht zu\\.$", ignore_case = TRUE)) ~ 1,
    str_detect(col, regex("^Stimme (nicht|eher nicht) zu\\.$", ignore_case = TRUE)) ~ 2,
    str_detect(col, regex("^Stimme weder zu noch nicht zu\\.$", ignore_case = TRUE)) ~ 3,
    str_detect(col, regex("^Stimme (eher )?zu\\.$", ignore_case = TRUE)) ~ 4,
    str_detect(col, regex("^Stimme völlig zu\\.$", ignore_case = TRUE)) ~ 5,
    TRUE ~ NA_real_
  )
})

merged_data <- merged_data %>%
  rowwise() %>%
  mutate(attachment_score = mean(c_across(all_of(paq_cols)), na.rm = TRUE)) %>%
  ungroup()

clean_data <- clean_data %>%
  left_join(merged_data %>% select(UID, attachment_score), by = "UID")

# ----------------------------------------------------------------------------
# STATISTICAL TESTS
# ----------------------------------------------------------------------------

## T-Tests
t_test_accuracy <- t.test(score_accuracy ~ LSGroup, data = clean_data, var.equal = TRUE)
t_test_time <- t.test(time_spent ~ LSGroup, data = clean_data)

## ANOVA
anova_result <- aov(score_accuracy ~ LSGroup, data = clean_data)

## Assumptions
shapiro.test(clean_data$score_accuracy)
leveneTest(score_accuracy ~ LSGroup, data = clean_data)

## Mann-Whitney U Tests for each trial
trial_cols <- paste0("Score L", 2:5)
mann_whitney_results <- lapply(trial_cols, function(trial) {
  data_trial <- clean_data %>% select(LSGroup, all_of(trial)) %>% filter(!is.na(.[[trial]]))
  test <- wilcox.test(data_trial[[trial]] ~ data_trial$LSGroup)
  tibble(Trial = trial, W_statistic = test$statistic, p_value = test$p.value)
})
mann_whitney_results_df <- bind_rows(mann_whitney_results)

## Correlations (Pooled)
cor_data_clean <- clean_data %>%
  filter(!is.na(attachment_score), !is.na(score_accuracy), score_accuracy <= 100)
cor_pearson <- cor.test(cor_data_clean$attachment_score, cor_data_clean$score_accuracy, method = "pearson")
cor_spearman <- cor.test(cor_data_clean$attachment_score, cor_data_clean$score_accuracy, method = "spearman")

## Correlations by Group
group_b1 <- clean_data %>% filter(LSGroup == "B1")
group_b2 <- clean_data %>% filter(LSGroup == "B2")
cor_b1_pearson <- cor.test(group_b1$attachment_score, group_b1$Score, method = "pearson")
cor_b1_spearman <- cor.test(group_b1$attachment_score, group_b1$Score, method = "spearman")
cor_b2_pearson <- cor.test(group_b2$attachment_score, group_b2$Score, method = "pearson")
cor_b2_spearman <- cor.test(group_b2$attachment_score, group_b2$Score, method = "spearman")

## Correlation by Trial
trial_long <- clean_data %>%
  select(ID, LSGroup, attachment_score, all_of(trial_cols)) %>%
  pivot_longer(cols = all_of(trial_cols), names_to = "Trial", values_to = "Score") %>%
  mutate(Trial = factor(Trial, levels = trial_cols))

results_by_trial <- trial_long %>%
  group_by(Trial) %>%
  summarise(
    pearson_cor = cor.test(attachment_score, Score, method = "pearson")$estimate,
    pearson_p = cor.test(attachment_score, Score, method = "pearson")$p.value,
    spearman_cor = cor.test(attachment_score, Score, method = "spearman")$estimate,
    spearman_p = cor.test(attachment_score, Score, method = "spearman")$p.value
  )

# ----------------------------------------------------------------------------
# VISUALIZATIONS 
# ----------------------------------------------------------------------------

## Boxplot: Score Accuracy by Group
ggplot(clean_data, aes(x = LSGroup, y = score_accuracy, fill = LSGroup)) +
  geom_boxplot(alpha = 0.85, outlier.shape = 21, outlier.size = 3, color = "gray40") +
  scale_fill_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f")) +
  theme_minimal(base_family = "pressstart") +
  theme(
    plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, color = "darkgreen"),
    axis.text = element_text(size = 12, color = "darkgreen")
  ) +
  labs(title = "Score Accuracy by Group", x = "Game Condition", y = "Score Accuracy (%)")

## Descriptive Statistics Table
desc_stats <- clean_data %>%
  select(LSGroup, score_accuracy, time_spent, rounds_played, attachment_score) %>%
  group_by(LSGroup) %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                      sd = ~sd(.x, na.rm = TRUE),
                                      n = ~sum(!is.na(.x))))) %>%
  gt() %>%
  tab_header(title = "Descriptive Statistics by Group")
print(desc_stats)

## Heatmap: Correlation Matrix
cor_mat <- clean_data %>%
  select(score_accuracy, time_spent, rounds_played, attachment_score) %>%
  na.omit() %>%
  cor()
corrplot(cor_mat, method = "color", addCoef.col = "black",
         tl.col = "black", tl.cex = 0.9, number.cex = 0.8,
         col = colorRampPalette(c("#0072B2", "#56B4E9", "#009E73", "#66CC99"))(20))

## Violin: Time Spent by Group
ggplot(clean_data, aes(x = LSGroup, y = time_spent, fill = LSGroup)) +
  geom_violin(alpha = 0.7) +
  scale_fill_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                    labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  scale_color_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                     labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  theme_minimal(base_family = "pressstart") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, color = "darkgreen"),
    axis.text = element_text(size = 12, color = "darkgreen")
  ) +
  labs(title = "Time Spent by Group", x = "Game Condition", y = "Time Spent (minutes)")

## Boxplot: Attachment Score Distribution
ggplot(clean_data, aes(x = LSGroup, y = attachment_score, fill = LSGroup)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                    labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  scale_color_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                     labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  theme_minimal(base_family = "pressstart") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, color = "darkgreen"),
    axis.text = element_text(size = 12, color = "darkgreen")
  ) +
  labs(title = "Attachment Score by Group", x = "Game Condition", y = "Attachment Score")

## Line Plot: Learning Progress Over Trials
ggplot(trial_long, aes(x = Trial, y = Score, group = LSGroup, color = LSGroup)) +
  stat_summary(fun = mean, geom = "line", size = 2) +  # Increased line size
  stat_summary(fun = mean, geom = "point", size = 4, shape = 21, fill = "white") +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2, color = "black") +
  scale_fill_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                    labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  scale_color_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                     labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  theme_minimal(base_family = "pressstart") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, color = "darkgreen"),
    axis.text = element_text(size = 12, color = "darkgreen")
  ) +
  labs(title = "Learning Progress Over Trials by Group", x = "Trial", y = "Mean Score", color = "Game Condition",
       fill = "Game Condition")

## Faceted Scatter: Attachment vs. Trial Score by Group
trial_long <- clean_data %>%
  select(ID, LSGroup, attachment_score, `Score L2`, `Score L3`, `Score L4`, `Score L5`) %>%
  pivot_longer(cols = starts_with("Score L"), names_to = "Trial", values_to = "Score")

ggplot(trial_long, aes(x = attachment_score, y = Score, color = LSGroup, fill = LSGroup)) +
  geom_point(alpha = 0.5, size = 4, position = position_jitter(width = 0.02)) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2, aes(fill = LSGroup), alpha = 0.3) +
  facet_wrap(~Trial, ncol = 2) +
  scale_fill_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                    labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  scale_color_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                     labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  theme_minimal(base_family = "pressstart") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, color = "darkgreen"),
    axis.text = element_text(size = 12, color = "darkgreen")
  ) +
  labs(
    title = "Attachment Score vs Game Scores by Group",
    x = "Attachment Score (PAQ mean)",
    y = "Trial Score",
    color = "Game Condition",
    fill = "Game Condition"
  )

## Highlighted Plot: Attachment vs. L2 & L5 by Group
selected_trials <- c("Score L2", "Score L5")

trial_long_filtered <- clean_data %>%
  select(ID, LSGroup, attachment_score, all_of(selected_trials)) %>%
  pivot_longer(cols = all_of(selected_trials), names_to = "Trial", values_to = "Score")

ggplot(trial_long_filtered, aes(x = attachment_score, y = Score, color = LSGroup, fill = LSGroup)) +
  geom_point(alpha = 0.5, size = 4, position = position_jitter(width = 0.02)) +
  geom_smooth(method = "lm", se = TRUE, size = 1.2, aes(fill = LSGroup), alpha = 0.3) +
  facet_wrap(~Trial, ncol = 2) +
  scale_fill_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                    labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  scale_color_manual(values = c("B1" = "#3aa9ff", "B2" = "#4a7a3f"),
                     labels = c("B1" = "B1 (Non-Narrative Group)", "B2" = "B2 (Narrative Group)")) +
  theme_minimal(base_family = "pressstart") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    plot.title = element_text(size = 14, color = "darkgreen", face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, color = "darkgreen"),
    axis.text = element_text(size = 12, color = "darkgreen")
  ) +
  labs(
    title = "Attachment Score vs Game Scores by Group",
    x = "Attachment Score (PAQ mean)",
    y = "Trial Score",
    color = "Game Condition",
    fill = "Game Condition"
  )