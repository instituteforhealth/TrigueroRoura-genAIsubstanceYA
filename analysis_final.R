library(tidyverse)
library(here) ## assumes project at root directory
library(janitor)
library(broom)
library(gtsummary)
library(gt)


## theme for plots 
theme_plots <- function(base_size = 20, base_family = "Arial", point_size = 1) {
  # Update defaults for point and line sizes
  #update_geom_defaults("point", list(size = point_size))  # Bigger points
  #update_geom_defaults("line", list(linewidth = point_size/fu2))   # Thicker lines
  
  list(theme_minimal(base_size = base_size, base_family = base_family) +
         theme(
           plot.title = element_text(size = base_size - 4, face = "bold", hjust = 0.5),
           plot.subtitle = element_text(size = base_size - 2, hjust = 0.5),
           axis.title = element_text(size = base_size),
           axis.text = element_text(size = base_size - 6),
           legend.title = element_text(size = base_size - 2, face = "bold"),
           legend.text = element_text(size = base_size - 6),
           strip.text = element_text(size = base_size - 2),
           axis.title.y = element_text(size = 14, margin = margin(r = 10)), # Y-axis title
           panel.background = element_rect(fill = "#FAFAFA",
                                           colour = "white",
                                           size = 1, linetype = "solid"
                                           ),
           panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
           panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                           colour = "white"), 
           panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                           colour = "white"),
           plot.margin = margin(10, 10, 10, 10),
           axis.title.x = element_blank(),
           axis.text.x = element_blank(),
           axis.ticks.x = element_blank()
           #plot.background  = element_rect(fill = "white", color = "black", linewidth = 0.8)
           
         ),
       guides(
         color = guide_legend(override.aes = list(size = 1, linewidth = 1)),  # Smaller legend symbols
         shape = guide_legend(override.aes = list(size = 1))  # If using shape aesthetics
       )
  )
}

## Load data
prolific_demographics <- read_csv("data/prolific_demographic_export_69e6856c5214c202652a22f8.csv") %>%
  clean_names() %>%
  mutate(age_prolific = as.numeric(age),
         student_status = ifelse(student_status %in% c("CONSENT_REVOKED", "DATA_EXPIRED"), NA, student_status), # creates NAs for "Constent revoked" profiles
         employment_status = ifelse(employment_status %in% c("CONSENT_REVOKED", "DATA_EXPIRED"), NA, employment_status),
         race = ifelse(ethnicity_simplified == "CONSENT_REVOKED", NA, ethnicity_simplified),
         sex = ifelse(sex == "CONSENT_REVOKED", NA, sex),
         female = ifelse(sex == "Female", 1, 0)) %>% 
  select(-age) ## we use the age values from our survey 

qualtrics_raw <- read_csv("data/LLMs for substance use_04212026.csv", col_types = c(specific_usage = "c")) %>%
  filter(!is.na(prolific_id)) %>% ## filter out responses from testing stages of survey implementation
  filter(Q_RecaptchaScore >= 0.5) %>% ## filter out potential bots
  filter(Q_TerminateFlag != "Screened") %>% ## remove people who were screened out
  mutate(age = str_replace(age, "\n", ""), ## remove white space at the end of age
         age = ifelse(str_length(age) == 4, 2026 - as.numeric(age), as.numeric(age)), ## some people entered their age by using DOB year
         frequency = case_when(frequency == 1 ~ "Never", 
                               frequency == 2 ~ "Hardly ever",
                               frequency == 3 ~ "At least once a month",
                               frequency == 4 ~ "At least once a week",
                               frequency == 5 ~ "Daily or almost daily",
                               T ~ NA_character_),
         frequency = factor(frequency, 
                         levels = c("Never", "Hardly ever", "At least once a month", "At least once a week", "Daily or almost daily"),
                         ordered = TRUE)
         ) %>%
  separate(specific_usage, into = paste0("usage_", 1:11)) %>% ## separate multiple choice usage into individual columns
  mutate(use_dosage = if_any(starts_with("usage_"), ~ .x == 1),
         use_how_to_use = if_any(starts_with("usage_"), ~ .x == 2),
         use_how_to_obtain = if_any(starts_with("usage_"), ~ .x == 3),
         use_side_effects = if_any(starts_with("usage_"), ~ .x == 4),
         use_risks = if_any(starts_with("usage_"), ~ .x == 5),
         use_problems = if_any(starts_with("usage_"), ~ .x == 6),
         use_interactions = if_any(starts_with("usage_"), ~ .x == 7),
         use_addiction = if_any(starts_with("usage_"), ~ .x == 8),
         use_treatment = if_any(starts_with("usage_"), ~ .x == 9),
         use_cessation = if_any(starts_with("usage_"), ~ .x == 10),
         use_other = if_any(starts_with("usage_"), ~ .x == 11),
         across(starts_with("use_"), as.integer),
         across(starts_with("use_"), ~ replace_na(.x, 0)), ## NAs were created in the recoding above by people not entering that specific use code, so they are natural 0s
         information_on_substance = ifelse(use_how_to_use == 1 | use_dosage == 1 | use_how_to_obtain == 1, 1, 0),
         information_on_consequences = ifelse(use_side_effects == 1 | use_risks == 1 | use_problems == 1 | use_interactions == 1, 1, 0),
         information_on_SUD = ifelse(use_addiction == 1 | use_treatment == 1 | use_cessation == 1, 1, 0),
         information_on_other = use_other,
         age_group = ifelse(age < 24, "age18_23", "age24_29"),
         google_SU = coalesce(google_substance...25, google_substance...19), ## respondents got asked this question through different branches. 
         GenAI_substance = ifelse(is.na(GenAI_substance), 0, GenAI_substance) ## includes 27 people who had never used AI for any purposes
        ) %>%
  select(-starts_with("usage_"))
  
## Join both datasets for analyses:
final_survey <- qualtrics_raw %>%
  left_join(prolific_demographics, by = c("prolific_id" = "participant_id")) 
  

## Check quotas worked as expected:

final_survey %>% 
  count(race) %>%
  mutate(p = n/sum(n))

final_survey %>% 
  count(sex) %>%
  mutate(p = n/sum(n))

final_survey %>%
  group_by(age_group) %>%
  summarize(mean_age = mean(age_prolific, na.rm = T),
            n = n())

## Statistics in the paper

# Have you ever used genAI?

final_survey %>%
  count(GenAI) %>%
  mutate(p = n/sum(n) * 100)

# Have you ever used genAI for SU:

final_survey %>%
  count(GenAI_substance) %>%
  mutate(p = n/sum(n) * 100)

## Google for SU:
final_survey %>%
  count(google_SU) %>%
  mutate(p = n/sum(n) * 100)


final_survey %>% 
  count(GenAI_substance, google_SU) %>% 
  group_by(GenAI_substance) %>% 
  mutate(n/sum(n)*100)

## Frequency of use

frequency_df <- final_survey %>%
  filter(GenAI_substance == 1) %>%
  count(frequency) %>%
  mutate(pct = n / sum(n),
         frequency = paste0(frequency, " (N=", n, ")"),
         frequency = factor(frequency, c("Never (N=2)", "Hardly ever (N=164)", "At least once a month (N=103)", 
                                         "At least once a week (N=39)", "Daily or almost daily (N=13)")),
         pct_text = paste0(round(pct * 100, 1), " %"))

frequency_df

## Types of use:

information_df <- final_survey %>%
  filter(GenAI_substance == 1) %>%
  pivot_longer(cols = starts_with("information_"), 
               names_to = "information", values_to = "information_count") %>%
  group_by( information) %>%
  summarise(information_count = sum(information_count, na.rm = TRUE),
            n = n()) %>%
  mutate(pct = information_count / n,
         information = str_replace_all(str_to_sentence(information), "_", " "),
         information = str_to_sentence(str_remove(information, "Information on ")), 
         information = str_replace(information, "Sud", "SUD"),
         information_n = paste0(information, " (n=", information_count, ")"),
         pct_text = paste0(round(pct * 100, 1), " %"),
         information = factor(information, c("Consequences", "Substance", "SUD", "Other")),
         information_n = factor(information_n, c("Consequences (n=298)", "Substance (n=156)", "SUD (n=112)", "Other (n=14)")))

information_df

## FIGURE 1:
behavior_data <- final_survey %>%
  filter(GenAI_substance == 1) %>%
  pivot_longer(cols = c(drugs_AI_attitudes_1, drugs_AI_attitudes_2, drugs_AI_attitudes_3, Gen_ai_attitudes_1, Gen_ai_attitudes_2, Gen_ai_attitudes_3), 
               names_to = "attitude_specific", values_to = "AI_SU_attitude") %>%
  mutate(attitude_type = case_when(attitude_specific == "drugs_AI_attitudes_1" | attitude_specific == "Gen_ai_attitudes_1" ~ "Trustworthy",
                                       attitude_specific == "drugs_AI_attitudes_2" | attitude_specific == "Gen_ai_attitudes_2" ~ "Reliable",
                                       attitude_specific == "drugs_AI_attitudes_3" | attitude_specific == "Gen_ai_attitudes_3"~ "Helpful",
                                       T ~ NA_character_),
         type_attitude = ifelse(str_detect(attitude_specific, "drugs_AI"), "Substance-related", "General")) 

t_trust <- t.test(behavior_data$AI_SU_attitude[behavior_data$type_attitude == "Substance-related" & behavior_data$attitude_type == "Trustworthy"],
       behavior_data$AI_SU_attitude[behavior_data$type_attitude == "General" & behavior_data$attitude_type == "Trustworthy"], paired = T,  alternative = "two.sided")

t_reliable <- t.test(behavior_data$AI_SU_attitude[behavior_data$type_attitude == "Substance-related" & behavior_data$attitude_type == "Reliable"],
       behavior_data$AI_SU_attitude[behavior_data$type_attitude == "General" & behavior_data$attitude_type == "Reliable"], paired = T, alternative = "two.sided")

t_helpful <- t.test(behavior_data$AI_SU_attitude[behavior_data$type_attitude == "Substance-related" & behavior_data$attitude_type == "Helpful"],
       behavior_data$AI_SU_attitude[behavior_data$type_attitude == "General" & behavior_data$attitude_type == "Helpful"], paired = T, alternative = "two.sided")

behavior_plot <- behavior_data %>%
  group_by(type_attitude, attitude_type) %>%
  summarise(n = n(),
            AI_SU_attitude_mean = mean(AI_SU_attitude, na.rm = TRUE),
            AI_SU_attitude_sd = sd(AI_SU_attitude, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(total_n = sum(n),
         se = AI_SU_attitude_sd / sqrt(total_n),
         ci_lower = AI_SU_attitude_mean - 1.96 * se,
         ci_upper = AI_SU_attitude_mean + 1.96 * se
  )

## all types:

sig_df <- data.frame(
  type_attitude = c("General", "General"),
  y = c(5, 5),
  attitude_type = c("Helpful", "Trustworthy"),
  label = c("***", "***") ## based on the t.tests above
)

fig1 <- behavior_plot %>%
  ggplot(aes(x = type_attitude, y = AI_SU_attitude_mean, color = type_attitude)) +
  geom_point(position = "dodge", stat = "identity", size = 1.5) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    size = 1.5
  ) +
  theme_plots() +
  scale_color_manual(values = c("#A7C7E7", "#E4572E","#A8D5BA", "#F6D365", "#FFF3C4"), name = "Domain") +
  facet_wrap(~ attitude_type) +
  ylim(3, 5) +
  geom_text(data = sig_df, aes(y = y), label = "***",  hjust = -2, color = "black") +
  labs(x = "", y = "Disagree - Agree", title = "When using it for substances or substance use information, generative AI is...")

fig1

ggsave(
  filename = "figures/final_plot_rnr.png",
  plot = fig1,
  width = 16,
  height = 6,
  dpi = 300
)


## Table 1:


tbl1_group <- final_survey %>%
  select(GenAI_substance, race, sex, age_group, student_status) %>%
  mutate(across(everything(), ~replace_na(.x, "Unknown"))) %>%
  tbl_summary(
    by = GenAI_substance,  
    missing = "ifany",
    digits = all_categorical() ~ c(0, 1)
  )

final_survey %>%
  select(race, sex, age_group, student_status) %>%
  mutate(across(everything(), ~replace_na(.x, "Unknown"))) %>%
  map(~ chisq.test(table(.x, final_survey$GenAI_substance)))

tbl1 <- final_survey %>%
  select(race, sex, age_group, student_status) %>%
  mutate(across(everything(), ~replace_na(.x, "Unknown"))) %>%
  tbl_summary(
    missing = "ifany",
    digits = all_categorical() ~ c(0, 1)
  )

tbl1 %>% as_gt %>% gtsave("tables/table1.docx")
tbl1_group %>% as_gt %>% gtsave("tables/table1_group.docx")

## ORs on the GEN_AI outcome

final_survey_regression <- final_survey %>% 
  select(GenAI_substance, race, sex, age_group, student_status, employment_status) %>%
  mutate(across(where(is.character), ~replace_na(.x, "Unknown"))) %>%
  mutate(race = relevel(as.factor(race), ref = "White"))

model1 <- glm(GenAI_substance ~ race + sex + age_group + student_status, family = binomial(link = "logit"), data = final_survey_regression)

summary(model1)
model1 %>% 
  tidy(exponentiate = T, conf.int = T) %>%
  mutate(across(where(is.numeric), ~ round(.x, digits = 2)))

## disaggregated use categories (appendix):

use_type <- final_survey %>%
  filter(GenAI_substance == 1) %>%
  pivot_longer(cols = starts_with("use_"), 
               names_to = "use_type", values_to = "count") %>%
  group_by(use_type) %>%
  summarise(count = sum(count, na.rm = TRUE),
            n = n()) %>%
  mutate(pct = round(count / n * 100, 1))
use_type
