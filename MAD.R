
## Hand movements reveal the time course of gender stereotypes during facial categorization  

# Francisco Gutiérrez-Blanco, Ana F. Palenciano and María Ruz
# Department of Experimental Psychology
# Mind, Brain and Behavior Research Center
# University of Granada. Spain



### Descriptive statistics for MAD Indices and RTs

# data frame

trial_data <- merge(mt_data$data, mt_data$measures, by = "mt_id")


### Descriptive Analysis----

summary_stats <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  data.frame(mean = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x)
}


mean_MAD_cong<- trial_data %>%
  group_by(CONDICION) %>%
  summarise(
    mean_MAD = mean(MAD, na.rm = TRUE),
    sd_MAD = sd(MAD, na.rm = TRUE),
    n = n(),
    se_MAD = sd_MAD / sqrt(n),
    .groups = 'drop'
  )

mean_MAD_block<- trial_data %>%
  group_by(TIPOBLOQUE) %>%
  summarise(
    mean_MAD = mean(MAD, na.rm = TRUE),
    sd_MAD = sd(MAD, na.rm = TRUE),
    n = n(),
    se_MAD = sd_MAD / sqrt(n),
    .groups = 'drop'
  )


mean_MAD_int <- trial_data %>%
  group_by(TIPOBLOQUE, CONDICION) %>%
  summarise(
    mean_MAD = mean(MAD, na.rm = TRUE),
    sd_MAD = sd(MAD, na.rm = TRUE),
    n = n(),
    se_MAD = sd_MAD / sqrt(n),
    .groups = 'drop'
  )


### MIXED EFFETS MODEL  

# We recode the variables of interest to the values 0.5 and -0.5

trial_data <- trial_data %>%
  mutate(CONDICION = ifelse(CONDICION == "INCONGRUENTE", 0.5, 
                            ifelse(CONDICION == "CONGRUENTE", -0.5, GENEROESTIMULO)))

trial_data <- trial_data %>%
  mutate(TIPOBLOQUE = ifelse(TIPOBLOQUE == "GENERO", -0.5, 
                             ifelse(TIPOBLOQUE == "EMOCION", 0.5, EMOESTIMULO)))


# Selecting the best-fitting model. Fixed effects = CONDICION (Congruency; congruent vs. incongruent), 
# TIPOBLOQUE (block type; gender vs. emotion), and the interaction.
# The structure of the final model was built by testing all possible combinations of random effects and random slopes.

# All possibles Models

lmer1_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = trial_data)

lmer2_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1 + CONDICION|subject_nr), data = trial_data)

lmer3_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = trial_data)

lmer4_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = trial_data)

lmer5_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1|subject_nr), data = trial_data)



lmer6_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = trial_data)

lmer7_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 + CONDICION|subject_nr), data = trial_data)

lmer8_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = trial_data)

lmer9_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = trial_data)

lmer10_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 |subject_nr), data = trial_data)


lmer11_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = trial_data)

lmer12_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + CONDICION|subject_nr), data = trial_data)

lmer13_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = trial_data)

lmer14_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = trial_data)

lmer15_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 |subject_nr), data = trial_data)


lmer16_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = trial_data)

lmer17_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 + CONDICION|subject_nr), data = trial_data)

lmer18_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = trial_data)

lmer19_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = trial_data)

lmer20_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 |subject_nr), data = trial_data)


lmer21_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = trial_data)

lmer22_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 + CONDICION|subject_nr), data = trial_data)

lmer23_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = trial_data)

lmer24_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = trial_data)

lmer25_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 |subject_nr), data = trial_data)


# Models without fit problems from the most complex to the simplest.

# lmer3_RE, nd lmer5_RE

# To select the best fitting model, the  models were  compared 
# based on their complexity, using likelihood ratio tests and Akaike's information criterion, 
# AIC (a lower value indicates a better fit). 

anova(lmer3_RE, lmer5_RE, refit=FALSE)

# Since we found no significant differences between the fit of both models, 
# we selected the one with a more complex structure.

# The best fitting model was lmer3_RE. 

# Theses these results suggest that the model that best explains the 
# variability of the MAD indices in our data considers as fixed effects the Congruence, the Task and its interaction,
# and as random effects the participants, the stimuli and their interaction with the Task.


## Post Hoc Analysis

library(emmeans)

posthoc_model <- emmeans(lmer3_RE, ~ CONDICION*TIPOBLOQUE, pbkrtest.limit = 5786)

posthoc_results <- contrast(posthoc_model, method = "pairwise", adjust = "bonferroni")

posthoc_results


## Plots Figure 5 

theme_set(theme_minimal())
custom_colors <- turbo(5)
custom_colors_2 <- custom_colors[c(2, 4)]
custom_colors_1<- custom_colors[c(7, 3)]
custom_colors_3 <- custom_colors[c(1, 4, 5)]
custom_colors_4 <- custom_colors[c(1, 4, 5, 6)]


# Plot B

mt_plot_aggregate(mt_data, use="tn_trajectories", x="xpos", y="ypos", color="CONDICION",size = 3, subject_id="subject_nr") + ggplot2::scale_color_manual(name = "", values = custom_colors[c(2, 4)])

# Plot C


mean_MAD_cong<- trial_data %>%
  group_by(CONDICION) %>%
  summarise(
    mean_MAD = mean(MAD, na.rm = TRUE),
    sd_MAD = sd(MAD, na.rm = TRUE),
    n = n(),
    se_MAD = sd_MAD / sqrt(n),
    .groups = 'drop'
  )

mean_MAD_block<- trial_data %>%
  group_by(TIPOBLOQUE) %>%
  summarise(
    mean_MAD = mean(MAD, na.rm = TRUE),
    sd_MAD = sd(MAD, na.rm = TRUE),
    n = n(),
    se_MAD = sd_MAD / sqrt(n),
    .groups = 'drop'
  )

mean_MAD_int<- trial_data %>%
  group_by(TIPOBLOQUE,CONDICION) %>%
  summarise(
    mean_MAD = mean(MAD, na.rm = TRUE),
    sd_MAD = sd(MAD, na.rm = TRUE),
    n = n(),
    se_MAD = sd_MAD / sqrt(n),
    .groups = 'drop'
  )


plot_cong <- ggplot(mean_MAD_cong, aes(x = CONDICION, y = mean_MAD, fill = CONDICION)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.4), width = 0.5) + 
  geom_errorbar(aes(ymin = mean_MAD - se_MAD, ymax = mean_MAD + se_MAD), 
                width = 0.15, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +  
  labs(x = "Congruence", y = "mean MAD", fill = "Congruence") +
  coord_cartesian(ylim = c(NA, NA)) + 
  theme_minimal() +
  theme(legend.position = "none")

plot_interaccion <- ggplot(mean_MAD_int, aes(x = TIPOBLOQUE, y = mean_MAD, fill = CONDICION)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) + 
  geom_errorbar(aes(ymin = mean_MAD - se_MAD, ymax = mean_MAD + se_MAD), 
                width = 0.15, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +  
  labs(x = "Block type", y = "mean MAD", fill = "Condición") +
  coord_cartesian(ylim = c(NA, NA)) + 
  theme_minimal() +
  theme(legend.position = "right")



# Plot A

summary_stats <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  data.frame(mean = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x)
}



plot_A_ <- ggplot(trial_data, aes(x = CONDICION, y = MAD, color = CONDICION)) +
  geom_point(position = position_jitter(width = 0.15), size = 1, alpha = 0.5, aes(color = CONDICION)) +
  geom_boxplot(width = 0.05, position = position_dodge(0.75), outlier.shape = NA, color = "black", size = 0.5, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "crossbar", width = 0.1, position = position_dodge(0.75), color = "black", size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +  
  labs(x = "Congruence", y = "MAD values") +
  theme_minimal() +
  theme(legend.position = "none")

plot_A_

plot_A <- ggplot(trial_data, aes(x = TIPOBLOQUE, y = MAD, color = CONDICION)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.75), 
             size = 1, alpha = 0.5) +
  geom_boxplot(aes(fill = CONDICION), width = 0.1, position = position_dodge(0.75), 
               outlier.shape = NA, color = "black", size = 0.5, alpha = 0.4) +
  stat_summary(aes(group = interaction(TIPOBLOQUE, CONDICION)), 
               fun.data = mean_se, geom = "crossbar", 
               width = 0.2, position = position_dodge(0.75), 
               color = "black", size = 0.3, alpha = 0.7) +
  scale_color_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +
  scale_fill_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +
  labs(x = "Task type", y = "MAD values", color = "Condition", fill = "Condition") +
  theme_minimal() +
  theme(legend.position = "none")

plot_A



### Analisis Revision----

# At the suggestion of the reviewers, a new linear mixed model was implemented to test whether 
# the sex of the faces could modulate the congruency effect.

trial_data <- trial_data %>%
  mutate(GENEROESTIMULO = ifelse(GENEROESTIMULO == "MUJER", 0.5, 
                                 ifelse(GENEROESTIMULO == "HOMBRE", -0.5, GENEROESTIMULO)))


lmer_Cong_Sex <- lmer(MAD ~ 1 + CONDICION*GENEROESTIMULO + (1|ESTIMULO) + (1|subject_nr), data = trial_data)

summary(lmer_Cong_Sex)



