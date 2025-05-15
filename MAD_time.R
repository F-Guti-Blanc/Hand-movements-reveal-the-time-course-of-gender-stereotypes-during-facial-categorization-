## Hand movements reveal the time course of gender stereotypes during facial categorization  

# Francisco Gutiérrez-Blanco, Ana F. Palenciano and María Ruz
# Department of Experimental Psychology
# Mind, Brain and Behavior Research Center
# University of Granada. Spain



### Descriptive statistics for MAD_time---- 

# data frame

md_time_data <- merge(mt_data$data, mt_data$measures, by = "mt_id")

# Descriptive Analysis

summary_stats <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  data.frame(mean = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x)
}


mean_MAD_time_cong<- md_time_data %>%
  group_by(CONDICION) %>%
  summarise(
    mean_MAD_time = mean(MAD_time, na.rm = TRUE),
    sd_MAD_time = sd(MAD_time, na.rm = TRUE),
    n = n(),
    se_MAD_time = sd_MAD_time / sqrt(n),
    .groups = 'drop'
  )

mean_MAD_time_block<- md_time_data %>%
  group_by(TIPOBLOQUE) %>%
  summarise(
    mean_MAD_time = mean(MAD_time, na.rm = TRUE),
    sd_MAD_time = sd(MAD_time, na.rm = TRUE),
    n = n(),
    se_MAD_time = sd_MAD_time / sqrt(n),
    .groups = 'drop'
  )


mean_MAD_time_int <- md_time_data %>%
  group_by(TIPOBLOQUE, CONDICION) %>%
  summarise(
    mean_MAD_time = mean(MAD_time, na.rm = TRUE),
    sd_MAD_time = sd(MAD_time, na.rm = TRUE),
    n = n(),
    se_MAD_time = sd_MAD_time / sqrt(n),
    .groups = 'drop'
  )


### MIXED EFFETS MODEL with MAD_time 

# We recode the variables of interest to the values 0.5 and -0.5

md_time_data <- md_time_data %>%
  mutate(CONDICION = ifelse(CONDICION == "INCONGRUENTE", 0.5, 
                            ifelse(CONDICION == "CONGRUENTE", -0.5, GENEROESTIMULO)))

md_time_data <- md_time_data %>%
  mutate(TIPOBLOQUE = ifelse(TIPOBLOQUE == "GENERO", -0.5, 
                             ifelse(TIPOBLOQUE == "EMOCION", 0.5, EMOESTIMULO)))

# Selecting the best-fitting model. Fixed effects = CONDICION (Congruency; congruent vs. incongruent), 
# TIPOBLOQUE (block type; gender vs. emotion), and the interaction.
# The structure of the final model was built by testing all possible combinations of random effects and random slopes.

# All possibles Models


lmer1_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = md_time_data)

lmer2_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1 + CONDICION|subject_nr), data = md_time_data)

lmer3_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = md_time_data)

lmer4_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = md_time_data)

lmer5_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1|subject_nr), data = md_time_data)



lmer6_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = md_time_data)

lmer7_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 + CONDICION|subject_nr), data = md_time_data)

lmer8_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = md_time_data)

lmer9_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = md_time_data)

lmer10_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION|ESTIMULO) + (1 |subject_nr), data = md_time_data)


lmer11_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = md_time_data)

lmer12_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + CONDICION|subject_nr), data = md_time_data)

lmer13_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = md_time_data)

lmer14_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = md_time_data)

lmer15_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 |subject_nr), data = md_time_data)


lmer16_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = md_time_data)

lmer17_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 + CONDICION|subject_nr), data = md_time_data)

lmer18_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = md_time_data)

lmer19_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = md_time_data)

lmer20_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE|ESTIMULO) + (1 |subject_nr), data = md_time_data)


lmer21_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = md_time_data)

lmer22_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 + CONDICION|subject_nr), data = md_time_data)

lmer23_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = md_time_data)

lmer24_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE + CONDICION|subject_nr), data = md_time_data)

lmer25_RE <- lmer(MAD_time ~ 1 + CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE|ESTIMULO) + (1 |subject_nr), data = md_time_data)


# Models without fit problems from the most complex to the simplest.

# lmer3_RE, lmer5_RE, lmer2_RE lmer12_RE, lmer15_RE

# To select the best fitting model, using  Akaike's information criterion, 
# AIC (a lower value indicates a better fit). 

AIC(lmer5_RE)
AIC(lmer3_RE)
AIC(lmer2_RE)
AIC(lmer12_RE)
AIC(lmer15_RE)

final_model <- lmer3_RE
summary(final_model)


### Post Hoc Analyses ----

library(emmeans)

posthoc_model <- emmeans(final_model, ~ CONDICION*TIPOBLOQUE, pbkrtest.limit = 5786)

posthoc_results <- contrast(posthoc_model, method = "pairwise", adjust = "bonferroni")

posthoc_results





### Plots Figure 7 ----

theme_set(theme_minimal())
custom_colors <- turbo(5)
custom_colors_2 <- custom_colors[c(2, 4)]
custom_colors_1<- custom_colors[c(7, 3)]
custom_colors_3 <- custom_colors[c(1, 4, 5)]
custom_colors_4 <- custom_colors[c(1, 4, 5, 6)]


library(ggplot2)

plot_interaccion <- ggplot(mean_MAD_time_int, aes(x = TIPOBLOQUE, y = mean_MAD_time, fill = CONDICION)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) + 
  geom_errorbar(aes(ymin = mean_MAD_time - se_MAD_time, ymax = mean_MAD_time + se_MAD_time), 
                width = 0.15, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +  
  labs(x = "Block type", y = "MAD Time", fill = "Condición") +
  coord_cartesian(ylim = c(200, 500)) + 
  theme_minimal() +
  theme(legend.position = "none")






