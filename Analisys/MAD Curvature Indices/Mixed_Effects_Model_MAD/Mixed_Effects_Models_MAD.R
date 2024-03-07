
## Hand movements reveal the time course of gender stereotypes during facial categorization  

# Francisco Gutiérrez-Blanco, Ana F. Palenciano and María Ruz
# Department of Experimental Psychology
# Mind, Brain and Behavior Research Center
# University of Granada. Spain



### Descriptive statistics for MAD Indices and RTs

# data frame
trial_data <- merge(mt_data$data, mt_data$measures, by = "mt_id")




aggregate(MAD ~ CONDICION, data = trial_data, FUN = function(x) c(Mean = mean(x), SD = sd(x), Median = median(x), Q1 = quantile(x, 0.25), Q3 = quantile(x, 0.75), Min = min(x), Max = max(x)))
aggregate(MAD ~ TIPOBLOQUE, data = trial_data, FUN = function(x) c(Mean = mean(x), SD = sd(x), Median = median(x), Q1 = quantile(x, 0.25), Q3 = quantile(x, 0.75), Min = min(x), Max = max(x)))

aggregate(RT ~ CONDICION, data = trial_data, FUN = function(x) c(Mean = mean(x), SD = sd(x), Median = median(x), Q1 = quantile(x, 0.25), Q3 = quantile(x, 0.75), Min = min(x), Max = max(x)))
aggregate(RT ~ TIPOBLOQUE, data = trial_data, FUN = function(x) c(Mean = mean(x), SD = sd(x), Median = median(x), Q1 = quantile(x, 0.25), Q3 = quantile(x, 0.75), Min = min(x), Max = max(x)))



### MIXED EFFETS MODEL with MAD 


# To find the optimal model to fit the MAD variation in our data, 
# we performed two procedures. The first to identify the optimal structure for fixed 
# effects and the second for random effects.


###  First, we designed all possible models considering the structure of fixed effects,
# Congruence (CONDICION), Task type, gender and emotion (TIPOBLOQUE) and their interaction. 
# We set the random effects as stimuli (ESTIMULO) and participants (subjects_nr).

lmer1_FE <-lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1|ESTIMULO) + (1|subject_nr), data = trial_data)
lmer2_FE <- lmer(MAD ~ 1 + CONDICION + TIPOBLOQUE + (1|ESTIMULO) + (1|subject_nr), data = trial_data)
lmer3_FE <- lmer(MAD ~ 1 + CONDICION + (1|ESTIMULO) + (1|subject_nr), data = trial_data)

# To select the best fitting model, the 3 models were gradually compared one by one, 
# based on their complexity, using likelihood ratio tests and Akaike's information criterion, 
# AIC (a lower value indicates a better fit). 

anova(lmer1_FE,lmer2_FE, refit=FALSE)
anova(lmer1_FE,lmer3_FE, refit=FALSE)

# The best fitting model was lmer1_FE




### Second. We performed the same procedure, this time varying the structure of the random effects

# All posibles Models


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

# lmer13_RE, lmer3_RE, lmer15_RE nd lmer5_RE

# To select the best fitting model, the 4 models were gradually compared one by one, 
# based on their complexity, using likelihood ratio tests and Akaike's information criterion, 
# AIC (a lower value indicates a better fit). 

anova(lmer13_RE, lmer3_RE, refit=FALSE)
anova(lmer13_RE, lmer15_RE, refit=FALSE)
anova(lmer13_RE, lmer5_RE, refit=FALSE)

# The best fitting model was lmer13_RE. 

# Theses these results suggest that the model that best explains the 
# variability of the MAD indices in our data considers as fixed effects the Congruence, the Task and its interaction,
# and as random effects the participants, the stimuli and their interaction with the Task.



## MIXED EFFETS MODEL with MAD 

lmer13_RE <- lmer(MAD ~ 1 + CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE|ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = trial_data)
summary(lmer13_RE)



## Plots Figure 5

theme_set(theme_minimal())
custom_colors <- turbo(5)
custom_colors_2 <- custom_colors[c(2, 4)]
custom_colors_1<- custom_colors[c(7, 3)]
custom_colors_3 <- custom_colors[c(1, 4, 5)]
custom_colors_4 <- custom_colors[c(1, 4, 5, 6)]


# A and B

mt_plot_aggregate(mt_data, use="tn_trajectories", x="xpos", y="ypos", color="CONDICION",size = 3, subject_id="subject_nr") + ggplot2::scale_color_manual(name = "", values = custom_colors[c(2, 4)])
mt_plot_aggregate(mt_data, use="tn_trajectories", x="xpos", y="ypos", color="TIPOBLOQUE", size = 3, subject_id="subject_nr") + ggplot2::scale_color_manual(name = "", values = custom_colors[c(7, 3)])

# C and D

summary_stats <- function(x) {
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  data.frame(mean = mean_x, ymin = mean_x - sd_x, ymax = mean_x + sd_x)
}



plot_C <- ggplot(trial_data, aes(x = CONDICION, y = MAD, color = CONDICION)) +
  geom_point(position = position_jitter(width = 0.15), size = 1, alpha = 0.5, aes(color = CONDICION)) +
  geom_violin(fill = NA, draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE, position = position_dodge(0.75), size = 1.5, aes(color = CONDICION)) +
  geom_boxplot(width = 0.05, position = position_dodge(0.75), outlier.shape = NA, color = "black", size = 0.5, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "crossbar", width = 0.1, position = position_dodge(0.75), color = "black", size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +  
  labs(x = "Congruence", y = "MAD") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_C

plot_D <- ggplot(trial_data, aes(x = TIPOBLOQUE, y = MAD, color = TIPOBLOQUE)) +
  geom_point(position = position_jitter(width = 0.15), size = 1, alpha = 0.5, aes(color = TIPOBLOQUE)) +
  geom_violin(fill = NA, draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE, position = position_dodge(0.75), size = 1.5, aes(color = TIPOBLOQUE)) +
  geom_boxplot(width = 0.05, position = position_dodge(0.75), outlier.shape = NA, color = "black", size = 0.5, alpha = 0.5) +
  stat_summary(fun.data = mean_se, geom = "crossbar", width = 0.1, position = position_dodge(0.75), color = "black", size = 0.5, alpha = 0.5) +
  scale_color_manual(values = c(custom_colors_1[1], custom_colors_1[2])) +  
  labs(x = "Task_Type", y = "MAD") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_D





