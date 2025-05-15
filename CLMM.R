
## Hand movements reveal the time course of gender stereotypes during facial categorization  

# Francisco Gutiérrez-Blanco, Ana F. Palenciano and María Ruz
# Department of Experimental Psychology
# Mind, Brain and Behavior Research Center
# University of Granada. Spain




## 1
# Check the homogeneity of the trajectories by assessing the 
# bimodality of the distribution of MAD indices. 

mt_check_bimodality(mt_data, use_variables=c("MAD"),grouping_variables="CONDICION")
mt_check_bimodality(mt_data, use_variables=c("MAD"),grouping_variables="TIPOBLOQUE")



## 2
# Descriptive statistics 

# data frame
prototype_data <- merge(mt_data$data, mt_data$prototyping, by = "mt_id")


df_levels <- table(prototype_data$prototype_label)
print(df_levels)

df_levels_ <- prototype_data %>%
  group_by(prototype_label) %>%
  summarise(frecuencia = n()) %>%
  arrange(desc(frecuencia)) 

print(df_levels_)

ggplot(prototype_data, aes(x = prototype_label)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of the trajectories through the prototypes",
       x = "Prototypes",
       y = "Number of trayectories") +
  theme_minimal()

# Bias by de distribution of trajectories:

install.packages("e1071")
library(e1071)

install.packages("moments")
library(moments)

bias_distribution <- skewness(frecuencia_niveles_df$frecuencia)
print(bias_distribution)


## Descriptive statistics ----

### By number of trajectories----

count_congruence<- prototype_data %>%
  group_by(CONDICION, prototype_label) %>%
  count() %>%
  pivot_wider(names_from = CONDICION, values_from = n)

count_block<- prototype_data %>%
  group_by(TIPOBLOQUE, prototype_label) %>%
  count() %>%
  pivot_wider(names_from = TIPOBLOQUE, values_from = n)

count_inter <- prototype_data %>%
  group_by(CONDICION, TIPOBLOQUE, prototype_label) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(names_from = c(CONDICION,TIPOBLOQUE, ), values_from = n)


counts_prototypes <- count_congruence %>%
  full_join(count_block, by = "prototype_label") %>%
  full_join(count_inter, by = "prototype_label")

counts_prototypes <- counts_prototypes %>%
  pivot_longer(cols = -prototype_label, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = prototype_label, values_from = Value)

valores <- c("congruente", "incongruente", "genero", "emocion", 
             "congruente-genero", "congruente-emocion", 
             "incongruente-genero", "incongruente-emocion")


counts_prototypes$Metric <- valores



prototypes_by_congruence <- table(mt_data$data$CONDICION,mt_data$prototyping$prototype_label)
prototypes_by_congruence

prototypes_by_congruence / c(table(mt_data$data$CONDICION))



prototypes_by_type_task <- table(mt_data$data$TIPOBLOQUE,mt_data$prototyping$prototype_label)
prototypes_by_type_task

prototypes_by_type_task / c(table(mt_data$data$TIPOBLOQUE))



# Cumulative Mixed-Effects Model---- 

prototype_data <- prototype_data %>%
  mutate(CONDICION = ifelse(CONDICION == "INCONGRUENTE", 0.5, 
                            ifelse(CONDICION == "CONGRUENTE", -0.5, GENEROESTIMULO)))

prototype_data <- prototype_data %>%
  mutate(TIPOBLOQUE = ifelse(TIPOBLOQUE == "GENERO", -0.5, 
                             ifelse(TIPOBLOQUE == "EMOCION", 0.5, EMOESTIMULO)))


# all models

clm1_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data,link = "cloglog")
 
clm2_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm3_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm4_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm5_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")


clm6_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm7_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm8_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm9_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm10_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")


clm11_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm12_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1  + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm13_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm14_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1 + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")

clm15_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = prototype_data, link = "cloglog")


clm16_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 + CONDICION |subject_nr), data = prototype_data, link = "cloglog")

clm17_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION |subject_nr), data = prototype_data, link = "cloglog")

clm18_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION |subject_nr), data = prototype_data, link = "cloglog")

clm19_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1 + CONDICION |subject_nr), data = prototype_data, link = "cloglog")

clm20_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1 + CONDICION |subject_nr), data = prototype_data, link = "cloglog")


clm21_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 |subject_nr), data = prototype_data, link = "cloglog")

clm22_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 |subject_nr), data = prototype_data, link = "cloglog")

clm23_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 |subject_nr), data = prototype_data, link = "cloglog")

clm24_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1|subject_nr), data = prototype_data, link = "cloglog")

clm25_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1  |subject_nr), data = prototype_data, link = "cloglog")


# After that, we calculated the AICs for each of the models, 
# in order to obtain the model that best fit our data.

aic_clm1_RE<-AIC(clm1_RE)
aic_clm2_RE<-AIC(clm2_RE)
aic_clm3_RE<-AIC(clm3_RE)
aic_clm4_RE<-AIC(clm4_RE)
aic_clm5_RE<-AIC(clm5_RE)
aic_clm6_RE<-AIC(clm6_RE)
aic_clm7_RE<-AIC(clm7_RE)
aic_clm8_RE<-AIC(clm8_RE)
aic_clm9_RE<-AIC(clm9_RE)
aic_clm10_RE<-AIC(clm10_RE)
aic_clm11_RE<-AIC(clm11_RE)
aic_clm12_RE<-AIC(clm12_RE)
aic_clm13_RE<-AIC(clm13_RE)
aic_clm14_RE<-AIC(clm14_RE)
aic_clm15_RE<-AIC(clm15_RE)
aic_clm16_RE<-AIC(clm16_RE)
aic_clm17_RE<-AIC(clm17_RE)
aic_clm18_RE<-AIC(clm18_RE)
aic_clm19_RE<-AIC(clm19_RE)
aic_clm20_RE<-AIC(clm20_RE)
aic_clm21_RE<-AIC(clm21_RE)
aic_clm22_RE<-AIC(clm22_RE)
aic_clm23_RE<-AIC(clm23_RE)
aic_clm24_RE<-AIC(clm24_RE)
aic_clm25_RE<-AIC(clm25_RE)



model_comparison <- data.frame(Model = c("clm1_RE","clm2_RE","clm3_RE","clm4_RE","clm4_RE","clm6_RE",
                                         "clm7_RE","clm8_RE","clm9_RE","clm10_RE","clm11_RE","clm12_RE",
                                         "clm13_RE","clm14_RE","clm5_RE","clm16_RE","clm7_RE","clm18_RE",
                                         "clm19_RE","clm20_RE","clm21_RE","clm22_RE","clm23_RE","clm24_RE","clm25_RE"),
                               AIC = c(aic_clm1_RE, aic_clm2_RE, aic_clm3_RE, aic_clm4_RE, aic_clm5_RE, aic_clm6_RE, 
                                       aic_clm7_RE, aic_clm8_RE, aic_clm9_RE, aic_clm10_RE, aic_clm11_RE, aic_clm12_RE,
                                       aic_clm13_RE, aic_clm14_RE, aic_clm15_RE, aic_clm16_RE, aic_clm17_RE, aic_clm18_RE,
                                       aic_clm19_RE, aic_clm20_RE, aic_clm21_RE, aic_clm22_RE, aic_clm23_RE, aic_clm24_RE, aic_clm25_RE))

best_model <- model_comparison[which.min(model_comparison$AIC), ]

print(best_model)

# The best fitting model was the model 1 "clm22_RE" 


## Cumulative Mixed-Effects Model 

summary(clm22_RE)

# . To ensure that this distribution did not influence our results, we tested the goodness
# of fit between models with a cumulative logit link, and models with a cloglog link (which
# assumes a positive asymmetric distribution). 

clm22_RE_logit<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 |subject_nr), data = prototype_data)
clm22_RE_cloglog <-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 |subject_nr), data = prototype_data, link = "cloglog")

anova(clm22_RE_cloglog,clm22_RE_logit)




### PostHoc Results----

library(emmeans)

posthoc_model <- emmeans(clm22_RE, ~ CONDICION*TIPOBLOQUE, pbkrtest.limit = 5786)

posthoc_results <- contrast(posthoc_model, method = "pairwise", adjust = "bonferroni")

posthoc_results


## Plots Figure 6

theme_set(theme_minimal())
custom_colors <- turbo(5)
custom_colors_2 <- custom_colors[c(2, 4)]
custom_colors_1<- custom_colors[c(7, 3)]
custom_colors_3 <- custom_colors[c(1, 4, 5)]
custom_colors_4 <- custom_colors[c(1, 4, 5, 6)]



mt_data$prototyping <- cbind(mt_data$prototyping, CONDICION = mt_data$data$CONDICION)

mt_plot_congruent <- mt_plot(mt_data, use2 = "prototyping", facet_col = "prototype_label", alpha = 0.05, subset = CONDICION == "CONGRUENTE") 
mt_plot_incongruent <- mt_plot(mt_data, use2 = "prototyping", facet_col = "prototype_label", alpha = 0.05, subset = CONDICION == "INCONGRUENTE") 

mt_plot_congruent
mt_plot_incongruent


mt_data$prototyping <- cbind(mt_data$prototyping, TIPOBLOQUE = mt_data$data$TIPOBLOQUE)

mt_plot_gender <- mt_plot(mt_data, use2 = "prototyping", facet_col = "prototype_label", alpha = 0.05, subset = TIPOBLOQUE == "GENERO")
mt_plot_emotion <- mt_plot(mt_data, use2 = "prototyping", facet_col = "prototype_label", alpha = 0.05, subset = TIPOBLOQUE == "EMOCION")

mt_plot_gender
mt_plot_emotion



### PLOT_ congruence----

plot_cogruence <- counts_prototypes %>% filter(Metric %in% c("congruente", "incongruente"))

plot_cogruence <- plot_cogruence %>%
  pivot_longer(cols = -Metric, names_to = "Prototype", values_to = "Count")

prototype_order <- c("straight", "curved", "cCoM", "dCoM", "dCoM2")

plot_cogruence$Prototype <- factor(plot_cogruence$Prototype, levels = prototype_order)

Plot_Congruence <- ggplot(plot_cogruence, aes(x = Prototype, y = Count, fill = Metric, group = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +  # Aplicar las paletas de colores personalizadas
  labs(title = "Congruence",
       x = "",
       y = "Number of trajectories",
       fill = "Congruence") +
  theme_minimal() +
  theme(legend.position = "none")

Plot_Congruence

### Plot_block----


plot_block <- counts_prototypes %>% filter(Metric %in% c("genero", "emocion"))

plot_block <- plot_block %>%
  pivot_longer(cols = -Metric, names_to = "Prototype", values_to = "Count")

prototype_order <- c("straight", "curved", "cCoM", "dCoM", "dCoM2")

plot_block$Prototype <- factor(plot_block$Prototype, levels = prototype_order)

Plot_Block <- ggplot(plot_block, aes(x = Prototype, y = Count, fill = Metric, group = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(custom_colors_1[1], custom_colors_1[2])) +  # Aplicar las paletas de colores personalizadas
  labs(title = "Block Type",
       x = "",
       y = "",
       fill = "Block") +
  theme_minimal()+
  theme(legend.position = "none")

Plot_Block


### Gender*Congruence----

plot_inter_gen <- counts_prototypes %>% filter(Metric %in% c("congruente-genero", "incongruente-genero"))

plot_inter_gen <- plot_inter_gen %>%
  pivot_longer(cols = -Metric, names_to = "Prototype", values_to = "Count")

prototype_order <- c("straight", "curved", "cCoM", "dCoM", "dCoM2")

plot_inter_gen$Prototype <- factor(plot_inter_gen$Prototype, levels = prototype_order)

Plot_Inter_Gender<- ggplot(plot_inter_gen, aes(x = Prototype, y = Count, fill = Metric, group = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +  # Aplicar las paletas de colores personalizadas
  labs(title = "Gender Block",
       x = "",
       y = "Number of trajectories",
       fill = "Congruence") +
  theme_minimal() +
  theme(legend.position = "none")

Plot_Inter_Gender

### Emotion*Congruence----

plot_inter_emo <- counts_prototypes %>% filter(Metric %in% c("congruente-emocion", "incongruente-emocion"))

plot_inter_emo <- plot_inter_emo %>%
  pivot_longer(cols = -Metric, names_to = "Prototype", values_to = "Count")

prototype_order <- c("straight", "curved", "cCoM", "dCoM", "dCoM2")

plot_inter_emo$Prototype <- factor(plot_inter_emo$Prototype, levels = prototype_order)

Plot_Inter_Emotion<- ggplot(plot_inter_emo, aes(x = Prototype, y = Count, fill = Metric, group = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c(custom_colors_2[1], custom_colors_2[2])) +  # Aplicar las paletas de colores personalizadas
  labs(title = "Emotion Block",
       x = "",
       y = "",
       fill = "Congruence") +
  theme_minimal()+
  theme(legend.position = "none")

Plot_Inter_Emotion


Final_plot <- (Plot_Congruence|Plot_Block)/(Plot_Inter_Gender|Plot_Inter_Emotion)
Final_plot







# Analysis Revision----

### To examine whether movement initiation times vary as a 
# function of trajectory type and congruency levels.

prototype_data_ <- merge(prototype_data, mt_data$measures, by = "mt_id")

anova_model <- aov(initiation_time ~ prototype_label*CONDICION, data = prototype_data_)
summary(anova_model)


## PostHoc Analysis----

library(emmeans)

posthoc_prototype <- emmeans(anova_model, pairwise ~ prototype_label, adjust = "bonferroni")
print(posthoc_prototype)




