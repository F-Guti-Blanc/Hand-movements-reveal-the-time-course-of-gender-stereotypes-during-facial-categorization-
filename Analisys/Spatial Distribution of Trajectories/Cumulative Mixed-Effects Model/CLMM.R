
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



prototypes_by_congruence <- table(mt_data$data$CONDICION,mt_data$prototyping$prototype_label)
prototypes_by_congruence

prototypes_by_congruence / c(table(mt_data$data$CONDICION))



prototypes_by_type_task <- table(mt_data$data$TIPOBLOQUE,mt_data$prototyping$prototype_label)
prototypes_by_type_task

prototypes_by_type_task / c(table(mt_data$data$TIPOBLOQUE))



## 3
# Cumulative Mixed-Effects Model 


# To identify the cumulative mixed-effects model that best explains the distribution of trajectories
# in the 5 prototypes, we followed a process similar to that described above for the mixed-effects models of the MAD indices.


## First. WE determined the structure of the fixed effects. To achieve this, we created various models by 
# altering the structure of the fixed effects, Congruence and Task, while keeping the random effects unaltered. 
# This led to a total of 3 models. 


clm1_FE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) +(1|subject_nr), data = prototype_data)


clm2_FE<-clmm(prototype_label ~ CONDICION + TIPOBLOQUE + (1 |ESTIMULO) +(1|subject_nr), data = prototype_data)


clm3_FE<-clmm(prototype_label ~ CONDICION + (1 |ESTIMULO) +(1 |subject_nr), data = prototype_data)

# We calculated the AIC indices for each model and selected the most fit model. 

aic_clm1_FE<-AIC(clm1_FE)
aic_clm2_FE<-AIC(clm2_FE)
aic_clm3_FE<-AIC(clm3_FE)

anova(clm1_FE,clm2_FE)
anova(clm1_FE,clm3_FE)


# The best fitting model was the model 1 "clm1_FE" 




## Second, we chose the structure of the random effects that best fit our data. 
# To achieve this, we designed all possible models, 25 in total, 
# always keeping the fixed effects of congruence, task and their interaction. 

# all models

clm1_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data)
 
clm2_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data)

clm3_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data)

clm4_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data)

clm5_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1 + CONDICION*TIPOBLOQUE|subject_nr), data = prototype_data)


clm6_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data)

clm7_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data)

clm8_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data)

clm9_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data)

clm10_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1 + CONDICION + TIPOBLOQUE|subject_nr), data = prototype_data)


clm11_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 + TIPOBLOQUE|subject_nr), data = prototype_data)

clm12_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1  + TIPOBLOQUE|subject_nr), data = prototype_data)

clm13_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 + TIPOBLOQUE|subject_nr), data = prototype_data)

clm14_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1 + TIPOBLOQUE|subject_nr), data = prototype_data)

clm15_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1 + TIPOBLOQUE|subject_nr), data = prototype_data)


clm16_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 + CONDICION |subject_nr), data = prototype_data)

clm17_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION |subject_nr), data = prototype_data)

clm19_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 + CONDICION |subject_nr), data = prototype_data)

clm19_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1 + CONDICION |subject_nr), data = prototype_data)

clm20_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1 + CONDICION |subject_nr), data = prototype_data)


clm21_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION*TIPOBLOQUE |ESTIMULO) +(1 |subject_nr), data = prototype_data)

clm22_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION + TIPOBLOQUE |ESTIMULO) +(1 |subject_nr), data = prototype_data)

clm23_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + TIPOBLOQUE |ESTIMULO) +(1 |subject_nr), data = prototype_data)

clm24_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 + CONDICION |ESTIMULO) +(1|subject_nr), data = prototype_data)

clm25_RE<-clmm(prototype_label ~ CONDICION*TIPOBLOQUE + (1 |ESTIMULO) + (1  |subject_nr), data = prototype_data)


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



## Finally, we compared the two resulting models using likelihood ratio tests 
# and Akaike's information criterion, AIC. 

anova(clm1_FE, clm22_RE)

## the model that best explains the distribution of trajectories among the 5 prototypes is model clm22_RE, 
# including Congruence, Task and their interaction as fixed effects 
# and participants, stimuli and the interaction of the latter with Congruence and Task as random effects.



## Cumulative Mixed-Effects Model 

summary(clm22_RE)






## Plots Figure 6

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



