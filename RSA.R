## Hand movements reveal the time course of gender stereotypes during facial categorization  

# Francisco Gutiérrez-Blanco, Ana F. Palenciano and María Ruz
# Department of Experimental Psychology
# Mind, Brain and Behavior Research Center
# University of Granada. Spain


## RSA; Spatio-temporal Development of the Trajectories----

# data_frame----

xpos_long <- mt_export_long(mt_data,use = "tn_trajectories",use_variables = c("steps", "xpos","timestamps"),use2_variables = c("CONDICION", "TIPOBLOQUE", "ESTIMULO","TIPOESTIMULO", "response_time_get_response","subject_nr"))
write.csv(xpos_long, file = "xpos_long.csv", row.names = FALSE)


# We use this data frame for the RSA in matlab. 



### After RSA----

## we import the p-values of the RSA correlations from Matlab and apply 
# the correction by multiple comparisons using the FDR method.


library(readr)
p_values <- read_csv("..../p_values.txt", 
                     col_names = FALSE)

View(p_values)

adjusted_p_values <- p.adjust(p_values, method = 'fdr')


cat("P-Valores Originals:\n")
print(p_values)
cat("\n")

cat("P-Valores Corrected (FDR):\n")
print(adjusted_p_values)

write.table(adjusted_p_values, "adjusted_p_values.txt", col.names = FALSE, row.names = FALSE)



