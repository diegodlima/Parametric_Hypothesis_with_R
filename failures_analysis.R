setwd('C:/FCD/BigDataRAzure/Projetos/Failures/')
getwd()

# Loading the packages
library(readxl)
library(dplyr)
library(ggplot2)
library(data.table)
library(gridExtra)
library(rlang)
library(stringr)

# Loading the file
# Checking the sheets
excel_sheets('failures.xlsm')

# Failures
failures <- read_excel('failures.xlsm', sheet = 'FAILURES', skip = 2)
production <- read_excel('failures.xlsm', sheet = 'PRODUCTION', skip = 2)

# Preparing the dataset
# Checking the type of variables
str(failures)
# Setting failures$MACHINE as categorical
failures$MACHINE <- as.factor(failures$MACHINE)

# Setting production$MACHINE and production$VERSION as categorical
production$MACHINE <- as.factor(production$MACHINE)
# Replacing VERSION NAs with 0
production$VERSION[is.na(production$VERSION)] <- 0
production$VERSION <- as.factor(production$VERSION)
# Setting 0 as BEFORE and 1 as AFTER
levels(production$VERSION) <- c('BEFORE', 'AFTER')

# Standardizing IDs of the failures set
head(production$ID)
head(failures$ID)
# To standardize the ID of failures set, it's only needed to remove the letter from the end of the ID
failures_original <- failures
failures$ID <- gsub('\\D$', '', failures$ID)

# PERFOMANCE ANALYSES
# Considering that the current situation regards to the failures of a production, it's important to consider
# relational metrics intead of absolute: Perfomance instead of Production or Failures.
# The performance metric that will be chosen is an adaptation of MTBF (Mean Tima Between Failures) concept.
# For this project the performance will be measure by: Produced Lenght Between Failures.
# To be able to calculate this performance, it's important to discard failures registers that don't have
# production registers.
# Removing registers from the failures set which don't have production registers.
registers <- as.vector(unlist(production$ID))
failures <- failures %>% filter(ID %in% registers)

# Merging productions and failures data sets by ID
colnames(failures)
production_failures <- merge(production, failures[, c(2, 6:45)], by = 'ID', all = T)
View(production_failures)
colnames(production_failures)
production_failures[, c(29:47)][is.na(production_failures[, c(29:47)])] <- 0
View(production_failures)

# Calculating the performance
production_failures <- production_failures %>%
  mutate(PERFORMANCE = if_else(TOTAL == 0, LENGHT, LENGHT / TOTAL))
View(production_failures)

# Checking the occurrences of each failure before the changes
num_failures_before = production_failures[, c(7, 28:46)] %>% filter(VERSION == 'BEFORE')
num_failures_before[is.na(num_failures_before)] <- 0
num_failures_before <- num_failures_before[, -1]
num_failures_before <- data.frame(cbind(
  FAILURES = as.factor(gsub('_', '', colnames(production_failures[, c(28:46)]))),
  FREQUENCY = apply(num_failures_before, 2, sum)))
num_failures_before <- num_failures_before[order(-num_failures_before$FREQUENCY),]
num_failures_before <- num_failures_before %>% mutate(
  CUM_FAILURES = cumsum(FREQUENCY),
  CUM_PERC = round((CUM_FAILURES / sum(FREQUENCY) * 100), 2)
)

# Plotting the occurrences
ggplot(num_failures_before, aes(x = reorder(FAILURES, -FREQUENCY))) +
  geom_col(aes(y = FREQUENCY), fill = 'lightblue', color = 'darkblue', alpha = 0.7, group = 1) +
  ggtitle('FREQUENCY OF FAILURES BEFORE THE CHANGES') +
  geom_line(aes(y = CUM_PERC*264/100), group = 2, color = 'darkred') +
  geom_point(aes(y = CUM_PERC*264/100), group = 2, shape = 21, color = 'darkred', fill = 'darkred', size = 2) +
  geom_text(aes(y = CUM_PERC*264/100, label = paste(CUM_PERC, '%', sep = '')), vjust = -1, size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~.*100/264, name = 'Cumulative percentage'), limits = c(0, 264)) +
  theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = 'bold'))

# As shown in the plot, 6 failures represent about 80% of the total of failures BEFORE the changes:
# 4, 3, 16, 2, 5 and 17.

# Checking the occurrences of each failure after the changes
num_failures_after = production_failures[, c(7, 28:46)] %>% filter(VERSION == 'AFTER')
num_failures_after[is.na(num_failures_after)] <- 0
num_failures_after <- num_failures_after[, -1]
num_failures_after <- data.frame(cbind(
  FAILURES = as.factor(gsub('_', '', colnames(production_failures[, c(28:46)]))),
  FREQUENCY = apply(num_failures_after, 2, sum)))
num_failures_after <- num_failures_after[order(-num_failures_after$FREQUENCY),]
num_failures_after <- num_failures_after %>% mutate(
  CUM_FAILURES = cumsum(FREQUENCY),
  CUM_PERC = round((CUM_FAILURES / sum(FREQUENCY) * 100), 2)
)

# Plotting the occurrences
ggplot(num_failures_after, aes(x = reorder(FAILURES, -FREQUENCY))) +
  geom_col(aes(y = FREQUENCY), fill = 'lightblue', color = 'darkblue', alpha = 0.7, group = 1) +
  ggtitle('FREQUENCY OF FAILURES AFTER THE CHANGES') +
  geom_line(aes(y = CUM_PERC*61/100), group = 2, color = 'darkred') +
  geom_point(aes(y = CUM_PERC*61/100), group = 2, shape = 21, color = 'darkred', fill = 'darkred', size = 2) +
  geom_text(aes(y = CUM_PERC*61/100, label = paste(CUM_PERC, '%', sep = '')), vjust = -1, size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~.*100/61, name = 'Cumulative percentage'), limits = c(0, 61)) +
  theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = 'bold'))

# After the changes there are again 6 failures which represent about 80% of the total.
# But only the failure 16 from the list of before changes is repeated.
# It shows that probably the changes have contributed to the improvements of that failures.

# Even though only the mean of performance can't be enough to make decisions, it's a simple way to check the evolution
# between before and after modifications

# MEAN OF PERFORMANCE BEFORE AND AFTER
ggplot(distinct(production_failures %>%
           group_by(VERSION) %>%
           mutate(PERFORMANCE = mean(PERFORMANCE)) %>%
           select(VERSION, PERFORMANCE),
         VERSION, .keep_all = T)) +
  geom_col(aes(x = VERSION, y = PERFORMANCE), fill = 'lightblue', color = 'darkblue', alpha = 0.7) +
  ggtitle('MEAN OF PERFORMANCE BEFORE AND AFTER MODIFICATIONS') +
  theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = 'bold'))

# As expected, the mean of performance after modifications is higher than before
# Function to plot the comparisons of performance between before and after
compare_performances <- function(df, variable){
  target <- parse_quosure(variable) #quo
  # Listing the occurrences of variable to be compared
  list_to_compare <- as.vector(df[, variable])
  
  lapply(list_to_compare, function(x){
    before = ggplot(df %>% filter(!!target == x), aes(x = !!target)) +
      geom_col(aes(y = BEFORE), fill = 'lightblue', color = 'darkblue', alpha = 0.7) +
      ggtitle('PERFORMANCE BEFORE CHANGES') +
      geom_text(aes(y = BEFORE, label = round(BEFORE)), vjust = -0.5, size = 3) +
      theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = 'bold')) +
      ylim(0, if_else(max(df$BEFORE) > max(df$AFTER), max(df$BEFORE), max(df$AFTER)))
    
    after = ggplot(df %>% filter(!!target == x), aes(x = !!target)) +
      geom_col(aes(y = AFTER), fill = 'lightblue', color = 'darkblue', alpha = 0.7) +
      ggtitle('PERFORMANCE AFTER CHANGES') +
      geom_text(aes(y = AFTER, label = round(AFTER)), vjust = -0.5, size = 3) +
      theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = 'bold')) +
      ylim(0, if_else(max(df$BEFORE) > max(df$AFTER), max(df$BEFORE), max(df$AFTER)))
    
    grid.arrange(before, after, ncol = 2)
  })
}

# MEAN OF PERFORMANCE BEFORE AND AFTER THE CHANGES BY PRODUCT
product_performance <- dcast(distinct(production_failures %>%
                                        group_by(PRODUCT, VERSION) %>%
                                        mutate(PERFORMANCE = mean(PERFORMANCE)) %>%
                                        select(PRODUCT, VERSION, PERFORMANCE),
                                      PRODUCT, VERSION, PERFORMANCE, .keep_all = T),
                             PRODUCT ~ VERSION)
# Replacing NA values with 0
product_performance[is.na(product_performance)] <- 0
# Calculating the evolution of performance for products that was produced both before and after changes
product_performance <- product_performance %>%
  mutate(DIF_PERFORMANCE = if_else(BEFORE == 0 | AFTER == 0, 0, AFTER - BEFORE),
         STATUS_EVOLUTION = if_else(DIF_PERFORMANCE > 0, 1, -1))
# Plotting the performances
compare_performances(product_performance, 'PRODUCT')

# Having 0 performance, it means that the production don't repeat for both versions.
# Either it stoped the production after changes or it started the production.
# But keeping these products in the dataset is quite important to check the variation of performances.

# Checking the evolution between products
ggplot(product_performance %>% filter(DIF_PERFORMANCE != 0)) +
  geom_col(aes(x = PRODUCT,
               y = DIF_PERFORMANCE,
               fill = factor(STATUS_EVOLUTION, labels = c('Worse', 'Better'))), alpha = 0.5) +
  ggtitle('EVOLUTION OF PERFORMANCE BY PRODUCT') +
  labs(fill = 'Situation') +
  geom_text(aes(x = PRODUCT, y = DIF_PERFORMANCE, label = round(DIF_PERFORMANCE, 2)), vjust = -0.5, size = 3) +
  theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = 'bold'),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))

# Between the products with production before and after changes, only TYPE A got worse.
# List of products which has gotten worse
worse_products <- as.vector(unlist(product_performance %>% filter(DIF_PERFORMANCE < 0) %>% select(PRODUCT)))
# List of products which has improved
best_products <- as.vector(unlist(product_performance %>% filter(DIF_PERFORMANCE > 0) %>% select(PRODUCT)))

# MEAN OF PERFORMANCE BEFORE AND AFTER CHANGEs BY MACHINE
machine_performance <- dcast(distinct(production_failures %>%
                                        group_by(MACHINE, VERSION) %>%
                                        mutate(PERFORMANCE = mean(PERFORMANCE)) %>%
                                        select(MACHINE, VERSION, PERFORMANCE),
                                      MACHINE, VERSION, PERFORMANCE, .keep_all = T),
                             MACHINE ~ VERSION)
# Replacing NA values with 0
machine_performance[is.na(machine_performance)] <- 0
# Calculating the evolution of performance for machines that was produced both before and after changes
machine_performance <- machine_performance %>%
  mutate(DIF_PERFORMANCE = if_else(BEFORE == 0 | AFTER == 0, 0, AFTER - BEFORE),
         STATUS_EVOLUTION = if_else(DIF_PERFORMANCE > 0, 1, -1))
# Plotting the performances
compare_performances(machine_performance, 'MACHINE')

# Having 0 of performance, it means that the production wont repeat for both versions.
# Either it stoped the production after changes or it started the production.
# But keeping these machines in the dataset is quite important to check the variation of performances.

# Checking the evolution between machines
ggplot(machine_performance %>% filter(DIF_PERFORMANCE != 0)) +
  geom_col(aes(x = MACHINE,
               y = DIF_PERFORMANCE,
               fill = factor(STATUS_EVOLUTION, labels = c('Worse', 'Better'))), alpha = 0.5) +
  ggtitle('EVOLUTION OF PERFORMANCE BY MACHINE') +
  labs(fill = 'Situation') +
  geom_text(aes(x = MACHINE, y = DIF_PERFORMANCE, label = round(DIF_PERFORMANCE, 2)), vjust = -0.5, size = 3) +
  theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = 'bold'),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))

# Between the machines with production before and after changes, three of them got worse: BA_13,
# BA_14, CA_06 and DT_12.
# But for the whole department, the most machines improved.
# List of machines which has gotten worse
worse_machines <- as.vector(unlist(machine_performance %>% filter(DIF_PERFORMANCE < 0) %>% select(MACHINE)))
# List of machines which has improved
best_machines <- as.vector(unlist(machine_performance %>% filter(DIF_PERFORMANCE > 0) %>% select(MACHINE)))

# Checking what machines have produced the worse products
demand_by_machine_worse_products <- dcast(distinct(production_failures %>%
           filter(PRODUCT %in% worse_products) %>%
           group_by(PRODUCT, MACHINE, VERSION) %>%
           mutate(PRODUCTION = sum(LENGHT)) %>%
           select(PRODUCT, MACHINE, VERSION, PRODUCTION),
         MACHINE, VERSION, .keep_all = T),
      MACHINE + PRODUCT ~ VERSION)
demand_by_machine_worse_products[is.na(demand_by_machine_worse_products)] <- 0
demand_by_machine_worse_products %>% mutate(DIF_DEMAND = if_else(BEFORE > AFTER, 'REDUCED', 'INCREASED'))

# As checked above, all the machines that produced the worse products have reduced the demand.
# There are at least two basic conclusions:
# 1st: The machines have not to do with the descreasement of the mean of performance
# 2nd: As shorter the products are, as more it's fails
# To test this second one, it's important to check if the demand by machine of the best products have
# increased.
demand_by_machine_best_products <- dcast(distinct(production_failures %>%
                                                    filter(PRODUCT %in% best_products) %>%
                                                    group_by(PRODUCT, MACHINE, VERSION) %>%
                                                    mutate(PRODUCTION = sum(LENGHT)) %>%
                                                    select(PRODUCT, MACHINE, VERSION, PRODUCTION),
                                                  MACHINE, VERSION, .keep_all = T),
                                         MACHINE + PRODUCT ~ VERSION)
demand_by_machine_best_products[is.na(demand_by_machine_best_products)] <- 0
demand_by_machine_best_products <- demand_by_machine_best_products %>%
  mutate(DIF_DEMAND = if_else(BEFORE > AFTER, 'REDUCED', 'INCREASED'))
table(demand_by_machine_best_products$DIF_DEMAND)
# In general, the most cases show that the demand has decreased.
# But to assume it's true, it's important to measure the difference of length mean.
demand_by_machine_best_products <- dcast(distinct(production_failures %>%
                                                    filter(PRODUCT %in% best_products) %>%
                                                    group_by(PRODUCT, MACHINE, ID, VERSION) %>%
                                                    mutate(PRODUCTION = mean(LENGHT)) %>%
                                                    select(PRODUCT, MACHINE, ID, VERSION, PRODUCTION),
                                                  MACHINE, VERSION, .keep_all = T),
                                         MACHINE + ID + PRODUCT ~ VERSION)
demand_by_machine_best_products
# Calculating the difference between length mean before and after changes.
mean(demand_by_machine_best_products$AFTER, na.rm = T)-mean(demand_by_machine_best_products$BEFORE, na.rm = T)

# Even it's shown that the products have incresead the lenght by unit, 62 meters aren't enough to affirm that
# the lenght has impact to the number of failures.
# So, at this point, the first suggestion that the machines have not to do with the performance decreasement
# of the product TYPE A is being considered true, and the second one isn't comproved.

# ANALYSIS OF FREQUENCE OF FAILURES BY SECTIONS OF THE PRODUCT.
# Since the product can be measured by lenght, by analysing the number of failures per secction can bring up
# some insights that could help to understand if the failures occur more at the begin of the proccess, at the
# middle of the proccess or at the end of the proccess.

# Calculating the occurrences by secction of the product
# Considering that the failures ID has a letter at the end, and this letters represent the
# secction of the product in a sequential order (A = begin, B = 2nd part, C = 3rd part, ...)
# it may be used to calculate if the products have a tendence of more failures in a specific
# secction.
failures_by_section <- failures_original
failures_by_section <- failures_by_section %>% mutate(ID = str_extract(ID, '\\w$'))

# Checking unique values
unique(failures_by_section$ID)

# 0 and 1 should not be part of this analysis, because it's due to products that aren't splitted
failures_by_section <- failures_by_section %>% filter(ID != '0' & ID != '1')

# Checking the frequency by secction
table(failures_by_section$ID)

# The secctions between A and D has the most number of occurrences. It regards to the most products
# are splitted into only four secctions.
# To guarantee a better annalysis, it's being considered to discard secctions above D
failures_by_section <- failures_by_section %>% filter(
  ID == 'A' | ID == 'B' | ID == 'C' | ID == 'D'
)

# Counting the occurrence of failures by secction
failures_by_section <- distinct(
  failures_by_section %>%
    group_by(ID) %>%
    mutate(FAILURES = sum(TOTAL)) %>%
    select(ID, FAILURES),
  ID, .keep_all = T)

# Plotting the failures by secction
ggplot(failures_by_section, aes(x = ID)) + 
  geom_col(aes(y = FAILURES), fill = 'lightblue', color = 'darkblue', alpha = 0.7) +
  ggtitle('FAILURES BY SECCTION') +
  geom_text(aes(y = FAILURES, label = FAILURES), vjust = -0.5, size = 3) +
  theme(plot.title = element_text(vjust = 0.5, hjust = 0.5, face = 'bold'))

# As shown in the plot in general the most failures are at the begin of the products.
# At this point it will be considered that it important to check if the setup of the machines
# have to do with this occurrences.

# Checking if the performance has improved with the changes
# To consider if the performance has improved, it's important to select machines that have productions
# before and after the changes.
performance_comparison <- machine_performance %>%
  filter(STATUS_EVOLUTION == 1) %>%
  select(MACHINE, BEFORE, AFTER)

# Testing the normality of the distribution of performance before and after changes
shapiro.test(performance_comparison$BEFORE)
shapiro.test(performance_comparison$AFTER)

# Since the both distribution was tested as normal, and the machines are the same, so the hypothesis
# test that will be reproduced is t test for parametric statistic

# The alternative hypothesis is: (mean of AFTER - mean of BEFORE) > 0
# The confidence level is setted with 0.95
t.test(performance_comparison$AFTER, performance_comparison$BEFORE,
       alternative = 'greater',
       paired = T)

# With a p-value reaching less than 0.05 the alternative hypothesis has been accepted, confirming that the
# changes have made the difference in the process.

# Plotting the results
performance_comparison <- melt(performance_comparison,
     id.vars = c('MACHINE'),
     variable.name = 'VERSION',
     value.name = 'PERFORMANCE',
     variable.factor = T)

ggplot(performance_comparison, aes(x = VERSION, y = PERFORMANCE, fill = VERSION)) +
  geom_boxplot(alpha = 0.5) +
  ggtitle('PERFORMANCE BEFORE AND AFTER CHANGES IN THE PROCESS') +
  stat_summary(fun.y = mean, colour = 'red', geom = 'point', shape = 3, size = 2) +
  theme(plot.title = element_text(vjus = 0.5, hjust = 0.5, face = 'bold'),
        legend.position = 'bottom',
        legend.title = element_text(face = 'bold'))

# FINAL CONCLUSIONS
# 1 - There's a evidence that probably the changes have contributed to reduce the frequency of the
# failures 4, 3, 2, 5 and 17.
# 2 - The failures aren't centered in specific machines.
# 3 - The most failures are concentrated at the beginning of the products.
# 4 - The changes have done difference and contributed to improve the performance.