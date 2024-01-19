setwd("C:/Users/grace/Desktop/NorthWestern/Incidence rates")


library(readxl)
library(stringr)
library(stringdist)
#library(tmap)
#library(sf)
library(raster)
library(remotes)
library(openxlsx)


year = "2018"

data = read_excel("C:/Users/grace/Desktop/NorthWestern/Incidence rates/hmis_clean.xlsx",sheet="2018")


#data = read_excel("hmis_clean.xlsx",sheet=year)
#metadata = read_excel("guinea_staging.xlsx",sheet="meta")
## only keep admin2 level data
#data = data[data$aggregation_level=="admin_level_2",]


adjusted_testing = function(D,E,F) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  (D)/(D+F)
}
adjusted_testing_bydistrict = sapply(1:dim(data)[1], function (x) 
  adjusted_testing(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x]))
# remove negative values (obvious data errors)
adjusted_testing_bydistrict [adjusted_testing_bydistrict <0] = NA



### mean annual adjusted testing rate by district
mean_adjusted_testing= aggregate(adjusted_testing_bydistrict ~ data$admin_level_2, data, mean)
colnames(mean_adjusted_testing) = c("district","value")

#####crude incidence
crude_incidence_bydistrict = aggregate(confirmed_cases/Population ~ data$admin_level_2, data, mean)
colnames(crude_incidence_bydistrict) = c("district","value")
crude_incidence_bydistrict$value = 12*1000*as.numeric(crude_incidence_bydistrict$value)

crude_incidence_bydistrict

output_file <- "crude2018.xlsx"
write.xlsx(crude_incidence_bydistrict, file = output_file)
cat("Data saved to", output_file, "\n")

##Corrected incidence calculation


### transformation describing adjustment for malaria-attributable proportion

lambdafunction = function(TPR,lambdamin) {
  1+(lambdamin - 1)*TPR
}

### main correction calculations
correct_incidence = function(D, E, F, Population, beta, alpha, standardincidence,lambdamin) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  TPR_nontested = alpha * E/(D+E)
  B = (F-D*(1-beta)/beta) / ((1-beta)/beta+TPR_nontested/(1-TPR_nontested) +1)
  C = TPR_nontested / (1-TPR_nontested) * B
  A = F - B - C
  if (Population == 0 | is.na(Population)) { Population = 1 }
  
  lambda = lambdafunction((C+E)/(B+C+D+E),lambdamin)
  nonmalariafever_incidence = (B+D + (1-lambda)*(C+E))/Population*1000
  
  #   nonmalariafever_incidence = (B+D)/pop*1000
  corrected_incidence = (C+E)/Population*(standardincidence/nonmalariafever_incidence)*1000
  corrected_incidence2 = (C+E)*lambda/Population*(standardincidence/nonmalariafever_incidence)*1000
  
  testing_correction = (C+E)/E
  incidence_correction = standardincidence/nonmalariafever_incidence
  c(corrected_incidence, testing_correction,incidence_correction, corrected_incidence2)
}

###
adjusted_testing = function(D,E,F) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  (D)/(D+F)
}

alpha_estimate = 0.475
beta_estimate =0.589
lambdamin_median = 0.75

corrected_incidence_byadmin_level_2 = sapply(1:dim(data)[1], function (x) 
  correct_incidence(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x], 
                    data$Population[x], beta_estimate, alpha_estimate, 1000,lambdamin_median)[1])

### annual incidence by district
mean_corrected_incidence = aggregate(corrected_incidence_byadmin_level_2 ~ data$admin_level_2, data, mean)
colnames(mean_corrected_incidence) = c("district","value")
mean_corrected_incidence$value = round(mean_corrected_incidence$value)

mean_corrected_incidence

output_file <- "MeanIncidence2018.xlsx"
write.xlsx(mean_corrected_incidence, file = output_file)
cat("Data saved to", output_file, "\n")

##########################2019##############################################################################

year = "2019"

data = read_excel("C:/Users/grace/Desktop/NorthWestern/Incidence rates/hmis_clean.xlsx",sheet="2019")


#data = read_excel("hmis_clean.xlsx",sheet=year)
#metadata = read_excel("guinea_staging.xlsx",sheet="meta")
## only keep admin2 level data
#data = data[data$aggregation_level=="admin_level_2",]


adjusted_testing_bydistrict = sapply(1:dim(data)[1], function (x) 
  adjusted_testing(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x]))
# remove negative values (obvious data errors)
adjusted_testing_bydistrict [adjusted_testing_bydistrict <0] = NA



### mean annual adjusted testing rate by district
mean_adjusted_testing= aggregate(adjusted_testing_bydistrict ~ data$admin_level_2, data, mean)
colnames(mean_adjusted_testing) = c("district","value")

#####crude incidence
crude_incidence_bydistrict = aggregate(confirmed_cases/Population ~ data$admin_level_2, data, mean)
colnames(crude_incidence_bydistrict) = c("district","value")
crude_incidence_bydistrict$value = 12*1000*as.numeric(crude_incidence_bydistrict$value)

crude_incidence_bydistrict

output_file <- "crude2019.xlsx"
write.xlsx(crude_incidence_bydistrict, file = output_file)
cat("Data saved to", output_file, "\n")
##Corrected incidence calculation


### transformation describing adjustment for malaria-attributable proportion

lambdafunction = function(TPR,lambdamin) {
  1+(lambdamin - 1)*TPR
}

### main correction calculations
correct_incidence = function(D, E, F, Population, beta, alpha, standardincidence,lambdamin) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  TPR_nontested = alpha * E/(D+E)
  B = (F-D*(1-beta)/beta) / ((1-beta)/beta+TPR_nontested/(1-TPR_nontested) +1)
  C = TPR_nontested / (1-TPR_nontested) * B
  A = F - B - C
  if (Population == 0 | is.na(Population)) { Population = 1 }
  
  lambda = lambdafunction((C+E)/(B+C+D+E),lambdamin)
  nonmalariafever_incidence = (B+D + (1-lambda)*(C+E))/Population*1000
  
  #   nonmalariafever_incidence = (B+D)/pop*1000
  corrected_incidence = (C+E)/Population*(standardincidence/nonmalariafever_incidence)*1000
  corrected_incidence2 = (C+E)*lambda/Population*(standardincidence/nonmalariafever_incidence)*1000
  
  testing_correction = (C+E)/E
  incidence_correction = standardincidence/nonmalariafever_incidence
  c(corrected_incidence, testing_correction,incidence_correction, corrected_incidence2)
}

###
adjusted_testing = function(D,E,F) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  (D)/(D+F)
}

alpha_estimate = 0.475
beta_estimate = 0.589
lambdamin_median = 0.75

corrected_incidence_byadmin_level_2 = sapply(1:dim(data)[1], function (x) 
  correct_incidence(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x], 
                    data$Population[x], beta_estimate, alpha_estimate, 1000,lambdamin_median)[1])

### annual incidence by district
mean_corrected_incidence = aggregate(corrected_incidence_byadmin_level_2 ~ data$admin_level_2, data, mean)
colnames(mean_corrected_incidence) = c("district","value")
mean_corrected_incidence$value = round(mean_corrected_incidence$value)

mean_corrected_incidence

output_file <- "MeanIncidence2019.xlsx"
write.xlsx(mean_corrected_incidence, file = output_file)
cat("Data saved to", output_file, "\n")


##########################2020##############################################################################

year = "2020"

data = read_excel("C:/Users/grace/Desktop/NorthWestern/Incidence rates/hmis_clean.xlsx",sheet="2020")


#data = read_excel("hmis_clean.xlsx",sheet=year)
#metadata = read_excel("guinea_staging.xlsx",sheet="meta")
## only keep admin2 level data
#data = data[data$aggregation_level=="admin_level_2",]


adjusted_testing_bydistrict = sapply(1:dim(data)[1], function (x) 
  adjusted_testing(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x]))
# remove negative values (obvious data errors)
adjusted_testing_bydistrict [adjusted_testing_bydistrict <0] = NA



### mean annual adjusted testing rate by district
mean_adjusted_testing= aggregate(adjusted_testing_bydistrict ~ data$admin_level_2, data, mean)
colnames(mean_adjusted_testing) = c("district","value")

#####crude incidence
crude_incidence_bydistrict = aggregate(confirmed_cases/Population ~ data$admin_level_2, data, mean)
colnames(crude_incidence_bydistrict) = c("district","value")
crude_incidence_bydistrict$value = 12*1000*as.numeric(crude_incidence_bydistrict$value)

crude_incidence_bydistrict

output_file <- "crude2020.xlsx"
write.xlsx(crude_incidence_bydistrict, file = output_file)
cat("Data saved to", output_file, "\n")



##Corrected incidence calculation


### transformation describing adjustment for malaria-attributable proportion

lambdafunction = function(TPR,lambdamin) {
  1+(lambdamin - 1)*TPR
}

### main correction calculations
correct_incidence = function(D, E, F, Population, beta, alpha, standardincidence,lambdamin) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  TPR_nontested = alpha * E/(D+E)
  B = (F-D*(1-beta)/beta) / ((1-beta)/beta+TPR_nontested/(1-TPR_nontested) +1)
  C = TPR_nontested / (1-TPR_nontested) * B
  A = F - B - C
  if (Population == 0 | is.na(Population)) { Population = 1 }
  
  lambda = lambdafunction((C+E)/(B+C+D+E),lambdamin)
  nonmalariafever_incidence = (B+D + (1-lambda)*(C+E))/Population*1000
  
  #   nonmalariafever_incidence = (B+D)/pop*1000
  corrected_incidence = (C+E)/Population*(standardincidence/nonmalariafever_incidence)*1000
  corrected_incidence2 = (C+E)*lambda/Population*(standardincidence/nonmalariafever_incidence)*1000
  
  testing_correction = (C+E)/E
  incidence_correction = standardincidence/nonmalariafever_incidence
  c(corrected_incidence, testing_correction,incidence_correction, corrected_incidence2)
}

###
adjusted_testing = function(D,E,F) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  (D)/(D+F)
}

alpha_estimate = 0.475
beta_estimate = 0.589
lambdamin_median = 0.75

corrected_incidence_byadmin_level_2 = sapply(1:dim(data)[1], function (x) 
  correct_incidence(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x], 
                    data$Population[x], beta_estimate, alpha_estimate, 1000,lambdamin_median)[1])

### annual incidence by district
mean_corrected_incidence = aggregate(corrected_incidence_byadmin_level_2 ~ data$admin_level_2, data, mean)
colnames(mean_corrected_incidence) = c("district","value")
mean_corrected_incidence$value = round(mean_corrected_incidence$value)

mean_corrected_incidence

output_file <- "MeanIncidence2020.xlsx"
write.xlsx(mean_corrected_incidence, file = output_file)
cat("Data saved to", output_file, "\n")





##########################2021##############################################################################

year = "2021"

data = read_excel("C:/Users/grace/Desktop/NorthWestern/Incidence rates/hmis_clean2.xlsx",sheet="2021")


#data = read_excel("hmis_clean.xlsx",sheet=year)
#metadata = read_excel("guinea_staging.xlsx",sheet="meta")
## only keep admin2 level data
#data = data[data$aggregation_level=="admin_level_2",]

data$tested_cases <- as.numeric(data$tested_cases)
data <- data[!is.na(data$tested_cases), ]

adjusted_testing_bydistrict = sapply(1:dim(data)[1], function (x) 
  adjusted_testing(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x]))
# remove negative values (obvious data errors)
adjusted_testing_bydistrict [adjusted_testing_bydistrict <0] = NA



### mean annual adjusted testing rate by district
mean_adjusted_testing= aggregate(adjusted_testing_bydistrict ~ data$admin_level_2, data, mean)
colnames(mean_adjusted_testing) = c("district","value")

#####crude incidence
crude_incidence_bydistrict = aggregate(confirmed_cases/Population ~ data$admin_level_2, data, mean)
colnames(crude_incidence_bydistrict) = c("district","value")
crude_incidence_bydistrict$value = 12*1000*as.numeric(crude_incidence_bydistrict$value)

crude_incidence_bydistrict

output_file <- "crude2021.xlsx"
write.xlsx(crude_incidence_bydistrict, file = output_file)
cat("Data saved to", output_file, "\n")
##Corrected incidence calculation


### transformation describing adjustment for malaria-attributable proportion

lambdafunction = function(TPR,lambdamin) {
  1+(lambdamin - 1)*TPR
}

### main correction calculations
correct_incidence = function(D, E, F, Population, beta, alpha, standardincidence,lambdamin) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  TPR_nontested = alpha * E/(D+E)
  B = (F-D*(1-beta)/beta) / ((1-beta)/beta+TPR_nontested/(1-TPR_nontested) +1)
  C = TPR_nontested / (1-TPR_nontested) * B
  A = F - B - C
  if (Population == 0 | is.na(Population)) { Population = 1 }
  
  lambda = lambdafunction((C+E)/(B+C+D+E),lambdamin)
  nonmalariafever_incidence = (B+D + (1-lambda)*(C+E))/Population*1000
  
  #   nonmalariafever_incidence = (B+D)/pop*1000
  corrected_incidence = (C+E)/Population*(standardincidence/nonmalariafever_incidence)*1000
  corrected_incidence2 = (C+E)*lambda/Population*(standardincidence/nonmalariafever_incidence)*1000
  
  testing_correction = (C+E)/E
  incidence_correction = standardincidence/nonmalariafever_incidence
  c(corrected_incidence, testing_correction,incidence_correction, corrected_incidence2)
}

###
adjusted_testing = function(D,E,F) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  (D)/(D+F)
}

alpha_estimate = 0.475
beta_estimate = 0.589
lambdamin_median = 0.75

corrected_incidence_byadmin_level_2 = sapply(1:dim(data)[1], function (x) 
  correct_incidence(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x], 
                    data$Population[x], beta_estimate, alpha_estimate, 1000,lambdamin_median)[1])

### annual incidence by district
mean_corrected_incidence = aggregate(corrected_incidence_byadmin_level_2 ~ data$admin_level_2, data, mean)
colnames(mean_corrected_incidence) = c("district","value")
mean_corrected_incidence$value = round(mean_corrected_incidence$value)

mean_corrected_incidence

output_file <- "MeanIncidence2021.xlsx"
write.xlsx(mean_corrected_incidence, file = output_file)
cat("Data saved to", output_file, "\n")


##########################2022##############################################################################

year = "2022"

data = read_excel("C:/Users/grace/Desktop/NorthWestern/Incidence rates/hmis_clean2.xlsx",sheet="2022")


#data = read_excel("hmis_clean.xlsx",sheet=year)
#metadata = read_excel("guinea_staging.xlsx",sheet="meta")
## only keep admin2 level data
#data = data[data$aggregation_level=="admin_level_2",]

data$tested_cases <- as.numeric(data$tested_cases)
##data$suspected_cases <- as.numeric(data$suspected_cases)
data$new_consultation_all_cause <- as.numeric(data$new_consultation_all_cause)



data <- data[!is.na(data$tested_cases), ]
#data <- data[!is.na(data$suspected_cases), ]
#data <- data[!is.na(data$new_consultation_all_cause), ]



adjusted_testing_bydistrict = sapply(1:dim(data)[1], function (x) 
  adjusted_testing(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x]))
# remove negative values (obvious data errors)
adjusted_testing_bydistrict [adjusted_testing_bydistrict <0] = NA



### mean annual adjusted testing rate by district
mean_adjusted_testing= aggregate(adjusted_testing_bydistrict ~ data$admin_level_2, data, mean)
colnames(mean_adjusted_testing) = c("district","value")

#####crude incidence
crude_incidence_bydistrict = aggregate(confirmed_cases/Population ~ data$admin_level_2, data, mean)
colnames(crude_incidence_bydistrict) = c("district","value")
crude_incidence_bydistrict$value = 12*1000*as.numeric(crude_incidence_bydistrict$value)

crude_incidence_bydistrict

output_file1 <- "crude2022.xlsx"
write.xlsx(crude_incidence_bydistrict, file = output_file)
cat("Data saved to", output_file, "\n")
##Corrected incidence calculation


### transformation describing adjustment for malaria-attributable proportion

lambdafunction = function(TPR,lambdamin) {
  1+(lambdamin - 1)*TPR
}

### main correction calculations
correct_incidence = function(D, E, F, Population, beta, alpha, standardincidence,lambdamin) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  TPR_nontested = alpha * E/(D+E)
  B = (F-D*(1-beta)/beta) / ((1-beta)/beta+TPR_nontested/(1-TPR_nontested) +1)
  C = TPR_nontested / (1-TPR_nontested) * B
  A = F - B - C
  if (Population == 0 | is.na(Population)) { Population = 1 }
  
  lambda = lambdafunction((C+E)/(B+C+D+E),lambdamin)
  nonmalariafever_incidence = (B+D + (1-lambda)*(C+E))/Population*1000
  
  #   nonmalariafever_incidence = (B+D)/pop*1000
  corrected_incidence = (C+E)/Population*(standardincidence/nonmalariafever_incidence)*1000
  corrected_incidence2 = (C+E)*lambda/Population*(standardincidence/nonmalariafever_incidence)*1000
  
  testing_correction = (C+E)/E
  incidence_correction = standardincidence/nonmalariafever_incidence
  c(corrected_incidence, testing_correction,incidence_correction, corrected_incidence2)
}

###
adjusted_testing = function(D,E,F) {
  # D is test negatives
  # E is test positives
  # F is non-tested
  (D)/(D+F)
}

alpha_estimate = 0.475
beta_estimate = 0.589
lambdamin_median = 0.75

corrected_incidence_byadmin_level_2 = sapply(1:dim(data)[1], function (x) 
  correct_incidence(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x], 
                    data$Population[x], beta_estimate, alpha_estimate, 1000,lambdamin_median)[1])

### annual incidence by district
mean_corrected_incidence = aggregate(corrected_incidence_byadmin_level_2 ~ data$admin_level_2, data, mean)
colnames(mean_corrected_incidence) = c("district","value")
mean_corrected_incidence$value = round(mean_corrected_incidence$value)

mean_corrected_incidence

output_file <- "MeanIncidence2022.xlsx"
write.xlsx(mean_corrected_incidence, file = output_file)
cat("Data saved to", output_file, "\n")


