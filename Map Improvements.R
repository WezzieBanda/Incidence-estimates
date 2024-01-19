setwd("C:/Users/grace/Desktop/NorthWestern/Incidence rates")

library(readxl)
library(stringr)
library(stringdist)
library(tmap)
library(sf)
library(raster)
library(remotes)
library(openxlsx)
library(geodata)
library(sp)
library(RColorBrewer)

display.brewer.pal(n = 10, name = 'RdYlBu')# couleur 

brewer.pal(n = 10, name = "RdYlBu")



year = "2022"

data = read_excel("C:/Users/grace/Desktop/NorthWestern/Incidence rates/hmis_clean.xlsx",sheet="2022")

data$tested_cases <- as.numeric(data$tested_cases)
data <- data[!is.na(data$tested_cases), ]

data$tested_cases <- as.numeric(data$tested_cases)
##data$suspected_cases <- as.numeric(data$suspected_cases)
data$new_consultation_all_cause <- as.numeric(data$new_consultation_all_cause)

#data = read_excel("hmis_clean.xlsx",sheet=year)
#metadata = read_excel("guinea_staging.xlsx",sheet="meta")
## only keep admin2 level data
#data = data[data$aggregation_level=="admin_level_2",]


#adjusted_testing = function(D,E,F) {
#  # D is test negatives
  # E is test positives
  # F is non-tested
 # (D)/(D+F)
#}
#adjusted_testing_bydistrict = sapply(1:dim(data)[1], function (x) 
 # adjusted_testing(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x]))
# remove negative values (obvious data errors)
#adjusted_testing_bydistrict [adjusted_testing_bydistrict <0] = NA



### mean annual adjusted testing rate by district
mean_adjusted_testing= aggregate(adjusted_testing_bydistrict ~ data$admin_level_2, data, mean)
colnames(mean_adjusted_testing) = c("district","value")


#####crude incidence
crude_incidence_bydistrict = aggregate(confirmed_cases/Population ~ data$admin_level_2, data, mean)
colnames(crude_incidence_bydistrict) = c("district","value")
crude_incidence_bydistrict$value = 12*1000*as.numeric(crude_incidence_bydistrict$value)

crude_incidence_bydistrict

## map
guinea_adm2 = getData('GADM', country='GIN', level=2)
guinea_adm3 = getData('GADM', country='GIN', level=3)

guinea_prefectures = guinea_adm2[guinea_adm2$NAME_1 != "Conakry",]
guinea_conakry = guinea_adm3[guinea_adm3$NAME_1 == "Conakry",]

###
### append incidence
guinea_prefectures$CrudeIncidence = crude_incidence_bydistrict$value[amatch(guinea_prefectures$NAME_2,crude_incidence_bydistrict$district,maxDist=10)]
guinea_conakry$CrudeIncidence = crude_incidence_bydistrict$value[amatch(guinea_conakry$NAME_3,crude_incidence_bydistrict$district,maxDist=10)]



### incidence 
#scaleinterval =100
incidencebreaks = seq(from = min(crude_incidence_bydistrict$value)%/%scaleinterval*scaleinterval, to = max((crude_incidence_bydistrict$value)%/%scaleinterval+1)*scaleinterval,by=scaleinterval)



# Create a thematic map for Guinea prefectures

tmap_mode("plot")
tm_shape(guinea_prefectures)+tm_borders(col="black", lwd = 2)+tm_polygons(title = "Incidence\n per 1000", col = "CrudeIncidence",breaks=c( 0, 100, 200, 300, 500, 1000, 1500, 2000),
                                                                          palette = c("#313695", "#4575B4","#74ADD1", "#FFFFBF",  "#FDAE61","#F46D43" , "#D73027","#A50026" ),id="NAME_2")+tm_layout(title.position = c("centre", "bottom"), title="2021", legend.outside = FALSE,legend.position = c("left","bottom"),legend.title.size = 1.5, legend.height=0.5, legend.text.size = 1.5,frame = FALSE, legend.show=FALSE)+
  tm_shape(guinea_conakry )+tm_borders(col="black", lwd = 2)+tm_polygons(col = "CrudeIncidence",breaks=incidencebreaks,id="NAME_3",legend.show=FALSE) 

                                                                                                #"#FF8C00" "#FFD700" "#FFA500" "#DC143C" "#8B0000" "#FFC0CB""#FF0000"

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
beta_estimate =0.65
lambdamin_median = 0.75


corrected_incidence_byadmin_level_2 = sapply(1:dim(data)[1], function (x) 
  correct_incidence(data$tested_cases[x] - data$confirmed_cases[x], data$confirmed_cases[x], data$new_consultation_all_cause[x] - data$tested_cases[x], 
                    data$Population[x], beta_estimate, alpha_estimate, 1000,lambdamin_median)[1])


### annual incidence by district
mean_corrected_incidence = aggregate(corrected_incidence_byadmin_level_2 ~ data$admin_level_2, data, mean)
colnames(mean_corrected_incidence) = c("district","value")
mean_corrected_incidence$value = round(mean_corrected_incidence$value)

mean_corrected_incidence


### append incidence
guinea_prefectures$CorrectedIncidence = mean_corrected_incidence$value[amatch(guinea_prefectures$NAME_2,mean_corrected_incidence$district,maxDist=10)]
guinea_conakry$CorrectedIncidence = mean_corrected_incidence$value[amatch(guinea_conakry$NAME_3,mean_corrected_incidence$district,maxDist=10)]

### incidence 
#scaleinterval =100
incidencebreaks = seq(from = min(mean_corrected_incidence$value)%/%scaleinterval*scaleinterval, to = max((mean_corrected_incidence$value)%/%scaleinterval+1)*scaleinterval,by=scaleinterval)

tmap_mode("plot")
tm_shape(guinea_prefectures)+tm_borders(col="black", lwd = 2)+tm_polygons(title = "Incidence\n per 1000", col = "CorrectedIncidence",breaks=c(0, 100, 200, 300, 500, 1000, 1500, 2000),
                                                                          palette = c( "#313695", "#4575B4","#74ADD1", "#FFFFBF",  "#FDAE61","#F46D43" , "#D73027","#A50026"),id="NAME_2")+tm_layout(title.position = c("centre", "bottom"), title="2022",legend.outside = FALSE,legend.position = c("left","bottom"), legend.height=0.5,legend.title.size = 1.5, legend.text.size = 1.5,frame = FALSE, legend.show=FALSE)+
  tm_shape(guinea_conakry )+tm_borders(col="black", lwd = 2)+tm_polygons(col = "CorrectedIncidence",breaks=incidencebreaks,id="NAME_3",legend.show=FALSE ) 

 #                                                                                 "#A50026" "#D73027" "#F46D43" "#FDAE61" "#FEE090" "#FFFFBF" "#E0F3F8" "#ABD9E9" "#74ADD1" "#4575B4" "#313695" # les couleurs disponible
