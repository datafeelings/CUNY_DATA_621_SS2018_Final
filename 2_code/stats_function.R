library(MASS)

stats = function (dataframe) {
  Min = sapply(dataframe, function(x) {min(x, na.rm=TRUE)})
  Max = sapply(dataframe, function(x) {max(x, na.rm=TRUE)})
  Mean = sapply(dataframe, function(x) {round(mean(x, na.rm=TRUE),2)})
  Median = sapply(dataframe, function(x) {round(median(x, na.rm=TRUE),2)})
  St.dev = sapply(dataframe, function(x) {round(sd(x, na.rm=TRUE),2)})
  Skew = sapply(dataframe, function(x) {round(skewness(x, na.rm=TRUE),2)})
  Kurtosis = sapply(dataframe, function(x) {round(kurtosis(x, na.rm=TRUE),2)})
  NA_Count = sapply(dataframe, function(x) {sum(length(which(is.na(x))))})
  Percent_NA = sapply(dataframe, function(x) {round(sum(length(which(is.na(x))))/length(x)*100, 2)})
  Outliers_Count = sapply(dataframe, function(x) {round(length(which(x>mean(x, na.rm=TRUE)+ 3*sd(x, na.rm=TRUE) | 
                                                                     x<mean(x,na.rm=TRUE )-3*sd(x, na.rm=TRUE))),2)})
  Percent_Outliers = sapply(dataframe, function(x) {round(length(which(x>mean(x, na.rm=TRUE)+ 3*sd(x, na.rm=TRUE) | 
                                                                       x<mean(x, na.rm=TRUE)- 3*sd(x, na.rm=TRUE))) / length(x),2)*100})
  return(cbind(Min, Max, Mean, Median, St.dev, Skew, Kurtosis, NA_Count, Percent_NA, Outliers_Count, Percent_Outliers))
}
