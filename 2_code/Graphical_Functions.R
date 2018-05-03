# load libraries
if (!require(pacman)) install.packages('pacman'); library(pacman)
p_load(ggplot2, ggthemes)


# read data
# mydata <- read.csv("bcn_listings.csv")
# 
# ###### scrub  data fields, for example graphical onlyt################################
# 
# # fix price field
# mydata$price <- as.character(mydata$price)
# mydata$price <- as.numeric(str_replace_all(mydata$price, "[\\$,]",""))
# 
# # convert host_since
# mydata$host_since <- as.Date(mydata$host_since)
# 
# # convert dates to years from minimum start date
# min_dt <- min(mydata$host_since, na.rm=T)
# 
# mydata$host_since <- as.numeric(mydata$host_since - min_dt) / 365.25  # convert to years

###################################################################################



# histogram function
hist621 <- function(df, x, xlab="", title="", bins=30) {
  
  x1 <- eval(substitute(x),df, parent.frame())  
  xlab_default <- deparse(substitute(x)) # default x label if argument left blank
  title_default <- paste("Distribution of",xlab_default) # default title if argument left blank
  
  ggplot(df, aes(x1)) + geom_histogram(bins=bins, fill='steelblue4', color="gray") + theme_classic() + 
    labs(x = ifelse(xlab =="",xlab_default,xlab), 
         title= ifelse(title=="",title_default,title)) + 
    theme(plot.title = element_text(hjust = 0.5))
} 


# example histogram output
# hist621(mydata, price)


# scatterplot function
scat621 <- function(df, x, y, xlab="", ylab="", title="", method="loess") {
  x1 <- eval(substitute(x),df, parent.frame()) 
  y1 <- eval(substitute(y),df, parent.frame())
  xlab_default <- deparse(substitute(x)) # default x label if argument left blank
  ylab_default <- deparse(substitute(y)) # default y label if argument left blank
  title_default <- paste(ylab_default,"vs.",xlab_default)  # default title if argument left blank
  
  
  ggplot(df, aes(x1,y1)) + geom_point(color="steelblue4", position="jitter") + theme_classic() + 
    labs(x = ifelse(xlab =="",xlab_default,xlab),
         y = ifelse(ylab =="",ylab_default,ylab),
         title= ifelse(title=="",title_default,title)) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    geom_smooth(method=method, color = "darkred", size=1.2)  
    
}

# example scatterplot output
# scat621(mydata,host_since,log(price))


# boxplot function
box621 <- function(df, x, y, xlab="", ylab="", title="") {
  x1 <- eval(substitute(x),df, parent.frame()) 
  y1 <- eval(substitute(y),df, parent.frame())
  xlab_default <- deparse(substitute(x)) # default x label if argument left blank
  ylab_default <- deparse(substitute(y)) # default y label if argument left blank
  title_default <- paste(ylab_default,"vs.",xlab_default)  # default title if argument left blank
  
  
  ggplot(df, aes(x1,y1)) + geom_boxplot(fill="steelblue4") + theme_classic() + 
    labs(x = ifelse(xlab =="",xlab_default,xlab),
         y = ifelse(ylab =="",ylab_default,ylab),
         title= ifelse(title=="",title_default,title)) +
    theme(plot.title = element_text(hjust = 0.5)) 

}

# example boxplot output
# box621(mydata,host_identity_verified, log(price))


# barplot function
bar621 <- function(df,x,xlab="",title="", vadj=-0.5, hadj=0.5) {
  x1 <- eval(substitute(x),df, parent.frame()) 
  xlab_default <- deparse(substitute(x)) # default x label if argument left blank
  title_default <- paste(xlab_default, "by type")  # default title if argument left blank
  
  ggplot(df, aes(x1)) + geom_bar(fill="steelblue4", color="gray") + theme_classic() + 
    labs(x = ifelse(xlab =="",xlab_default,xlab),
         title= ifelse(title=="",title_default,title)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(y =  ..count..,
                  label = paste0(round(prop.table(..count..) * 100,1), '%')), 
              stat = 'count', 
              position = position_dodge(.9),
              vjust=vadj,
              hjust=hadj,
              size = 5,
              color = 'darkred')
 
}

# example barplot output
# bar621(mydata, host_identity_verified) 

# another barplot example
# bar621(mydata, host_identity_verified, hadj=-0.2) + coord_flip()



