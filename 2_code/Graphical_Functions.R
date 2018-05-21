

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

# barplot function
bar621 <- function(df,x,xlab="",title="", vadj=-0.5, hadj=0.5, fntsize=5) {
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
              size = fntsize,
              color = 'darkred')
 
}

