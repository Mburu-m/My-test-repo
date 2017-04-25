p = function(...)paste(..., sep = "")

if(Sys.info()['sysname'] == 'Linux') path = "/run/media/mburu/Windows/" else path = "C:"
my_direx = p(path, "/Users/mburu/Desktop/work/repeat/note books/nws2")
source(paste(path,"/Users/mburu/Desktop/work/rebiweekly1and2/sgmvars.R", sep = ""))
setwd(my_direx)
code7 <- setDT(read_excel(p(path,"/Users/mburu/Dropbox/FEDU Graphs/Codebooks/Biweekly7 code.xlsx")))

library(pacman)
p_load(data.table, stringr, magrittr, stringdist, checkmate, foreach, ggplot2, zoo, knitr, plyr, xlsx, DescTools,
       gridExtra, viridis, plotly, devEMF, jpeg, doBy, dplyr, ggthemes, readxl, stringi, Hmisc,
       doParallel)
code7 <- setDT(read_excel(p(path, "/Users/mburu/Dropbox/FEDU Graphs/Codebooks/Biweekly7 code.xlsx")))
load(p(path, "/Users/mburu/Desktop/work/repeat/note books/df1.rda"))
q100 = df[(grep("Q_100$", df$codeq)),]
q100 = q100[order(q100$bw),]
q100[, value := ifelse(value == 1 | value == 2, NA, value)]
q100 = q100[!is.na(q100$value),]
View(q100)
q100$name %<>% as.factor()
lvls100 <- as.character(levels(q100$name))
lvls100
q100$value %<>% as.numeric()
q100$value[q100$value < 0]= NA

chisq.test(xtabs(value~ name + ppicut , data = q100), simulate.p.value = F)
boxplot(value ~name, data = q100)
q100s <- split(q100, q100$name)
df_q100_1 <- foreach(i = 1:length(q100s)) %do% {
  this = q100s[[i]]
  this <- this[!is.na(this$bw) & !is.na(this$value),]%>% group_by(bw) %>%
    summarise(Average = mean(value, na.rm = T), standard_deviation  = sd(value, na.rm = T),
              Maximum = max(value), Minimum = min(value, na.rm = T),
              median_savings = median(value), percentile_75th = quantile(value, 3/4), freq = n())
  this =  sapply(this, round, 2)
}
kph = as.data.frame(df_q100_1[[5]])
kph

lvls100 <- gsub("/", " or ", lvls100)
for (i in 1:length(df_q100_1)) {
  this = df_q100_1[[i]]
  this = as.data.frame(this)
  write.csv(this, file = paste(path,"/Users/mburu/Desktop/work/repeat/note books/nws2/", lvls100[i], ".csv", sep = ""), row.names = F)
}

b = c()
df_q100_g <- foreach(i = 1:length(df_q100_1)) %do% {
  this = df_q100_1[[i]]
  this = as.data.frame(this)
  if("bw" %in% names(this)) this =dplyr::select(this, bw, Average, median_savings, percentile_75th, freq) else this = NULL
  if(is.null(this)) b = append(b, i) 
  if(is.null(this))this = this else this = melt(this, id.vars = c("bw", "freq")) 
}

df_q100_g <- df_q100_g[lapply(df_q100_g,length)>0]


Mode <- function(x) {
  ux <- unique(x)
  if(length(x) == length(ux)) ux <- unique(sample(ux, length(x)+1000, replace = T)) else ux = ux
  ux[which.max(tabulate(match(x, ux)))]
}

brks = function(x) {
  ty = max(x)/10
  if(ty > 1000) y = signif(ty, digits = 2) else y = signif(ty, digits = 1)
  z = signif(min(x), 2)
  tx = seq(z, max(x), by = y)
  return(tx)
  
  
}
f = brks(this$value)
f

firstcap <- function(x)gsub("^([[:alpha:]])", "\\U\\1", x, perl=TRUE)
firstsec <-function(x) gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
lvls100 %<>% firstsec
lvls100 <- lvls100[-b]
lvls100
plots = foreach(i = 1:length(df_q100_g)) %do% {
  this = df_q100_g[[i]] 
  #xlbs =  rep("Scale", 4)
  
  # reverses the levels of employment variable
  
  #this <- mutate( ungroup(this),variable = factor(variable, rev(levels(variable))))
  
  p =  ggplot(this, aes(x= bw, y =  value, colour = variable))+
    geom_line(size = 1)+ theme_excel()+
    # geom_text(label = paste(this$perc, "%", sep = " ") ,position =position_dodge(width = 0.3),
    #           size = 4, vjust= 0.05,hjust = 0.4, colour = "black") +
    labs(list(x = paste("Biweekly", "( n = ",Mode(this$freq), " )", sep = ""), y = "Amount (UGX)",
              title = lvls100[i]))+
    scale_color_manual(name = "", values = c("#A9711E", "#E6A168" ,"#83A593"))+
    scale_y_continuous(breaks = round(brks(this$value),0))+
    scale_x_continuous(breaks =seq(0, 12, by = 1))
  
  
  #require(scales)
  
  #p =  p + scale_y_continuous(label = comma, limits = c(min(this$Average), max(this$Average)))
  p + theme1 
  
  
}

plots

foreach(i = 1:length(plots)) %do% {
  #nm = title1[i]
  x11(width = 6.7 ,height = 4.3,  pointsize = 10)
  print(plots[[i]])
  savePlot( paste("q100_amount paid", lvls100[i], ".eps" , sep = ""))
  dev.off()
}
