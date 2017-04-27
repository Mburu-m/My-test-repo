p = function(...)paste(..., sep = "")

if(Sys.info()['sysname'] == 'Linux') path = "/run/media/mburu/Windows/" else path = "C:"
source(paste(path,"/Users/mburu/Desktop/work/rebiweekly1and2/sgmvars.R", sep = ""))
my_dir = p(path, "/Users/mburu/Desktop/kibunjamoloolenguruoned316roaddata")
setwd(my_dir)
dir()

dat <- read_excel( "molo olenguruone data.xlsx", skip = 1)

names(dat)
some_colnames <- which(!is.na(dat[1,]))
some_colnames2 <- which(!is.na(dat[2,]))
some_colnames2
empty_names <- which(names(dat) == "")
names(dat)[some_colnames2] <- c(dat[2, some_colnames2])
names(dat)[some_colnames] <- c(dat[1, some_colnames])

dat2 <- dat[-c(1:2),]

datm <- melt(dat2, id.vars = "Area/Subcounty")
my_vars <- names(dat2)[c(6,7, 16,17,18,21,24,25,33:37)]

datm_mars <- filter(datm, variable %in% my_vars)
datm_mars$variable <- factor(datm_mars$variable, levels = my_vars)
q33 <- c(dat[2, 33]) %>% unlist()
lvl_lbl <- c(my_vars[1:6], q33)
datm_mars_split <- split(datm_mars,datm_mars$variable)
lvl <- list(c(1,2), c(1,2),c(1,2),c(1,2),c(1:3), c(1,2), c(1,2), c(1:4), c(1:6))
lbl <- list(c("Yes", "No"),c("Yes", "No"),c("Yes", "No"), c("Yes", "No"), c( "Subsistence", "Commercial", "Both"),
           c("Yes", "No"), c("Male", "Female"),c("Primary","Secondary", "Tertiary","None"),
            c("Walking", "Matatu/Bus", "Motorcycle (boda boda)", "Taxi", "Personal Car", "Others "))

sq1 <- foreach(i = 1:length(datm_mars_split)) %do%{
  
  this = datm_mars_split[[i]]
  if(i > 8) i = 9 else i = i
  lvl1 = lvl[[i]]
  lbl1 = lbl[[i]]
  this = mutate(this, value = factor(value, levels = lvl1,
                                     labels = lbl1))
  
  
}

sq1[[1]] %>% View


dat_perc <- foreach(i = 1:length(sq1)) %do% {
  this = sq1[[i]]
  this <- this[!is.na(this$value) & !is.na(this$variable),]%>% group_by(value) %>% 
    summarise(freq = n())%>%mutate(perc = round(freq * 100/sum(freq), 2))
  
}

title1 <- c("Are you aware of this road","Do you believe this project shall be \n of any economic benefit to the area? ", "Do you have a farm?", "Do you have a Title Deed", "Type of Farming","Do you practice Horticulture farming",
            "Respondent Gender","Respondent Education Level",paste("Mode of transport to",
                                                                       c("Education facility", "Health facility",
                                                                         "Government facility", "the Market",
                                                                         "Transport farm products")))
xlabs <- c("Are you aware of this road","Will be the project beneficial?" ,"Do you have a farm?", "Do you have a Title Deed", "Type of Farming", "Do you practice Horticulture farming",
           "Gender", "Education Level",  rep("Mode of transport", 5))

plots = foreach(i = 1:length(dat_perc)) %do% {
  this = dat_perc[[i]] 
  
  p =  ggplot(this, aes(x= value, y =  perc, fill = value))+
    geom_bar(size = 0.5, stat = "identity")+ theme_excel()+
    geom_text(label = paste(this$perc, "%", sep = " ") ,position =position_dodge(width = 0.3),
              size = 4, vjust= 0.05,hjust = 0.4, colour = "black") +
    labs(list(x =xlabs[i], y = "Percentage of the Households",
              title = title1[i]))+
    scale_fill_hc(name = "")
   
  p + theme1 +theme(legend.position = "none")
  
  
}

plots
f = letters[1:13]
foreach(i = 1:length(plots)) %do% {
  #nm = title1[i]
  x11(width = 6.7 ,height = 4.3,  pointsize = 10)
  print(plots[[i]])
  savePlot( paste(f[i], ".eps" , sep = ""))
  dev.off()
}
x = 39
y = c()
while(x <= 57){
  y = append(y, x)
  x = x+3
}
firstsec <-function(x) gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
my_seq <- function(x)seq(from = x, to = x+2, by = 1)
my_seq(39)
dat_sumary <- foreach(i = 1:length(y)) %do% {
  town <- dat2[,my_seq(y[i])]
  town[, 2:3] <- lapply(town[,2:3], as.numeric)
  names(town)[1] <- "place"
  names(town)[2] <- "Time in min"
  town$place <- firstsec(tolower(town$place))
  town <- town[!is.na(town$place),] %>% group_by(place)%>%
    summarise(Average_Time = round(mean(`Time in min`, na.rm = T),2), Median_Time = median(`Time in min`, na.rm = T),
          Average_Cost = round(mean(`cost one way`, na.rm = T),2), Media_Cost = median(`cost one way`, na.rm = T)  )
}
dat_sumary
my_names <- c("visiting_town", "transport goods to the market","going to school","seek medical services",
              "seek government services", "seek vetinary services", "agriculture extension")
for (i in 1:length(dat_sumary)) {
  this = dat_sumary[[i]]
  this = as.data.frame(this)
  write.csv(this, file = paste(my_dir,"/", my_names[i], ".csv", sep = ""), row.names = F)
}

dat4 <- as.data.frame(lapply(dat2[,31:32], as.numeric))
sapply(dat4, mean, na.rm = T)
sapply(dat4, mean, na.rm = T)
sapply(dat4, mean, na.rm = T)
sapply(dat4, median, na.rm = T)

inome <- summarise(dat4, average_income = mean(dat4[,1], na.rm = T), Median_income = median(dat4[,1], na.rm = T),
          Average_Expenditure = mean(dat4[,2], na.rm = T), Median_Expenditure = median(dat4[,2], na.rm = T))
write.csv(inome, file = "income.csv", row.names = F)

idx = c(2,3,5,20, 19, 20, 22)
f= which(str_detect(tolower(dat2$occupation) ,"business"))
z= which(str_detect(tolower(dat2$occupation) ,"mpesa"))
dat2$occupation[f] <- "business"
dat2$occupation[z] <- "Mpesa Agent"
dat_perc2 <- foreach(i = 1:length(idx)) %do% {
  town <- dat2[,idx[i]]
  names(town)[1] <- "var"
  town$var <- removeNumPunct(firstsec(str_trim(tolower(town$var))))
  town$var[town$var == ""] <- NA
  town <- town[!is.na(town$var),] %>% group_by(var)%>%
    summarise(freq = n())%>%mutate(perc = round(freq * 100/sum(freq), 2))
    
}
dat_perc2
library(ggmap)
loc_subcounty <- geocode(na.omit(dat2$`Area/Subcounty`))
loc <- geocode(na.omit(dat2$`Location/ area of residence`))
my_names2 <- c("loacation", "occupation", "residence","crops4", "livestock","crops" )
for (i in 1:length(dat_perc2)) {
  this = dat_perc2[[i]]
  this = as.data.frame(this)
  write.csv(this, file = paste(my_dir,"/", my_names2[i], ".csv", sep = ""), row.names = F)
}

loc1 <- loc[loc$lat < 5 & !is.na(loc$lon),]
loc1
loc2 <- loc_subcounty[loc_subcounty$lat < 5 & !is.na(loc_subcounty$lon),]
molo_map <- get_map(location = c(mean(loc2$lon), mean(loc2$lat)), zoom = 10, maptype = "terrain")
molo_map1 <- ggmap(molo_map)+ geom_point(data = loc2, aes(lon, lat), col = "blue", size = 3)
molo_map1

png("mymap.png", width = 6, height = 5, units = "in", res = 500, pointsize = 15)
print(molo_map1)
dev.off()
library(tm)
library(wordcloud)
s = 8:15
stop_words <- c(stopwords(), c("the", "of", "to"))
dat_corpus <-foreach(i = 1:length(s)) %do% {
  that = dat2[,s[i]]
  names(that)[1] <- "var"
  #that$var <- removeWords(tolower(that$var), stop_words)
  corpus_dat <- Corpus(VectorSource(na.omit(that$var)))
  corpus_dat <-  tm_map(corpus_dat, content_transformer(tolower))
  corpus_dat <- tm_map(corpus_dat, removeWords, stop_words)
  corpus_dat <- tm_map(corpus_dat, stripWhitespace)
}


dat_tdm <-foreach(i = 1:length(dat_corpus)) %do% {
  my_corpus <- dat_corpus[[i]] 
 my_tdm <- TermDocumentMatrix(my_corpus, control = list(wordLengths = c(1, Inf)))
}

dat_term_freq <-foreach(i = 1:length(dat_corpus)) %do% {
  my_tdm <- dat_tdm[[i]] 
  freq_terms <-  rowSums(as.matrix(my_tdm))
  if(i == 3) j = 4 else j = 6
  freq_terms <- subset(freq_terms, freq_terms >= j)
  df <- data.frame(word = names(freq_terms), freq = freq_terms)
}

names(dat[,s])
my_titles <- c("How will the project benefit you?", "Current Issues with existing Roads",
               "Environmental and Social Concerns", "Positive Environmental and Social Impacts \n of the new Road",
               "Negative Concerns", "Proposed Mitigation", "How would you like the local community to \n be involved in the road project",
               "Other comments or concerns")
my_plots <- foreach(i = 1:length(dat_term_freq))%do%{
  that1 = dat_term_freq[[i]]
  p = ggplot(that1, aes(x = freq, y = reorder(word, freq))) + 
    geom_point(colour = "blue", size = 2)+ theme_bw()+
    #scale_x_continuous(breaks=c(150,300,450, 600, 750, 900))+
    labs(list(title = my_titles[i],
              y = "Words", x = "Frequency"))

}

my_plots
nms <- letters[13+1:8]
foreach(i = 1:length(my_plots)) %do% {
  #nm = title1[i]
  x11(width = 6.7 ,height = 4.3,  pointsize = 10)
  print(my_plots[[i]])
  savePlot( paste(nms[i], ".eps" , sep = ""))
  dev.off()
}


dat_assocs <-foreach(i = 1:length(dat_tdm)) %do% {
  that = dat_tdm[[i]]
  this = dat_term_freq[[i]]
 d= findAssocs(that, as.character(this$word), 0.19)
o = lapply(d, as.data.frame)
}
lapply(dat_assocs, length)
library(RColorBrewer)
pal <- brewer.pal(8, "Dark2")
pal <- pal[-(1:4)]


word_freqs <- foreach(i = 1:length(dat_tdm)) %do%{
  this = dat_tdm[[i]]
  m = as.matrix(this)
  word_freq <- sort(rowSums(m), decreasing = T)
  
}

foreach(i = 1:length(word_freqs)) %do% {
  word_freq <- word_freqs[[i]]
  png(p(nms[i], "z", ".png"), width = 6, height = 6, units = "in", res = 100,
      pointsize =  12)
  if(i == 4) j = 4 else j = 1
  wordcloud(words = names(word_freq), freq = word_freq, min.freq = j,
            random.order = F, colors = pal)
  title(my_titles[i])
  dev.off()
  
}


library(wordcloud)
png(p(nms[i], ".png"), width = 10, height = 10, units = "in", res = 300,
    pointsize = 25 )
wordcloud(words = names(word_freq), freq = word_freq, min.freq = 3,
          random.order = F, colors = pal)
title(my_titles[i])
dev.off()