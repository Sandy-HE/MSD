#statistically analyze emotion tags info 
#In the input file, one tag info like this: happy*100;indie rock*82;rock*65

library(stringr)
library(data.table)
library(ggplot2)
library(dplyr)


#initial
alltags <- fread("./statwords.csv")
#in the final statistical form, remove some meaningless words
exword <- c("the","to","of","and","a","in","this","-","for","it","that","on","is")
#exgenre <- c("pop","rock","punk","jazz","folk","indie","classic","electronic","latin")
exgenre <- c("pop|rock|punk|jazz|folk|indie|classic|electronic|latin|hip hop|blues|rnb|rap|vocal|country|ballad")


# This function is extracting emotion tags info and its word(phrases) frequency and weight.
# Input: ekey - specify emotion type
# Output: one list contains two data frames. 
# One dataset is single words statistic. The other is phrases statistic.

statwords <- function(ekey){
  result <- grep(ekey,alltags$tags,ignore.case = TRUE)
  if(length(result)==0 ){
    return(NULL)
  }
  subtags <- alltags[result,]
  obsv <- str_to_lower(subtags$tags)
  obsv <- str_split(obsv,pattern = ";")      #list
  obsv1 <- unlist(obsv)                      #phrases vector with '*' and weight
  obsv2 <- sapply(strsplit(obsv1, "\\*"), "[", 1)  #phrases vector
  obsv3 <- sapply(strsplit(obsv1, "\\*"), "[", 2)  #weight vector
  
  
  #keep original phrases
  obsv4 <- as.data.frame(obsv2, stringsAsFactors = FALSE)   #data frame
  names(obsv4) <- c("phrases")
  obsv4$freq <- 1L
  obsv4$weight <- as.numeric(obsv3)
  #obsv4 <- obsv4[!(obsv4$phrases %in% exgenre),] 
  obsv4 <- obsv4[!(grepl(exgenre,obsv4$phrases)),] 
  
  #split into single words
  obsv5 <- paste0(obsv2,collapse=" ")  #only one string containing all words
  obsv6 <- str_split(obsv5,pattern = " ") 
  obsv6 <- unlist(obsv6)               # word vector
  
  
  #split single weight for each word
  times <- lengths(str_split(obsv2,pattern = " "))
  obsv7 <- as.data.frame(obsv6, stringsAsFactors = FALSE)   #data frame
  names(obsv7) <- c("words")
  obsv7$freq <- 1L
  obsv7$weight <- as.numeric(rep(obsv3,times))
  obsv7 <- obsv7[!(obsv7$words %in% exword),]  
  obsv7 <- obsv7[!(obsv7$words %in% exgenre),]  
  
  #add obsv4,obsv7 into one list as return
  list(obsv4,obsv7)
  
}

#==== Setting one keyword, then use API statwords(key) to generate word statistic data for specified emotion tag
#Here you can change key word such as happy, sad, angry, relax, fear
keyword <- c("calm")
statlist <- statwords(keyword)
if(!is.null(statlist)){
  form1 <- statlist[[1]]
  form2 <- statlist[[2]]
}

#==== Backup data ====
#statwords3.csv is for phrases(original tags) and their frequency and weight(strength)
#statwords4.csv is for single words and their frequency and weight(strength)
#fwrite(statlist[[1]], file = paste0(keyword,"_statwords3.csv"))   
#fwrite(statlist[[2]], file = paste0(keyword,"_statwords4.csv"))


#==== data handling ====
# phrase statistics with frequency
statwords1 <- aggregate(freq~phrases,data=form1,FUN=sum)
statwords1 <- statwords1[order(statwords1$freq, decreasing = TRUE),]
rownames(statwords1) <- c()
fwrite(statwords1,file = paste0(keyword,"_statwords1.csv") )

# phrase statistics with weight*frequency
statwords2 <- aggregate(weight~phrases,data=form1,FUN=sum)
statwords2 <- statwords2[order(statwords2$weight, decreasing = TRUE),]
rownames(statwords2) <- c()
fwrite(statwords2,file = paste0(keyword,"_statwords2.csv") )

# single word statistics without adding weight
statwords3 <- aggregate(freq~words,data=form2,FUN=sum)
statwords3 <- statwords3[order(statwords3$freq, decreasing = TRUE),]
rownames(statwords3) <- c()

# single word statistics with weight
statwords4 <- aggregate(weight~words,data=form2,FUN=sum)
statwords4 <- statwords4[order(statwords4$weight, decreasing = TRUE),]
rownames(statwords4) <- c()


# get the topN words or phrases
top <- 30L
topswords1 <- head(statwords1,top)
topswords2 <- head(statwords2,top)
topswords3 <- head(statwords3,top)
topswords4 <- head(statwords4,top)


#==== data visualization ====
# bar chart for top words(phrases) and their quantity
barplot(topswords1$freq,names.arg = topswords1$phrases,las=2,main = "phrase statistics with frequency",  ylab = "frequency")
barplot(topswords2$weight,names.arg = topswords2$phrases,las=2,main = "phrase statistics with weight", ylab = "weight")
barplot(topswords3$freq,names.arg = topswords3$words,las=2,main = "single word statistics with frequency", ylab = "frequency")
barplot(topswords4$weight,names.arg = topswords4$words,las=2,main = "single word statistics with weight", ylab = "weight")


# other styles of data show
# option1: add color and label frequency for each bar
wordbar <- barplot(topswords1$freq,names.arg = topswords1$phrases,main = "phrase statistics without weight",  ylab = "frequency",las=2, col = rainbow(30))
text(wordbar, topswords1$freq,topswords1$freq,cex=1,pos=1) 

# option2: horizontal bar
topswords1 <- topswords1[order(topswords1$freq),]
barplot(topswords1$freq,names.arg = topswords1$phrases,las=2,main = "phrase statistics without weight",  xlab = "frequency", horiz = T, col = rainbow(40) )

# merge all statistical result
sumstat <- cbind(topswords1,topswords2,topswords3,topswords4,stringsAsFactors=F)
fwrite(sumstat, file = paste0(keyword,"_allstatword.csv"))

# wordcloud
library(wordcloud)
wordcloud(words = statwords1$phrases, freq = statwords1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#==== collect emotion words data ====
sadwords <- as.data.frame(fread("sad_statwords2.csv"))
happywords <- as.data.frame(fread("happy_statwords2.csv"))
angrywords <- as.data.frame(fread("angry_statwords2.csv"))
relaxwords <- as.data.frame(fread("relax_statwords2.csv"))
calmwords <- as.data.frame(fread("calm_statwords2.csv"))

#==== get top80 phrases for each emotion type, then find common phrases for all emotions
#remove these phrases in each emotion statwords table
#only get top30 for each emotion type, then merge non-common phrases as total tags collection

top <- 80L
topsadwords <- head(sadwords,top)
tophappywords <- head(happywords,top)
topangrywords <- head(angrywords,top)
toprelaxwords <- head(relaxwords,top)
topcalmwords <- head(calmwords,top)
  
totalwords <- rbind(topsadwords,tophappywords,topangrywords,topcalmwords)
totalwords$freq <- 1L

uniquewords <- aggregate(freq~phrases,data=totalwords,FUN=sum)
uniquewords <- uniquewords[order(uniquewords$freq, decreasing = TRUE),]
rownames(uniquewords) <- c()

#totally, 31 common words when top80 for each emotion
commonwords <- uniquewords[uniquewords$freq==4,]$phrases

top <- 30L
top30sadwords <- head(sadwords,top)
top30happywords <- head(happywords,top)
top30angrywords <- head(angrywords,top)
top30relaxwords <- head(relaxwords,top)
top30calmwords <- head(calmwords,top)
totaltop30words <- rbind(top30sadwords,top30happywords,top30angrywords,top30calmwords)
totaltop30words$freq <- 1L

uniquewords <- aggregate(freq~phrases,data=totaltop30words,FUN=sum)
uniquewords <- uniquewords[order(uniquewords$freq, decreasing = TRUE),]
rownames(uniquewords) <- c()

#finally, get 40 phrases
uniquewords <- uniquewords[!(uniquewords$phrases %in% commonwords),]$phrases

uniquehappywords <- happywords[happywords$phrases %in% uniquewords,]
colnames(uniquehappywords) <- c("phrases","happy")
uniquesadwords <- sadwords[sadwords$phrases %in% uniquewords,]
colnames(uniquesadwords) <- c("phrases","sad")
uniqueangrywords <- angrywords[angrywords$phrases %in% uniquewords,]
colnames(uniqueangrywords) <- c("phrases","angry")
uniquerelaxwords <- relaxwords[relaxwords$phrases %in% uniquewords,]
colnames(uniquerelaxwords) <- c("phrases","relax")
uniquecalmwords <- calmwords[calmwords$phrases %in% uniquewords,]
colnames(uniquecalmwords) <- c("phrases","calm")

etmatrix <- merge(uniquehappywords,uniquesadwords,by="phrases")
etmatrix <- merge(etmatrix,uniqueangrywords,by="phrases")
etmatrix <- merge(etmatrix,uniquecalmwords,by="phrases")
row.names(etmatrix) <- etmatrix$phrases

#==== tags-emotion matrix====
etmatrix1 <- as.matrix(etmatrix[,2:5])     #40*4

#==== build songs set covering 4 emotion types====
#alltags <- fread("./statwords.csv")
#result <- grep(keyword,alltags$tags,ignore.case = TRUE)
#subtags <- alltags[result,]
#fwrite(subtags,paste0("tagswith",keyword,".csv"))


happydata <- as.data.frame(fread("tagswithhappy.csv"))
saddata <- as.data.frame(fread("tagswithsad.csv"))
angrydata <- as.data.frame(fread("tagswithangry.csv"))
relaxdata <- as.data.frame(fread("tagswithrelax.csv"))
calmdata <- as.data.frame(fread("tagswithcalm.csv"))

#====balancing emotion data ====
#use angry data size 3256 as the base size, other emotion data is sampled.
samp <- sample(nrow(happydata), nrow(angrydata)) 
samp <- sort(samp)
happydata <- happydata[samp,]

samp <- sample(nrow(saddata), nrow(angrydata))
samp <- sort(samp)
saddata <- saddata[samp,]

samp <- sample(nrow(relaxdata), nrow(angrydata))
samp <- sort(samp)
relaxdata <- relaxdata[samp,]

samp <- sample(nrow(calmdata), nrow(angrydata))
samp <- sort(samp)
calmdata <- calmdata[samp,]

#====

songstags <- rbind(happydata,saddata,angrydata,calmdata)
songstags <- unique(songstags$track_id)
songstags <- alltags[alltags$track_id %in% songstags,]
#fwrite(songstags, "4emotionsongtags53302.csv")
#fwrite(songstags, "balancehasrsongtags12587.csv")
fwrite(songstags, "balancehascsongtags12587.csv")
#==== for each track, calculate the weight for each tag in total tags collection
strtrans <- function(x){
  obsv1 <- strsplit(x,"\\*")
  obsv2 <- data.frame(matrix(unlist(obsv1),ncol = 2, byrow = TRUE),stringsAsFactors=F)
  colnames(obsv2) <- c("phrases", "weight")
  obsv2$weight <- as.numeric(obsv2$weight)
  
  
  obsv3 <- merge(dfuniwords,obsv2, by="phrases",all.x = T)
  
  obsv3$weight[is.na(obsv3$weight)] <- 0
  
  obsv3$weight
}

uniquewords<- uniquewords[1:39]
dfuniwords <- data.frame(uniquewords,stringsAsFactors=F)
colnames(dfuniwords) <- c("phrases")
obsv <- str_to_lower(songstags$tags)
obsv <- str_split(obsv,pattern = ";")      #list

obsv8 <- sapply(obsv,strtrans)     #tags-songs matrix, value is weight
stmatrix <- t(obsv8)             #transpose to songs-tags matrix (53302*40)
dimnames(stmatrix) <- list(songstags$track_id,sort(uniquewords))

#==== calculate songs-emotion matrix (53302*4)====
sematrix <- stmatrix %*% etmatrix1  
sematrix1 <- sematrix/100000L


#==== for some songs, the weight of emotion tag is 0. That's why some items in sematrix are 0-0-0-0.
#use below method, we will remove these all-0 items. That is, the songs with weight zero are filtered out as well.
sedf <- as.data.frame(sematrix)
sedf$valid <- sedf$happy|sedf$sad|sedf$angry|sedf$calm
#nrow(sedf[!sedf$invalid,])  
sedf1 <- sedf[sedf$valid,]
sedf1$valid <- c()
sedf1 <- sedf1/100000L


sedfmax <- sapply(sedf1,max)
maxweight <- max(sedfmax)
sedfnorm <- sedf1/maxweight
sedfnorm1 <- sedfnorm*100
N <- ncol(sedfnorm1)
avalue <- apply(sedfnorm1[,c('happy','sad','angry','calm')],1,function(x){(x[1]-x[2]+x[3]-x[4])/N/sqrt(2)})
vvalue <- apply(sedfnorm1[,c('happy','sad','angry','calm')],1,function(x){(x[1]-x[2]-x[3]+x[4])/N/sqrt(2)})

adf <- as.data.frame(avalue)
adf <- cbind(track_id = rownames(adf), adf) 
rownames(adf) <- c()
adf$alevel <- ifelse(adf$avalue>=0,1,3)
vdf <- as.data.frame(vvalue)
vdf <- cbind(track_id = rownames(vdf), vdf) 
rownames(vdf) <- c()
vdf$vlevel <- ifelse(vdf$vvalue>=0,0,1)

dimvalue <- merge(vdf,adf,by='track_id')
dimvalue$quadrant <- dimvalue$vlevel+dimvalue$alevel
dimvalue$quadrant <- as.character(dimvalue$quadrant)

#the whole view
dimvalue%>%
  group_by(quadrant)%>%
  ggplot(aes(x = vvalue,y= avalue,color=quadrant))+
  geom_point(alpha=0.2)+
  scale_color_manual(values= c('orange','red','green','blue'),
                       name="Emotion Type",
                      breaks=c('1','2', '3','4'),
                      labels=c("Happy", "Angry", "Calm","Sad")
                      )
#micro-view
dimvalue%>%
  group_by(quadrant)%>%
  ggplot(aes(x = vvalue,y= avalue,color=quadrant))+
  geom_point(alpha=0.2)+
  scale_color_manual(values= c('orange','red','green','blue'),
                     name="Emotion Type",
                     breaks=c('1','2', '3','4'),
                     labels=c("Happy", "Angry", "Calm","Sad")
  )+
  coord_cartesian(xlim = c(-0.5,0.5),ylim = c(-0.5, 0.5))

#statistical data for each quadrant
summary_stats<- dimvalue%>%
  group_by(quadrant)%>%
  summarise(size=n())
summary_stats_tag<- filteredtags%>%
  group_by(tag_name)%>%
  summarise(size=n())


filteredtags <- as.data.frame(fread("./emotiondata.csv"))   #8354
happytrack <- happydata$track_id  #2444
angrytrack <- filteredtags[filteredtags$tag_name == "angry",]$track_id  #191
sadtrack <- filteredtags[filteredtags$tag_name == "sad",]$track_id    #2422
angrytrack <- filteredtags[filteredtags$tag_name == "angry",]$track_id
quad1 <- dimvalue[dimvalue$quadrant == 1,] #3205
happytruth <- quad1[quad1$track_id %in% happytrack,]  #188, only >=50  1830  rate: 1830/3205=57%
othertruth <- quad1[!quad1$track_id %in% happytrack,] 
othertruth <- othertruth[!othertruth$track_id %in% angrytrack,]
othertruth <- othertruth[!othertruth$track_id %in% sadtrack,]
lefttruth <- alltags[alltags$track_id %in% othertruth$track_id,]
item <- lefttruth[lefttruth$track_id=='TRAAOIK128F424BFED',]

#==== 53302 tracks, and 6008 is all-0.====
#keep them as backup data

write.csv(stmatrix, file="songtagsmatrix.csv")
write.csv(etmatrix1,file = "tagsemomatrix.csv")
write.csv(sematrix1,file="songsemomatrix.csv")
