
library("rhdf5")
library("data.table")

fileset = list.files("/projects/MSDdata/data",pattern = "\\.h5$",recursive = TRUE, full.names = TRUE)


pitchNum = 12
segmentNum = 300
songNum = length(fileset)
half_segmentNum = segmentNum/2

print(paste("Total files number: ",songNum))

#pitch_vars <- c("segment_id", "pitch_id")

#check the segment number statistic, the mean of segment is 800
# segstat = c()
# for(i in 1:songNum){
# item <- h5read(fileset[i], "analysis/segments_pitches")
# item <- t(item)
# segstat = c(segstat,nrow(item))
# }
# flag = c("A")
# df = data.frame(segstat)
# df['flag']=factor(flag)
# ggplot(df,aes(flag,segstat))+
#   geom_boxplot()

#include: track_id, tempo, loudness, mode, artist_name, title, genre, segment_number
getSongsData <- function(){
  analysis_vars <- c("track_id","tempo","loudness","mode")
  metadata_vars <- c("artist_name","title","genre")
  songsData <- data.frame()
  for(i in 1:songNum){
    analysis <-h5read(fileset[i],"analysis/songs")
    metadata <-h5read(fileset[i],"metadata/songs")
    segstart <- h5read(fileset[i], "analysis/segments_start")
    trackData <-cbind(subset(analysis, select = analysis_vars),subset(metadata, select = metadata_vars))
    trackData['segnum'] <- length(segstart) 
    songsData <- rbind(songsData,trackData)
  }
  songsData
}

#pitch data, size is segmentNum*12*n
getPitchData<- function(){
  pitchData <- array(-1, c(segmentNum, pitchNum, songNum))
  n=0
  for(i in 1:songNum){
    
    item <- h5read(fileset[i], "analysis/segments_pitches")
    item <- t(item)
    item_nrow = nrow(item)
    if (item_nrow >= segmentNum){
      middle = as.integer(item_nrow/2)
      item = item[(middle-half_segmentNum+1):(middle+half_segmentNum),]
      n=n+1
      pitchData[,,n] = item
    }  
    #solution2
    # else {
    #   nafill = matrix(NA,nrow=segmentNum-item_nrow,ncol=pitchNum)
    #   item = rbind(item,nafill)
    # }
    # pitchData[,,i] = item
  }
  if(n>0){
    pitchData = pitchData[,,1:n]
  }
  
  pitchData
  
}

#timbre data, size is segmentNum*12*n
timbreNum = 12
getTimbreData<- function(){
  timbreData <- array(-1, c(segmentNum, timbreNum, songNum))
  n=0
  for(i in 1:songNum){
    
    item <- h5read(fileset[i], "analysis/segments_timbre")
    item <- t(item)
    item_nrow = nrow(item)
    if (item_nrow >= segmentNum){
      middle = as.integer(item_nrow/2)
      item = item[(middle-half_segmentNum+1):(middle+half_segmentNum),]
      n=n+1
      timbreData[,,n] = item
    }  
  }  
  if(n>0){
    timbreData = timbreData[,,1:n]
  }
  
  timbreData
}

#segment_loudness_max data, size is n*segmentNum
getLoudnessData<- function(){
  loudnessData <- array(NA, c(songNum,segmentNum))
  n=0
  for(i in 1:songNum){
    
    item <- h5read(fileset[i], "analysis/segments_loudness_max")
    item_num = length(item)
    if (item_num >= segmentNum){
      middle = as.integer(item_num/2)
      item = item[(middle-half_segmentNum+1):(middle+half_segmentNum)]
      n=n+1
      loudnessData[n,] = item
      
    }  
  }  
  if(n>0){
    loudnessData = loudnessData[1:n,]
  }
  
  loudnessData
}

#segment_loudness_start data, size is n*segmentNum
getLoudnessStart<- function(){
  loudnessStart <- array(NA, c(songNum,segmentNum))
  n=0
  for(i in 1:songNum){
    
    item <- h5read(fileset[i], "analysis/segments_loudness_start")
    item_num = length(item)
    if (item_num >= segmentNum){
      middle = as.integer(item_num/2)
      item = item[(middle-half_segmentNum+1):(middle+half_segmentNum)]
      n=n+1
      loudnessStart[n,] = item
    }  
  }  
  if(n>0){
    loudnessStart = loudnessStart[1:n,]
  }
  
  loudnessStart
}

#segment_loudness_max_time data, size is n*segmentNum
getLoudnessMaxtime<- function(){
  loudnessMaxtime <- array(NA, c(songNum,segmentNum))
  n=0
  for(i in 1:songNum){
    
    item <- h5read(fileset[i], "analysis/segments_loudness_max_time")
    item_num = length(item)
    if (item_num >= segmentNum){
      middle = as.integer(item_num/2)
      item = item[(middle-half_segmentNum+1):(middle+half_segmentNum)]
      n=n+1
      loudnessMaxtime[n,] = item
    }  
  }  
  if(n>0){
    loudnessMaxtime = loudnessMaxtime[1:n,]
  }
  
  loudnessMaxtime
}

#saveRDS(pitchData, file="A_pitchdata_300seg.rds")

library(reticulate)
np <- import("numpy")
#np$savez("A_pitchdata_300seg.npz", getPitchData())



#filter songs data only with tags
print("Start to get songs data...")
songsData = getSongsData()
print(paste("The number of songs is: ",length(songsData$segnum)))
print("Start to write songs...")
fwrite(songsData, file = "allsongsdata.csv")

songsData = songsData[songsData$segnum >=segmentNum,]
print(paste("The number of 300seg-songs is: ",length(songsData$segnum)))
print("Start to write songs with more than 300 segments...")
fwrite(songsData, file = "allsongsdata_300seg.csv")


print("Start to get pitch data...")
pitchData=getPitchData()
np$savez("All_pitchdata_300seg_withtags.npz", pitchData)
save(pitchData,file="All_pitchdata_300seg_withtags.RData")
# pitchData = pitchData[,,index]
# 
print("Start to get timbre data...")
timbreData = getTimbreData()
np$savez("All_timbredata_300seg_withtags.npz", timbreData)
save(timbreData, file="All_timbredata_300seg_withtags.RData")
# timbreData = timbreData[,,index]
# 
print("Start to get loudness data...")
loudnessData = getLoudnessData()
# loudnessData = loudnessData[index,]
# 
print("Start to get loudness start data...")
loudnessStart = getLoudnessStart()
# loudnessStart=loudnessStart[index,]
# 
print("Start to get loudness maxtime data...")
loudnessMaxtime=getLoudnessMaxtime()
# loudnessMaxtime=loudnessMaxtime[index,]

#loudnessAll = array(0, c(length(index),segmentNum,3))
loudnessAll = array(0, c(length(songsData$segnum),segmentNum,3))
loudnessAll[,,1]=loudnessData
loudnessAll[,,2]=loudnessMaxtime
loudnessAll[,,3]=loudnessStart
np$savez("All_loudnessdata_300seg_withtags.npz", loudnessAll)
save(loudnessAll, file="All_loudnessdata_300seg_withtags.RData")





#np$savez("A_pitchdata_300seg_withtags_Y_new.npz",temp6)


# if(sum(is.na(loudnessAll))>0) {
#   print("true")
# }
