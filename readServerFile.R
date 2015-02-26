readServerFile <- function(serverFn)
{
  
  #lines <- readLines(paste("logs/", serverFn,sep=""),4) #header
  lines <- readLines(serverFn,4) #header
  #lines[4] gives the descriptive name
  serverFields <- strsplit(lines[4]," ")
  serverFields <- serverFields[[1]][3:length(serverFields[[1]])]
  serverFields <- gsub("-",".",serverFields)
  serverContent <- read.table(serverFn,skip=4)
  colnames(serverContent) <- serverFields
  #remove all audio requests
  serverContent <- serverContent[-grep("audio",serverContent$cs.uri.query) , ]
  #remove log which cs.uri.query == '-'
  serverContent <-  serverContent[serverContent$cs.uri.query != "-", ]
  #keep the time for retrievent Manifest file, as a clue for a new episode
  manifestContent <- serverContent[grep("Manifest",serverContent$cs.uri.query) , ]
  #remove log which cs.uri.query == "Manifest"
  serverContent <- serverContent[-grep("Manifest",serverContent$cs.uri.query) , ]
  if(length(grep("manifest",serverContent$cs.uri.query)>0))
  {
    serverContent <- serverContent[-grep("manifest",serverContent$cs.uri.query) , ]
  }
  #only keep some columns
  keeps <- c("date","time","cs.uri.query","sc.bytes")
  serverContent <- serverContent[keeps]
  #manifestContent$time <- times(manifestContent$time)- trunc(times(manifestContent$time))
  manifestContent$time <- as.POSIXct(paste(manifestContent$date,manifestContent$time))
  manifestContent <- manifestContent[c("time")]
  #extract quality and segment index information
  serverQuality_SegNum <- matrix(t(as.numeric(unlist(strsplit(gsub(")","",gsub("QualityLevels\\(","",as.character(serverContent$cs.uri.query))),"&Fragments\\(video=")))),ncol=2,byrow=TRUE)
  serverQuality_SegNum[, 2] <- serverQuality_SegNum[, 2]/10^7 #convert time from milisecond to second
  serverContent$quality <- serverQuality_SegNum[,1]
  serverContent$segidx <- serverQuality_SegNum[,2]
  #serverContent$timestamp <- paste(serverContent$date, serverContent$time)
  serverContent$time <- times(serverContent$time)- trunc(times(serverContent$time))
  serverContent$time <- as.POSIXct(paste(serverContent$date, serverContent$time));
  serverContent <- serverContent[c("time","segidx","sc.bytes", "quality")] #remove cs.uri.query, date and time
  
  
  serverLog <- serverContent;
  manifestLog <- manifestContent;
  
 
  
  
  #separate serverLog into multiple episodes, according to the time of loading manifest file
  serverLog$episode <- NA
  for (i in 1:nrow(manifestLog)-1)
  {
    duration_1 <- as.numeric(as.duration(new_interval(manifestLog$time[i],serverLog$time)))
    duration_2 <- as.numeric(as.duration(new_interval(manifestLog$time[i+1],serverLog$time)))
    #this was supposed to be duration_2<0, because some beginning requests are very close to manifest. to reduct 
    #this confusion, we set duration_2< -2
    serverLog$episode[duration_1 >= -2 & duration_2< -2] <- i   
  }
  serverLog$episode[is.na(serverLog$episode)] <- nrow(manifestLog)
  #split into different episodes
  server_episode <- split(serverLog,serverLog$episode)
  #for unknown reasons, after loading html, before loading manifest, Smooth Streaming SOMETIMES requests video=0 and audio=0
  # the following action delete them.
  for (i in 1:length(server_episode))
  {
    if((server_episode[[i]]$segidx[nrow(server_episode[[i]])] == 0))
      server_episode[[i]] <- server_episode[[i]][1:nrow(server_episode[[i]])-1,]
    if((server_episode[[i]]$segidx[nrow(server_episode[[i]])] == 0))
      server_episode[[i]] <- server_episode[[i]][1:nrow(server_episode[[i]])-1,]
    
    if((server_episode[[i]]$segidx[1] == 0) && (server_episode[[i]]$segidx[2] == 0))
      server_episode[[i]] <- server_episode[[i]][2:nrow(server_episode[[i]]),]
  }
  ###"Fri Feb 06 14:17:51 2015"
  ###try to delete the duplicated segments. The criterion is: if there are multiple requests for a segment
  #with different qualities, we keep the last one whose sc.bytes is not 0.
  for (i in 1:length(server_episode))
  {
   
    removed_rows <- NULL
    
    
    for (j in 1:(length(server_episode[[i]]$segidx)-1))
    {
      pointer <- j+1 #begin
      e <- j
      while (1)
      {
        if(server_episode[[i]]$segidx[j] == server_episode[[i]]$segidx[pointer])
        {
          pointer <- pointer+1
          if(pointer > length(server_episode[[i]]$segidx))
          {
            e <- pointer -1
            break
          }
        }else
        {
          e <- pointer-1
          break
        }
      }
      if(j !=e )
      {
        scBytes <- server_episode[[i]]$sc.bytes[j:e]
        keep_idx <- NULL
        min_Quality <- 2962000
        for (k in 1:length(scBytes))
        {
           if((scBytes[k]<min_Quality) && (scBytes[k] != 0))
           {
             min_Quality <- scBytes[k]
             keep_idx <- k
           }
        }
        tiny_removed_idx <- j:e
        
        tiny_removed_idx <- tiny_removed_idx[-k]
        removed_rows <- c(removed_rows,tiny_removed_idx)
      }
      
      
    }
    if(length(removed_rows) > 1)
    {
      server_episode[[i]] <- server_episode[[i]][-removed_rows,]
    }
  }
  
  #return value  
  server_episode
}
