#read client log
readClientFile <- function(clientFn)
{
  # feat_vectors <- feature_extraction(episode)
  
  #client Log
  clientLog <- NULL
  
 # lines <- readLines(paste("logs/", clientFn,sep=""),4) #header
 lines <- readLines(clientFn,4) #header
  #lines[4] gives the descriptive name
  clientFields <- strsplit(lines[4]," ")
  clientFields <- clientFields[[1]][3:length(clientFields[[1]])]
  clientFields <- gsub("-",".",clientFields)
  clientContent <- read.table(clientFn,skip=4)
  colnames(clientContent) <- clientFields
  #remove all irrelevant requests
  clientContent <- clientContent[-grep("-",clientContent$c.playerState) , ]
  clientContent$time <- times(clientContent$time)- trunc(times(clientContent$time))
  clientContent$client_time <- as.POSIXct(paste(clientContent$date, clientContent$time));
  #only keep some columns
  keeps <- c("client_time","x.duration","c.playerState","cs.uri.stem","c.buffercount","clientStartTime","bandwidthAvg","c.starttime","playbackStartTime")
  clientContent <- clientContent[keeps]
  # clientContent<- clientContent[do.call(order,clientContent),]
  clientLog <- rbind(clientLog,clientContent)
  
  
  #seperate into a single episode
  episode_start_place <- grep("Initialized", clientLog$c.playerState)
  clientLog <- clientLog[episode_start_place[1]:nrow(clientLog),] #remove the beginning few lines which do not have the initialization.
  #remove a few multiple "Initialized" state. This happens when the bandwidh is limited
  #the criterion is: if two "Initialized"s are too close( <20 second), only keep the first one  
  episode_start_place <- grep("Initialized", clientLog$c.playerState)
  dup_start <- NULL
  for (i in (1:length(episode_start_place)))
  {
    d <- difftime(clientLog$client_time[episode_start_place[i+1:length(episode_start_place)]] , clientLog$client_time[episode_start_place[i]], units="secs")
    dup_start <- c(dup_start,i + which(d<20))
  }
  if(length(dup_start) != 0)
    clientLog <- clientLog[-episode_start_place[dup_start],]
  
  t <- grep("Initialized", clientLog$c.playerState)
  
  clientLog$Idxepisode <- rep(1:length(t),c(t[2:length(t)]-t[1:length(t)-1],nrow(clientLog)-t[length(t)]+1))
  client_episode <- split(clientLog,clientLog$Idxepisode)
  
  for (i in (1:length(client_episode)))
  {

    row <- nrow(client_episode[[i]])
    #single_episode <- data.frame(FromTime = clientLog[[i]]$time[1], ToTime = clientLog[[i]]$time[row],cs.uri.stem = clientLog[[i]]$cs.uri.stem[1], c.buffercount = clientLog[[i]]$c.buffercount[row])
    pause_jump_info <- pause_jump_in_practice(client_episode[[i]]$cs.uri.stem[1])
    client_episode[[i]] <- cbind(client_episode[[i]], cbind(numPause = rep(pause_jump_info$numPause, row),numJump = rep(pause_jump_info$numJump, row),pauseInfo = rep(as.character(pause_jump_info$pauseInfo), row), jumpInfo = rep(as.character(pause_jump_info$jumpInfo), row)))
    client_episode[[i]] <- subset(client_episode[[i]], select = -cs.uri.stem)
    
    #re-order the client report, this especially is important for re-positioning, where the order of report can be reverse.
   # client_episode[[i]] <- client_episode[[i]][order(client_episode[[i]]$client_time, -client_episode[[i]]$x.duration),]
  }
 
  #return value  
  client_episode
}  

# function: to extract pause and jump information in reality. Note that extracting information from cs.uri.stem is not enough
#because in some cases, for instance, Pause_position: 200, duration:10, Jump_from 190 to 210.
#then the pause is not executed. that's why we need to re-order them.
#actually, this much be done in the design of advanced logging, but I forgot it, so have to do this here

pause_jump_in_practice <- function(cs.uri.stem)
{
  totalSeconds <- 596 # the length of BIG_BUCK_BUNNY is 596 second
  
  pause_and_jump_info <- gsub(".log","",gsub("/Big_Buck_Bunny_ui/","",cs.uri.stem))
  t <- strsplit(pause_and_jump_info,"_")
  nPauseExecuted <- rep(0,length(t))
  nJumpExecuted <- rep(0,length(t))
  
  pauseInfo <- NULL
  jumpInfo <- NULL
  for (i in 1:length(t))
  {
    numPause <- as.numeric(t[[i]][which(t[[i]]=="numPause")+1])
    Position <- as.numeric(t[[i]][which(t[[i]]=="Position")+1])
    Duration <- as.numeric(t[[i]][which(t[[i]]=="Duration")+1])
    
    numJump <- as.numeric(t[[i]][which(t[[i]]=="numJump")+1])
    From <- as.numeric(t[[i]][which(t[[i]]=="from")+1])
    To <- as.numeric(t[[i]][which(t[[i]]=="To")+1])
    
    r <- rep(0,totalSeconds)  
    
    j <- 0
    
    single_pauseInfo <- NULL
    single_jumpInfo <- NULL
    while (j < totalSeconds)
    {
    
      if((j %in% Position) && r[j]==0)
      {
        fi_pos <- which (Position %in% j)
        if(length(fi_pos)>1)
          fi_pos <- fi_pos[1]
        nPauseExecuted[i] <- nPauseExecuted[i] +1
        single_pauseInfo <- paste(single_pauseInfo,"p_",j,"_d_",Duration[fi_pos],"_",sep="")
        r[j] <- 1;
        Duration <- Duration[-fi_pos]
        Position <- Position[-fi_pos]
      }
      if((j %in% From) && r[j]==0)
      {
        fi_pos <- which (From %in% j)
        if(length(fi_pos)>1)
          fi_pos <- fi_pos[1]
        nJumpExecuted[i] <- nJumpExecuted[i]+1 
        single_jumpInfo <- paste(single_jumpInfo,"f_",j,"_t_",To[fi_pos],"_",sep="")
        r[j] <- 1;
        j <- To[fi_pos]
        From <- From[-fi_pos]
        To <- To[-fi_pos]
        
      }
      j <- j+1;
    }
    if(nPauseExecuted[i]==0)
    {pauseInfo <- rbind(pauseInfo,NA)
    }else    
    {pauseInfo <- rbind(pauseInfo,single_pauseInfo)}
    
    if(nJumpExecuted[i]==0)
    {jumpInfo <- rbind(jumpInfo, NA)
    }    else
    {jumpInfo <- rbind(jumpInfo, single_jumpInfo)}
    
    #pause_jump_info <- rbind(pause_jump_info,paste(pauseInfo,"_",jumpInfo,sep=""))
  }
  rownames(pauseInfo) <- NULL
  rownames(jumpInfo) <- NULL
  c <- data.frame(numPause = nPauseExecuted, numJump = nJumpExecuted, pauseInfo = pauseInfo, jumpInfo = jumpInfo)
  c
}
