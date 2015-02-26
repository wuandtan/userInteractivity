#GenerateFreezeInfo: retrieve rebuffering information from the client log.
#the basic idea is (1) look at c.buffercount, which at the official website of Silverlight is explained:
#
#we'll add two columns for the single_episode, the one is the number of rebuffering, which is directly obtained from c.buffercount.
#but considerting the intial delay in the beginning and after the repositioning, we'll subtract 
GenerateFreezeInfo <- function(single_episode)
{
  
  serverInfo <- subset(single_episode[single_episode$type=="server",], select=c(time,segidx,sc.bytes,quality,episode))
  clientInfo <- subset(single_episode[single_episode$type=="client",],select=c(time,x.duration,c.playerState,c.buffercount,
                                                                               clientStartTime,c.starttime,playbackStartTime,numPause,pauseInfo,numJump,jumpInfo ))
  
  #adjusted.x.duration is adjusted based on jumping info
  clientInfo$adjusted.x.duration <- clientInfo$x.duration 
  #unjustified buffercount
  unjustifiedBuffercount <- clientInfo$c.buffercount[length(clientInfo$c.buffercount)]
  #get number of rebuffering
  
  #2014-12-11: I found that directly getting jumpInfo from jumpInfo is not accurate enough; for example, jumping to an odd second will make 
  #smooth streaming jump to the beginning of the segment. I try to use c.starttime to re-create the jumpInfo
  #jumpInfo <- single_episode$jumpInfo[!is.na(single_episode$jumpInfo)] #Originall I used this
  
  jump <- GetJumpInfo(clientInfo)
  clientInfo$numJump <- as.numeric(jump$numJump)
  clientInfo$jumpInfo <- as.character(jump$jumpInfo)
  jumpInfo <- clientInfo$jumpInfo
  to <- 0
  from <- 0
  idx <- NULL
  if(jump$numJump != 0)
  {
    jumpInfo <- jumpInfo[1]
    split_jumpinfo <- unlist(strsplit(jumpInfo,"_"))
    from <- as.numeric(split_jumpinfo[seq(2,length(split_jumpinfo),4)])
    to <- as.numeric(split_jumpinfo[seq(4,length(split_jumpinfo),4)])
    #clientInfo$adjusted.x.duration <- clientInfo$x.duration
    
    i <- 1
    adjusted_to <- c(0, to)
    j <- 1
  
    while (i < nrow(clientInfo)-1)
    {
      if(j<= length(from) && abs(clientInfo$adjusted.x.duration[i]- from[j]) <10 && clientInfo$x.duration[i+1]<= 5)
      {
        idx[j] <- i
        j <- j+1
        i <- i+1
        clientInfo$adjusted.x.duration[i:nrow(clientInfo)] <- clientInfo$x.duration[i:nrow(clientInfo)] + adjusted_to[j]
        
      }
      
      i <- i+1
    }
#     if(length(idx)==0)
#     {
#       #for unknown reasons, sometime the required "jump" is not execudted. for instance: the episode between 2014-12-02 18:06:05 and 2014-12-02 18:16:04
#       #to solve this problem, I have to re-assign the jump info as NA.
#       single_episode$jumpInfo <- NA
#       single_episode$numJump <- 0
#       from <- 0
#       to <- 0
#     }
  }
  
  #length(idx): number of jumping;
  #1: the beginning of the episode
  unjustifiedBuffercount <- unjustifiedBuffercount-length(idx) - 1
  if(unjustifiedBuffercount < 0)
    unjustifiedBuffercount <- 0
  #calculate the time that was spent for the rebuffering. Here we have to substract the time for the pause, 
  #and add (if jump backward) or substract (if just forward) the time for repositioning.
  #note we do not know exactly the freeze time. this is a rough estimate
  pauseInfo <- single_episode$pauseInfo[!is.na(single_episode$pauseInfo)]
  if(length(pauseInfo) != 0)
  {
    pauseInfo <- pauseInfo[1]
    d <- unlist(strsplit(pauseInfo,"_"))
    pauseTime <- sum(as.numeric(d[seq(4,length(d),4)]))
  }else
  {
    pauseTime <- 0
  }
  total_time <- as.numeric(as.duration(new_interval(clientInfo$time[1],clientInfo$time[length(clientInfo$time)])))
  #rebufferting time is equal to the totally_spent_time - (length_of_played_episode- jumping_time) - pause_time)
 
  #rebufferingTime <- total_time - (serverInfo$segidx[length(serverInfo$segidx)]-serverInfo$segidx[1]  - sum(to-from)) - pauseTime
  rebufferingTime <- total_time - (clientInfo$adjusted.x.duration[length(clientInfo$adjusted.x.duration)]  - sum(to-from)) - pauseTime
  single_episode$buffercount <- NA
  single_episode$rebufferingtime <- NA
  single_episode$buffercount[single_episode$type=="client"] <- unjustifiedBuffercount
  single_episode$rebufferingtime[single_episode$type=="client"] <- rebufferingTime
  single_episode$adjusted.x.duration[single_episode$type=="client"] <- clientInfo$adjusted.x.duration
  single_episode$numJump[single_episode$type=="client"] <- clientInfo$numJump
  single_episode$jumpInfo[single_episode$type=="client"] <- clientInfo$jumpInfo
  #return value
  single_episode
}


#get jump info from c.starttime field
GetJumpInfo <- function(clientInfo)
{
  #this is "to"
  st <- unique(clientInfo$c.starttime)
  if(length(st)==1)
  {
    #only 0, no jumping
    jumpInfo <- NA
    numJump <- 0
  }
  else
  {
  #  from <- NA
    to <- 0
    jumpInfo <- NULL
    numJump = length(st)-1
    for (i in (2:length(st)))
    {
      t <- which(clientInfo$c.starttime == st[i])
   
      to <- c(to, clientInfo$c.starttime[t[1]])
   #normally i should write it as follows. But sometimes at the jumping bounary, c.starttime is 0, probably because this variable was not attributed.
   #see the episode start from 2014-11-20 02:05:32. Ep[13], 11-20. So i have to go back one track more.
      #jumpInfo <- paste(jumpInfo,"f_",clientInfo$x.duration[t[1]-1]+to[i-1],"_t_",clientInfo$c.starttime[t[1]],"_",sep="")
      if(clientInfo$x.duration[t[1]-1] == 0)
        jumpInfo <- paste(jumpInfo,"f_",clientInfo$x.duration[t[1]-2]+to[i-1],"_t_",clientInfo$c.starttime[t[1]],"_",sep="")
        else
        jumpInfo <- paste(jumpInfo,"f_",clientInfo$x.duration[t[1]-1]+to[i-1],"_t_",clientInfo$c.starttime[t[1]],"_",sep="")
    }
  }
  jump <- data.frame(numJump = numJump, jumpInfo=jumpInfo)
  # return 
  jump
  
}