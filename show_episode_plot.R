#function: show the plot of episode, with the information of server side and the client side. 
#Jump, pause and freeze info will be shown to better understand the behavior of user interactivity behaviors.

show_episode_plot <- function(single_episode, episodeIdx)
{
  
  
  plot(single_episode$time, single_episode$segidx, pch=19,col="green",xlab = paste("time",single_episode$time[1]," - ",single_episode$time[length(single_episode$time)]),ylab = "seconds", main = paste("Episode: ",episodeIdx)); 
  grid(14,14)
  legend(10+single_episode$time[1],600,c("request","playback"),text.col = c("green","red"), pch = c(19,3))
  area <- par("usr")
  
  clientInfo <- single_episode[single_episode$type=="client",]
  #adjusted.x.duration is adjusted based on jumping info
  clientInfo$adjusted.x.duration <- clientInfo$x.duration 
  
  
  #append jumpInfo in the graph, if any
  jumpInfo <- single_episode$jumpInfo[!is.na(single_episode$jumpInfo)]
  if(length(jumpInfo) != 0)
  {
    jumpInfo <- jumpInfo[1]
    split_jumpinfo <- unlist(strsplit(jumpInfo,"_"))
    from <- as.numeric(split_jumpinfo[seq(2,length(split_jumpinfo),4)])
    to <- as.numeric(split_jumpinfo[seq(4,length(split_jumpinfo),4)])
    #clientInfo$adjusted.x.duration <- clientInfo$x.duration
    
    i <- 1
    to <- c(0, to)
    j <- 1
    idx <- NULL
    while (i < nrow(clientInfo)-1)
    {
      if(j<= length(from) && abs(clientInfo$adjusted.x.duration[i]- from[j]) < 10 && clientInfo$x.duration[i+1]<= 5 )
      {
        idx[j] <- i
        j <- j+1
        i <- i+1
        clientInfo$adjusted.x.duration[i:nrow(clientInfo)] <- clientInfo$x.duration[i:nrow(clientInfo)] + to[j]
      }
      
      i <- i+1
    }
    if(length(idx)==0)
    {
      #for unknown reasons, sometime the required "jump" is not execudted. for instance: the episode between 2014-12-02 18:06:05 and 2014-12-02 18:16:04
      #to solve this problem, I have to re-assign the jump info as NA.
      jumpInfo <- NA
      single_episode$jumpInfo <- NA
      single_episode$numJump <- 0
      points(single_episode$time, single_episode$x.duration,pch=3, col=2)
    }
    else
    {
      text(clientInfo$time[idx],100,"Jump",col=2,adj=c(0,0))
      segments(clientInfo$time[idx],area[3],clientInfo$time[idx],clientInfo$adjusted.x.duration[idx],col=2,lty = "dotted",lwd=5)
      points(clientInfo$time, clientInfo$adjusted.x.duration,pch=3, col=2)
    }
    
  }
  else
  {
    jumpInfo <- NA
    points(single_episode$time, single_episode$x.duration,pch=3, col=2)
  }
  
  #append pauseInfo in the graph, if any
  pauseInfo <- single_episode$pauseInfo[!is.na(single_episode$pauseInfo)]
  if(length(pauseInfo) != 0)
  {
    pauseInfo <- pauseInfo[1]
    t <- which(clientInfo$c.playerState=="Paused")
    d <- t[2:length(t)]-t[1:length(t)-1]
    #abline(v=clientInfo$time[t[c(1,which(d!=1),which(d!=1)+1, length(t))]],col=4,lty = "dotted",lwd=5)
    idx <- t[c(1,which(d!=1),which(d!=1)+1, length(t))]
    segments(clientInfo$time[idx],area[3],clientInfo$time[idx],clientInfo$adjusted.x.duration[idx],col=4,lty = "dotted",lwd=5)
    text(clientInfo$time[idx[seq(1,length(idx),2)]],100,"Pause",col=4,adj=c(0,0))
  }
  else
  {pauseInfo <- NA}
  
  #add text for Pause and Jump info 
  text(10+single_episode$time[1],450,paste("Pause:", pauseInfo), adj=c(0,0), col=4);
  text(10+single_episode$time[1],400,paste("Jump:",jumpInfo),adj=c(0,0),col=2);
  if("buffercount" %in% colnames(single_episode) )
  {
    text(10+single_episode$time[1],350,paste("nFreeze:", clientInfo$buffercount[1]), adj=c(0,0), col=4);
  }
  if("rebufferingtime" %in% colnames(single_episode) )
  {
    text(10+single_episode$time[1],300,paste("Freeze Time:", clientInfo$rebufferingtime[1]), adj=c(0,0), col=4);
  }
  t <- clientInfo$c.buffercount
  t <- sort(t)
  rebufferingPosition <- which(t[2:length(t)]-t[1:length(t)-1] != 0)
  if(length(rebufferingPosition) != 0)
    segments(clientInfo$time[rebufferingPosition], area[3],clientInfo$time[rebufferingPosition], 10,col=1,lty="longdash",lwd=5)
  
  c <- subset(single_episode, select = c(time, segidx, c.buffercount,c.playerState,x.duration,pauseInfo,jumpInfo,type))
  c
}