#merge the client info with the server info. Look for matching episodes from client info for server info
MakeEpisode <- function(sever_episode, client_episode)
{
  client_name <- names(client_episode[[1]])
  server_name <- names(episode[[1]])
  
  for (i in (1:length(episode)))
  {
    for (j in (i:length(client_episode)))
    {
      t <- as.numeric(as.duration(new_interval(episode[[i]]$time[1],client_episode[[j]]$client_time[1])))
      if(abs(t)<5)
      {
        tmp_server_episode <- episode[[i]]
        
        for (k in (1:length(client_name)))
        {
          tmp_server_episode[client_name] <- NA
        }
        tmp_server_episode$type <- c("server")
        
        tmp_client_episode <- client_episode[[j]]
        for (k in (1:length(server_name)))
        {
          tmp_client_episode[server_name[k]] <- NA
        }
        tmp_client_episode$time <- tmp_client_episode$client_time
        tmp_client_episode$type <- c("client")
        keeps <- colnames(tmp_server_episode);
        tmp_client_episode <- tmp_client_episode[keeps] # make the same order
        
        episode[[i]] <- rbind(tmp_server_episode, tmp_client_episode)
        
        #episode[[i]] <- t[do.call(order,t),]
        episode[[i]] <- subset(episode[[i]], select = -c(client_time, bandwidthAvg,Idxepisode))
        episode[[i]]$x.duration <- as.numeric(episode[[i]]$x.duration)
        episode[[i]]$c.starttime <- as.numeric(episode[[i]]$c.starttime)
        t <- episode[[i]]
        episode[[i]] <- arrange(t,time, c.starttime,x.duration) #t[order(t$time,t$segidx,-t$x.duration), ]
        break
      }
      
    }
    
  }
  #return
  episode
}
