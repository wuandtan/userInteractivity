userInteractivity_in_HAS <- function(fn = "test.txt")
{ 
  
  rm(list=ls())
  require(chron)
  library(lubridate)
  library(plyr)
  setwd("C://Work//Projects//Projects//ML-based Session Reconstruction//UserInteractivity");
  
  debugSource("readClientFile.R")
  debugSource("readServerFile.R")
  debugSource("MakeEpisode.R")
  debugSource("show_episode_plot.R")
  debugSource("GenerateFreezeInfo.R")
  debugSource("feature_extraction.R")
  debugSource("writeArffHeader.R")
  
  
  segmentLen <- 2
  Num_place_for_pause <- 1
  chosen_window_size <- 15
  Num_place_for_freeze <- 5
  filename <- c("feature_all_debug.arff")
  allFn <- dir("logs/biglog")
  #get the file names of client log and server logs
  clientFn <- allFn[grep("Client",allFn)]
  serverFn <- allFn[grep("Server",allFn)]
  
  
  
  for (dday in (37:length(serverFn)))
#   for (dday in (1:1))
  {
    server_episode <- readServerFile(paste("logs/biglog/", serverFn[dday],sep="")) 
    client_episode <- readClientFile(paste("logs/biglog/", clientFn[dday],sep=""))
    episode <- MakeEpisode(server_episode,client_episode)
    if(dday == 1)
    {
      filename <- c("feature_train_tmp.arff")
      writeArffHeader(filename,Num_place_for_pause,Num_place_for_freeze,max_window_size)    
    }
    if(dday == 37)
    {
      filename <- c("feature_test_tmp.arff")
      writeArffHeader(filename,Num_place_for_pause,Num_place_for_freeze,max_window_size)    
    }
    
    for(epIdx in (2:length(episode)))
    {
      show_episode_plot(episode[[epIdx]],epIdx)
      print(paste(episode[[epIdx]]$time[1], "   day: ",dday, "   ep: ", epIdx))
      episode[[epIdx]] <- GenerateFreezeInfo(episode[[epIdx]])
      #feature extraction is here!
      feature <- feature_extraction(episode[[epIdx]],segmentLen,Num_place_for_pause,Num_place_for_freeze ,chosen_window_size )
      if(!is.null(feature) && nrow(feature)>0)
      {
        for (i in (1:nrow(feature)))
        {
          cat(sprintf("\"%s\", ",episode[[epIdx]]$time[1]),file=filename,append=TRUE)
          if(any(is.na(feature[i,1:(ncol(feature)-1)])))
          {
            for(j in 1:(ncol(feature)-1))
            {
              if(is.na(feature[i,j]))
                cat("?  ,  ",file=filename,append=TRUE)
              else
                cat(sprintf("%3.1f , ",feature[i,j]),file=filename,append=TRUE)
            }
            
          }else
          {
            cat(sprintf("%3.1f , ",feature[i,1:(ncol(feature)-1)]),file=filename,append=TRUE)
          }
          
          cat(feature[i,ncol(feature)],file=filename,append=TRUE)
          cat("\n",file=filename,append=TRUE)
        }
        
        #         if(any(is.na(feature[1:(length(feature)-7)])))
        #         {
        #           for(i in 1:(length(feature)-7))
        #           {
        #             if(is.na(feature[i]))
        #                 cat("?  ,  ",file=filename,append=TRUE)
        #               else
        #                 cat(sprintf("%3.1f , ",feature[i]),file=filename,append=TRUE)
        #           }
        #         }
        #         else
        #           {
        #             cat(sprintf("%3.1f , ",feature[1:(length(feature)-7)]),file=filename,append=TRUE)
        #           }
        #         
        #         cat(feature[(length(feature)-6):length(feature)],file=filename,append=TRUE, sep=", ")
        #         cat("\n",file=filename,append=TRUE)
        #       }
        #      
        #c <- show_episode_plot(episode[[epIdx]],epIdx)
        
        #print(c)
        #check ep 65. 2014-12-11
        #line <- readline()
      }
      
    }
    
    
  }
  
}



