feature_extraction <- function (single_episode,segmentLen = 2,Num_place_for_pause = 2,Num_place_for_freeze = 5, chosen_window_size = 15)
{
        #here i am thinking to (1) see if there is any re-positioning. if yes, then start to consider the period between each re-positioning
        #(2) among the whole episode, choose the 
        quality <- c(230000,  331000,  477000,  688000,  991000, 1427000, 2056000, 2962000)
        
        #we assume segment length is 2
        
        #here we can only use the serverInfo as the input to extract the featres.
        #the idea is to get the symptoms for re-positioning in the beginning. usually the re-position is relatively easy to be identified.
        
        serverInfo <- subset(single_episode[single_episode$type=="server",], select=c(time,segidx,sc.bytes,quality,episode))
        clientInfo <- subset(single_episode[single_episode$type=="client",],select=c(time,x.duration,c.playerState,buffercount,
                                                                                     c.starttime,numPause,pauseInfo,numJump,jumpInfo,rebufferingtime,adjusted.x.duration ))
        
        serverInfo <- arrange(serverInfo,time, segidx)  
        serverInforow <- nrow(serverInfo)
        clientInforow <- nrow(clientInfo)
        
        if(serverInforow <= 20)
        {
                feature <- NULL
                
                return(feature)
        }
        
        isShutdown <- grep("Shutdown",clientInfo$c.playerState)
        if(length(isShutdown) > 0)
        {
                feature <- NULL
                return(feature)
        }
        ##########first to extract the features for re-positioning
        #######the feature used in the training and in the test is the same.
        ###########note that we assume there is no caching between the server and the client
        
        #an important factor for the repositioning is the discontinuity of segidx
        Feat_discontinuity <- 0
        jump_pos <- NULL
        for (i in (3:(serverInforow-2)))
        {
                t <- serverInfo$segidx[(i-2):(i+2)] - serverInfo$segidx[i]
                
                if(!any(t == segmentLen ) )
                { #print(i)
                        #print(serverInfo$segidx[i])
                        if(length(which(t==0)) <= 1)
                        {
                                Feat_discontinuity <- Feat_discontinuity + 1
                                jump_pos <- c(jump_pos, i)
                        }
                }
                
        }
        jump_pos <- c(0,jump_pos,serverInforow)
        #calculate the moving average of the inter-segment time between adjacent segments
        ma <- function(x,n=5){filter(x,rep(1/n,n), sides=1)} 
        ave_sequence <- NULL
        idx_sequence <- NULL
        chosen_window_size <- 15
        for (pos in (1:(length(jump_pos)-1)))
        {
                #seperate the episode into a few period, in each of which only contains one jump.
                serverInfo_in_each_jump <- serverInfo[(jump_pos[pos]+1):jump_pos[pos+1],]
                r <- nrow(serverInfo_in_each_jump)
                interval <- as.numeric(as.duration(serverInfo_in_each_jump$time[2:r]-serverInfo_in_each_jump$time[1:(r-1)]))
                if(length(interval) > chosen_window_size)
                {
                        t1 <- cumsum(interval[1:chosen_window_size])/(1:chosen_window_size)
                        t2 <- ma(interval,chosen_window_size)
                        ave <- c(t1,t2[chosen_window_size:length(t2)])
                }else
                {
                        ave <- cumsum(interval)/(1:length(interval))
                }
                ave_sequence <- c(ave_sequence,ave)
                # idx_sequence <- c(idx_sequence,(jump_pos[pos]+1):jump_pos[pos+1])
                idx_sequence <- c(idx_sequence,1:length(ave))
        }
        
        #suspicious_place <- sort.int(ave_sequence, na.rm=TRUE, decreasing=TRUE,index.return=TRUE)
        #look for the peaks
        peaks <- NULL
        l <- length(ave_sequence)
        
        peaks_pos <- which((ave_sequence[2:(l-2)]> ave_sequence[1:(l-3)]) & (ave_sequence[2:(l-2)]> ave_sequence[3:(l-1)]))
        if(length(peaks_pos)<Num_place_for_freeze)
        { #when bandwith is very large, the client fast reaches the steady state and never goes down. This could happen: no peaks
                suspicious_place <- sort.int(ave_sequence, decreasing = TRUE,index.return = TRUE) 
                peaks <- suspicious_place$x[1:Num_place_for_freeze]
                relative_peaks_pos <- idx_sequence[suspicious_place$ix[1:Num_place_for_freeze]]
                ori_peaks_pos <- suspicious_place$ix[1:Num_place_for_freeze]
        }else
        {
                peaks_pos <- peaks_pos +1 #because the previous statement starts from the second position.
                peaks <- ave_sequence[peaks_pos]
                relative_peaks_pos <- idx_sequence[peaks_pos]
                suspicious_place <- sort.int(peaks, decreasing=TRUE,index.return=TRUE)
                #select the top 5 
                peaks <- suspicious_place$x[1:Num_place_for_freeze]
                relative_peaks_pos <- relative_peaks_pos[suspicious_place$ix[1:Num_place_for_freeze]]
                ori_peaks_pos <- peaks_pos[suspicious_place$ix[1:Num_place_for_freeze]]
                
        }
        frequency_Window_size_in_seconds <- 10 #the window size to check how fast the client requests the segments
        nSegment_within_Window <- frequency_Window_size_in_seconds/segmentLen
        
        feature <- NULL
        interval <- as.numeric(as.duration(serverInfo$time[2:serverInforow]-serverInfo$time[1:(serverInforow-1)]))
        for (i in (1:Num_place_for_freeze))
        {
                #features for a single suspicious place
                if(ori_peaks_pos[i]+nSegment_within_Window < serverInforow)
                {
                        freq <- mean(interval[(ori_peaks_pos[i]+1):(ori_peaks_pos[i]+nSegment_within_Window)])
                }
                else
                {
                        freq <- mean(interval[(ori_peaks_pos[i]+1):(serverInforow-1)])
                }
                feat <- c(peaks[i],relative_peaks_pos[i],which(quality==serverInfo$quality[ori_peaks_pos[i]]),which(quality==serverInfo$quality[ori_peaks_pos[i]+1]),freq) 
                #ground truth for this single suspicious place
                #if nothing, 0; if pause, 1, if short freeze, 2, if long freeze 2
                class <- ground_truth_retrieval(serverInfo,clientInfo,ori_peaks_pos[i])
                
                feature <- rbind(feature,c(feat,class))
        }
        feature 
}

ground_truth_retrieval <- function(serverInfo,clientInfo,pos)
{
        time <- serverInfo$time[pos]
        freeze_due_to_repositioning <- 0
        #look around this time at the client side
        
        t <- which((clientInfo$time-serverInfo$time[pos] < 0)== TRUE)
        client_range_begin <- t[length(t)]
        if((length(client_range_begin)==0) || (client_range_begin - 1 < 1)  )
        {
                client_range_begin <- 1;
        } else 
        {
                client_range_begin <- client_range_begin - 1;
        }
        
        t <- which((clientInfo$time-serverInfo$time[pos] > 0)== TRUE)
        if(length(t)>0)
        {
                if(t[1]+2< nrow(clientInfo))
                        client_range_end <- t[1]+1
                else
                        client_range_end <- nrow(clientInfo)
        }else{
                #unlikely happen
                print("client_range_end reaches the end of the clientInfo! Unlikely happen!")
                client_range_end <- nrow(clientInfo)
        }
        
        #now we have client_range_begin and client_range_end
        class <- NULL
        IsPause <- grep("Pause",clientInfo$c.playerState[client_range_begin:client_range_end])
        if(length(IsPause)>0)
        {
                class <- 1
                return(class)
        }
        #not a pause, then is it a freeze? will look back and look fordward and see it is a short freeze or a long freeze
        #or maybe the freeze is caused by the re-positioning?
        st <- client_range_begin
        en <- client_range_end
        
        if(clientInfo$numJump[1] > 0)
        {
                split_jumpinfo <- unlist(strsplit(clientInfo$jumpInfo[1],"_"))
                from <- as.numeric(split_jumpinfo[seq(2,length(split_jumpinfo),4)])
                for (i in (st:en))
                {
                        if(any(abs(from-clientInfo$adjusted.x.duration[i])<2))
                        {
                                freeze_due_to_repositioning <- 1
                                break
                        }
                }
        }
        
        
        while (st>=1 && st>=client_range_begin - 5)
        {
                log_time_diff <- clientInfo$time[st+1]-clientInfo$time[st]
                if(clientInfo$adjusted.x.duration[st+1] >= clientInfo$adjusted.x.duration[st] && clientInfo$adjusted.x.duration[st+1] <= clientInfo$adjusted.x.duration[st]+10)
                {
                        #to garanttee no jumping, still want to play back in order
                        playback_time_diff <- clientInfo$adjusted.x.duration[st+1]-clientInfo$adjusted.x.duration[st]
                        if(log_time_diff == playback_time_diff)
                                break
                }
                else
                {#there might be a re-positioning
                        
                        #check if around there is a re-positioning
                        if(clientInfo$numJump[1] > 0)
                        {
                                split_jumpinfo <- unlist(strsplit(clientInfo$jumpInfo[1],"_"))
                                from <- as.numeric(split_jumpinfo[seq(2,length(split_jumpinfo),4)])
                                if(any(abs(from-clientInfo$adjusted.x.duration[st])<2))
                                {
                                        freeze_due_to_repositioning <- 1
                                        break
                                }
                                
                        }
                }
                st <- st - 1
        }
        if(st == 0)
                st <- 1
        
        
        while (en < nrow(clientInfo) && en<=client_range_end + 5)
        {
                log_time_diff <- clientInfo$time[en+1]-clientInfo$time[en]
                if(clientInfo$adjusted.x.duration[en+1] >= clientInfo$adjusted.x.duration[en] && clientInfo$adjusted.x.duration[en+1] <= clientInfo$adjusted.x.duration[en]+10)
                {
                        #to garanttee no jumping, still want to play back in order
                        playback_time_diff <- clientInfo$adjusted.x.duration[en+1]-clientInfo$adjusted.x.duration[en]
                        if(log_time_diff == playback_time_diff)
                                break
                }
                else
                {#there is a re-positioning
                        
                        if(clientInfo$numJump[1] > 0)
                        {
                                split_jumpinfo <- unlist(strsplit(clientInfo$jumpInfo[1],"_"))
                                from <- as.numeric(split_jumpinfo[seq(2,length(split_jumpinfo),4)])
                                if(any(abs(from-clientInfo$adjusted.x.duration[en])<2))
                                {
                                        freeze_due_to_repositioning <- 1
                                        break
                                }
                                
                        }
                }
                en <- en + 1
        }
        
        if(freeze_due_to_repositioning == 1)
        {
                class <- 4
                return(class)
        }
        log_time_diff <- difftime(clientInfo$time[en],clientInfo$time[st],units = "secs")
        playback_time_diff <- clientInfo$adjusted.x.duration[en]-clientInfo$adjusted.x.duration[st]
        IsPause <- grep("Pause",clientInfo$c.playerState[st:en])
        if(length(IsPause)>0)
        {
                class <- 1
                return(class)
        }
        
        lag <- log_time_diff - playback_time_diff
        if(lag <=2)
        {
                class <- 0
                return(class)
        }
        if(lag <=10)
        {
                class <- 2
                return(class)
        }
        if(lag >10)
        {
                class <- 3
                return(class)
        }
        
}
#feature_extraction_old_individual_task: we extract features for each episode for each single task, like jumping, pause ,or repositioning. The input is the individual episodes with  "time" "sc.bytes" "quality"  "segidx"   "episode" columns

feature_extraction_old_individual_task <- function (single_episode,segmentLen = 2,Num_place_for_pause = 2,Num_place_for_freeze = 2, max_window_size = 20)
{
        quality <- c(230000,  331000,  477000,  688000,  991000, 1427000, 2056000, 2962000)
        
        #we assume segment length is 2
        
        #here we can only use the serverInfo as the input to extract the featres.
        #the idea is to get the symptoms for re-positioning in the beginning. usually the re-position is relatively easy to be identified.
        
        serverInfo <- subset(single_episode[single_episode$type=="server",], select=c(time,segidx,sc.bytes,quality,episode))
        clientInfo <- subset(single_episode[single_episode$type=="client",],select=c(time,x.duration,c.playerState,buffercount,
                                                                                     clientStartTime,c.starttime,playbackStartTime,numPause,pauseInfo,numJump,jumpInfo,rebufferingtime,adjusted.x.duration ))
        serverInfo <- arrange(serverInfo,time, segidx)  
        serverInforow <- nrow(serverInfo)
        clientInforow <- nrow(clientInfo)
        
        if(serverInforow <= 20)
        {
                feature <- NULL
                
                return(feature)
        }
        
        ##########first to extract the features for re-positioning
        #######the feature used in the training and in the test is the same.
        ###########note that we assume there is no caching between the server and the client
        
        #an important factor for the repositioning is the discontinuity of segidx
        Feat_discontinuity <- 0
        
        for (i in (3:(serverInforow-2)))
        {
                if(!any(serverInfo$segidx[(i-2):(i+2)] - serverInfo$segidx[i] == segmentLen ))
                { #print(i)
                        #print(serverInfo$segidx[i])
                        Feat_discontinuity <- Feat_discontinuity + 1
                }
                
        }
        #a small forward reposition is problemic: when the client have a long, e.g 20 second buffer, if it re-positions to 10 seconds ahead, the server side will not receive
        #the notice. 
        #another noticeable feature for re-positioning could be the time difference between the seconds of the delivered segments (2seconds * number of segments) and 
        #the time duration from the first delivery to the last delivery. 
        #this feature is expected to be efficient because both freeze and pause enlarge the real playback time, while forward repositioning reduces the real playback time.
        #so when neither freeze nor pause happens, by this way small forward repositioning can be identified. However, when freeze or pause exists, the feature looses its effect.
        Feat_Episode_time_difference <- 0
        Feat_Episode_time_difference <- as.numeric(as.duration(new_interval(serverInfo$time[1],serverInfo$time[serverInforow]))) - as.numeric(serverInfo$segidx[serverInforow]-serverInfo$segidx[1])
        
        if(clientInfo$numJump[1] == 0)
                Class_repositioning <- 0
        else
                Class_repositioning <- 1
        
        #########features for pause
        #########the features used in training and in the test is DIFFERENT.
        
        ##for pause training ##
        
        #Num_place_for_pause <- 2 #obain the two most possible places for occuring pause
        frequency_Window_size_in_seconds <- 10 #the window size to check how fast the client requests the segments
        nSegment_within_Window <- frequency_Window_size_in_seconds/segmentLen
        
        pauseInfo <- clientInfo$pauseInfo[1]
        if(is.na(pauseInfo))
        {
                #obtain the two most possible place
                Class_Pause <- 0
                Feat_train_pause <- no_pause(serverInfo,Num_place_for_pause,quality,nSegment_within_Window)
        }else
        {
                
                idx <- which(clientInfo$c.playerState=="Paused")
                if(length(idx)==0)
                {
                        Class_Pause <- 0
                        Feat_train_pause <- no_pause(serverInfo,Num_place_for_pause,quality,nSegment_within_Window) 
                }else
                {
                        #Class_Pause <- as.numeric(clientInfo$numPause[1])
                        Class_Pause <- 1
                        Feat_train_pause <- has_pause(serverInfo,clientInfo,Num_place_for_pause,quality,nSegment_within_Window)
                }
                
        }
        
        ##for pause test ##
        interval <- as.numeric(as.duration(serverInfo$time[2:serverInforow]-serverInfo$time[1:(serverInforow-1)]))
        k <-sort.int(interval, decreasing = TRUE,index.return = TRUE) 
        k <- k$ix[1:Num_place_for_pause]
        Feat_test_pause <- NULL
        for (i in (1:Num_place_for_pause))
        {
                if(k[i]+nSegment_within_Window < serverInforow)
                {
                        freq <- mean(interval[(k[i]+1):(k[i]+nSegment_within_Window)])
                }
                else
                {
                        freq <- mean(interval[(k[i]+1):serverInforow])
                }
                Feat_test_pause <- c(Feat_test_pause,interval[k[i]],which(quality==serverInfo$quality[k[i]]),which(quality==serverInfo$quality[k[i]+1]), freq)
                
        }
        
        
        ########Features for re-buffering
        #here I try not to consider anything about repositioning or pause (pretending that they do not exist), and use the old way (the same as in the IWQos paper) to extract 
        #the features, and see how much classification we can obtained. In the future, we'll add more the above two events and obtain better results.
        
        
        Class_Freeze_nonFreeze <- NULL
        Class_max_Freeze <- NULL
        Class_multiFreeze <- NULL
        Class_totalFreezeTime <- NULL
        
        
        
        ma <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
        interval <- as.numeric(as.duration(serverInfo$time[2:serverInforow]-serverInfo$time[1:(serverInforow-1)]))
        Feat_freeze <- NULL
        for (i in (1:max_window_size))
        {
                #calcuate the moving average within the window size
                ave <- sort(ma(interval,i), na.rm=TRUE, decreasing=TRUE)
                Feat_freeze <- c(Feat_freeze,ave[1:Num_place_for_freeze])
        }
        if(clientInfo$rebufferingtime[1] > 2)
        {
                Class_Freeze_nonFreeze <- 1
                #looking for the longest single freeze
                t <- clientInfo$adjusted.x.duration[2:clientInforow] - clientInfo$adjusted.x.duration[1:clientInforow-1]
                b <- 1
                e <- 1
                count <- 0
                maxCount <- 0
                longest_freeze <- 0
                for (j in (1:length(t)))
                {
                        if(t[j] == 0)
                        {
                                if(count ==0)
                                        tmp_b <- j
                                count <- count+1
                                
                        }else
                        {
                                if(count !=0 && count >= maxCount)
                                {
                                        tmp_freeze <- as.numeric(as.duration(clientInfo$time[j] - clientInfo$time[tmp_b]))
                                        if(tmp_freeze > longest_freeze)                                   
                                        {e <- j
                                         b <- tmp_b
                                         maxCount <- count
                                         longest_freeze <- tmp_freeze
                                        }
                                }
                                count <- 0
                        }
                }
                if(longest_freeze>10)
                        Class_max_Freeze <- 1
                else
                        Class_max_Freeze <- 0
        }else{
                Class_Freeze_nonFreeze <- 0
                Class_max_Freeze <- 0
        }
        Class_totalFreezeTime <- clientInfo$rebufferingtime[1]
        if(clientInfo$buffercount[1]>=2)
                Class_multiFreeze <- 1
        else
                Class_multiFreeze <- 0
        
        #integrate all features
        feature <-c(Feat_discontinuity,Feat_Episode_time_difference,Feat_train_pause,Feat_test_pause,Feat_freeze,
                    Class_repositioning,Class_Pause, 
                    Class_Freeze_nonFreeze,Class_max_Freeze,Class_multiFreeze,Class_totalFreezeTime)
        feature
        
}

no_pause <- function(serverInfo,Num_place_for_pause,quality,nSegment_within_Window)
{
        serverInforow <- nrow(serverInfo)
        feat <- NULL
        interval <- as.numeric(as.duration(serverInfo$time[2:serverInforow]-serverInfo$time[1:(serverInforow-1)]))
        k <-sort.int(interval, decreasing = TRUE,index.return = TRUE) 
        k <- k$ix[1:Num_place_for_pause]
        Feat_train_pause <- NULL
        for (i in (1:Num_place_for_pause))
        {
                if(k[i]+nSegment_within_Window < serverInforow)
                {
                        freq <- mean(interval[(k[i]+1):(k[i]+nSegment_within_Window)])
                }
                else
                {
                        freq <- mean(interval[(k[i]+1):serverInforow])
                }
                feat <- c(feat,interval[k[i]],which(quality==serverInfo$quality[k[i]]),which(quality==serverInfo$quality[k[i]+1]), freq)
                
        }
        feat
}

has_pause <- function(serverInfo,clientInfo,Num_place_for_pause,quality,nSegment_within_Window)
{
        serverInforow <- nrow(serverInfo)
        clientInforow <- nrow(clientInfo)
        feat <- NULL
        idx <- which(clientInfo$c.playerState=="Paused")
        pause_begin <- idx[1]
        pause_end <- idx[length(idx)]
        i <- 1
        while (i<length(idx))
        {
                if(idx[i+1] - idx[i] > 1)
                {
                        pause_end <- c(idx[i],pause_end)
                        pause_begin <- c(pause_begin,idx[i+1])
                }
                i <- i+1
        }
        t <- sort(pause_end-pause_begin, decreasing = TRUE, index.return = TRUE)
        pause_begin <- pause_begin[t$ix]
        pause_end <- pause_end[t$ix]
        Feat_train_pause <- NULL
        # for (i in (1:length(pause_begin)))
        for (i in (1:1)) #choose the largest one
        {
                #map the time log at the client to the time log at the server
                t <- which((serverInfo$time-clientInfo$time[pause_begin[i]] < 0)== TRUE)
                server_pause_begin <- t[length(t)]
                
                if(server_pause_begin - 2 < 1)  {server_pause_begin <- 1;} else {server_pause_begin <- server_pause_begin - 2;}
                t <- which((serverInfo$time-clientInfo$time[pause_end[i]] > 0)== TRUE)
                server_pause_end <- t[1]
                if(is.na(server_pause_end))
                {#the pause happens in the end of the episode, when all segments have been requested. 
                        feat <- c(feat, NA,NA,NA, NA)
                }
                else
                {
                        #the pause does not lies at the end.
                        if(server_pause_end + 2 > serverInforow){server_pause_end <- serverInforow;} else{server_pause_end <- server_pause_end + 2; }
                        interval <- as.numeric(as.duration(serverInfo$time[(server_pause_begin+1):server_pause_end] - serverInfo$time[server_pause_begin:(server_pause_end-1)]))
                        t <- server_pause_begin
                        server_pause_begin <- t+which.max(interval)-1
                        server_pause_end <- t+which.max(interval)
                        
                        interval <- as.numeric(as.duration(serverInfo$time[2:serverInforow]-serverInfo$time[1:(serverInforow-1)]))
                        freq <- NULL
                        if ((server_pause_end+nSegment_within_Window-1) < serverInforow)
                        {
                                freq <- mean(interval[(server_pause_end):(server_pause_end+nSegment_within_Window-1)])
                        } else{
                                freq <- mean(interval[(server_pause_end):serverInforow])
                        }
                        feat <- c(feat, interval[server_pause_begin],which(quality==serverInfo$quality[server_pause_begin]),which(quality==serverInfo$quality[server_pause_end]), freq)
                }
        }
        feat
}
