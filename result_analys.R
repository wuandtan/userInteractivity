result_analysis <- function(grouth_truth_fn = "feature_test.arff", test_result_fn = "feature_test_result.txt")
{
  library("foreign")
  #read groundtruth
  groundtruth <- read.arff(grouth_truth_fn)
  
  mat_groundtruth <- t(matrix(as.numeric(as.character(groundtruth[,7])),nrow=5))
  truth_Isfreeze <- NULL
  truth_Longfreeze <-NULL
  truth_MultiFreeze <-NULL
  for (i in (1:nrow(mat_groundtruth)))
  {
    if(any(mat_groundtruth[i,]==2) || any(mat_groundtruth[i,]==3) )
    {
      truth_Isfreeze <- c(truth_Isfreeze,1)
    }
    else
    {
      truth_Isfreeze <- c(truth_Isfreeze,0)
    }
  
    if( any(mat_groundtruth[i,]==3) )
    {
      truth_Longfreeze <- c(truth_Longfreeze,1)
    }
    else
    {
      truth_Longfreeze <- c(truth_Longfreeze,0)
    }
    count_freeze= length(which(mat_groundtruth[i,]==2)) + length(which(mat_groundtruth[i,]==3))
    if(count_freeze >=2)
      truth_MultiFreeze <- c(truth_MultiFreeze, 1)
    else
      truth_MultiFreeze <- c(truth_MultiFreeze, 0)
  }
  #read recognition result
  tline <- readLines(test_result_fn)
  content <- tline[(grep("inst#",tline)+1): (grep("Evaluation on",tline)-1)]
  content <- strsplit(content," +")
  result <- NULL
  for (i in (1:length(content)))
  {
    if(length(content[[i]])>0)
      result <- c(result,as.numeric(strsplit(content[[i]][4],":")[[1]][2]))
  }
  
  recog_result <- t(matrix(result,nrow=5))
  recog_Isfreeze <- NULL
  recog_Longfreeze <-NULL
  recog_MultiFreeze <-NULL
  for (i in (1:nrow(recog_result)))
  {
    if(any(recog_result[i,]==2) || any(recog_result[i,]==3) )
    {
      recog_Isfreeze <- c(recog_Isfreeze,1)
    } else    {
      recog_Isfreeze <- c(recog_Isfreeze,0)
    }
    
    if( any(recog_result[i,]==3) )
    {
      recog_Longfreeze <- c(recog_Longfreeze,1)
    }else    {
      recog_Longfreeze <- c(recog_Longfreeze,0)
    }
    count_freeze= length(which(recog_result[i,]==2)) + length(which(recog_result[i,]==3))
    if(count_freeze >=2)
      recog_MultiFreeze <- c(recog_MultiFreeze, 1)
    else
      recog_MultiFreeze <- c(recog_MultiFreeze, 0)
  }
  acc_isFreeze <- sum(abs(truth_Isfreeze-recog_Isfreeze))/length(truth_Isfreeze);
  acc_longFreeze <- sum(abs(truth_Longfreeze-recog_Longfreeze))/length(truth_Isfreeze);
  acc_multiFreeze <- sum(abs(truth_MultiFreeze-recog_MultiFreeze))/length(truth_Isfreeze);
  print(paste("acc_isFreeze: ",acc_isFreeze, " acc_longFreeze: ",acc_longFreeze, " Multifreeze: ",acc_multiFreeze, "\n" ))
  
  total_result <- data.frame(truth_Isfreeze, truth_Longfreeze, truth_MultiFreeze, recog_Isfreeze,recog_Longfreeze,recog_MultiFreeze)
  total_result$note <- c("")
  for (i in (1:nrow(total_result)))
  {
    
    if(total_result[i,1] != total_result[i,4])
    {
      total_result$note[i] <- paste(total_result$note[i],"N")
    }
    if(total_result[i,2] != total_result[i,5])
    {
      total_result$note[i] <- paste(total_result$note[i],"L")
    }
    if(total_result[i,3] != total_result[i,6])
    {
      total_result$note[i] <- paste(total_result$note[i],"M")
    }
    
  }
  
  #further analysis
  l <- cbind(t(matrix(as.numeric(as.character(groundtruth[,2])),nrow=1)),t(matrix(as.numeric(as.character(groundtruth[,3])),nrow=1)),t(matrix(as.numeric(as.character(groundtruth[,4])),nrow=1)),t(matrix(as.numeric(as.character(groundtruth[,5])),nrow=1)),t(matrix(as.numeric(as.character(groundtruth[,6])),nrow=1)),t(matrix(as.numeric(as.character(groundtruth[,7])),nrow=1)))
}