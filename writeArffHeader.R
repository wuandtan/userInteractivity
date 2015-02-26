writeArffHeader <- function(filename="data", Num_place_for_pause = 2,Num_place_for_freeze = 2,max_window_size = 20)
{
  sink(filename)
  cat("@RELATION userInteractivity\n");
  cat("@ATTRIBUTE timestamp DATE \"yyyy-MM-dd HH:mm:ss\" \n")
  cat("@ATTRIBUTE ave_interval NUMERIC \n")
  cat("@ATTRIBUTE position NUMERIC \n")
 cat("@ATTRIBUTE q_before NUMERIC \n") 
 cat("@ATTRIBUTE q_after NUMERIC \n")
 cat("@ATTRIBUTE freq NUMERIC \n")
 cat("@ATTRIBUTE class {0,1,2,3,4} \n")
 cat("@DATA\n")
 sink()
#   #in total, there are 64 features to write.
#   #1. Feat_discontinuity
#   #2. Feat_Episode_time_difference
#   #3. 
#   sink(filename)
#   
#   cat("@RELATION userInteractivity\n");
#   cat("@ATTRIBUTE timestamp DATE \"yyyy-MM-dd HH:mm:ss\" \n")
#   cat("@ATTRIBUTE Jump_discontinuity  NUMERIC\n");
#   cat("@ATTRIBUTE Jump_Episode_time_difference  NUMERIC\n");
#   for (i in (1:Num_place_for_pause))
#   {
#     ln <- paste("@ATTRIBUTE pause_tr_intervel_",i,"  NUMERIC",sep="") 
#     cat(ln,"\n")
#     ln <- paste("@ATTRIBUTE pause_tr_q_prev_",i,"  NUMERIC",sep="") 
#     cat(ln,"\n")
#     ln <- paste("@ATTRIBUTE pause_tr_q_behi_",i,"  NUMERIC",sep="") 
#     cat(ln,"\n")
#     ln <- paste("@ATTRIBUTE pause_tr_freq_",i,"  NUMERIC",sep="") 
#     cat(ln,"\n")
#   }
#   for (i in (1:Num_place_for_pause))
#   {
#     ln <- paste("@ATTRIBUTE pause_te_intervel_",i,"  NUMERIC",sep="") 
#     cat(ln,"\n")
#     ln <- paste("@ATTRIBUTE pause_te_q_prev_",i,"  NUMERIC",sep="") 
#     cat(ln,"\n")
#     ln <- paste("@ATTRIBUTE pause_te_q_behi_",i,"  NUMERIC",sep="") 
#     cat(ln,"\n")
#     ln <- paste("@ATTRIBUTE pause_te_freq_",i,"  NUMERIC",sep="") 
#     cat(ln,"\n")
#   }
#   for (i in (1:Num_place_for_freeze ))
#   {
#     for (window_size in (1:max_window_size))
#     {
#       ln <- paste("@ATTRIBUTE freeze_top_",i,"_window_",window_size,"  NUMERIC",sep="") 
#       cat(ln,"\n")
#       
#     }
#   }
#   
#   cat("@ATTRIBUTE Class_repositioning  {0,1}\n");
#   cat("@ATTRIBUTE Class_Pause  {0,1}\n");
#   cat("@ATTRIBUTE Class_Freeze_nonFreeze  {0,1}\n");
#   cat("@ATTRIBUTE Class_max_Freeze  {0,1}\n");
#   cat("@ATTRIBUTE Class_multiFreeze  {0,1}\n");
#   cat("@ATTRIBUTE Class_totalFreezeTime  NUMERIC\n");
#   cat("@DATA\n"); 
#   
#   sink()
}

