#' Predict cause of death by comparing with plp prediction values in result files.
#' @name CausePrediction
#' @import dplyr 
#' @import randomForest
#' @import ROCR
#' @import caret
#' @importFrom dplyr %>%
#' @param outputFolder your output folder
#' @param TAR          Time At Risk window end
#' @param model        machine learning model 1- lassologistic regression, 2-Gradient boosting machine
#' @param nTree        Number of Tree of Random Forest model
#' @param seedNum      Seed number
#' @export
NULL

CausePrediction <- function (outputFolder, TAR, model = 1, nTree = 100, seedNum =NULL) {
  
  ###Announcement
  ParallelLogger::logInfo("classification start...")

  ### 1. Read RDS file in plpResult folder.
      outputFolder <- Sys.getenv("outputFolder")
      
      outpath <- file.path(outputFolder, "settings.csv")
      settings <- utils::read.csv(outpath)
      
      TAR <- TAR
      model <- model
      
      settings <- settings %>% filter(settings$riskWindowEnd == TAR) 
      settings <- settings %>% filter(settings$modelSettingId == model)
      settings <- settings %>% filter(settings$outcomeId != 161)
      analysispath <- paste0(settings$plpResultFolder)
      
      i<- as.numeric(length(analysispath))
      out_list <- vector(mode = "list", length = length(analysispath))
  
  
  ### 2. read RDS 
      for (j in 1:i) {
        rds <- readRDS(file.path(analysispath[j],"plpResult","prediction.rds"))
        out_list[[j]] <- rds
        names(out_list)[j] <- paste("prediction", j, sep = "_")
      }
  
  
  ### 3. Merge prediction values and outcomes 
  
      out_df_value <- data.frame()
      for (j in 1:i) {
        df1 <- out_list[[j]] %>% select(subjectId, value)
        colnames(df1)[2]<- paste0(settings$outcomeName[j])
        if (length(out_df_value) == 0) {
          out_df_value <- df1
        }
        else{
          out_df_value <- dplyr::left_join(out_df_value, df1, by = "subjectId")
        }
      }
      
      out_df_outcome <- data.frame()
      for (j in 1:i) {
        df2 <- out_list[[j]] %>% select(subjectId, outcomeCount)
        colnames(df2)[2]<- paste0("In real", sep = " _ ", settings$outcomeName[j])
        if (length(out_df_outcome) == 0) {
          out_df_outcome <- out_list[[j]] %>% select(indexes, subjectId, outcomeCount)
        }
        else{
          out_df_outcome <- left_join(out_df_outcome, df2, by = "subjectId")
        }
      }
      
      out_df <- left_join(out_df_outcome, out_df_value, by = "subjectId")
  
  ### 4. Death 
      i<- as.numeric(length(analysispath))
      m <- i+3
  
      #Death Labeled in Database
      out_df <- rename(out_df, DeathLabel = outcomeCount)
      out_df <- na.omit(out_df)
      
  
  ### 5. Cause of Death
      p <- 4
      q <- p+i-2
      max2 <- apply(out_df[,p:q],1,which.max)
      max2 <- as.numeric(max2)
      max3 <- max2 + i
  
      #Cause Labeled in Database
      CauseLabel <- max2
      sum <- apply(out_df[,p:q],1, sum)
      CauseLabel <- ifelse(sum == 0, 99, CauseLabel)
      CauseLabel <- ifelse(out_df[,3] == 0, 0, CauseLabel)
      out_df$CauseLabel <- CauseLabel
  
      
  ###################################################################################          
  
  ###Announcement
      ParallelLogger::logInfo("Doing Random Forest...")    
      
  ### 6. Random Forest
      
      # Set seed number
      set.seed(1234)
      
      # Train dataset preparation
      data_train <- out_df %>% filter(indexes != -1) %>% select(subjectId, CauseLabel, "[CSKIM] Any death", 
                                                                "[CSKIM] CV death", "[CSKIM] Cancer death")   # "[CSKIM] Infection related death"
      data_train <- rename(data_train, NoDeath = "[CSKIM] Any death", 
                           CV = "[CSKIM] CV death", Cancer = "[CSKIM] Cancer death") #, infection = "[CSKIM] Infection related death"  )
      data_train <- na.omit(data_train)
      
      # Test dataset preparation
      data_test <- out_df %>% filter(indexes == -1) %>% select(subjectId, CauseLabel, "[CSKIM] Any death", 
                                                               "[CSKIM] CV death", "[CSKIM] Cancer death") #, "[CSKIM] Infection related death")
      data_test <- rename(data_test, NoDeath = "[CSKIM] Any death", 
                          CV = "[CSKIM] CV death", Cancer = "[CSKIM] Cancer death" ) #, infection = "[CSKIM] Infection related death" ) 
      data_test <- na.omit(data_test)
     
      # classification settings    
      data_train$CauseLabel <- as.character(data_train$CauseLabel)
      data_train$CauseLabel <- as.factor(data_train$CauseLabel)
      data_test$CauseLabel <- as.character(data_test$CauseLabel)
      data_test$CauseLabel <- as.factor(data_test$CauseLabel)
      
      
      # Training model
      cause.model.rf <- randomForest(CauseLabel ~ NoDeath + CV + Cancer, 
                                     data = data_train, ntree = 500, mtry = floor(sqrt(3)), importance = T, proximity = F)
      
      # Test
      
      data_test$cause.prediction <- predict(cause.model.rf, data_test, type = "response")
      data_test$cause.value <- predict(cause.model.rf, data_test, type = "prob")[,2]
      data_test_val <- predict(cause.model.rf, data_test,type = "prob")
      predictionRoc <- predict(cause.model.rf, data_test, type = "prob")
 
  ### 7. Result 
      
      colnames(data_test_val)<-c("No Death","CV","Cancer","Other")
      
      dt<-data_test
      dt$CauseLabel <- as.character (dt$CauseLabel)
      dt$CauseLabel <- ifelse(dt$CauseLabel=="0", "No Death" , dt$CauseLabel)
      dt$CauseLabel <- ifelse(dt$CauseLabel=="1","CV",dt$CauseLabel)
      dt$CauseLabel <- ifelse(dt$CauseLabel=="2","Cancer",dt$CauseLabel)
      dt$CauseLabel <- ifelse(dt$CauseLabel=="99","Other",dt$CauseLabel)
      #dt$CauseLabel <- ifelse(dt$CauseLabel==3,"infection",dt$CauseLabel)

      AUC<- pROC::multiclass.roc(dt$CauseLabel, data_test_val)
      print(AUC)
      
      
      # table
      table1 <- table(data_test$CauseLabel, data_test$cause.prediction, dnn=c("Label","Prediction"))
      table2 <- prop.table(table(data_test$CauseLabel, data_test$cause.prediction , dnn=c("Label","Prediction")),1)
      table3 <- importance(cause.model.rf)
      
      # # plot
      # colours <- c("#F8766D", "#00BA38", "#619CFF", "#00816A")
      # classes <- levels(data_test$CauseLabel)
      # 
      # i <- as.numeric(length(analysispath))
      # 
      # for (j in 1 : i + 1 ) {
      # true_values <- ifelse(data_test[,6] == classes[j], 1, 0)
      # pred <- prediction(predictionRoc[,j],true_values)
      # perf <- performance(pred,"tpr","fpr")
      #  
      #    if (j==1){
      #    plot(perf, type = "l", main = "ROC curve", col = colours[j])
      #      }
      # 
      #    else {
      #   plot(perf, type = "l", main = "ROC curve", col = colours[j], add = TRUE)
      #  }
      #  auc.perf <- performance(pred, measure = "auc")
      #  print(auc.perf@y.values)
      # }
      # legend("topright", legend = unique(dt$CauseLabel), col = colours, pch = 15)

   ### 8. save rds file in saveFolder
      ParallelLogger::logInfo("saving the results in your outputFolder/CausePredictionResults")
      
      saveFolder <- file.path(outputFolder, "CausePredictionResults")
      if (!file.exists(saveFolder))
      dir.create(saveFolder)
      
      savepath0 <- file.path(saveFolder, "data_test.rds")
      saveRDS(data_test, file = savepath0)

      # savepath1 <- file.path(saveFolder, "ROC curve.pdf")
      # dev.print(pdf, savepath1)

      plot(cause.model.rf)
      legend("topright", legend = colnames(cause.model.rf$err.rate), col = 1:5, cex = 0.5, fill = 1:5)
      savepath2 <- file.path(saveFolder, "randomForestPlot.pdf")
      dev.print(pdf, savepath2)

      # plot(margin(cause.model.rf, data_test$CauseLabel))
      # savepath3 <- file.path(saveFolder, "plot3.pdf")
      # dev.print(pdf, savepath3)

      varImpPlot(cause.model.rf)
      savepath4 <- file.path(saveFolder, "varImpPlot.pdf")
      dev.print(pdf, savepath4)

      # savepath5 <- file.path(saveFolder, "scatter.pdf")
      # featurePlot(x = data_test[,3:6],
      #             y = data_test$CauseLabel,
      #             plot = "pairs",
      #             auto.key = list(columns = 2))
      # dev.print(pdf, savepath5)

      savepath <- file.path(saveFolder, "table1.csv")
      write.csv(table1, file = savepath)
      savepath <- file.path(saveFolder, "table2.csv")
      write.csv(table2, file = savepath)
      savepath <- file.path(saveFolder, "table3.csv")
      write.csv(table3, file = savepath)
      
    
}
