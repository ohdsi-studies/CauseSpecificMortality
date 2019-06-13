#' Predict cause of death by comparing with plp prediction values in result files.
#' @name CausePrediction
#' @import dplyr 
#' @import randomForest
#' @import ROCR
#' @import caret
#' @importFrom dplyr %>%
#' @param outputFolder your output folder
#' @param TAR          Time At Risk window end
#' @param nTree        Number of Tree of Random Forest model
#' @param seedNum      Seed number
#' @export
NULL

CausePrediction <- function (outputFolder, TAR, nTree = 100, seedNum =NULL) {
  
  ###Announcement
  ParallelLogger::logInfo("prediction start...")
  
  ### 1. Read RDS file in plpResult folder.
  outputFolder <- Sys.getenv("outputFolder")
  outpath <- file.path(outputFolder, "settings.csv")
  settings <- utils::read.csv(outpath)
  
  TAR <- TAR
  
  settings <- settings %>% filter(settings$riskWindowEnd == TAR) 
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
    colnames(df1)[2]<- paste(settings$outcomeName[j], settings$modelSettingsId[j], sep = "_")
    if (length(out_df_value) == 0) {
      out_df_value <- df1
    }
    else{
      out_df_value <- dplyr::left_join(out_df_value, df1, by = "subjectId")
    }
  }
  
  out_df_outcome <- data.frame()
  for (j in seq(1,i,2)) {
    df2 <- out_list[[j]] %>% select(subjectId, outcomeCount)
    colnames(df2)[2]<- paste(paste("Label", settings$outcomeName[j], sep = "_"), settings$modelSettingsId[j], sep = "_")
    if (length(out_df_outcome) == 0) {
      out_df_outcome <- out_list[[j]] %>% select(indexes, subjectId, outcomeCount)
      colnames(out_df_outcome)[3] <- paste(paste("Label", settings$outcomeName[j], sep = "_"), settings$modelSettingsId[j], sep = "_")
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
  
  out_df <- rename(out_df, DeathLabel = paste(paste("Label", settings$outcomeName[1], sep = "_"), settings$modelSettingsId[1], sep = "_"))
  out_df <- rename(out_df, CancerLabel = paste(paste("Label", settings$outcomeName[3], sep = "_"), settings$modelSettingsId[3], sep = "_"))
  out_df <- rename(out_df, CardioLabel = paste(paste("Label", settings$outcomeName[5], sep = "_"), settings$modelSettingsId[5], sep = "_"))
  out_df <- rename(out_df, CerebroLabel = paste(paste("Label", settings$outcomeName[7], sep = "_"), settings$modelSettingsId[7], sep = "_"))
  out_df <- rename(out_df, PneumoLabel = paste(paste("Label", settings$outcomeName[9], sep = "_"), settings$modelSettingsId[9], sep = "_"))
  out_df <- rename(out_df, DMLabel = paste(paste("Label", settings$outcomeName[11], sep = "_"), settings$modelSettingsId[11], sep = "_"))
  out_df <- rename(out_df, LiverLabel = paste(paste("Label", settings$outcomeName[13], sep = "_"), settings$modelSettingsId[13], sep = "_"))
  out_df <- rename(out_df, LowResLabel = paste(paste("Label", settings$outcomeName[15], sep = "_"), settings$modelSettingsId[15], sep = "_"))
  out_df <- rename(out_df, HTLabel = paste(paste("Label", settings$outcomeName[17], sep = "_"), settings$modelSettingsId[17], sep = "_"))
  
  out_df <- rename(out_df, DeathValue1 = paste(settings$outcomeName[1], settings$modelSettingsId[1], sep = "_"))
  out_df <- rename(out_df, DeathValue2 = paste(settings$outcomeName[2], settings$modelSettingsId[2], sep = "_"))
  out_df <- rename(out_df, CancerValue1 = paste(settings$outcomeName[3], settings$modelSettingsId[3], sep = "_"))
  out_df <- rename(out_df, CancerValue2 = paste(settings$outcomeName[4], settings$modelSettingsId[4], sep = "_"))
  out_df <- rename(out_df, CardioValue2 = paste(settings$outcomeName[5], settings$modelSettingsId[5], sep = "_"))
  out_df <- rename(out_df, CardioValue1 = paste(settings$outcomeName[6], settings$modelSettingsId[6], sep = "_"))
  out_df <- rename(out_df, CerebroValue2 = paste(settings$outcomeName[7], settings$modelSettingsId[7], sep = "_"))
  out_df <- rename(out_df, CerebroValue1 = paste(settings$outcomeName[8], settings$modelSettingsId[8], sep = "_"))
  out_df <- rename(out_df, PneumoValue2 = paste(settings$outcomeName[9], settings$modelSettingsId[9], sep = "_"))
  out_df <- rename(out_df, PneumoValue1 = paste(settings$outcomeName[10], settings$modelSettingsId[10], sep = "_"))
  out_df <- rename(out_df, DMValue1 = paste(settings$outcomeName[11], settings$modelSettingsId[11], sep = "_"))
  out_df <- rename(out_df, DMValue2 = paste(settings$outcomeName[12], settings$modelSettingsId[12], sep = "_"))
  out_df <- rename(out_df, LiverValue2 = paste(settings$outcomeName[13], settings$modelSettingsId[13], sep = "_"))
  out_df <- rename(out_df, LiverValue1 = paste(settings$outcomeName[14], settings$modelSettingsId[14], sep = "_"))
  out_df <- rename(out_df, LowResValue2 = paste(settings$outcomeName[15], settings$modelSettingsId[15], sep = "_"))
  out_df <- rename(out_df, LowResValue1 = paste(settings$outcomeName[16], settings$modelSettingsId[16], sep = "_"))
  out_df <- rename(out_df, HTValue2 = paste(settings$outcomeName[17], settings$modelSettingsId[17], sep = "_"))
  out_df <- rename(out_df, HTValue1 = paste(settings$outcomeName[18], settings$modelSettingsId[18], sep = "_"))
  
  
  out_df <- na.omit(out_df)
  
  
  ### 5. Cause of Death
  p <- 4
  q <- 2+i/2
  max1 <- apply(out_df[,p:q],1,which.max)
  max1 <- as.numeric(max1)
  max2 <- max1 + 3
  
  #Cause Labeled in Database
  CauseLabel <- max1
  sum <- apply(out_df[,p:q],1, sum)
  CauseLabel <- ifelse(sum == 0, 99, CauseLabel)
  CauseLabel <- ifelse(out_df[,3] == 0, 0, CauseLabel)
  out_df$CauseLabel <- CauseLabel
  
  
  #####################################################################################################################################              
  
  ###Announcement
  ParallelLogger::logInfo("Doing Random Forest...")    
  
  ### 6. Random Forest
  
  # Set seed number
  set.seed(1234)
  
  # Train dataset preparation
  data_train <- out_df %>% filter(indexes != -1)    
  data_train <- na.omit(data_train)
  
  # Test dataset preparation
  data_test <- out_df %>% filter(indexes == -1)
  data_test <- na.omit(data_test)
  
  # classification settings    
  data_train$CauseLabel <- as.character(data_train$CauseLabel)
  data_train$CauseLabel <- as.factor(data_train$CauseLabel)
  
  data_test$CauseLabel <- as.character(data_test$CauseLabel)
  data_test$CauseLabel <- as.factor(data_test$CauseLabel)
  
  
  # Training model
  cause.model.rf <- randomForest(CauseLabel ~ DeathValue1 + DeathValue2 + CancerValue1 + CancerValue2 + CardioValue1 + CardioValue2
                                 + CerebroValue1 + CerebroValue2 + PneumoValue1 + PneumoValue2 + DMValue1 + DMValue2 + LiverValue1 + LiverValue2
                                 + LowResValue1 + LowResValue2 + HTValue1 + HTValue2
                                 , data = data_train, ntree = nTree, mtry = floor(sqrt(18)), importance = T, proximity = F)
  
  # Test
  data_test$cause.prediction <- predict(cause.model.rf, data_test, type = "response")
  data_test$cause.value <- predict(cause.model.rf, data_test, type = "prob")[,2]
  data_test_val <- predict(cause.model.rf, data_test,type = "prob")
  predictionRoc <- predict(cause.model.rf, data_test, type = "prob")
  
  ### 7. Result 
  
  colnames(data_test_val)<-c("NoDeath","Cancer","Cardio", "Cerebro", "Pneumonia", "DM", "Liver", "LowRes", "HT","Others")
  
  dt<-data_test
  dt$CauseLabel <- as.character (dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="0", "NoDeath" , dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="1","Cancer",dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="2","Cardio",dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="3","Cerebro",dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="4","Pneumonia",dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="5","DM",dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="6","Liver",dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="7","LowRes",dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="8","HT",dt$CauseLabel)
  dt$CauseLabel <- ifelse(dt$CauseLabel=="99","Others",dt$CauseLabel)
  
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
  
  # Performance measure
  df_acc <- data_test
  calculate.accuracy <- function(predictions, ref.labels) {
    return(length(which(predictions == ref.labels)) / length(ref.labels))
  }
  calculate.w.accuracy <- function(predictions, ref.labels, weights) {
    lvls <- levels(ref.labels)
    if (length(weights) != length(lvls)) {
      stop("Number of weights should agree with the number of classes.")
    }
    if (sum(weights) != 1) {
      stop("Weights do not sum to 1")
    }
    accs <- lapply(lvls, function(x) {
      idx <- which(ref.labels == x)
      return(calculate.accuracy(predictions[idx], ref.labels[idx]))
    })
    acc <- mean(unlist(accs))
    return(acc)
  }
  acc <- calculate.accuracy(df_acc$cause.prediction, df_acc$CauseLabel)
  print(paste0("Accuracy is: ", round(acc, 2)))
  
  weights <- rep(1 / length(levels(df_acc$cause.prediction)), length(levels(df_acc$CauseLabel)))
  w.acc <- calculate.w.accuracy(df_acc$cause.prediction, df_acc$CauseLabel, weights)
  print(paste0("Weighted accuracy is: ", round(w.acc, 2)))
  
  
  
  cm <- vector("list", length(levels(df_acc$CauseLabel)))
  for (i in seq_along(cm)) {
    positive.class <- levels(df_acc$CauseLabel)[i]
    # in the i-th iteration, use the i-th class as the positive class
    cm[[i]] <- confusionMatrix(df_acc$cause.prediction, df_acc$CauseLabel, 
                               positive = positive.class)
  }
  
  
  metrics <- c("Precision", "Recall")
  print(cm[[1]]$byClass[, metrics])
  
  
  get.conf.stats <- function(cm) {
    out <- vector("list", length(cm))
    for (i in seq_along(cm)) {
      x <- cm[[i]]
      tp <- x$table[x$positive, x$positive] 
      fp <- sum(x$table[x$positive, colnames(x$table) != x$positive])
      fn <- sum(x$table[colnames(x$table) != x$positie, x$positive])
      # TNs are not well-defined for one-vs-all approach
      elem <- c(tp = tp, fp = fp, fn = fn)
      out[[i]] <- elem
    }
    df <- do.call(rbind, out)
    rownames(df) <- unlist(lapply(cm, function(x) x$positive))
    return(as.data.frame(df))
  }
  
  # Micro F1
  get.micro.f1 <- function(cm) {
    cm.summary <- get.conf.stats(cm)
    tp <- sum(cm.summary$tp)
    fn <- sum(cm.summary$fn)
    fp <- sum(cm.summary$fp)
    pr <- tp / (tp + fp)
    re <- tp / (tp + fn)
    f1 <- 2 * ((pr * re) / (pr + re))
    return(f1)
  }
  micro.f1 <- get.micro.f1(cm)
  print(paste0("Micro F1 is: ", round(micro.f1, 2)))
  
  # Macro F1
  get.macro.f1 <- function(cm) {
    c <- cm[[1]]$byClass # a single matrix is sufficient
    re <- sum(c[, "Recall"]) / nrow(c)
    pr <- sum(c[, "Precision"]) / nrow(c)
    f1 <- 2 * ((re * pr) / (re + pr))
    return(f1)
  }
  macro.f1 <- get.macro.f1(cm)
  print(paste0("Macro F1 is: ", round(macro.f1, 2)))
  
  
  
  
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
