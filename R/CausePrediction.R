#' Predict cause of death by comparing with plp prediction values in result files.
#' @name CausePrediction
#' @import dplyr 
#' @import randomForest
#' @import ROCR
#' @import pROC
#' @import caret
#' @importFrom dplyr %>%
#' @param outputFolder your output folder
#' @param TAR          Time At Risk window end
#' @param model        machine learning model 1- lassologistic regression, 2-Gradient boosting machine
#' @param nTree        Number of Tree of Random Forest model
#' @param seedNum      Seed number
#' @export
NULL

CausePrediction <- function (outputFolder, TAR = 30, nTree = 200, seedNum = NULL) {
  
  ###Announcement
  ParallelLogger::logInfo("prediction start...")
  
  saveFolder <- file.path(outputFolder, "CausePredictionResults")
  if (!file.exists(saveFolder))
    dir.create(saveFolder)
  
  
  ### 1. Read RDS file in plpResult folder.
  outpath <- file.path(outputFolder, "settings.csv")
  settings <- utils::read.csv(outpath)
  
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
  out_df_value_1 <- data.frame()
  out_df_value_2 <- data.frame()
  model1 <- which(settings$modelSettingId == 1)
  model2 <- which(settings$modelSettingId == 2)
  
  for (j in model1) {
    df1 <- out_list[[j]] %>% select(subjectId, value)
    colnames(df1)[2]<- paste(settings$outcomeName[j], settings$modelSettingsId[j], sep = "_")
    if (length(out_df_value_1) == 0) {
      out_df_value_1 <- df1
    }
    else {
      out_df_value_1 <- dplyr::left_join(out_df_value_1, df1, by = "subjectId")
    }
  }
  valueName <- c("subjectId", "DeathValue1", "CancerValue1",
                 "IHDValue1", "CerebroValue1", "PneumoValue1",
                 "DMValue1", "LiverValue1", "CLRDValue1", "HTValue1")
  names(out_df_value_1) <- valueName
  
  for (j in model2) {
    df2 <- out_list[[j]] %>% select(subjectId, value)
    colnames(df2)[2]<- paste(settings$outcomeName[j], settings$modelSettingsId[j], sep = "_")
    if (length(out_df_value_2) == 0) {
      out_df_value_2 <- df2
    }
    else{
      out_df_value_2 <- dplyr::left_join(out_df_value_2, df2, by = "subjectId")
    }
  }
  
  valueName <- c("subjectId", "DeathValue2", "CancerValue2",
                 "IHDValue2", "CerebroValue2", "PneumoValue2",
                 "DMValue2", "LiverValue2", "CLRDValue2", "HTValue2")
  names(out_df_value_2) <- valueName
  
  
  out_df_outcome <- data.frame()
  for (j in model1) {
    df3 <- out_list[[j]] %>% select(subjectId, outcomeCount)
    colnames(df3)[2]<- paste(paste("Label", settings$outcomeName[j], sep = "_"), settings$modelSettingsId[j], sep = "_")
    if (length(out_df_outcome) == 0) {
      out_df_outcome <- out_list[[j]] %>% select(indexes, subjectId, outcomeCount)
      colnames(out_df_outcome)[3] <- paste(paste("Label", settings$outcomeName[j], sep = "_"), settings$modelSettingsId[j], sep = "_")
    }
    else{
      out_df_outcome <- left_join(out_df_outcome, df3, by = "subjectId")
    }
  }
  
  labelName <- c("indexes", "subjectId", "DeathLabel", "CancerLabel",
                 "IHDLabel", "CerebroLabel", "PneumoLabel",
                 "DMLabel", "LiverLabel", "CLRDLabel", "HTLabel")
  names(out_df_outcome) <- labelName
  
  out_df <- left_join(out_df_outcome, out_df_value_1, by = "subjectId")
  out_df <- left_join(out_df, out_df_value_2, by = "subjectId")
  out_df <- na.omit(out_df)
  
  ### save file in save directory
  ParallelLogger::logInfo("Save preprocessed data file in save folder...")
  savepath <- file.path(saveFolder, "out_df_")
  savepath <- paste(savepath,TAR,".rds")
  saveRDS(out_df, file = savepath)
  
  
  ### 5. Cause of Death
  p <- 4
  q <- 2+i/2
  max <- apply(out_df[,p:q], 1, which.max)
  max <- as.numeric(max)
  
  
  #Cause Labeled in Database
  CauseLabel <- max
  sum <- apply(out_df[,p:q], 1, sum)
  CauseLabel <- ifelse(out_df[,3] == 0 , 0, CauseLabel)
  out_df$CauseLabel <- CauseLabel
  
  #####################################################################################################################################              
  
  ###Announcement
  ParallelLogger::logInfo("Doing Random Forest...")    
  
  ### 6. Random Forest
  
  # Set seed number
  set.seed(seedNum)
  
  # Train dataset preparation (indexes = c(1,2,3))
  data_train <- out_df %>% filter(indexes != -1)    
  data_train <- na.omit(data_train)
  
  # Test dataset preparation (indexes = -1)
  data_test <- out_df %>% filter(indexes == -1)
  data_test <- na.omit(data_test)
  
  # classification settings    
  data_train$CauseLabel <- as.character(data_train$CauseLabel)
  data_train$CauseLabel <- as.factor(data_train$CauseLabel)
  
  data_test$CauseLabel <- as.character(data_test$CauseLabel)
  data_test$CauseLabel <- as.factor(data_test$CauseLabel)
  
  
  # Training model
  cause.model.rf <- randomForest(CauseLabel ~ DeathValue1 + DeathValue2 + CancerValue1 + CancerValue2 + IHDValue1 + IHDValue2
                                 + CerebroValue1 + CerebroValue2 + PneumoValue1 + PneumoValue2 + DMValue1 + DMValue2 + LiverValue1 + LiverValue2
                                 + CLRDValue1 + CLRDValue2 + HTValue1 + HTValue2
                                 , data = data_train, ntree = nTree, mtry = floor(sqrt(i)), importance = T, proximity = F)
  saveModel <- paste(saveFolder, "final_model", sep = "/")
  saveModel <- paste(saveModel, TAR, nTree, sep ="_")
  saveModel <- paste(saveModel, "rds", sep = ".")
  saveRDS(cause.model.rf, saveModel)
  
  ## 7. Result 
  
  data_test_result <- data_test 
  
  data_test_result$cause.prediction <- predict(cause.model.rf, data_test, type = "response")
  data_test_result$cause.value <- predict(cause.model.rf, data_test, type = "prob")
  data_test_result$cause.value0 <- predict(cause.model.rf, data_test, type = "prob")[,1]
  data_test_result$cause.value1 <- predict(cause.model.rf, data_test, type = "prob")[,2]
  data_test_result$cause.value2 <- predict(cause.model.rf, data_test, type = "prob")[,3]
  data_test_result$cause.value3 <- predict(cause.model.rf, data_test, type = "prob")[,4]
  data_test_result$cause.value4 <- predict(cause.model.rf, data_test, type = "prob")[,5]
  data_test_result$cause.value5 <- predict(cause.model.rf, data_test, type = "prob")[,6]
  data_test_result$cause.value6 <- predict(cause.model.rf, data_test, type = "prob")[,7]
  data_test_result$cause.value7 <- predict(cause.model.rf, data_test, type = "prob")[,8]
  data_test_result$cause.value8 <- predict(cause.model.rf, data_test, type = "prob")[,9]
  
  data_test_val <- predict(cause.model.rf, data_test, type = "prob")
  colnames(data_test_val)<-c("NoDeath","Cancer","IHD", "Cerebro", "Pneumonia", "DM", "Liver", "CLRD", "HT")
  
  # Accuracy 
  df_acc <- data_test_result
  levels(df_acc$CauseLabel) <- c("0", "1","2","3", "4", "5", "6", "7", "8")
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
    accs <- unlist(accs)
    accs <- accs[is.nan(accs) == FALSE]
    acc <- mean(accs)
    return(acc)
  }
  acc <- calculate.accuracy(df_acc$cause.prediction, df_acc$CauseLabel)
  print(paste0("Accuracy is: ", round(acc, 4)))
  
  weights <- rep(1 / length(levels(df_acc$cause.prediction)), length(levels(df_acc$CauseLabel)))
  w.acc <- calculate.w.accuracy(df_acc$cause.prediction, df_acc$CauseLabel, weights)
  print(paste0("Weighted accuracy is: ", round(w.acc, 4)))
  
  # Confusion Matrix 
  cm <- vector("list", length(levels(df_acc$CauseLabel)))
  for (i in seq_along(cm)) {
    positive.class <- levels(df_acc$CauseLabel)[i]
    cm[[i]] <- confusionMatrix(df_acc$cause.prediction, df_acc$CauseLabel, 
                               positive = positive.class)
  }
  
  print(paste0("Confusion Matrix"))
  table1 <- cm[[1]]$table
  print(table1)
  table2 <- cm[[1]]$byClass
  print(table2)
  
  get.conf.stats <- function(cm) {
    out <- vector("list", length(cm))
    for (i in seq_along(cm)) {
      x <- cm[[i]]
      tp <- x$table[x$positive, x$positive] 
      fp <- sum(x$table[x$positive, colnames(x$table) != x$positive])
      fn <- sum(x$table[colnames(x$table) != x$positive, x$positive])
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
  print(paste0("Micro F1 is: ", round(micro.f1, 4)))
  
  # Macro F1
  get.macro.f1 <- function(cm) {
    c <- cm[[1]]$byClass # a single matrix is sufficient
    c <- na.omit(c)
    re <- sum(c[, "Recall"]) / nrow(c)
    pr <- sum(c[, "Precision"]) / nrow(c)
    f1 <- 2 * ((re * pr) / (re + pr))
    return(f1)
  }
  macro.f1 <- get.macro.f1(cm)
  print(paste0("Macro F1 is: ", round(macro.f1, 4)))
  
  
  # Precision Recall curve (PR curve)
  classes <- data_test_result$CauseLabel
  levels(classes) <- c("0", "1","2","3", "4", "5", "6", "7", "8")
  
  plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1), ylab="Precision", xlab="Recall", bty="n")
  colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#cab2d6","#6a3d9a")
  aucs <- rep(NA, length(levels(classes)))
  for (i in seq_along(levels(classes))) {
    cur.classes <- levels(classes)[i]
    test.labels <- data_test_result$cause.prediction == cur.classes
    pred <- prediction(data_test_val[,i], test.labels)
    perf <- performance(pred, "prec", "rec")
    roc.x <- unlist(perf@x.values)
    roc.y <- unlist(perf@y.values)
    # for baseline
    # ab <- get.conf.stats(cm)
    # ab <- ab %>% mutate(p = tp + fn, total = length(df_acc$CauseLabel)) %>% mutate(baseline = p/total)
    # abline(a= ab$baseline[i], b=0, col = colors[i], lwd = 2)
    lines(roc.y ~ roc.x, col = colors[i], lwd = 2)
    
    data_test_true <- as.data.frame(data_test_val)
    data_test_true$trueClass <- ifelse(data_test_result$cause.prediction == cur.classes, 1 ,0)
    data_test_pos <- data_test_true %>% filter(trueClass == 1)
    data_test_neg <- data_test_true %>% filter(trueClass == 0)
    pr <- PRROC::pr.curve(scores.class0 = data_test_pos[,i], scores.class1 = data_test_neg[,i], curve = T)
    aucs[i] <- pr$auc.integral
    
  }
  legend("bottomleft", bty = "n", 
         legend=c("No Death", "Malignant cancer", "Ischemic heart disease", "Cerebrovascular disease", 
                  "Pneumonia", "Diabetes", "Liver disease", "Chronic lower respiratory disease", "Hypertensive disease"),
         col=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#cab2d6","#6a3d9a"), lwd = 2)
  print(paste0("Mean AUC under the precision-recall curve is :", round(mean(aucs), 4)))
  
  savepath <- paste("randomForest PR curve", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".pdf")
  savepath <- file.path(saveFolder, savepath)
  dev.print(pdf, savepath) 
  
  # Receiver Operating Characteristics Plot
  
  df_roc <- data_test
  levels(df_roc$CauseLabel) <- c("0", "1","2","3", "4", "5", "6", "7", "8")
  df_roc$CauseLabel <- as.character (df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="0", "NoDeath", df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="1", "Cancer", df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="2", "IHD", df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="3", "Cerebro", df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="4", "Pneumonia", df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="5", "DM", df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="6", "Liver", df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="7", "CLRD", df_roc$CauseLabel)
  df_roc$CauseLabel <- ifelse(df_roc$CauseLabel=="8", "HT", df_roc$CauseLabel)
  
  auroc<- pROC::multiclass.roc(df_roc$CauseLabel, data_test_val)
  print("The receiver operating characteristics curve :")
  print(auroc$auc)
  
  par(pty = "s")
  try(plot0 <- plot.roc(data_test_result$DeathLabel, data_test_result$cause.value0, legacy.axes = TRUE, percent = F, col = "#a6cee3"))
  try(plot1 <- lines.roc(data_test_result$CancerLabel, data_test_result$cause.value1, col = "#1f78b4"))
  try(plot2 <- lines.roc(data_test_result$IHDLabel, data_test_result$cause.value2, col = "#b2df8a"))
  try(plot3 <- lines.roc(data_test_result$CerebroLabel, data_test_result$cause.value3, col = "#33a02c"))
  try(plot4 <- lines.roc(data_test_result$PneumoLabel, data_test_result$cause.value4, col = "#fb9a99"))
  try(plot5 <- lines.roc(data_test_result$DMLabel, data_test_result$cause.value5, col = "#e31a1c"))
  try(plot6 <- lines.roc(data_test_result$LiverLabel, data_test_result$cause.value6, col = "#fdbf6f"))
  try(plot7 <- lines.roc(data_test_result$CLRDLabel, data_test_result$cause.value7, col = "#cab2d6"))
  try(plot8 <- lines.roc(data_test_result$HTLabel, data_test_result$cause.value8, col = "#6a3d9a"))
  
  legend("bottomright", bty = "n", 
         legend=c("No Death", "Malignant cancer", "Ischemic heart disease", "Cerebrovascular disease", 
                  "Pneumonia", "Diabetes", "Liver disease", "Chronic lower respiratory disease", "Hypertensive disease"),
         col=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#cab2d6","#6a3d9a"), lwd = 2)
  
  
  savepath <- paste("randomForest ROC curve", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".pdf")
  savepath <- file.path(saveFolder, savepath)
  dev.print(pdf, savepath) 
  
  
  ### 8. Save files in saveFolder
  ParallelLogger::logInfo("saving the results in your outputFolder/CausePredictionResults")
  
  savepath <- paste("data_test_result", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".rds")
  savepath <- file.path(saveFolder, savepath)
  saveRDS(data_test_result, file = savepath)
  
  savepath <- paste("data_test_val", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".rds")
  savepath <- file.path(saveFolder, savepath)
  saveRDS(data_test_val, file = savepath)
  
  plot(cause.model.rf)
  legend("topright", legend = colnames(cause.model.rf$err.rate), fill = 1:5)
  
  savepath <- paste("randomForestPlot", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".pdf")
  savepath <- file.path(saveFolder, savepath)
  dev.print(pdf, savepath)
  
  varImpPlot(cause.model.rf)
  savepath <- paste("varImpPlot", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".pdf")
  savepath <- file.path(saveFolder, savepath)
  dev.print(pdf, savepath)
  
  savepath <- paste("table1", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".csv")
  savepath <- file.path(saveFolder, savepath)
  write.csv(table1, file = savepath)
  
  savepath <- paste("table2", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".csv")
  savepath <- file.path(saveFolder, savepath)
  write.csv(table2, file = savepath)
  
  # table
  table3 <- prop.table(table(data_test_result$CauseLabel, data_test_result$cause.prediction , dnn=c("Label","Prediction")),1)
  table4 <- importance(cause.model.rf)
  
  savepath <- paste("table3", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".csv")
  savepath <- file.path(saveFolder, savepath)
  write.csv(table3, file = savepath)
  
  savepath <- paste("table4", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".csv")
  savepath <- file.path(saveFolder, savepath)
  write.csv(table4, file = savepath)
  
  ParallelLogger::logInfo("DONE")

}
