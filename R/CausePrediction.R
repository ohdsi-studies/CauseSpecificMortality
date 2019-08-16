#' Predict cause of death by comparing with plp prediction values in result files.
#' @name causePrediction
#' @import dplyr 
#' @import randomForest
#' @import ROCR
#' @import pROC
#' @import caret
#' @importFrom dplyr %>%
#' @param outputFolder your output folder
#' @param TAR          Time At Risk window end
#' @param nTree        Number of Tree of Random Forest model
#' @param seedNum      Seed number
#' @export
NULL

causePrediction <- function (outputFolder, TAR = 30, nTree = 200, seedNum = NULL) {
  
  ###Announcement
  ParallelLogger::logInfo("prediction start...")
  
  saveFolder <- file.path(outputFolder, "causePredictionResults")
  if (!file.exists(saveFolder))
    dir.create(saveFolder)
  
  
  ### 1. Read RDS file in plpResult folder.
  outpath <- file.path(outputFolder, "settings.csv")
  settings <- utils::read.csv(outpath)
  
  settings <- settings %>% filter(settings$riskWindowEnd == TAR) 
  analysispath <- paste0(settings$plpResultFolder)
  
  length <- as.numeric(length(analysispath))
  outList <- vector(mode = "list", length = length(analysispath))
  
  
  ### 2. read RDS 
  for (j in 1:length) {
    rds <- readRDS(file.path(analysispath[j],"plpResult","prediction.rds"))
    outList[[j]] <- rds
    names(outList)[j] <- paste("prediction", j, sep = "_")
  }
  
  
  ### 3. Merge prediction values and outcomes 
  outDFvalue1 <- data.frame()
  outDFvalue2 <- data.frame()
  model1 <- which(settings$modelSettingId == 1)
  model2 <- which(settings$modelSettingId == 2)
  
  for (j in model1) {
    df1 <- outList[[j]] %>% select(subjectId, value)
    colnames(df1)[2]<- paste(settings$outcomeName[j], settings$modelSettingsId[j], sep = "_")
    if (length(outDFvalue1) == 0) {
      outDFvalue1 <- df1
    }
    else {
      outDFvalue1 <- dplyr::left_join(outDFvalue1, df1, by = "subjectId")
    }
  }
  valueName <- c("subjectId", "DeathValue1", "CancerValue1",
                 "IHDValue1", "CerebroValue1", "PneumoValue1",
                 "DMValue1", "LiverValue1", "CLRDValue1", "HTValue1")
  names(outDFvalue1) <- valueName
  
  for (j in model2) {
    df2 <- outList[[j]] %>% select(subjectId, value)
    colnames(df2)[2]<- paste(settings$outcomeName[j], settings$modelSettingsId[j], sep = "_")
    if (length(outDFvalue2) == 0) {
      outDFvalue2 <- df2
    }
    else{
      outDFvalue2 <- dplyr::left_join(outDFvalue2, df2, by = "subjectId")
    }
  }
  
  valueName <- c("subjectId", "DeathValue2", "CancerValue2",
                 "IHDValue2", "CerebroValue2", "PneumoValue2",
                 "DMValue2", "LiverValue2", "CLRDValue2", "HTValue2")
  names(outDFvalue2) <- valueName
  
  
  outDFoutcome <- data.frame()
  for (j in model1) {
    df3 <- outList[[j]] %>% select(subjectId, outcomeCount)
    colnames(df3)[2]<- paste(paste("Label", settings$outcomeName[j], sep = "_"), settings$modelSettingsId[j], sep = "_")
    if (length(outDFoutcome) == 0) {
      outDFoutcome <- outList[[j]] %>% select(indexes, subjectId, outcomeCount)
      colnames(outDFoutcome)[3] <- paste(paste("Label", settings$outcomeName[j], sep = "_"), settings$modelSettingsId[j], sep = "_")
    }
    else{
      outDFoutcome <- left_join(outDFoutcome, df3, by = "subjectId")
    }
  }
  
  labelName <- c("indexes", "subjectId", "DeathLabel", "CancerLabel",
                 "IHDLabel", "CerebroLabel", "PneumoLabel",
                 "DMLabel", "LiverLabel", "CLRDLabel", "HTLabel")
  names(outDFoutcome) <- labelName
  
  outDF <- left_join(outDFoutcome, outDFvalue1, by = "subjectId")
  outDF <- left_join(outDF, outDFvalue2, by = "subjectId")
  outDF <- na.omit(outDF)
  
  ### save file in save directory
  ParallelLogger::logInfo("Save preprocessed data file in save folder...")
  savepath <- file.path(saveFolder, "outDF_")
  savepath <- paste(savepath,TAR,".rds")
  saveRDS(outDF, file = savepath)
  
  
  ### 5. Cause of Death
  labelStart <- 4
  labelEnd <- 2+length/2
  max <- apply(outDF[,labelStart:labelEnd], 1, which.max)
  max <- as.numeric(max)
  
  
  #Cause Labeled in Database
  CauseLabel <- max
  sum <- apply(outDF[,labelStart:labelEnd], 1, sum)
  CauseLabel <- ifelse(outDF[,3] == 0 , 0, CauseLabel)
  outDF$CauseLabel <- CauseLabel
  
  #####################################################################################################################################              
  
  ###Announcement
  ParallelLogger::logInfo("Doing Random Forest...")    
  
  ### 6. Random Forest
  
  # Set seed number
  set.seed(seedNum)
  
  # Train dataset preparation (indexes = c(1,2,3))
  dataTrain <- outDF %>% filter(indexes != -1)    
  dataTrain <- na.omit(dataTrain)
  
  # Test dataset preparation (indexes = -1)
  dataTest <- outDF %>% filter(indexes == -1)
  dataTest <- na.omit(dataTest)
  
  # classification settings    
  dataTrain$CauseLabel <- as.character(dataTrain$CauseLabel)
  dataTrain$CauseLabel <- as.factor(dataTrain$CauseLabel)
  
  dataTest$CauseLabel <- as.character(dataTest$CauseLabel)
  dataTest$CauseLabel <- as.factor(dataTest$CauseLabel)
  
  
  # Training model
  cause.model.rf <- randomForest(CauseLabel ~ DeathValue1 + DeathValue2 + CancerValue1 + CancerValue2 + IHDValue1 + IHDValue2
                                 + CerebroValue1 + CerebroValue2 + PneumoValue1 + PneumoValue2 + DMValue1 + DMValue2 + LiverValue1 + LiverValue2
                                 + CLRDValue1 + CLRDValue2 + HTValue1 + HTValue2
                                 , data = dataTrain, ntree = nTree, mtry = floor(sqrt(length)), importance = T, proximity = F)
  saveModel <- paste(saveFolder, "final_model", sep = "/")
  saveModel <- paste(saveModel, TAR, nTree, sep ="_")
  saveModel <- paste(saveModel, "rds", sep = ".")
  saveRDS(cause.model.rf, saveModel)
  
  ## 7. Result 
  
  dataTestResult <- dataTest 
  
  dataTestResult$cause.prediction <- predict(cause.model.rf, dataTest, type = "response")
  dataTestResult$cause.value <- predict(cause.model.rf, dataTest, type = "prob")
  
  for(j in 0:length){
    colname <- paste("cause.value", j)
    dataTestResult[colname] <- predict(cause.model.rf, dataTest, type = "prob")[,j+1]
  }
  
  dataTestValue <- predict(cause.model.rf, dataTest, type = "prob")
  
  # Accuracy 
  dfAccuracy <- dataTestResult
  levels(dfAccuracy$CauseLabel) <- c("0", "1","2","3", "4", "5", "6", "7", "8")
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
  acc <- calculate.accuracy(dfAccuracy$cause.prediction, dfAccuracy$CauseLabel)
  print(paste0("Accuracy is: ", round(acc, 4)))
  
  weights <- rep(1 / length(levels(dfAccuracy$cause.prediction)), length(levels(dfAccuracy$CauseLabel)))
  w.acc <- calculate.w.accuracy(dfAccuracy$cause.prediction, dfAccuracy$CauseLabel, weights)
  print(paste0("Weighted accuracy is: ", round(w.acc, 4)))
  
  # Confusion Matrix 
  cm <- vector("list", length(levels(dfAccuracy$CauseLabel)))
  for (i in seq_along(cm)) {
    positive.class <- levels(dfAccuracy$CauseLabel)[i]
    cm[[i]] <- confusionMatrix(dfAccuracy$cause.prediction, dfAccuracy$CauseLabel, 
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
  classes <- dataTestResult$CauseLabel
  levels(classes) <- c("0", "1","2","3", "4", "5", "6", "7", "8")
  
  plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1), ylab="Precision", xlab="Recall", bty="n")
  colors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#cab2d6","#6a3d9a")
  aucs <- rep(NA, length(levels(classes)))
  for (i in seq_along(levels(classes))) {
    cur.classes <- levels(classes)[i]
    test.labels <- dataTestResult$cause.prediction == cur.classes
    pred <- prediction(dataTestValue[,i], test.labels)
    perf <- performance(pred, "prec", "rec")
    roc.x <- unlist(perf@x.values)
    roc.y <- unlist(perf@y.values)
    # for baseline
    # ab <- get.conf.stats(cm)
    # ab <- ab %>% mutate(p = tp + fn, total = length(dfAccuracy$CauseLabel)) %>% mutate(baseline = p/total)
    # abline(a= ab$baseline[i], b=0, col = colors[i], lwd = 2)
    lines(roc.y ~ roc.x, col = colors[i], lwd = 2)
    
    dataTestTrueCase <- as.data.frame(dataTestValue)
    dataTestTrueCase$trueClass <- ifelse(dataTestResult$cause.prediction == cur.classes, 1 ,0)
    dataTestPositive <- dataTestTrueCase %>% filter(trueClass == 1)
    dataTestNegative <- dataTestTrueCase %>% filter(trueClass == 0)
    pr <- PRROC::pr.curve(scores.class0 = dataTestPositive[,i], scores.class1 = dataTestNegative[,i], curve = T)
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
  
  auroc<- pROC::multiclass.roc(dataTestResult$CauseLabel, dataTestValue, levels = levels(dataTestResult$CauseLabel))
  print("The receiver operating characteristics curve :")
  print(auroc$auc)
  
  par(pty = "s")
  try(plot0 <- plot.roc(dataTestResult$DeathLabel, dataTestResult$cause.value0, legacy.axes = TRUE, percent = F, col = "#a6cee3"))
  try(plot1 <- lines.roc(dataTestResult$CancerLabel, dataTestResult$cause.value1, col = "#1f78b4"))
  try(plot2 <- lines.roc(dataTestResult$IHDLabel, dataTestResult$cause.value2, col = "#b2df8a"))
  try(plot3 <- lines.roc(dataTestResult$CerebroLabel, dataTestResult$cause.value3, col = "#33a02c"))
  try(plot4 <- lines.roc(dataTestResult$PneumoLabel, dataTestResult$cause.value4, col = "#fb9a99"))
  try(plot5 <- lines.roc(dataTestResult$DMLabel, dataTestResult$cause.value5, col = "#e31a1c"))
  try(plot6 <- lines.roc(dataTestResult$LiverLabel, dataTestResult$cause.value6, col = "#fdbf6f"))
  try(plot7 <- lines.roc(dataTestResult$CLRDLabel, dataTestResult$cause.value7, col = "#cab2d6"))
  try(plot8 <- lines.roc(dataTestResult$HTLabel, dataTestResult$cause.value8, col = "#6a3d9a"))
  
  legend("bottomright", bty = "n", 
         legend=c("No Death", "Malignant cancer", "Ischemic heart disease", "Cerebrovascular disease", 
                  "Pneumonia", "Diabetes", "Liver disease", "Chronic lower respiratory disease", "Hypertensive disease"),
         col=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#cab2d6","#6a3d9a"), lwd = 2)
  
  
  savepath <- paste("randomForest ROC curve", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".pdf")
  savepath <- file.path(saveFolder, savepath)
  dev.print(pdf, savepath) 
  
  
  ### 8. Save files in saveFolder
  ParallelLogger::logInfo("saving the results in your outputFolder/causePredictionResults")
  
  savepath <- paste("dataTestResult", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".rds")
  savepath <- file.path(saveFolder, savepath)
  saveRDS(dataTestResult, file = savepath)
  
  savepath <- paste("dataTestValue", TAR, nTree, sep = "_")
  savepath <- paste(savepath, ".rds")
  savepath <- file.path(saveFolder, savepath)
  saveRDS(dataTestValue, file = savepath)
  
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
  table3 <- prop.table(table(dataTestResult$CauseLabel, dataTestResult$cause.prediction , dnn=c("Label","Prediction")),1)
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
