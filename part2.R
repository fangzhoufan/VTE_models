if(T){
  rm(list=ls())
  library(readxl)
  library(tidyverse)
  library(finalfit)
  library(gapminder)
  library(ggplot2)
  library(GGally)
  library(broom)
  library(caret)
  library(e1071)
  library(openxlsx)
  library(catboost)
  library(lightgbm)
  library(tidymodels)
  library(baguette)
  library(discrim)
  library(gtsummary)
  library(tidymodels)
  library(baguette)
  library(discrim)
  library(pROC)
  library(ggsci)
  library(ResourceSelection)
  library(yulab.utils)
}

if(T){
  data <- read_excel("original.xls")
  data<-data%>%
    select(-c(22,23))
  data<-data[!is.na(data$`身高（m）`), ]
  data<-data[rowSums(is.na(data)) <= 2,]
  
  glimpse(data) # each variable as line, variable type, first values
  data_na<-missing_glimpse(data) # missing data for each variable
  data_sum<-ff_glimpse(data) # summary statistics for each variable
  
  ##数据填充
  median_lipoprotein <- median(data$脂蛋白, na.rm = TRUE)
  data$脂蛋白[is.na(data$脂蛋白)]<-median_lipoprotein
  data$载脂蛋白B<-as.numeric(data$载脂蛋白B)
  median_apob<-median(data$载脂蛋白B, na.rm = TRUE)
  data$载脂蛋白B[is.na(data$载脂蛋白B)]<-median_apob
  
  
  table(data$性别)
  table(data$术前彩超)
  table(data$Carprini)
  table(data$疾病)
  table(data$手术)
  table(data$手术时间)
  table(data$抗凝药)
  table(data$术后彩超)
  table(data$载脂蛋白B)
  
  data$sex<-ifelse(data$性别=='男','1','0')
  data$age<-gsub('岁',"",data$年龄)
  data$BMI<-round(data$`体重(Kg)`/(data$`身高（m）`^2),2)
  data$urate<-data$尿酸
  data$BG<-data$血糖
  data$ApoA<-data$载脂蛋白A
  data$ApoB<-round(data$载脂蛋白B,2)
  data$lipoprotein<-data$脂蛋白
  data$Preoperative_ultrasound<-ifelse(data$术前彩超 %in% c('未见明显异常','未见异常'),0,1)
  data$Carprini[data$Carprini=='4？7']<-4
  data$Carprini[data$Carprini=='7?']<-7
  data$Carprini[data$Carprini=='5？']<-5
  data$Hypertension<-ifelse(grepl('高血压', data$疾病), 1, 0)
  data$Hyperlipidemia<-ifelse(grepl('高脂血', data$疾病), 1, 0)
  data$Hyperglycemia<-ifelse(grepl('糖尿', data$疾病), 1, 0)
  data$Hyperuricemia<-ifelse(grepl('尿酸', data$疾病), 1, 0)
  data$Meniscal_tear_injury<-ifelse(grepl('半月板', data$疾病), 1, 0)
  data$Cruciate_Ligament_Injury<-ifelse(grepl('交叉韧带', data$疾病), 1, 0)
  data$Osteoarthritis1<-ifelse(grepl('骨性关节炎', data$疾病), 1, 0)
  data$Osteoarthritis2<-ifelse(grepl('骨关节炎', data$疾病), 1, 0)
  data$Osteoarthritis<-ifelse(data$Osteoarthritis1==1|data$Osteoarthritis2==1,1,0)
  data$Osteoporosis<-ifelse(grepl('骨质疏松', data$疾病), 1,0)
  data$surgical_time<-data$手术时间
  data$Anticoagulant<-ifelse(grepl('无', data$抗凝药), 1,0)
  data$Postoperative_ultrasound<-ifelse(data$术后彩超 %in% c('未见明显异常','未见异常','无','无？','未见血栓形成',NA),0,1)
  
  data<-data%>%
    select(22:29,8:11,30:36,39:41,17,42:43)
  glimpse(data) # each variable as line, variable type, first values
  data_na<-missing_glimpse(data) # missing data for each variable
  data_sum<-ff_glimpse(data) # summary statistics for each variable
  
  data$Carprini<-as.numeric(data$Carprini)
  
  
  data<-data%>%
    mutate(surgical_time = cut(surgical_time,breaks = c(0,30,60,Inf),labels = c('1','2','3')),
           Carprini = cut(Carprini, breaks = c(0, 2, 4, Inf), labels = c("1", "2", "3"))
    )%>%
    mutate(
      sex=factor(sex) %>%          
        fct_recode("Female" = "0",
                   "Male"   = "1") %>% 
        ff_label("Sex"),
      age = as.numeric(age)%>%
        ff_label('Age'),
      BMI = as.numeric(BMI)%>%
        ff_label('BMI'),
      urate = as.numeric(urate)%>%
        ff_label('Uric Acid'),
      BG = as.numeric(BG)%>%
        ff_label('Blood Glucose'),
      ApoA = as.numeric(ApoA)%>%
        ff_label('ApoA'),
      ApoB = as.numeric(ApoB)%>%
        ff_label('ApoB'),
      lipoprotein = as.numeric(lipoprotein)%>%
        ff_label('Lipoprotein'),
      CHOL = as.numeric(CHOL)%>%
        ff_label('Cholesterol,CHOL'),
      TG = as.numeric(TG)%>%
        ff_label('Triglyceride,TG'),
      HDL = as.numeric(HDL)%>%
        ff_label('high-density lipoprotein,HDL'),
      LDL = as.numeric(LDL)%>%
        ff_label('low-density lipoprotein,LDL'),
      Preoperative_ultrasound = factor(Preoperative_ultrasound)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Preoperative Ultrasound"),
      Hypertension = factor(Hypertension) %>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Hypertension"),
      Hyperlipidemia = factor(Hyperlipidemia)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Hyperlipidemia"),
      Hyperglycemia = factor(Hyperglycemia)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Hyperglycemia"),
      Hyperuricemia = factor(Hyperuricemia)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Hyperuricemia"),
      Meniscal_tear_injury = factor(Meniscal_tear_injury)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Meniscal tear injury,MTI"),
      Cruciate_Ligament_Injury = factor(Cruciate_Ligament_Injury)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Cruciate Ligament Injury,CLI"),
      Osteoarthritis = factor(Osteoarthritis)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Osteoarthritis,OA"),
      Osteoporosis = factor(Osteoporosis)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Osteoporosis,OP"),
      surgical_time = factor(surgical_time)%>%
        fct_recode('<60'= '1',
                   '60-120'='2',
                   '>120'='3') %>% 
        ff_label('Operation time(min)'),
      Carprini = factor(Carprini)%>%
        fct_recode("1-2(low)"= '1',
                   "3-4(medium)"='2',
                   ">5(high)"='3') %>% 
        ff_label('Carprini risk score'),
      Anticoagulant = factor(Anticoagulant)%>%
        fct_recode("No" = "0",
                   "Yes"   = "1") %>% 
        ff_label("Anticoagulant"),
      Postoperative_ultrasound=factor(Postoperative_ultrasound)%>%
        fct_recode("None-Venous Thrombosis" = "0",
                   "Venous Thrombosis"   = "1") %>% 
        ff_label("Postoperative Ultrasound")
      
    )
  
  
  
  
  set.seed(7)  # 设置随机种子以确保结果可复现
  testIndex <- createDataPartition(data$Postoperative_ultrasound, p = 0.2, list = FALSE)
  trainData <- data[-testIndex, ]
  testData <- data[testIndex, ]
  
  data_split<-data
  data_split$index <-ifelse(row.names(data) %in% testIndex, 2,1)
  table(data_split$index,data_split$Postoperative_ultrasound)
  
  
  data_split<-data_split%>%
    mutate(
      index=factor(index) %>%          
        fct_recode("Training cohort" = "1",
                   "Validation cohort"   = "2") %>% 
        ff_label("Index"))
  table(data_split$index)
  dependent<-colnames(data_split)[26]
  explanatory <- colnames(data_split)[-26]
  datasplit_three<-data_split %>% 
    summary_factorlist(dependent, explanatory, p = TRUE,
                       add_dependent_label = TRUE,p_cat = 'fisher')
  
  write.xlsx(datasplit_three,'supplementary1.xlsx')
  
  summary(trainData$Postoperative_ultrasound)
  summary(testData$Postoperative_ultrasound)
  
  # 1. 基线表####
  tbl_summary <- trainData %>%
    tbl_summary(by =Postoperative_ultrasound) %>%
    add_p() %>%
    add_overall() %>% 
    bold_labels()
  tbl_summary
  
  testData %>%
    tbl_summary(by =Postoperative_ultrasound) %>%
    add_p() %>%
    add_overall() %>% 
    bold_labels()
  
  dependent<-colnames(trainData)[25]
  explanatory <- colnames(trainData)[-25]
  data_three<-trainData %>% 
    summary_factorlist(dependent, explanatory, p = TRUE,
                       add_dependent_label = TRUE,p_cat = 'fisher')
  
  
  write.xlsx(data_three,'tabel1.risk.xlsx')
  
  
  
  # 2. multivariable logistic regression ####
  table_logi<-trainData %>% 
    finalfit(dependent, explanatory, metrics = TRUE)
  
  
  write.xlsx(table_logi[[1]],'tabel1.univariable.xlsx')
  write.table(table_logi[[2]],'tabel1.univariable.txt')
  
  table_logi[[2]]#Number in dataframe = 303, Number in model = 303, Missing = 0, AIC = 239.8, C-statistic = 0.873, H&L = Chi-sq(8) 3.77 (p=0.877)
  
  trainData %>%
    or_plot(dependent, explanatory,
            breaks = c(0.5, 1, 2, 5, 10, 25),
            table_text_size = 3.5,
            title_text_size = 16)
}


if(T){
  ##
  library(Boruta)
  Boruta <- Boruta(Postoperative_ultrasound ~ ., 
                   data = trainData, 
                   ntree = 500,pValue=0.01, mcAdj=T, 
                   maxRuns=300)
  attStats(Boruta)
  getSelectedAttributes(Boruta, withTentative = FALSE)#获得确认的特征
  plot(Boruta) #图形显示特征选择结果
  Boruta$finalDecision[Boruta$finalDecision=="Confirmed"]
  boruta.imp <- function(x){
    imp <- reshape2::melt(x$ImpHistory, na.rm=T)[,-1]
    colnames(imp) <- c("Variable","Importance")
    imp <- imp[is.finite(imp$Importance),]
    
    variableGrp <- data.frame(Variable=names(x$finalDecision), 
                              finalDecision=x$finalDecision)
    
    showGrp <- data.frame(Variable=c("shadowMax", "shadowMean", "shadowMin"),
                          finalDecision=c("shadowMax", "shadowMean", "shadowMin"))
    
    variableGrp <- rbind(variableGrp, showGrp)
    
    boruta.variable.imp <- merge(imp, variableGrp, all.x=T)
    
    sortedVariable <- boruta.variable.imp %>% group_by(Variable) %>% 
      summarise(median=median(Importance)) %>% arrange(median)
    sortedVariable <- as.vector(sortedVariable$Variable)
    
    
    boruta.variable.imp$Variable <- factor(boruta.variable.imp$Variable, levels=sortedVariable)
    
    invisible(boruta.variable.imp)
  }
  boruta.variable.imp <- boruta.imp(Boruta)
  head(boruta.variable.imp)
  library(YSX)
  p1<-sp_boxplot(boruta.variable.imp, melted=T, xvariable = "Variable", yvariable = "Importance",
                 legend_variable = "finalDecision", legend_variable_order = c("shadowMax", "shadowMean", "shadowMin", "Confirmed"),
                 xtics_angle = 90)+my_theme
  p1
  my_theme <- theme(
    text = element_text(size = 10),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    axis.line.x = element_line(colour = "black", linewidth = 0.4, linetype = "solid"),
    axis.line.y = element_line(colour = "black", linewidth = 0.4, linetype = "solid"),
    legend.background = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "right"
  )
  
  ggsave('Figure3.tiff', plot = p1, width = 7, height = 6, dpi = 600, compression = "lzw")
}


if(T){
  # 3.glm
  fit1 <- glm(Postoperative_ultrasound ~., data = trainData, family = binomial)
  fit2<- glm(Postoperative_ultrasound ~ sex+age+urate+ApoA+Preoperative_ultrasound + Hypertension
             +Hyperlipidemia+Carprini, data = trainData, family = binomial)
  fit3 <- glm(Postoperative_ultrasound ~ sex+age+Preoperative_ultrasound +
                +Hyperlipidemia, data = trainData, family = binomial)
  
  summary(fit1)
  summary(fit2)
  summary(fit3)
  
  anova0<-anova(fit1, fit2, fit3, test="Chisq")
  anova0
  
  write.table(anova0,'anova.txt')
  #如果你有三个模型（fit1，fit2，fit3）需要进行比较，并且这些模型是嵌套的，那么你可以使用anova函数进行比较。
  #这里，嵌套模型指的是一个模型是另一个模型的特殊情况。例如，fit1可能是一个简单的模型，fit2在fit1的基础上增加了一些变量，fit3在fit2的基础上再增加了一些变量
  
  
  predictions1 <- predict(fit1, newdata = trainData, type = "response")
  roc1 <- roc(trainData$Postoperative_ultrasound, predictions1)
  predictions2 <- predict(fit2, newdata = trainData, type = "response")
  roc2 <- roc(trainData$Postoperative_ultrasound, predictions2)
  predictions3<-  predict(fit3, newdata = trainData, type = "response")
  roc3<-roc(trainData$Postoperative_ultrasound, predictions3)
  
  predictions4<-predict(fit1, newdata = testData, type = "response")
  roc4<-roc(testData$Postoperative_ultrasound, predictions4)
  predictions5<-predict(fit2, newdata = testData, type = "response")
  roc5<-roc(testData$Postoperative_ultrasound, predictions5)
  predictions6<-predict(fit3, newdata = testData, type = "response")
  roc6<-roc(testData$Postoperative_ultrasound, predictions6)
  
  
  
  roc1_df <- data.frame(coords(roc1))
  roc2_df <- data.frame(coords(roc2))
  roc3_df <- data.frame(coords(roc3))
  roc4_df <- data.frame(coords(roc4))
  roc5_df <- data.frame(coords(roc5))
  roc6_df <- data.frame(coords(roc6))
  
  # Plot ROC curves and add AUC values
  # Plot ROC curves and add AUC values
  p1<-ggplot() +
    geom_smooth(data = roc4_df, aes(x = 1 - specificity, y = sensitivity, color = "model all"), 
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_smooth(data = roc5_df, aes(x = 1 - specificity, y = sensitivity, color = "model best"), 
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_abline(slope = 1, intercept = 0, color = "grey10", linetype = 2) +
    labs(x = "1 - Specificity", y = "Sensitivity", title = "ROC Curve") +
    scale_color_aaas()+
    theme_minimal(base_size = 14, base_family = "sans") +
    theme(
      panel.border = element_rect(fill = NA),
      axis.text = element_text(color = "black")) +
    annotate("text", x = 0.7, y = 0.2, label = paste("model all:", round(auc(roc4), 2)),
             color = "#377EB8", size = 4) +
    annotate("text", x = 0.7, y = 0.15, label = paste("model best:", round(auc(roc5), 2)),
             color = "#E41A1C", size = 4)
  p1
  pdf('Fig1_test.pdf',width = 7,height = 6)
  print(p1)
  dev.off()
  
  
  
  p2<-ggplot() +
    geom_smooth(data = roc1_df, aes(x = 1 - specificity, y = sensitivity, color = "full model(train)"),
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_smooth(data = roc2_df, aes(x = 1 - specificity, y = sensitivity, color = "reduced model(train)"),
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_smooth(data = roc3_df, aes(x = 1 - specificity, y = sensitivity, color = "final model(train)"),
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_smooth(data = roc4_df, aes(x = 1 - specificity, y = sensitivity, color = "full model(test)"),
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_smooth(data = roc5_df, aes(x = 1 - specificity, y = sensitivity, color = "reduced model(test)"),
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_smooth(data = roc6_df, aes(x = 1 - specificity, y = sensitivity, color = "final model(test)"),
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_abline(slope = 1, intercept = 0, linetype = 2) +
    labs(x = "1 - Specificity", y = "Sensitivity", title = "ROC Curve") +
    scale_color_aaas() +
    theme_minimal(base_size = 14, base_family = "sans") +
    theme(
      panel.border = element_rect(fill = NA),
      axis.text = element_text(color = "black")
    ) +
    annotate("text", x = 0.8, y = 0.3, label = paste("full model(train):", round(auc(roc1), 2)),
             size = 4) +
    annotate("text", x = 0.8, y = 0.25, label = paste("reduced model(train):", round(auc(roc2), 2)),
             size = 4) +
    annotate("text", x = 0.8, y = 0.20, label = paste("final model(train):", round(auc(roc3), 2)),
             size = 4) +
    annotate("text", x = 0.8, y = 0.15, label = paste("full model(test):", round(auc(roc4), 2)),
             size = 4) +
    annotate("text", x = 0.8, y = 0.1, label = paste("reduced model(test):", round(auc(roc5), 2)),
             size = 4) +
    annotate("text", x = 0.8, y = 0.05, label = paste("final model(test):", round(auc(roc6), 2)),
             size = 4)
  p2
  pdf('Fig1_all.pdf',width = 7,height = 6)
  print(p2)
  dev.off()
  
  
  
  # 使用fit2模型进行Hosmer-Lemeshow检验
  
  
  # 计算ROC曲线的敏感性和特异性
  roc <- roc(testData$Postoperative_ultrasound, predictions6)
  
  # 计算最大Youden指数
  roc_df <- data.frame(coords(roc))
  youden_index<-roc_df$sensitivity + roc_df$specificity
  Youden <- roc$thresholds[which.max(youden_index)]
  Youden
  
  roc_result <- coords(roc, "best")
  
  
  # 计算AUC、Youden指数、敏感性和特异性
  Hosmer=0.354
  AUC <- round(auc(roc), 3)
  sensitivity <- round(roc$sensitivities[which.max(roc$sensitivities + roc$specificities)], 3)
  specificity <- round(roc$specificities[which.max(roc$sensitivities + roc$specificities)], 3)
  Youden_index <- round(sensitivity+specificity-1, 3)
  
  
  # 绘制ROC曲线并添加AUC值和模型名称
  p3<-ggplot() +
    geom_smooth(data = roc6_df, aes(x = 1 - specificity, y = sensitivity,color='#E41A1C'), 
                se = FALSE, size = 1.2, linetype = "solid") +
    geom_abline(slope = 1, intercept = 0, color = "grey10", linetype = 2) +
    labs(x = "1 - Specificity", y = "Sensitivity", title = "ROC Curve") +
    theme_bw(base_size = 14, base_family = "sans") +
    theme(
      panel.border = element_rect(fill = NA),
      axis.text = element_text(color = "black")
    ) +
    annotate("text", x = 0.7, y = 0.2, label = paste("AUC=", AUC),
             color = "#377EB8", size = 4) +
    annotate("text", x = 0.7, y = 0.15, label = paste("Youden index=", Youden_index),
             color = "#377EB8", size = 4) +
    annotate("text", x = 0.7, y = 0.1, label = paste("Sensitivity=", sensitivity),
             color = "#377EB8", size = 4) +
    annotate("text", x = 0.7, y = 0.05, label = paste("Specificity=", specificity),
             color = "#377EB8", size = 4)+
    annotate("text", x = 0.7, y = 0, label = paste("Hosmer–Lemeshow,P=", Hosmer),
             color = "#377EB8", size = 4)+theme(legend.position = 'none')
  p3
  
  ggsave('Figure4.tiff', plot = p3, width = 7, height = 6, dpi = 600, compression = "lzw")
  
  
  
  # 打印Hosmer-Lemeshow检验结果和ROC曲线的相关指标
  cat("Hosmer-Lemeshow test p-value:", p_value, "\n")
  cat("AUC:", AUC, "\n")
  cat("Youden index:", Youden_index, "\n")
  cat("Sensitivity:", sensitivity, "\n")
  cat("Specificity:", specificity, "\n")
}