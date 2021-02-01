library(readtext)
library(tidyverse)
library(textreadr)
library(dplyr)
library(stringr)
library(readxl)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(viridis)
library(ggpubr)
library(rstatix)
library(plyr)
library(gplots)
library(FactoMineR)
library(factoextra)
library(ggsci)
library(tree)
library(rpart)
library(cluster)
library(caret)
library(e1071)
library(party)
library(randomForest)
library(rpart.plot)
library(pROC)
library(ROCR)


df <- read_excel("H:/LIWCproject/LIWCresultstotal.xlsx")
dfnon <- read_excel("H:/Results/ResultsNonMD.xlsx")

dftotal <- rbind(df, dfnon)

dftotal <- dftotal %>% select(-Filename, -WC)

dftotal$Segment <- revalue(dftotal$Segment, c("ADHD"="MD", "Autism"="MD", "Bipolar"='MD', "Depression"="MD", "Dissociation"="MD", "Eating"="MD", "Personality"="MD", "Personality+"="MD", "Psychosis"="MD", "Trauma"="MD", "NonMD"='NoMD'))


#dftotal$Segment<- revalue(dftotal$Segment, c('MD'= 1, 'NoMD' = 0))

#df <- df%>% select(-WC, -Filename)



#Descriptive statistics
ggplot(df, aes(Segment, WC))+geom_boxplot()+xlab("Mental disorder") + ylab("Words per text")+theme(text = element_text(size=),axis.text.x = element_text(angle=90, hjust=1))+theme_minimal()+theme(text = element_text(size=17),axis.text.x = element_text(angle=90, hjust=1))
ggplot(df, aes(Segment))+geom_bar() +  theme(axis.text.x = element_text(angle = 90))+xlab("Mental disorder") + ylab("Amount")+ theme_minimal()+theme(text = element_text(size=17),axis.text.x = element_text(angle=90, hjust=1))

dftotal$Segment<- revalue(dftotal$Segment, c('MD'= 1, 'NoMD' = 0))

#Predicting rpart
mental_idx = sample(1:nrow(dftotal), nrow(dftotal) / 5)
mental_trn = dftotal[-mental_idx,]
mental_tst1 = dftotal[mental_idx,]
mental_tst <- mental_tst1 %>% select(-Segment)

mental_trn$Segment <- as.factor(mental_trn$Segment)
mental_tst1$Segment <- as.factor(mental_tst1$Segment)


fit <- rpart(Segment ~ ., method="class", data= mental_trn )


prediction <- predict(fit, newdata=mental_tst, type ="class", na.action = na.pass)  # factor

confusionMatrix(mental_tst1$Segment, prediction)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
rpart.plot(fit)


#randomforest
rfmodel = randomForest(x = mental_trn[,2:89], y = mental_trn$Segment, importance=TRUE)
rfmodel
plot(rfmodel)
rfpredict <- predict(rfmodel, newdata = mental_tst[,2:89])
confusionMatrix(mental_tst1$Segment, rfpredict)

#random forest AUC

test.forest = predict(rfmodel, type = 'prob', newdata = mental_tst)

rocplot <- function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
  area <- auc(truth, pred)
  area <- format(round(area, 4), nsmall = 4)
  text(x=0.8, y=0.1, labels = paste("AUC =", area))
  
  # the reference x=y line
  segments(x0=0, y0=0, x1=1, y1=1, col="gray", lty=2)
}

rocplot(test.forest[,2], mental_tst1$Segment, col="blue")

# svm
svmfit = svm(Segment ~ ., data = mental_trn, method = "class")
y_pred = predict(svmfit, newdata = mental_tst, type='class')
confusionMatrix(mental_tst1$Segment, y_pred)

