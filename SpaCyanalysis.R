library(readtext)
library(tidyverse)
library(textreadr)
library(purrr)
library(dplyr)
library(stringr)
library(writexl)
library(readxl)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(viridis)
library(ggpubr)
library(rstatix)
library(plyr)
library(igraph)
library(ggraph)
library(rpart)
library(janitor)
library(cluster)
library(caret)
library(e1071)
library(party)
library(randomForest)
library(GA)

#SpaCy people with mental disorder
dfMD <- read_excel("H:/spacyresults.xlsx")

#SpaCy family
dfH <- read_excel("H:/Results/resultsfamilie.xlsx")

#SpaCy health care employees
dfN <- read_excel("H:/Results/resultsHulpverleners.xlsx")

#SpaCy no stopwords
dfN <- read_excel("H:/Results/MDSPACYnostop.xlsx")
dfL <- read_excel("H:/Results/noMDSPACYnostop.xlsx")

#if no stopwords
dfnosw <- dplyr::bind_rows(dfN, dfL)
#if with stopwords
dfsw <- dplyr::bind_rows(dfMD, dfH, dfN)

dfnosw[dfnosw == 0] <- NA
dfnosw[is.na(dfnosw)] <- 0
dfsw[dfsw == 0] <- NA
dfsw[is.na(dfsw)] <- 0


#If mental disorder vs no mental disorder
dfsw$Disorder <- revalue(dfsw$Disorder, c("ADHD"="MD", "Autism"="MD", "Bipolar"='MD', "Depression"="MD", "Dissociation"="MD", "Eating"="MD", "Personality"="MD", "Personality+"="MD", "Psychosis"="MD", "Trauma"="MD"))


#prediction
# svm
dfsvm <- dfsw %>% select(-combined)

mental_idx = sample(1:nrow(dfsvm), nrow(dfsvm) / 5)
mental_trn = dfsvm[-mental_idx,]
mental_tst1 = dfsvm[mental_idx,]
mental_tst <- mental_tst1 %>% select(-Disorder)

mental_trn <- remove_constant(mental_trn,  quiet = FALSE)

notconstantvar <- intersect(names(mental_trn), names(mental_tst1))
mental_tst1 <- mental_tst1 %>% select(notconstantvar)
mental_tst <- mental_tst1 %>% select(-Disorder)

mental_trn$Disorder <- as.factor(mental_trn$Disorder)
mental_tst1$Disorder <- as.factor(mental_tst1$Disorder)

svmfit = svm(Disorder ~ ., data = mental_trn, method = "class")
y_pred = predict(svmfit, newdata = mental_tst)
confusionMatrix(mental_tst1$Disorder, y_pred)

#rpart

fit <- rpart(Disorder ~ ., method="class", data= mental_trn)
prediction <- predict(fit, newdata=mental_tst, type ="class", na.action = na.pass) 
confusionMatrix(mental_tst1$Disorder, prediction)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
rpart.plot(fit)


#randomforest
rf = randomForest(x = mental_trn[,2:514], y = mental_trn$Disorder)
rf
rf$confusion[, 'class.error'] 
options('digits'=3)
plot(rf)

rfpredict <- predict(rf, newdata = mental_tst)
confusionMatrix(mental_tst1$Disorder, rfpredict)



