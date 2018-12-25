# Load dataset 
library(readr)
diabetic_data <- read_csv("R/Dataset/diabetic_data.csv")

library(dplyr)
diabetic_data<-diabetic_data %>% distinct(diabetic_data$patient_nbr, .keep_all = TRUE)

# Removal of discharge to hospice
df <- diabetic_data$discharge_disposition_id
diabetic_data <- diabetic_data[df != "13" & df != "14" & df != "11" & df != "19" & df != "20" & df != "21" , ]

#data of diag_1 into subcategories
diabetic_data$diag_1<-as.numeric(as.character(diabetic_data$diag_1))
diabetic_data$diag_1<-diabetic_data$diag_1*100
diabetic_data$diag_1<-as.integer(diabetic_data$diag_1)

circulatory<-subset(diabetic_data, diag_1>=39000& diag_1<=45900|diabetic_data$diag_1==78500)
respiratory<-subset(diabetic_data,diag_1>=46000& diag_1<=51900|diabetic_data$diag_1==78600)
digestive<-subset(diabetic_data,diag_1>=52000& diag_1<=57900|diabetic_data$diag_1==78700)
diabetes<-subset(filter(diabetic_data,diag_1>=25000&diag_1<25100))
injury<-subset(diabetic_data,diag_1>=80000& diag_1<=99900)
musculoskeletal<-subset(diabetic_data,diag_1>=71000& diag_1<=73900)
Genitourinary<-subset(diabetic_data,diag_1>=58000& diag_1<=62900|diabetic_data$diag_1==78800)
Neoplasms<-subset(diabetic_data,diag_1>=14000& diag_1<=23900)

#change the name of the category
circulatory$diag_1[circulatory$diag_1 %in% diabetic_data$diag_1]<-"circulatory"
respiratory$diag_1[respiratory$diag_1 %in% diabetic_data$diag_1]<-"respiratory"
digestive$diag_1[digestive$diag_1 %in% diabetic_data$diag_1]<-"digestive"
diabetes$diag_1[diabetes$diag_1 %in% diabetic_data$diag_1]<-"diabetes"
injury$diag_1[injury$diag_1 %in% diabetic_data$diag_1]<-"injury"
musculoskeletal$diag_1[musculoskeletal$diag_1 %in% diabetic_data$diag_1]<-"musculoskeletal"
Genitourinary$diag_1[Genitourinary$diag_1 %in% diabetic_data$diag_1]<-"genitourinary"
Neoplasms$diag_1[Neoplasms$diag_1 %in% diabetic_data$diag_1]<-"neoplasms"

#replace the data
new_data<-rbind(circulatory,respiratory,digestive,diabetes,injury,musculoskeletal,Genitourinary,Neoplasms)
other<-subset(diabetic_data,!(patient_nbr %in% new_data$patient_nbr))
other$diag_1[other$diag_1 %in% diabetic_data$diag_1]<-"other"
diabetic_data<-rbind(new_data,other)



#df<-diabetic_data[50]
#df[df==">30"]<-"NO"
#diabetic_data[50]<-df

### Show values
#table(diabetic_data$readmitted)
#table(diabetic_data$A1Cresult)

### Create training data
## Defining input data 
# HbA1c measurement
input_NoA1c <- diabetic_data[which(diabetic_data$A1Cresult == "None"),]
input_NormA1c <- diabetic_data[which(diabetic_data$A1Cresult == "Norm"), ]
input_A1cNoCh <- diabetic_data[which(diabetic_data$A1Cresult == ">8"       
                                     & diabetic_data$change == "No"), ]          
input_A1cCh <- diabetic_data[which(diabetic_data$A1Cresult == ">8" 
                                   & diabetic_data$change == "Ch"), ]
# Set seeds
set.seed(100) # for repeatability of samples

### Creating input training data
# readmission and HbA1c measurement 
input_NoA1c_training_rows <- sample(1:nrow(input_NoA1c), 0.7*nrow(input_NoA1c))
input_NormA1c_training_rows <- sample(1:nrow(input_NormA1c), 0.7*nrow(input_NormA1c))
input_A1cNoCh_training_rows <- sample(1:nrow(input_A1cNoCh), 0.7*nrow(input_A1cNoCh)) 
input_A1cCh_training_rows <- sample(1:nrow(input_A1cCh), 0.7*nrow(input_A1cCh))




## Combning training data
#HbA1c messurement
training_NoA1c <- input_NoA1c[input_NoA1c_training_rows, ]
training_NormA1c <- input_NormA1c[input_NormA1c_training_rows, ]
training_A1cNoCh <- input_A1cNoCh[input_A1cNoCh_training_rows, ] 
training_A1cCh <- input_A1cCh[input_A1cCh_training_rows, ]

training_A1cNoCh$A1Cresult<-as.character(training_A1cNoCh$A1Cresult)
training_A1cNoCh$A1Cresult[training_A1cNoCh$A1Cresult %in% diabetic_data$A1Cresult]<-">8No"
training_A1cNoCh$A1Cresult<-factor(training_A1cNoCh$A1Cresult)


training_A1cCh$A1Cresult<-as.character(training_A1cCh$A1Cresult)
training_A1cCh$A1Cresult[training_A1cCh$A1Cresult %in% diabetic_data$A1Cresult]<-">8Ch"
training_A1cCh$A1Cresult<-factor(training_A1cCh$A1Cresult)


trainingData <- rbind(training_NoA1c, training_NormA1c, training_A1cNoCh, training_A1cCh)

input_s30 <- trainingData[which(trainingData$readmitted == "<30"), ]
input_g30 <- trainingData[which(trainingData$readmitted == ">30"), ]
input_NO  <- trainingData[which(trainingData$readmitted == "NO"), ]

input_s30_rows <- sample(1:nrow(input_s30), nrow(input_s30), replace = F)
input_g30_rows <- sample(1:nrow(input_g30), nrow(input_s30), replace = F)
input_NO_rows  <- sample(1:nrow(input_NO), nrow(input_s30), replace = F)

newData_s30 <- input_s30[input_s30_rows, ]
newData_g30 <- input_g30[input_g30_rows, ]
newData_NO <- input_NO[input_NO_rows, ]


newData <- rbind(newData_s30, newData_g30, newData_NO)
table(newData$readmitted)
trainingData<-newData

###balance the data
#library(ROSE)


###trainingData<-str(trainingData)
#under<-ovun.sample(readmitted~A1cResult+ diag_1,data=trainingData,method="under", N=14385)$data

#newData <- SMOTE(readmitted ~ A1Cresult+diag_1+age+gender+race+time_in_hospital+diabetesMed+insulin+metformin+repaglinide, trainingData)

### Create Test Data 
# Readmission and HbA1c messurement
test_NoA1c <- input_NoA1c[-input_NoA1c_training_rows, ]
test_NormA1c <- input_NormA1c[-input_NormA1c_training_rows, ]
test_A1cNoCh <- input_A1cNoCh[-input_A1cNoCh_training_rows, ]
test_A1cCh <- input_A1cCh[-input_A1cCh_training_rows, ]

test_A1cNoCh$A1Cresult<-as.character(test_A1cNoCh$A1Cresult)
test_A1cNoCh$A1Cresult[test_A1cNoCh$A1Cresult %in% diabetic_data$A1Cresult]<-">8No"
test_A1cNoCh$A1Cresult<-factor(test_A1cNoCh$A1Cresult)

test_A1cCh$A1Cresult<-as.character(test_A1cCh$A1Cresult)
test_A1cCh$A1Cresult[test_A1cCh$A1Cresult %in% diabetic_data$A1Cresult]<-">8Ch"
test_A1cCh$A1Cresult<-factor(test_A1cCh$A1Cresult)

testData <- rbind(test_NoA1c, test_NormA1c, test_A1cNoCh, test_A1cCh)

input_s30 <- testData[which(testData$readmitted == "<30"), ]
input_g30 <- testData[which(testData$readmitted == ">30"), ]
input_NO  <- testData[which(testData$readmitted == "NO"), ]

input_s30_rows <- sample(1:nrow(input_s30), nrow(input_s30), replace = F)
input_g30_rows <- sample(1:nrow(input_g30), nrow(input_s30), replace = F)
input_NO_rows  <- sample(1:nrow(input_NO), nrow(input_s30), replace = F)

newData_s30 <- input_s30[input_s30_rows, ]
newData_g30 <- input_g30[input_g30_rows, ]
newData_NO <- input_NO[input_NO_rows, ]


newData <- rbind(newData_s30, newData_g30, newData_NO)
table(newData$readmitted)
testData<-newData


### Logistic Regression 
library(nnet)

trainingData$A1Cresult <- relevel(trainingData$A1Cresult, ref = "None")
logitMod <- multinom(readmitted ~A1Cresult+diag_1, data = trainingData)
predicted <- predict(logitMod,newdata=testData,type="class")

summary(logitMod)

accuracy <- table(predicted, testData[,"readmitted"])
sum(diag(accuracy))/sum(accuracy)

### Logistic Regression 
trainingData$A1Cresult <- relevel(trainingData$A1Cresult, ref = "None")
logitMod <- glm(readmitted ~ A1Cresult+diag_1, data = trainingData, family = binomial)
predicted <- predict(logitMod, testData, type = "response")
summary(logitMod)

##head(as.data.frame(predicted))
##mean(as.character(predicted) != as.character(testData$readmitted))

library(InformationValue)
### Probability 
optCutOff <- optimalCutoff(testData$readmitted, predicted)
#=>0.8934068
misClassError(testData$readmitted, predicted, threshold = optCutOff)


### Plot
plot(testData$readmitted, predicted)
