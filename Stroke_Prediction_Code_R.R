# List of packages for session
.packages = c("tidyverse", "knitr", "caret", "randomForest", "rpart", "reshape2", "polycor")

# Install missing packages
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages  
lapply(.packages, require, character.only=TRUE)

#Downloading the data
dl <- tempfile()
download.file("https://drive.google.com/uc?export=download&id=14o2diuGl0eS2B80yoW5bJDnfhyzi1Cu9", dl)

df <- read.csv(dl)
df2 <- read.csv(dl)

df$bmi <- as.numeric(df$bmi)
df <- df %>% filter(df$gender %in% c("Female", "Male"))

# Validation set will be 10% of Stroke Prediction data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = df$stroke, times = 1, p = 0.2, list = FALSE)
stroke <- df[-test_index,]
validation <- df[test_index,]

###########################
#Exploratory Data Analysis#
###########################

#Table 1: Dataset Dimensions
Table_1 <- data.frame(
  Dataset = c("stroke", "validation"),
  No_of_Rows = c(nrow(stroke), nrow(validation)),
  No_of_Cols = c(ncol(stroke), ncol(validation)))

knitr::kable(
  Table_1,
  format = "pipe",
  caption = "Dataset Dimensions",
  col.names = c("Dataset", "No. of Rows", "No of Columns"))


#Table 2: Missing Data
NAs <- function(x) {sum(is.na(x))}
stroke_NAs <- sapply(stroke, NAs) 
validation_NAs <- sapply(validation, NAs)


data.frame(cbind(stroke_NAs, validation_NAs)) %>%  
  knitr::kable(format = "pipe", 
               caption = "Number of N/A-s",
               col.names = c("Stroke", "Validation"))


#Replacing N/As with the median of the whole df dataset
df_temp <- df %>% filter(bmi != "NA" )
NA_value <- median(df_temp$bmi)
stroke$bmi <- ifelse(is.na(stroke$bmi), NA_value, stroke$bmi)
validation$bmi <- ifelse(is.na(validation$bmi), NA_value, validation$bmi)

#Table 3: Preview of the dataset
stroke %>%
  select(1:6) %>%
  head() %>% 
  knitr::kable(format = "pipe", caption = "Preview of the dataset")

#Table 4: Preview of the dataset2
stroke %>%
  select(7:12) %>%
  head() %>% 
  knitr::kable(format = "pipe", caption = "Preview of the dataset")


#Plot 1: Distribution of genders
stroke %>%
  group_by(gender) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = gender, y = Rating)) +
  geom_bar(stat = "identity", fill = "cyan2") +
  labs(title="Distribution of Genders", x = "Gender", y = "Rate") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()


#Plot 2: Distribution of Age
stroke %>%
  mutate(age_group = case_when(age < 15 ~ "0-14",
                               age < 25 & age >= 15 ~ "15-24",
                               age < 55 & age >= 25 ~ "25-54",
                               age < 65 & age >= 55 ~ "55-64",
                               age >= 65 ~ ">65")) %>%
  group_by(age_group) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = age_group, y = Rating)) +
  geom_bar(stat = "identity", fill = "tomato3") +
  labs(title="Distribution of Age", x = "Age Group", y = "Rate") +
  scale_x_discrete(limits=c("0-14", "15-24", "25-54", "55-64", ">65")) +
  scale_y_continuous(labels = scales::percent)

#Table 5: BMI categories
Table_5 <- data.frame(
  BMI_Cat = c("Underweight", "Normal Weight", "Overweight", "Obese"),
  BMI_Value = c("<20", "20-25", "25-30", ">30"))

knitr::kable(
  Table_5,
  format = "pipe",
  caption = "BMI Categories",
  col.names = c("BMI Category", "BMI Value"))


#Plot 3: Distribution of BMI
stroke %>%
  mutate(bmi_group = case_when(bmi < 20 ~ "<20",
                               bmi < 25 & bmi >= 20 ~ "20-25",
                               bmi < 30 & bmi >= 25 ~ "25-30",
                               bmi >= 30 ~ ">30")) %>%
  group_by(bmi_group) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = bmi_group, y = Rating)) +
  geom_bar(stat = "identity", fill = "burlywood2") +
  labs(title="Distribution of BMI", x = "BMI Group", y = "Rate") +
  scale_x_discrete(limits=c("<20", "20-25", "25-30", ">30")) +
  scale_y_continuous(labels = scales::percent) 


#Plot 4: Distribution of Stroke
stroke %>%
  group_by(stroke) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = stroke, y = Rating)) +
  geom_bar(stat = "identity", fill = "lightslateblue") +
  labs(title="Distribution of Strokes", x = "Stroke", y = "Rate") +
  scale_x_discrete(limits=c(0, 1), labels=c("0" = "No", "1" = "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip()


#Plot 5: Connection between Heart disease and Stroke
StrokeHD <- stroke %>%
  filter(heart_disease == 1)


StrokeHD %>%
  group_by(stroke) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = stroke, y = Rating)) +
  geom_bar(stat = "identity", fill = "brown3") +
  labs(title="Distribution of Strokes with underlying Heart Disease", x = "Stroke", y = "Rate") +
  scale_x_discrete(limits=c(0, 1), labels=c("0" = "No", "1" = "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13)) +
  coord_flip()

#Plot 5: Connection between High Blood Pressure and Stroke
StrokeHBP <- stroke %>%
  filter(hypertension == 1)

StrokeHBP %>%
  group_by(stroke) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = stroke, y = Rating)) +
  geom_bar(stat = "identity", fill = "brown3") +
  labs(title="Distribution of Strokes with High Blood Pressure", x = "Stroke", y = "Rate") +
  scale_x_discrete(limits=c(0, 1), labels=c("0" = "No", "1" = "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13)) +
  coord_flip()

#Plot 6: Connection between Smoking Status and Stroke
StrokeSS <- stroke %>%
  filter(smoking_status == "smokes")

StrokeSS %>%
  group_by(stroke) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = stroke, y = Rating)) +
  geom_bar(stat = "identity", fill = "brown3") +
  labs(title="Distribution of Strokes with Smoking", x = "Stroke", y = "Rate") +
  scale_x_discrete(limits=c(0, 1), labels=c("0" = "No", "1" = "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13)) +
  coord_flip()

mean(StrokeSS$stroke ==1)

#Plot 7: Effect of Rural Life an Marriage on Stroke
StrokeMR <- stroke %>%
  filter(ever_married == "Yes")

StrokeMR %>%
  group_by(stroke) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = stroke, y = Rating)) +
  geom_bar(stat = "identity", fill = "deepskyblue3") +
  labs(title="Effect of Marriage on Stroke", x = "Stroke", y = "Rate") +
  scale_x_discrete(limits=c(0, 1), labels=c("0" = "No", "1" = "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13)) +
  coord_flip()

mean(StrokeMR$stroke ==1)

StrokeRL <- stroke %>%
  filter(Residence_type == "Rural")

StrokeRL %>%
  group_by(stroke) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = stroke, y = Rating)) +
  geom_bar(stat = "identity", fill = "deepskyblue3") +
  labs(title="Effect of Rural Life on Stroke", x = "Stroke", y = "Rate") +
  scale_x_discrete(limits=c(0, 1), labels=c("0" = "No", "1" = "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13)) +
  coord_flip()

mean(StrokeRL$stroke ==1)

#Plot 8: Effect of Rural Life an Obesity on Stroke
StrokeOB <- stroke %>%
  filter(bmi > 30)

StrokeOB %>%
  group_by(stroke) %>%
  summarise(Rating = n()/nrow(.), number = n()) %>%
  ggplot(aes(x = stroke, y = Rating)) +
  geom_bar(stat = "identity", fill = "deepskyblue3") +
  labs(title="Effect of Obesity on Stroke", x = "Stroke", y = "Rate") +
  scale_x_discrete(limits=c(0, 1), labels=c("0" = "No", "1" = "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13)) +
  coord_flip()

mean(StrokeOB$stroke ==1)

#Converting and Removing Features
stroke <- stroke[,-1]
stroke$stroke <- as.factor(stroke$stroke)
stroke$gender <- as.factor(stroke$gender)
stroke$hypertension <- as.factor(stroke$hypertension)
stroke$heart_disease <- as.factor(stroke$heart_disease)
stroke$ever_married <- as.factor(stroke$ever_married)
stroke$work_type <- as.factor(stroke$work_type)
stroke$Residence_type <- as.factor(stroke$Residence_type)
stroke$smoking_status <- as.factor(stroke$smoking_status)

validation <- validation[,-1]
validation$stroke <- as.factor(validation$stroke)
validation$gender <- as.factor(validation$gender)
validation$hypertension <- as.factor(validation$hypertension)
validation$heart_disease <- as.factor(validation$heart_disease)
validation$ever_married <- as.factor(validation$ever_married)
validation$work_type <- as.factor(validation$work_type)
validation$Residence_type <- as.factor(validation$Residence_type)
validation$smoking_status <- as.factor(validation$smoking_status)

#Plot 9: Correlation Matrix
cormat <- round(hetcor(stroke)$correlations, digits=2)
melted_cormat <- melt(cormat)

get_lower_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)}

lower_tri <- get_lower_tri(cormat)

melted_cormat <- reshape2::melt(lower_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1), text=element_text(size=8))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal",
    axis.text=element_text(size=8),
    text=element_text(size=8),
    axis.title=element_text(size=8),
    legend.text=element_text(size=8)) +
  guides(fill = guide_colorbar(barwidth = 8, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

#################
#Building models#
#################


#Examination of the variability of the predictors
var_check <- stroke %>% select(age, avg_glucose_level, bmi)

nearZeroVar(var_check)


#Addressing the Prevalence
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
balanced_stroke <- upSample(x = stroke[, -ncol(stroke)],
                     y = stroke$stroke,
                     yname = "stroke")


#Table 6: Structure of the Balanced Train Set
knitr::kable(
  table(balanced_stroke$stroke),
  format = "pipe",
  caption = "Structure of the Balanced Train Set",
  col.names = c("Stroke", "Number"))


#5.1 glm model#######################################################

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
model_glm <- train(stroke ~ .,
               data = balanced_stroke,
               method = "glm",
               family=binomial())

prediction_glm <- predict(model_glm, validation, type = "raw")
cm_glm <- confusionMatrix(data = prediction_glm, reference = validation$stroke, dnn = c("Prediction", "Reference"))

#Plot 10: Confusion Matrix (glm model)
ggplot(as.data.frame(cm_glm$table), aes(y=rev(Prediction), x=Reference, fill= Freq)) +
  geom_tile(color = "black") + geom_text(aes(label=Freq),size=5) +
  scale_fill_gradient(low="white", high="lightgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("0","1")) +
  scale_y_discrete(labels=c("1","0")) +
  ggtitle("Confusion Matrix (glm model)") +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13))

#Table 7: Model Performance (glm model)
Summary_Table <- data.frame(glm = c(round(cm_glm$overall["Accuracy"], digits = 4), 
                                    round(cm_glm$byClass["Sensitivity"], digits = 4),
                                    round(cm_glm$byClass["Specificity"], digits = 4),
                                    round(cm_glm$byClass["F1"], digits = 4)))

knitr::kable(Summary_Table,
             format = "pipe",
             caption = "Performance Metrics")
          

#5.2 LDA model#######################################################

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
model_lda <- train(stroke ~ .,
                   data = balanced_stroke,
                   method = "lda")

prediction_lda <- predict(model_lda, validation, type = "raw")
cm_lda <- confusionMatrix(data = prediction_lda, reference = validation$stroke, dnn = c("Prediction", "Reference"))

#Plot 11: Confusion Matrix (LDA model)
ggplot(as.data.frame(cm_lda$table), aes(y=rev(Prediction), x=Reference, fill= Freq)) +
  geom_tile(color = "black") + geom_text(aes(label=Freq),size=5) +
  scale_fill_gradient(low="white", high="lightgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("0","1")) +
  scale_y_discrete(labels=c("1","0")) +
  ggtitle("Confusion Matrix (LDA model)") +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13))



#Table 8: Model Performance (LDA model)
Summary_Table["LDA"] <- c(round(cm_lda$overall["Accuracy"], digits = 4), 
                          round(cm_lda$byClass["Sensitivity"], digits = 4),
                          round(cm_lda$byClass["Specificity"], digits = 4),
                          round(cm_lda$byClass["F1"], digits = 4))

knitr::kable(Summary_Table,
             format = "pipe",
             caption = "Performance Metrics")


#5.3 kNN model#######################################################

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
model_knn <- train(stroke ~ .,
                   data = balanced_stroke,
                   method = "knn",
                   tuneGrid = data.frame(k = c(3, 5, 7, 9, 13, 17)))

prediction_knn <- predict(model_knn, validation, type = "raw")
cm_knn <- confusionMatrix(data = prediction_knn, reference = validation$stroke, dnn = c("Prediction", "Reference"))

#Plot 12: k ~ accuracy
ggplot(model_knn, highlight = TRUE) +
  ggtitle("k ~ Accuracy Plot")

#Plot 13: Confusion Matrix (kNN model)
ggplot(as.data.frame(cm_knn$table), aes(y=rev(Prediction), x=Reference, fill= Freq)) +
  geom_tile(color = "black") + geom_text(aes(label=Freq),size=5) +
  scale_fill_gradient(low="white", high="lightgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("0","1")) +
  scale_y_discrete(labels=c("1","0")) +
  ggtitle("Confusion Matrix (kNN model)") +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13))



#Table 12: Model Performance (knn model)
Summary_Table["kNN"] <- c(round(cm_knn$overall["Accuracy"], digits = 4), 
                          round(cm_knn$byClass["Sensitivity"], digits = 4),
                          round(cm_knn$byClass["Specificity"], digits = 4),
                          round(cm_knn$byClass["F1"], digits = 4))

knitr::kable(Summary_Table,
             format = "pipe",
             caption = "Performance Metrics")

model_knn$bestTune


#5.4 rpart model#######################################################

model_rp <- train(stroke ~ .,
                  data = balanced_stroke,
                  method = "rpart",
                  tuneGrid = data.frame(cp = seq(0.01, 0.04, 0.002)))

prediction_rp <- predict(model_rp, validation, type = "raw")
cm_rp <- confusionMatrix(data = prediction_rp, reference = validation$stroke, dnn = c("Prediction", "Reference"))

#Plot 14: cp plot
ggplot(model_rp, highlight = TRUE) +
  ggtitle("Complexity Parameter ~ Accuracy Plot")

#Plot 15: Confusion Matrix (rpart model)
ggplot(as.data.frame(cm_rp$table), aes(y=rev(Prediction), x=Reference, fill= Freq)) +
  geom_tile(color = "black") + geom_text(aes(label=Freq),size=5) +
  scale_fill_gradient(low="white", high="lightgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("0","1")) +
  scale_y_discrete(labels=c("1","0")) +
  ggtitle("Confusion Matrix (rpart model)") +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13))

model_rp$bestTune


#Table 13: Model Performance (rf model)
Summary_Table["rpart"] <- c(round(cm_rp$overall["Accuracy"], digits = 4), 
                            round(cm_rp$byClass["Sensitivity"], digits = 4),
                            round(cm_rp$byClass["Specificity"], digits = 4),
                            round(cm_rp$byClass["F1"], digits = 4))

knitr::kable(Summary_Table,
             format = "pipe",
             caption = "Performance Metrics")

#Plot 16: Decision tree (rpart model)
plot(model_rp$finalModel, margin = 0.01, main="CART Final Model", cex.main=1)
text(model_rp$finalModel, cex = 0.75)


#5.5 randomForest model#######################################################

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(3, 5, 7, 10))
model_rf <- train(stroke ~ .,
                  data = balanced_stroke,
                  trControl = control,
                  tuneGrid = grid,
                  ntree = 100,
                  cutoff = c(0.8, 0.2),
                  method = "rf")

prediction_rf <- predict(model_rf, validation, type = "raw")
cm_rf <- confusionMatrix(data = prediction_rf, reference = validation$stroke, dnn = c("Prediction", "Reference"))

#Plot 17: mtry plot
ggplot(model_rf, highlight = TRUE) +
  ggtitle("mtry ~ Accuracy Plot")

#Plot 18: Confusion Matrix (randomForest model)
ggplot(as.data.frame(cm_rf$table), aes(y=rev(Prediction), x=Reference, fill= Freq)) +
  geom_tile(color = "black") + geom_text(aes(label=Freq),size=5) +
  scale_fill_gradient(low="white", high="lightgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("0","1")) +
  scale_y_discrete(labels=c("1","0")) +
  ggtitle("Confusion Matrix (randomForest model)") +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13))


#Table 14: Model Performance (rf model)
Summary_Table["randomForest"] <- c(round(cm_rf$overall["Accuracy"], digits = 4), 
                          round(cm_rf$byClass["Sensitivity"], digits = 4),
                          round(cm_rf$byClass["Specificity"], digits = 4),
                          round(cm_rf$byClass["F1"], digits = 4))

knitr::kable(Summary_Table,
             format = "pipe",
             caption = "Performance Metrics")


#5.6 Ensemble model#######################################################

pred_df <- data.frame(prediction_glm,
                prediction_lda,
                prediction_knn,
                prediction_rp,
                prediction_rf)

prediction_ens <- round(rowSums(sapply(pred_df, function(x) as.numeric(as.character(x))))/5)
prediction_ens <- as.factor(prediction_ens)
cm_ens <- confusionMatrix(data = prediction_ens, reference = validation$stroke, dnn = c("Prediction", "Reference"))

#Plot 19: Confusion Matrix (ensemble model)
ggplot(as.data.frame(cm_ens$table), aes(y=rev(Prediction), x=Reference, fill= Freq)) +
  geom_tile(color = "black") + geom_text(aes(label=Freq),size=5) +
  scale_fill_gradient(low="white", high="lightgreen") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c("0","1")) +
  scale_y_discrete(labels=c("1","0")) +
  ggtitle("Confusion Matrix (Ensemble model)") +
  theme(axis.text=element_text(size=13), axis.title=element_text(size=13), title=element_text(size=13), 
        legend.text=element_text(size=13))


#Table 15: Model Performance (ensemble model)
Summary_Table["Ensemble"] <- c(round(cm_ens$overall["Accuracy"], digits = 4), 
                                   round(cm_ens$byClass["Sensitivity"], digits = 4),
                                   round(cm_ens$byClass["Specificity"], digits = 4),
                                   round(cm_ens$byClass["F1"], digits = 4))

knitr::kable(Summary_Table,
             format = "pipe",
             caption = "Performance Metrics")


#THE END#######################################################

