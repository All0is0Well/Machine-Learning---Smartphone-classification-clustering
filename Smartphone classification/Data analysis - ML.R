###############################################################################
###############################################################################

#-----------------------------------------------------------------------------------------------------------------
########################################## Installing and loading Packages #######################################
#-----------------------------------------------------------------------------------------------------------------

install.packages("tidyverse")
install.packages("skimr")
install.packages("readr")
install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")
install.packages("class")
install.packages("gmodels")
install.packages("e1071")
install.packages("OneR")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("C50")
install.packages('corrplot')
install.packages("rmarkdown")
install.packages("knitr")
install.packages("Hmisc")
install.packages("GGally")
install.packages("plotly")
install.packages("caret")


library(tidyverse)
library(skimr)
library(readr)
library(ggplot2)
library(cluster)
library(factoextra)
library(class)
library(gmodels)
library(e1071)
library(OneR)
library(rpart)
library(rpart.plot)
library(C50)
library(corrplot)
library(Hmisc)
library(GGally)
library(plotly)
library(caret)
#-----------------------------------------------------------------------------------------------------------------
########################################## Reading csv file and loading Data  ####################################
#-----------------------------------------------------------------------------------------------------------------

Data <- read_csv("C:/Users/deept/OneDrive/Desktop/study/Data Mining and Machine Learning/InputData.csv")
Data

 
#Checking skimr report
skim(Data)

#checking the structure of the Data
str(Data)

#Check for duplicated rows
Data_NA <- unique(Data)

#Check for NA values
is.na(Data_NA)

#Correlation Data before data correction
cor_data<- Data_NA %>% filter(Smartphone =="Android" | Smartphone == "iPhone")
cor_data <- cor_data %>% mutate(Female = ifelse(cor_data$Gender == "female", 1, 0))
cor_data <- cor_data %>% mutate(Male = ifelse(cor_data$Gender == "male", 1, 0))
cor_data <- cor_data %>% mutate(Other = ifelse(cor_data$Gender == "other", 1, 0))
cor_data <- cor_data %>% mutate(Android_phone = ifelse(cor_data$Smartphone == "Android", 1, 0))
cor_data <- cor_data %>% mutate(i_phone = ifelse(cor_data$Smartphone == "iPhone", 1, 0))


#------------------------------------------------------------------------------------------------------------------
################################################## Data Correction ################################################
#------------------------------------------------------------------------------------------------------------------

#Replacing empty strings with NA Values
Data_NA <- replace(Data, Data=='', NA)
Data_NA <- replace(Data_NA, Data_NA == 'g', NA)
Data_NA <- replace(Data_NA, Data_NA == 's', NA)
Data_NA <- replace(Data_NA, Data_NA == 'a', NA)
Data_NA$Age <- ifelse(as.numeric(Data_NA$Age) > 103, NA, Data_NA$Age)
Data_NA$`Social Economic Status` <- ifelse(as.numeric(Data_NA$`Social Economic Status`) >10, NA, Data_NA$`Social Economic Status`) 
skim(Data_NA)

#Replacing NA values with mean values
impute.mean <- function(x) mean(as.numeric(as.character(x)), na.rm = TRUE)
Data_NA$Age[is.na(Data_NA$Age)] <- impute.mean(Data_NA$Age)
Data_NA$`Honesty-Humility`[is.na(Data_NA$`Honesty-Humility`)] <- mean(as.numeric(as.character(Data_NA$`Honesty-Humility`)), na.rm = TRUE)
Data_NA$Emotionality[is.na(Data_NA$Emotionality)] <- mean(as.numeric(as.character(Data_NA$Emotionality)), na.rm = TRUE)
Data_NA$Extraversion[is.na(Data_NA$Extraversion)] <- mean(as.numeric(as.character(Data_NA$Extraversion)), na.rm = TRUE)
Data_NA$Agreeableness[is.na(Data_NA$Agreeableness)] <- mean(as.numeric(as.character(Data_NA$Agreeableness)), na.rm = TRUE)
Data_NA$Conscientiousness[is.na(Data_NA$Conscientiousness)] <- mean(as.numeric(as.character(Data_NA$Conscientiousness)), na.rm = TRUE)
Data_NA$Openness[is.na(Data_NA$Openness)] <- mean(as.numeric(as.character(Data_NA$Openness)), na.rm = TRUE)
Data_NA$`Phone as status object`[is.na(Data_NA$`Phone as status object`)] <- mean(as.numeric(as.character(Data_NA$`Phone as status object`)), na.rm = TRUE)
Data_NA$`Social Economic Status`[is.na(Data_NA$`Social Economic Status`)] <- mean(as.numeric(as.character(Data_NA$`Social Economic Status`)), na.rm = TRUE)
Data_NA$`Time owned current phone`[is.na(Data_NA$`Time owned current phone`)] <- mean(as.numeric(as.character(Data_NA$`Time owned current phone`)), na.rm = TRUE)

skim(Data_NA)

#------------------------------------------------------------------------------------------------------------------
################################################## Data standardization ###########################################
#------------------------------------------------------------------------------------------------------------------
SmartphoneTech <- Data_NA

#Converting gender from characters to numeric: 1 as female, 0 as male
SmartphoneTech <- SmartphoneTech %>% mutate(Female = ifelse(SmartphoneTech$Gender == "female", 1, 0))
SmartphoneTech <- SmartphoneTech %>% mutate(Male = ifelse(SmartphoneTech$Gender == "male", 1, 0))
SmartphoneTech <- SmartphoneTech %>% mutate(Other = ifelse(SmartphoneTech$Gender == "other", 1, 0))

#Converting Smartphone data to numerical: 1 as Android, 0 as iPhone
SmartphoneTech <- SmartphoneTech %>% mutate(Android_Phone = if_else(Smartphone=="Android",1,0))
SmartphoneTech <- SmartphoneTech %>% mutate(i_Phone = if_else(Smartphone=="iPhone",1,0))

#check for NA values
sum(sapply(SmartphoneTech, is.na)) #0
sum(sapply(SmartphoneTech, is.nan)) #0
sum(sapply(SmartphoneTech, is.infinite))#0


#------------------------------------------------------------------------------------------------------------------
################################################ Data Transformation ##############################################
#------------------------------------------------------------------------------------------------------------------

#convert to numeric 
SmartphoneTech<- transform(SmartphoneTech, Age = as.numeric(Age))
SmartphoneTech<- transform(SmartphoneTech, Emotionality = as.numeric(Emotionality))
SmartphoneTech<- transform(SmartphoneTech, Agreeableness = as.numeric(Agreeableness))
SmartphoneTech<- transform(SmartphoneTech, Openness = as.numeric(Openness))


#------------------------------------------------------------------------------------------------------------------
########################################## Exploratory Data Analysis ##############################################
#------------------------------------------------------------------------------------------------------------------

########## Section 1 ###########
#correlation plot without data correction
cor_data<- transform(cor_data, Age = as.numeric(Age))
cor_data<- transform(cor_data, Emotionality = as.numeric(Emotionality))
cor_data<- transform(cor_data, Agreeableness = as.numeric(Agreeableness))
cor_data<- transform(cor_data, Openness = as.numeric(Openness))
correlation_matrix <- rcorr(as.matrix(cor_data[3:17]))
corrplot(as.matrix(correlation_matrix$r), method = 'color', cl.lim=c(0,1), addCoef.col = 1, number.cex = 0.3, tl.cex = 0.3)

########## Section 2 ###########

#Leave out blackberry phone
SmartphoneTech <- SmartphoneTech %>% filter(SmartphoneTech$Smartphone == "Android" | SmartphoneTech$Smartphone == "iPhone")


#What is the count of Android and iPhone users?
ggplot(data = SmartphoneTech, mapping = aes(x = SmartphoneTech$Smartphone)) +
  geom_bar() + coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Count of Android and iPhone users")+
  labs(title = 'Count of Android and iPhone users',
       y='Count',x='Smartphone')

table(SmartphoneTech$Smartphone)

SmartphoneTech$Smartphone <- factor(SmartphoneTech$Smartphone, levels = c("Android", "iPhone"),
                         labels = c("Android", "iPhone"))

########## Section 3 ###########
#Does female users have more iPhones than male users?
By_Gender <- SmartphoneTech %>% group_by(SmartphoneTech$Smartphone) %>% count(SmartphoneTech$Gender)

By_Gender
ggplot(data = SmartphoneTech, mapping = aes(x = SmartphoneTech$Gender, fill= Smartphone)) +
geom_bar() + 
  labs(title = 'Count of Android and iPhone users by gender',
       y='Count',x='Gender')


########## Section 4 ###########
#Does the age factor influence the type of phone the user has? Compare the phone users and age group
Smartphone_Tech <- SmartphoneTech %>% mutate(Age_Range = NA)
Smartphone_Tech$Age_Range <- ifelse(as.numeric(SmartphoneTech$Age) >=16 & as.numeric(SmartphoneTech$Age)<=25, "16-25", ifelse(as.numeric(SmartphoneTech$Age) >=26 & as.numeric(SmartphoneTech$Age)<=35, "26-35", ifelse(as.numeric(SmartphoneTech$Age) >=36 & as.numeric(SmartphoneTech$Age)<=45, "36-45", ifelse(as.numeric(SmartphoneTech$Age) >=46 & as.numeric(SmartphoneTech$Age)<=55, "46-55", ifelse(as.numeric(SmartphoneTech$Age) >=56 & as.numeric(SmartphoneTech$Age)<=65, "56-65", ifelse(as.numeric(SmartphoneTech$Age) >=66 & as.numeric(SmartphoneTech$Age)<=75, "66-75", ifelse(as.numeric(SmartphoneTech$Age) >=76 & as.numeric(SmartphoneTech$Age)<=85, "76-85", ifelse(as.numeric(SmartphoneTech$Age) >=85, " > 85", NA))))))))
By_age_range <- Smartphone_Tech %>% group_by(Smartphone_Tech$Smartphone) %>% count(Smartphone_Tech$Age_Range)
ggplot(data = Smartphone_Tech, aes(x = Age_Range, fill = Smartphone)) +
  geom_bar()


########## Section 5 ###########
#What is the distribution of the length of time 
#that individuals have owned their current smartphones, segmented by smartphone brand?

#add a column to summarize the time owned current phone
Smartphone_Tech <- Smartphone_Tech %>% mutate(Time_Owned_By_Users_Range = NA)


Smartphone_Tech$Time_Owned_By_Users_Range<- ifelse(Smartphone_Tech$Time.owned.current.phone <13, 
                                                   "1 year",ifelse(Smartphone_Tech$Time.owned.current.phone>12 & Smartphone_Tech$Time.owned.current.phone<25, 
                                                                   "2 years", ifelse(Smartphone_Tech$Time.owned.current.phone>24 & Smartphone_Tech$Time.owned.current.phone<37, 
                                                                                     "3 years",ifelse(Smartphone_Tech$Time.owned.current.phone>36 & Smartphone_Tech$Time.owned.current.phone<49, 
                                                                                                      "4 years", ifelse(Smartphone_Tech$Time.owned.current.phone>48, "Greater than 4 year",NA )))))
                                                                                                     
                                                  
Smartphone_Tech %>% group_by(Smartphone_Tech$Time_Owned_By_Users_Range) %>% count(Smartphone_Tech$Smartphone)


ggplot(data = Smartphone_Tech) +
  geom_bar(mapping = aes(x = Time_Owned_By_Users_Range, fill = Smartphone))

########## Section 6 ###########
#Statistical summary of the entire data
summary(SmartphoneTech)


########## Section 7 ###########
hist(SmartphoneTech$Age) #Right skewed
summary(SmartphoneTech$Age) #median <mean
hist(SmartphoneTech$Honesty.Humility) #normal distribution
hist(SmartphoneTech$Emotionality)
hist(SmartphoneTech$Extraversion)#slightly right skewed
hist(SmartphoneTech$Agreeableness)#normal distribution
hist(SmartphoneTech$Conscientiousness)#right skewed
hist(SmartphoneTech$Openness)


########## Section 8 ###########
#correlation matrix
SmartphoneTech<- transform(SmartphoneTech, Age = as.numeric(Age))
SmartphoneTech<- transform(SmartphoneTech, Emotionality = as.numeric(Emotionality))
SmartphoneTech<- transform(SmartphoneTech, Agreeableness = as.numeric(Agreeableness))
SmartphoneTech<- transform(SmartphoneTech, Openness = as.numeric(Openness))
cor_mat <- rcorr(as.matrix(SmartphoneTech[3:17]))
corrplot(as.matrix(cor_mat$r), method = 'color', cl.lim=c(0,1), addCoef.col = 1, number.cex = 0.3, tl.cex = 0.3)

#-------------------------------------------------------------------------------------------------------------
################################################# Clustering##################################################
#-------------------------------------------------------------------------------------------------------------


#scale each variable to have a mean of 0 and sd of 1
Phones_z <- SmartphoneTech
Phones_z[3:14] <- scale(SmartphoneTech[3:17]) 

#Confirming that standardation works
summary(Phones_z$Age) #Mean = 0

#check for NA, Nan or Inf values
sum(sapply(Phones_z, is.na)) #5
sum(sapply(Phones_z, is.nan)) #0
sum(sapply(Phones_z, is.infinite)) #0

#Remove Na Values
SmartphoneTech <- SmartphoneTech[complete.cases(SmartphoneTech),]
Phones_z <- Phones_z[complete.cases(Phones_z),]


################# K Means ###########################

#create plot of number of clusters vs total within sum of squares
fviz_nbclust(Phones_z[3:14], kmeans, method = "wss") #k=3


#Set a random seed so we can replicate the clustering
set.seed(200)
#Run the k means algorithm

Phone_clusters <- kmeans(Phones_z[3:17], 3)
Phone_clusters_4 <- kmeans(Phones_z[3:17], 4)
Phone_clusters_5 <- kmeans(Phones_z[3:17], 5)
Phone_clusters_6 <- kmeans(Phones_z[3:17], 6)
Phone_clusters_7 <- kmeans(Phones_z[3:17], 7)

#visualize the clusters
fviz_cluster(Phone_clusters, data = Phones_z[3:17])

################## Hierarchichal clustering #############
Phones_z_hc <- Phones_z[3:12]
Phones_z_hc <- Phones_z_hc %>% mutate(gender_number = ifelse(Phones_z$Gender == "female",1,0))
Phones_z_hc <- Phones_z_hc %>% mutate(other_gender_number = ifelse(Phones_z$Gender == "other",1,0))
Phones_z_hc <- Phones_z_hc %>% mutate(Phone_type = ifelse(Phones_z$Smartphone == "Android",1,0))


#Distance Matrix
dist_matrix <- dist(Phones_z_hc, method = 'euclidean')

#Hierarchial clustering
Phone_hc <- dist_matrix %>%      # Scale the data
  dist(method = "euclidean") %>% # Compute distance matrix
  hclust(method = "ward.D") 


#Plot simple dendrogram
as.dendrogram(Phone_hc)
plot(Phone_hc, hang = -1, cex = 0.4)

#Plot dendrogram with colour coding
fviz_dend(Phone_hc, k = 3,
          cex = 0.5, # label size
          k_colors = c("yellow", "skyblue", "red", "green", "purple","brown","pink"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = T ) # Add rectangle around groups


#------------------------------------------------------------------------------------------------------------
###################################Performance Evaluation####################################################
#------------------------------------------------------------------------------------------------------------

################ K means ########################
#Copy the Number of Cluster to dataframe
Phone_k_cluster <- mutate(SmartphoneTech[1:12], gender_number = ifelse(Gender == "female",1,0))
Phone_k_cluster <- mutate(Phone_k_cluster, other_gender_number = ifelse(Gender == "other",1,0))
Phone_k_cluster <- mutate(Phone_k_cluster, Phone_type = ifelse(Smartphone == "Android",1,0))
Phone_k_cluster <- mutate(Phone_k_cluster, cluster = Phone_clusters$cluster)

#Visualize the clusters
agg_tbl <- Phone_k_cluster[3:15] %>% mutate (cluster = Phone_k_cluster$cluster)
data_long <- gather(agg_tbl, factors, value, Age:Phone_type, factor_key=TRUE)
data_long
data_long %>% 
  ggplot(aes(x = factors, y = value)) + 
  coord_flip() +
  geom_point() +
  facet_wrap(~cluster) + 
  theme_bw() # 1



#Silhouette Analysis
silhouette_k <- silhouette(Phone_clusters$cluster,  dist(Phones_z[3:17]), title=title(main = 'Good'))
plot(silhouette_k) #0.13
silhouette_4 <- silhouette(Phone_clusters_4$cluster,  dist(Phones_z[3:17]), title=title(main = 'Good'))
plot(silhouette_4) #0.12
silhouette_5 <- silhouette(Phone_clusters_5$cluster,  dist(Phones_z[3:17]), title=title(main = 'Good'))
plot(silhouette_5) #0.11
silhouette_6 <- silhouette(Phone_clusters_6$cluster,  dist(Phones_z[3:17]), title=title(main = 'Good'))
plot(silhouette_6) #0.10
silhouette_7 <- silhouette(Phone_clusters_7$cluster,  dist(Phones_z[3:17]), title=title(main = 'Good'))
plot(silhouette_7) #0.10

################ Hierarchichal clustering ####################
#Copy the Number of Cluster to dataframe
#Cluster numbers
Phone_hc_clust <- cutree(Phone_hc, k=3)
Phone_hc_cluster <- mutate(SmartphoneTech, cluster = Phone_hc_clust)

Phone_hc_cluster %>% 
  ggplot(aes(Smartphone)) + 
  geom_bar(col=1,fill="lightblue") + 
  coord_flip() + 
  facet_wrap(~cluster) + 
  theme_bw()

#cluster vis

clusplot(Phone_hc_cluster, Phone_hc_cluster$cluster, 
         color=TRUE, shade=TRUE, lines=0)

#Silhouette Analysis
silhouette_hc <- silhouette(Phone_hc_clust ,dist_matrix, title=title(main = 'bad'))


plot(silhouette_hc) #0.01



#-----------------------------------------------------------------------------------------------------------------
############################################### Classification ###################################################
#-----------------------------------------------------------------------------------------------------------------
Classifier_data <- SmartphoneTech[3:15]
Classifier_data <- Classifier_data %>% mutate(Smartphone = SmartphoneTech$Smartphone)
# 0 = iPhone, 1 = android
skim(Classifier_data)
#Split the training and testing set
set.seed(123)
trainIndex <- createDataPartition(Classifier_data$Smartphone, p = .8, list = FALSE)
Training_set <- Classifier_data[trainIndex,]
Testing_set <- Classifier_data[-trainIndex,]


#Set Labels
Training_set_labels <- as.factor(Training_set$Smartphone)
Testing_set_labels<- as.factor(Testing_set$Smartphone)


#Train C50 on the data
Phone_model <- C5.0(Training_set[1:13], Training_set_labels)

summary(Phone_model)


# create a factor vector of predictions on test data
Phone_prediction <- predict(Phone_model, Testing_set)


#Confusion matrix for rpart
Confusion_Matrix <- table(Phone_prediction, Testing_set$Smartphone)

# Compute the accuracy on the test dataset
mean(Phone_prediction == Testing_set$Smartphone)

# Calculate F-Score
f_score <- function(confusion_matrix){
  # calculate precision and recall
  tp <- confusion_matrix[1,1]
  fp <- confusion_matrix[1,2]
  fn <- confusion_matrix[2,1]
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  
  # calculate f1 score
  f1_score <- 2 * precision * recall / (precision + recall)
  
  return(f1_score)
}
f_score(Confusion_Matrix)

############################################################################################


