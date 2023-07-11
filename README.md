# Ontario_Public_Librabry_Revanue_Insights
---
title: "Project 2"
author: "Dhruv Sojitra"
date: "2022-11-17"
output: pdf_document
---

# * Background
Today's world is emerging with the advancements in Internet Technologies, which has empowered the human race with information at their fingertips. This may have caused an effect in operations of Libraries in Ontario and around the world. This analysis is intended to study the library data from Ontario's public libraries: (https://data.ontario.ca/dataset/ontario-public-library-statistics) and observe the effects/trends in Library Operations and suggest ideas for the libraries of Ontario to SUCCEED.

# * Business Objective and Success Criteria
The objectives of this project is to propose such solutions that the libraries of Ontario attain SUCCESS. By SUCCESS we mean the following terms:
1. Increase Revenue of the libraries
2. Increase Customer Base for the libraries, i.e., Increase outreach

# * Data Selection

Following are the data columns considered for the analysis
1.    Library Name
2.    Library Number
3.    City
4.    No of Cardholders
5.    Year
6.    Total Revenue
7.    In-person Visits
8.    Social Media Visits

# Research Question

Are the number of Social Media Visits and In Person Visits releated ? If they are related do they influence Revenue?

# Benefits of the analysis:

By doing this analysis we will be able to know if libraries work more on digital marketing and growing their presence on social media will influence their success or not.

### Importing Dataset

```{r}
# Importing Libraries
library(tidyverse)
```


```{r}
data2017 <- read.csv("2017.csv")
data2018 <- read.csv("2018.csv")
data2019 <- read.csv("2019.csv")
data2020 <- read.csv("2020.csv")

# Converting the data columns to interger for further interpretation

# To int for column G1.5.3.W..No..of.Social.Media.visits
data2017$G1.5.3.W..No..of.Social.Media.visits <- as.integer( gsub('[^[:alnum:] ]','',data2017$G1.5.3.W..No..of.Social.Media.visits))

data2018$G1.5.3.W..No..of.Social.Media.visits <- as.integer( gsub('[^[:alnum:] ]','',data2018$G1.5.3.W..No..of.Social.Media.visits))

data2019$G1.5.3.W..No..of.Social.Media.visits <- as.integer( gsub('[^[:alnum:] ]','',data2019$G1.5.3.W..No..of.Social.Media.visits))

# !!! DATASET 2020 DOES NOT HAVE THIS COLUMN !!!
#data2020$G1.5.3.W..No..of.Social.Media.visits <- as.integer( gsub('[^[:alnum:] ]','',data2020$G1.5.3.W..No..of.Social.Media.visits))

# To int for column G1.5.1.W..No..of.visits.to.the.library.made.in.person
data2017$G1.5.1.W..No..of.visits.to.the.library.made.in.person <- as.integer( gsub('[^[:alnum:] ]','',data2017$G1.5.1.W..No..of.visits.to.the.library.made.in.person))

data2018$G1.5.1.W..No..of.visits.to.the.library.made.in.person <- as.integer( gsub('[^[:alnum:] ]','',data2018$G1.5.1.W..No..of.visits.to.the.library.made.in.person))

data2019$G1.5.1.W..No..of.visits.to.the.library.made.in.person <- as.integer( gsub('[^[:alnum:] ]','',data2019$G1.5.1.W..No..of.visits.to.the.library.made.in.person))

data2020$G1.5.1.W..No..of.visits.to.the.library.made.in.person <- as.integer( gsub('[^[:alnum:] ]','',data2020$G1.5.1.W..No..of.visits.to.the.library.made.in.person))

# To int for column B2.9..Total.Operating.Revenues
data2017$B2.9..Total.Operating.Revenues <- as.integer( gsub('[^[:alnum:] ]','',data2017$B2.9..Total.Operating.Revenues))

data2018$B2.9..Total.Operating.Revenues <- as.integer( gsub('[^[:alnum:] ]','',data2018$B2.9..Total.Operating.Revenues))

data2019$B2.9..Total.Operating.Revenues <- as.integer( gsub('[^[:alnum:] ]','',data2019$B2.9..Total.Operating.Revenues))

data2020$B2.9..Total.Operating.Revenues <- as.integer( gsub('[^[:alnum:] ]','',data2020$B2.9..Total.Operating.Revenues))

```

```{r}
# Processing data for 2017

#selecting the columns
data2017_City <- select(data2017, Library.Full.Name, Library.Number, A1.10.City.Town, A1.14..No..of.Active.Library.Cardholders, Survey.Year.From, B2.9..Total.Operating.Revenues, G1.5.1.W..No..of.visits.to.the.library.made.in.person , G1.5.3.W..No..of.Social.Media.visits)

# Changing column names
colnames(data2017_City) <- c("Library_Name", "Library_Number", "City", "No_of_Cardholders", "Year", "Total_Revenue", "In-person_Visit", "Social_Media_Visit")

# Removing nulls
dim(data2017_City)
data2017_City <- na.omit(data2017_City)
dim(data2017_City)

# Grouping the data by City and aggregating it using Count.
Library_Count_2017 <- aggregate(data2017_City$Library_Number, by=list(data2017_City$City), FUN=length)

# Setting Column names
colnames(Library_Count_2017) <- c("City", "Count")
```

```{r}
# Processing data for 2018

#selecting the columns
data2018_City <- select(data2018, Library.Full.Name, Library.Number, A1.10.City.Town, A1.14..No..of.Active.Library.Cardholders, Survey.Year.From, B2.9..Total.Operating.Revenues, G1.5.1.W..No..of.visits.to.the.library.made.in.person , G1.5.3.W..No..of.Social.Media.visits)

# Changing column names
colnames(data2018_City) <- c("Library_Name", "Library_Number", "City", "No_of_Cardholders", "Year", "Total_Revenue", "In-person_Visit", "Social_Media_Visit")

# Removing nulls
dim(data2018_City)
data2018_City <- na.omit(data2018_City)
dim(data2018_City)

# Grouping the data by City and aggregating it using Count.
Library_Count_2018 <- aggregate(data2018_City$Library_Number, by=list(data2018_City$City), FUN=length)

# Setting Column names
colnames(Library_Count_2018) <- c("City", "Count")

```

```{r}
data2019_City <- select(data2019, Library.Full.Name, Library.Number, A1.10.City.Town, A1.14..No..of.Active.Library.Cardholders, Survey.Year.From, B2.9..Total.Operating.Revenues, G1.5.1.W..No..of.visits.to.the.library.made.in.person , G1.5.3.W..No..of.Social.Media.visits)
colnames(data2019_City) <- c("Library_Name", "Library_Number", "City", "No_of_Cardholders", "Year", "Total_Revenue", "In-person_Visit", "Social_Media_Visit")
dim(data2019_City)
data2019_City <- na.omit(data2019_City)
dim(data2019_City)
Library_Count_2019 <- aggregate(data2019_City$Library_Number, by=list(data2019_City$City), FUN=length)
colnames(Library_Count_2019) <- c("City", "Count")
#table(Library_Count_2019$Count_2019)
```

```{r}
data2020_City <- select(data2020, Library.Full.Name, Library.Number, A1.10.City.Town, A1.14..No..of.Active.Library.Cardholders, Survey.Year.From, B2.9..Total.Operating.Revenues, G1.5.1.W..No..of.visits.to.the.library.made.in.person)
#data2020_City["Social_Media_Visit"] <- NA
colnames(data2020_City) <- c("Library_Name", "Library_Number", "City", "No_of_Cardholders", "Year", "Total_Revenue", "In-person_Visit")
dim(data2020_City)
data2020_City <- na.omit(data2020_City)
data2020_City["Social_Media_Visit"] <- NA
dim(data2020_City)
Library_Count_2020 <- aggregate(data2020_City$Library_Number, by=list(data2020_City$City), FUN=length)
colnames(Library_Count_2020) <- c("City", "Count")
#table(Library_Count_2020$Count_2020)
```

### Below code fethes all the cities from all 4 data sources and generates a Library Count for each Year.

```{r}
# Combing all 4 dataframes
Library_Count <- rbind(Library_Count_2017, Library_Count_2018, Library_Count_2019, Library_Count_2020)
Library_Count <- Library_Count[c("City")]

# Getting the distinct names of the Cities. So, now we have all the cities from all 4 data sources.
Library_Count <- distinct(Library_Count, City)

# Checking if the city occurs in corresponding database and adding the library count of that city in assosiated column
Library_Count <- Library_Count %>% mutate("2017" = ifelse( City %in% Library_Count_2017$City, Library_Count_2017$Count, 0 ))

Library_Count <- Library_Count %>% mutate("2018" = ifelse( City %in% Library_Count_2018$City, Library_Count_2018$Count, 0 ))

Library_Count <- Library_Count %>% mutate("2019" = ifelse( City %in% Library_Count_2019$City, Library_Count_2019$Count, 0 ))

Library_Count <- Library_Count %>% mutate("2020" = ifelse( City %in% Library_Count_2020$City, Library_Count_2020$Count, 0 ))

head(Library_Count)
write.csv(Library_Count,"Librarycount.csv", row.names = FALSE)
```

### Below caode fetches all the city names count the number of Cardholders according to each year.

```{r}
# Combining all the library names
Library_Cardholder_Count <- rbind(data2017_City,data2018_City,data2019_City,data2020_City)

Library_Cardholder_Count <- Library_Cardholder_Count[c("Library_Name")]

# Getting Distinct Libraries
Library_Cardholder_Count <- distinct(Library_Cardholder_Count, Library_Name)

# Checking if the library exist in the perticular year dataset and adding its number of card holders to assosiated columns
Library_Cardholder_Count <- Library_Cardholder_Count %>% mutate("2017" = ifelse(Library_Name %in% data2017_City$Library_Name, data2017_City$No_of_Cardholders, 0))

Library_Cardholder_Count <- Library_Cardholder_Count %>% mutate("2018" = ifelse(Library_Name %in% data2018_City$Library_Name, data2018_City$No_of_Cardholders, 0))

Library_Cardholder_Count <- Library_Cardholder_Count %>% mutate("2019" = ifelse(Library_Name %in% data2019_City$Library_Name, data2019_City$No_of_Cardholders, 0))

Library_Cardholder_Count <- Library_Cardholder_Count %>% mutate("2020" = ifelse(Library_Name %in% data2020_City$Library_Name, data2020_City$No_of_Cardholders, 0))

head(Library_Cardholder_Count)
write.csv(Library_Cardholder_Count,"Librarycardholdercount.csv", row.names = FALSE)
```

### Below code Generates a table for Top 10 Libraries in terms of Total Revenue.

```{r}
# Combining all the data sources.
Library_Operating_Revenue <- rbind(data2017_City,data2018_City,data2019_City,data2020_City)

# Selecting Data columns
Library_Operating_Revenue <- Library_Operating_Revenue[c("Library_Name", "Total_Revenue")]

# Grouping the data by Library Name and aggregating the Total Revenue by mean.
Library_Operating_Revenue <- aggregate(Library_Operating_Revenue$Total_Revenue, by=list(Library_Operating_Revenue$Library_Name), FUN=mean)

# Renaming the columns
colnames(Library_Operating_Revenue) <- c("Library_Name", "Total_Revenue")

# Arranging the obtained result into decending and limiting it by 10 observations.
Library_Operating_Revenue <- Library_Operating_Revenue %>% arrange(desc(Total_Revenue))
head(Library_Operating_Revenue, 10)
write.csv(Library_Operating_Revenue,"Library_Operating_Revenue.csv",row.names = FALSE)
```

### Below code analyzes In-person Visits and Social Media visits and their influence on Total Revenue

```{r}
# Combining Data Sources
Library_Visits <- rbind(data2017_City,data2018_City,data2019_City)

# Selecting Columns
Library_Visits <- Library_Visits[c("Library_Name","In-person_Visit", "Social_Media_Visit", "Total_Revenue")]

# Grouping by using Library Name and summarizing using the average of In-Person Visits, Social Media Visits and Revenue
Library_Visits <- Library_Visits %>% group_by(Library_Name) %>% summarise(across(c(`In-person_Visit`,Social_Media_Visit, Total_Revenue), mean))

# Viewing the data
head(Library_Visits)
write.csv(Library_Visits,"Library_Visits.csv",row.names = FALSE)
```

### Observing the correlation between Inperson Visits and Social Media Visits

```{r}
cor(Library_Visits$`In-person_Visit`,Library_Visits$Social_Media_Visit, method = c("pearson"))
```

The Pearson Coefficient is 0.8761 which indicates a fairly strong positive correlationship. 

Now building a metric using In-person Visits and Social Media Visits.

```{r}
library(ggcorrplot)

# Visualizing Correlation matrix between Inperson visit and Social Media Visit
ggcorrplot::ggcorrplot(cor(select(Library_Visits,`In-person_Visit`,Social_Media_Visit,Total_Revenue)))

Library_Visits["Metric"] = Library_Visits$`In-person_Visit` + Library_Visits$Social_Media_Visit

head(Library_Visits)
```

# Normalizing the obtained Metric and correlating it with Revenue.

```{r}
library(caret)
process <-  preProcess(as.data.frame(Library_Visits$Metric), method = c("range"))
norm_scale <- predict(process, as.data.frame(Library_Visits$Metric))
colnames(norm_scale) <- c("Metric_Norm")
head(norm_scale)
Library_Visits <- cbind(Library_Visits, Norm_Metric = norm_scale$Metric_Norm)
Library_Visits <- Library_Visits %>% arrange(desc(Norm_Metric))
head(Library_Visits, 10)
Library_Operating_Revenue <- Library_Operating_Revenue %>% arrange(desc(Total_Revenue))

```

### Above are the top libraries accoring to the resulted Metric which almost coinsides with top performing libraries.

```{r}
cor(Library_Visits$Norm_Metric,Library_Visits$Total_Revenue, method = c("pearson"))
```

# With the correlation coefficient 0.98 we can state that there is sifnificant relationship between the obtained metric and reveenue of the libraries.

# Analysis Interpretation

Libraries which have a good presence in social media influences the in person visits and inturn generate more Revenue. Therefore, we can encourage other non performing libraris to get more social media presence to increase in person visits.
