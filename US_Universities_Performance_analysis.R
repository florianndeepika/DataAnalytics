#Load the required packages:
library(tidyverse)
library(ggplot2) #use ggplot2 to add layer for visualization
library(skimr) #SKIM function - For statistical summary
library(GGally)
library(ggplot2)
library(corrplot)
library(caTools)
library(caret)
library(usmap) #import the package
library(stringr) #Rename variable names
library(scatterplot3d)  #Visualization
library(moments) #skewness
library(DataExplorer)
library(ggpubr)
library(EnvStats) #Roseners test for outliers
library(gclus) #Scatterplot Matrices from the glus Package 
library(plotly) #world map
library(readxl) #Read UN dataset
library(superml)
library(RColorBrewer) #Bar chart
library(base64)
library(ggpubr)
library(correlationfunnel)
library(randomForest)
library(e1071) #SVM
library(class)
library(datasets)
suppressWarnings
suppressMessages
options(error = expression(NULL))

# Data Mining

## Problem 1
#Loading the universities dataset
data_university <-read.csv("universities.csv", header=T, stringsAsFactors = TRUE)
#Selecting only the required locations
data_univ <- filter(data_university,state %in% c("New York", "Massachusetts", "Connecticut","California","Washington","Alaska","North Dakota", "New Jersey", "Maryland","Delaware"))

#Loading the tution_income dataset
data_tution <-read.csv("tuition_income.csv", header=T, stringsAsFactors = TRUE)
head(data_tution,2)
#Selecting only the required locations with state_code
data_tution <- filter(data_tution,state %in% c("NY", "MA", "CT","CA","WA","AK","ND", "NJ", "MD","DE"))

#Insight : Tution dataset is selected with required variables and joined here for Problem 6 and Correlation 2 before performing the train test split.
#Net_Cost of the recent year from the tuition_income data set will be beneficial for predicting Out_of_state_total_cost.
#Net-cost is the average actually paid by the student after scholarship/award for different income levels
#average net_cost will help the students to know the actual cost for the university.
#net cost can also be categorized for different income levels to add additional predictor variables.
#However, taking the average of net cost will give a fair idea of the average cost to be incurred for the university extracting net_cost from tuition_income data set and add to university dataset

#select the recent year data data 
data_tution_maxdate <- data_tution %>% 
  select(name, state,total_price,year,campus,net_cost,income_lvl) %>% 
  group_by(state) %>% filter(year == max(year)) %>% # Take the most recent date per state
  group_by() 

#take net_Cost mean for all universities
data_tution_cost <-data_tution_maxdate %>%
  select(name,total_price,net_cost)%>%
  dplyr::filter(net_cost != 0) %>%
  group_by(name)%>% 
  summarise(net_cost = mean(net_cost))

#add net_cost value to university dataset
#left join of university and tuition_cost datasets
data_univ <- left_join(data_univ, data_tution_cost, by = c('name'))
apply(is.na(data_univ),2, sum)

#Insight : Details of the dataset:
#Universities data set (initially) : 2159 observations and 27 variables
#Tution income data set : 209012 observations and 7 variables
#Universities data (after filtering 10 states) : 563 observations and 27 variables

### How many universities are in the resulting dataset?
count(data_univ,"name")
#Insight :There are 563 universities in the resulting dataset.

### Graduates of how many universities have an estimated early career pay greater than 50000 dollars per year?
Univ_grad_estPay <- data_univ %>% filter (early_career_pay > 50000)
head(Univ_grad_estPay)

#Insight : There are 94 universities who have been estimated early career pay greater than 50000 dollars per year. Beside this, it can be observed that all these universities fall under public/private educational institute type.

### How many universities are not for-profit?
univ_nonProf <- data_univ %>% filter (type == c('Public','private'))
count(univ_nonProf,"name")

#Insight : 134 Universities fall under the category of not for-profit institutions, with a general assumption that universities which are explicitly mentioned as profit are the ones classified as profit making institutions.

## Problem 2: Making sense of your data
### Take a quick look at the data structure. Describe the dataset in a few sentences.
#The universities dataset is a list of all US universities from all states predominantly contains 3 components.
#a) University location and its enrollment size with type which includes its specialization, ownership status and the degree length they offer<br>
#b) Annual Enrollment count with student's demographic data<br>
#c) Fee and cost structure for both in state and out station students<br>
#d) Alumni status which includes their pay and how their social status <br>

### Which facts or principles do you need to know about the US university system to make a good sense of this dataset?
#The US education system is relatively independent from government regulation compared to other countries. It is intensively diversed
#and most of the universities accept students from all over the world. There are few universities run for profit but largely
#Private and Public universities are not run for profit. In state students are given benefits in the tution fees in all states.
#There are several merit scholarship options available for students which will reduce the cost burden for students.
#Student from all continents prefer to study in US because of a)the diverse course options b) Employment opportunities post study
#c) Technology advancement and research opportunities

### Which things about the data you have at hand you do not know for sure and have to assume?
#Several universities will provide Both Bachelor and Master degrees, Which is not clear from this data set.
#Assuming that the university has a 2 yrs course, it offers only a Master degree
#Assuming that 100% of the seats are available for all students irrespective of demography
#For profit is explicitly mentioned. Assuming that Private and Public category are 'Not For Profit'
#Assuming that if there is a STEM percentage value, it offers a STEM course. which is not explicitly mentioned

### In this project, you will have to predict out-of-state total cost. Do students choose university solely based on the cost? Which other factors might be important?
#Student choose universities not just based on the cost, the other factors include <br>
#  1) University Ranking <br>
#  2) State/city of the University <br>
#  3) Type Private/Public/For Profit <br>
#  4) Alumini status(pay and social status) <br>
#  5) STEM and research opportunities <br>
#  6) Number of international students

### To whom is this cost variable more important than the other three? Explain.
#Our target feature Out_of_State_total is more important for Outstation/international students.


### Formulate a reasonable organizational/business goal, execution of which would require analysis of out-of-state total cost in this dataset.
#Optimize the out-of-state total cost for students to attract best fit students from across the world and also ensure the university is sufficiently funded to focus staff welfare with research & development.

### Which variable would you have to optimize for this goal? What variables would you have to constrain for it to be reasonable?
#Room and Boarding variables need to be optimized to achieve the goal . In addition, support only the pure merit based students for scholarships to reduce the net cost.

### Which data manipulations would you have to perform for an analysis or an ML system for this goal? #What would you have to predict? Would classification or regression be more suitable, and why?
#Data Manipulation is the foundation of Data Analysis and Machine learning. It includes cleaning the data, making it readable by transforming or calculating additional variables, removing duplicates and outliers, arranging in order, removing less significant variables, summarizing and making visualizations to get inference, applying statistical methods like mean & median, rename variables to make more sense of data.
#Here the total cost of an university education in US is predicted for an out of state/international student.
#Regression is the more suitable algorithm here as we are going to predict the total cost of out-of-state students with the given features available in the dataset.

## Problem 3: EDA and Data preprocessing
#Correcting the state_code for Washington:
data_univ <- within(data_univ, state_code[state_code == 'OH' & state == 'Washington'] <- 'WA')

### Check how many categorical features exist and for each categorical feature see how many samples belong to each catego
#Performing summary statistics to analyse the categorical features:
skim (data_univ)

#SKIM function provides an overview of the tidy dataset we have for analysis. This summary represents clearly the column type and its frequency in our dataset. We have 2 column types namely factor and numeric. The Variable type and their characters are also described here with the data distribution in a series of histogram for each variable separately.
#There are 5 categorical variables in the dataset which are name, state, state_code, type and degree_length
#name - 562 samples, 
#state - 10 samples, 
#state_code  - 10 samples, 
#type - 3 samples
#degree_length - 2 samples

### Visualize the distributions of all features in the data set and summarize your findings.

data_univ %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density()

#Insight: From the above plots we can conclude that alomst all the variables are not normally distributed. Either they follow a pattern of right skewed or left skewed.
#Positively skewed distribution or right skewed means that the most frequent values are low which indicates Mode < Median < Mean.
#Negatively skewed distribution or left skewed means the most frequent values are high;which means Mode > Median > Mean.
#A normally distributed data will have zero skewness coefficient.

#checking skewness of the target variable
skewness(data_univ$out_of_state_total, na.rm = TRUE)

#Insight : Here we check the skewness of the output variable - out_of_state_total. The skewness coefficient is 0.5729084 which means distribution differs from a normal distribution. The larger the value of skewness, larger the distribution differs from a normal distribution.

#Normalisation
DataExplorer::plot_qq(data_univ[1:10])
DataExplorer::plot_qq(data_univ[11:20])
DataExplorer::plot_qq(data_univ[21:28])

#Insight :The qq plot illustrates the normality of the data for each observation. We can see that almost all the variables are mostly non-normal which gives us a hint that normalizing data can be one approach for feature scaling.

### Split the data set into a training (70%) and a test (30%) set with the help of stratified sampling based on the degree length. Report mean and standard deviation for out_of_state_total in train and test data set.

#Train test split
set.seed(101) 
split.index <- createDataPartition(data_univ$degree_length, p = .7, list = FALSE)
train_data <- data_univ[ split.index,]
test_data  <- data_univ[-split.index,]
dim(train_data)
dim(test_data)

# check the proportions
prop.table(table(train_data$degree_length))
prop.table(table(test_data$degree_length))

#Mean of the train dataset - out_of_state_total
print(mean(train_data$out_of_state_total))

#Mean of the test dataset - out_of_state_total
print(mean(test_data$out_of_state_total))

#Standard deviation of the train dataset - out_of_state_total
print(sd(train_data$out_of_state_total))

#Standard deviation of the test dataset - out_of_state_total
print(sd(test_data$out_of_state_total))

###Data cleaning

#Handling missing values
#Checking for the NA values
apply(is.na(train_data),2, sum)

#Mean imputer function:
mean_imputer <- function(x) 
{x = round(replace(x, is.na(x) , mean(x, na.rm = TRUE)), digits=0)}

##Handling missing value for room_and_board by stratified mean imputer with respect to state.
train_data <- train_data %>% group_by(state_code) %>%
  mutate(room_and_board=ifelse(is.na(room_and_board),mean(room_and_board,na.rm=TRUE),room_and_board))%>%
  group_by() #removing groupby

#There are 64 NA values in the newly created net_cost variable due to university name mismatch during Join.
#Mean imputer - net_cost
train_data <- train_data %>% mutate_at(28, mean_imputer)

# Handle NA for early_career_pay, mid_career_pay, make_world_better_percent, stem_percent 
train_data[is.na(train_data)] <- 0

#Checking Near Zero Variance for the train dataset
train_data_nzv <- nearZeroVar(train_data)
train_data_nzv

#Insight : It can be observed that only one feature has Near Zero Variance. Since it is small it can be neglible.

#Data Cleaning for data_univ as this dataset will be used for data visualization and EDA.
#Treating missing value room_and_board with startified mean imputer
data_univ <- data_univ %>% group_by(state_code) %>%
  mutate(room_and_board=ifelse(is.na(room_and_board),mean(room_and_board,na.rm=TRUE),room_and_board))%>%
  group_by() #removing groupby

#Treating missing value Net_cost with mean imputer
data_univ <- data_univ %>% mutate_at(28, mean_imputer)

#Replacing missing value cost variables with 0
data_univ[is.na(data_univ)] <- 0
apply(is.na(data_univ),2, sum)

## Problem 4: Data Visualization
#Selecting required variables for visualization purpose
data_univ_viz <- data_univ %>% 
  select( state
          , state_code , name , total_enrollment , n_native , n_asian , n_black , n_hispanic , n_pacific , n_nonresident , n_total_minority
          , n_multiracial , n_unknown , n_white , n_women ,room_and_board ,in_state_tuition ,in_state_total ,out_of_state_tuition
          ,out_of_state_total ,net_cost ,early_career_pay ,mid_career_pay ,stem_percent ,make_world_better_percent)

#Update short name and State Code for the universities
data_univ_viz <- data_univ_viz %>%
  mutate(univ_short_name= paste(str_extract(data_univ_viz$name,"(\\w+)"),data_univ_viz$state_code) )

### Describe what can be seen and interpret it.
#Observation and inferences are given below each visualization.

#Visualization 1 : To find outliers in the target variable
ggplot(data_univ_viz) +
  aes(x = "", y = out_of_state_total) +
  geom_boxplot(fill = "#0c4c8a") +
  ggtitle("Distribution plot for out_of_state_total") +
  theme_minimal()

#Insight : The box plot describes the range of the target variable. We can observer that there are no outliers and the range lies approximately between 0 and 8000

#Visualization 2
data_univ_usmap <-data_univ_viz %>%
  select(state,state_code,out_of_state_total)%>%
  group_by(state_code)%>% 
  summarise(out_of_state_total = round(mean(out_of_state_total)),2)

#US Map with average out_of_state_total
plot_ly(
  type = "choropleth",
  locations = data_univ_usmap$state_code,
  locationmode = "USA-states",
  z = data_univ_usmap$out_of_state_total) %>%
  layout(geo = list(scope = "usa")) %>%
  layout(title = 'Given 10 US states with thier out_of_state_total', plot_bgcolor = "#e5ecf6")

#Insight : This shows the world map of US with the out_of_state_total for each state.

#Visualization 3
#Scatterplot - out_of_state_total and total_enrollment with respect to each state 
ggplot(data = data_univ) + 
  geom_point(mapping = aes(x = total_enrollment, y = out_of_state_total, color = state)) +
  ggtitle("Scatterplot - Total enrollment with out_of_state_total with respect to each state")

#Insight : This scatterplot is represented with each state available in the dataset. We can see that almost all the state with less enrollment provide high out_of_state_total.
#There exist few exceptions like new york, North Dakoda, California which has increased number of total_enrollment with increased out_of_state_total.

#Visualization 4
#scatterplot3d:
attach(data_univ_viz) 
scatterplot3d(make_world_better_percent, stem_percent, out_of_state_total,
              pch=16, 
              highlight.3d=TRUE,
              type="h", 
              main="3D Scatterplot - STEM & Make world better",
              phi = 0,
              ticktype = "detailed")

#Insight : A 3D scatter plot is plotted with numerical variables. Here x and y axis is taken as make_world_better_percent and stem_percent with respect to our target variable in the z axis which is out_of_state_total. Here we can see all the data points are rising with increase in out_of_state_total  which also denotes a positive correlation between them. Here we can observe that as make_world_better_percent increases as stem_percent increases, which means students from STEM make the world a better place to live in.

#Visualization 5
#Select To5 and bottom 5 universities for out_of_state_total
data_high_Low <- data_univ_viz %>%
  select(univ_short_name,out_of_state_total)
#Top 5 and low 5 univ with wrt cost
data_univ_viz_high <- data_high_Low  %>% top_n(5)
data_univ_viz_low <- data_high_Low  %>% top_n(-5)
#Combine top and bottom countries
data_univ_viz_high_low<-bind_rows(data_univ_viz_high, data_univ_viz_low)

#Color for bar chart
coul <- brewer.pal(5, "Set2") 
barplot(data_univ_viz_high_low$out_of_state_total,
        main = "Top 5 and low 5 Universities - Out_of_state_total_fee",
        ylab = "",
        xlab = "Universities",
        names.arg = data_univ_viz_high_low$univ_short_name,
        col = coul,
        width=c(1),
        density=c(70),
        las=1,
        horiz = FALSE
)

#Insight : The barplot represents the most expensive universities and the low cost universities with the given states.

#Visualization 6
#Botom 5- low expensive Univ with all demo units
data_univ_viz_low<- data_univ_viz[order(data_univ_viz$out_of_state_total),][1:5,]
data_univ_viz_low_demo <- data_univ_viz_low  %>%
  select(univ_short_name,n_native, n_asian, n_black,n_women)
#convert to Pivot longer for plot
data_univ_viz_low_PL <- data_univ_viz_low_demo %>%
  pivot_longer(cols=-c("univ_short_name"), names_to = "Index" , values_to = "Value")
ggplot_all_param <- ggplot(data=data_univ_viz_low_PL, aes(x=univ_short_name, y=Value, fill=Index)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values = coul)+
  theme_minimal()
ggplot_all_param + ggtitle("Low expensive Universities with Demographics features")

#This bar graph represents the low cost universities and how the demographic features are distributed among them.
#We can observe a high range in women who have enrolled.

#Visualization 7
#Top10 High Expensive Universities - Total enrollment with all demographic features
data_univ_viz_high<- data_univ_viz[order(-data_univ_viz$out_of_state_total),][1:10,]
data_univ_viz_high_demo <- data_univ_viz_high  %>%
  select(univ_short_name,total_enrollment,n_women,n_total_minority,n_native,n_asian,n_hispanic,n_pacific,n_nonresident,n_black,n_white,
         n_multiracial,n_unknown)
#convert to Pivot longer for plot
data_univ_viz_high_PL <- data_univ_viz_high_demo %>%
  pivot_longer(cols=-c("univ_short_name"), names_to = "Index" , values_to = "Value")
#Horizontal Plot
ggplot(data_univ_viz_high_PL[order(data_univ_viz_high_PL$Index, decreasing = T),],
       aes(Value,  y=reorder(univ_short_name, -Value), label= round(Value,2), fill=factor(Index, levels=c("total_enrollment","n_women","n_total_minority","n_native","n_asian","n_hispanic","n_pacific","n_nonresident","n_white","n_black",
                                                                                                          "n_multiracial",",n_unknown")))) + 
  geom_bar(stat = "identity") +
  scale_fill_discrete(name = "Category", labels = c("total_enrollment" ,"n_women" ,"n_total_minority" ,"n_native" ,"n_asian", "n_hispanic",  "n_pacific","n_nonresident","n_white","n_black","n_multiracial",",n_unknown"))+
  scale_x_continuous(breaks=c(20000,40000,60000,80000,100000,120000,140000,160000,180000)) +
  ylab("Universities") + 
  ggtitle("Top10 Expensive Universities with Demographics features")
#Note: Applied log for all variables to fit them into scale for the bar chart visualization.

#Insight : This bar graph shows the distribution of women and minority group in expensive universities. We can observe that these two groups plays a major part in the dirtibution.
#Here major contribution can be observed from the non-native students. There is no much difference between number of white and black.
#We can see that both the community contribute almost same amount in occupying the expensive universities

#Visualization 8
#Plotting categorical variable - Type
plot(data_univ$type,col = c("red", "blue","green"))

#Insight : This plot shows that public and private universities are high.

#Visualization 9
#Plotting categorical variable - Degree Length
plot(data_univ$degree_length, col = c("pink", "purple"))

#Insight : This plot shows that degree type 4 is higher than degree 2.

### Which demographic characteristics are more pronounced for more expensive universities?
#With reference to visualization non_residents, women and minority group plays a major contribution to the expensive universities
#While number of native students are very low in number, Asian student are 5 to 10% and Spanish students are close or little less relative to Asian students.

### Write down your assumptions about why universities with lower costs tend to attract certain groups more than more expensive universities.
#We can observe that specific groups occupy more in the low expensive universities. This might be because,
#1. Affording high tution fee might be a challenge 
#2. Cost of living in city  (Room and board) can be afforded by them.

### Which important considerations might these visualization conceal, rather than reveal? Produce visualizations that illustrate the findings that might be unobvious from scatterplots above.
#Visualization 10
#Expensive states - fee components and actual cost
data_univ_viz_high<- data_univ_viz[order(-data_univ_viz$out_of_state_total),][1:10,]
data_univ_viz_high_cost <- data_univ_viz_high  %>%
  select(univ_short_name,room_and_board,in_state_tuition,out_of_state_tuition,net_cost)
#convert to Pivot longer for plot
data_univ_viz_high_PL <- data_univ_viz_high_cost %>%
  pivot_longer(cols=-c("univ_short_name"), names_to = "Index" , values_to = "Value")
ggplot_all_param <- ggplot(data=data_univ_viz_high_PL, aes(x=univ_short_name, y=Value, fill=Index)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values = coul)+
  theme_minimal()
ggplot_all_param + ggtitle("Expensive Universities - 4 Cost factors")

#Insight :This graph demonstrates the four cost factors in the expensive universities which is the main contribution factor for a university selection. We can see that in_state_tuition and out_of_state_tuition are super expensive comparing to other costs.

## Problem 5: Correlation 1
#Plotting a correlation map
data_univ_corr <- data_univ[c(3,5:15,19:22,24:28,23)]
data_univ_corr <- cor(data_univ_corr)
corrplot(data_univ_corr, method = "circle")

### Which feature has the strongest correlation to the target feature? Plot the correlation between them exclusively.
#It can be observe that the following variables have strong correlation with the target variable,
#1) instate_Tution
#2) instate_Total
#3) outstate_Tution

#Correlation and scatterplot
data_univ_corr2 <- data_univ[c(13:15,19:23)]
ggpairs(data_univ_corr2, 
        columns = c("in_state_tuition", "in_state_total", "out_of_state_tuition","out_of_state_total"), 
        upper = list(continuous = wrap("cor",  size = 10)), 
        lower = list(continuous = "smooth"))

#Insight : This illustrates how the data is plotted against the other variable. We can clearly see that when there is positive increase in all the cost varibles.

### Describe and interpret what you can see in this correlation plot. Any unusual occurrences?
#Form the correlation and scatter plot we can see that all the variables have same trend. This might be because spurious correlation which exhibits between the data. One reason for it might be residuals exhibit autocorrelation.

### Which three features correlate the most with make_world_better_percent, a percentage of alumni who make the world a better place?
# From the correlation plot, three features which correlates with make_world_better_percent are,
#early_career_pay
#mid_career_pay
#stem_percent

### Choose the strongest of these three correlations and propose four hypotheses about the nature of the link between these variables.
#Hypothesis testing is a statistical test performed to prove an idea or an assumption by using sample data.
#Hypothesis Statement: 
#  H0 (Null hypothesis) : There is no linear relationship between two variables
#  H1 (Alternate hypothesis) : There is a linear relationship between two variables
#  The H0 and H1 are mutually exclusive, and only one can be true. However, one of the two hypothesis will always be true
  
#Shapiro Normality test: 
#It is required to check the normality of the data before a hypothesis is performed.Here we have chosen Shapiro-Wilk normality test for evaluation as they are widely recommended and more powerful than the other tests. Shapiro-Wilk normality test depends on the correlation between the data and their normal scores.

#Shapiro test - to check the normality of the data before performing hypothesis testing:
shapiro.test(data_univ$early_career_pay) 
shapiro.test(data_univ$mid_career_pay) 
shapiro.test(data_univ$stem_percent)

#It is a better a approach to perform the normality testing before we proceed with hypothesis testing. Here we perform Shapiro test which states us if the variable is normally distributed or not. After performing the test it can be observed that the p value for all the 3 variables is 2.2e-16  which is less than the significance value of 0.05. This means the variables are Non-Normally distributed 
#Since the variables follow non normal distribution we can perform spearman correlation test which does not assume normality between the variables.

#### Hypothesis Statement 1:

#H0 (Null hypothesis) : Students having  early_career_pay does not contribute to STEM percentage
#H1 (Alternate hypothesis) : Students having  early_career_pay contribute to STEM percentage

hyp_test_1 <- cor.test(data_univ$early_career_pay, data_univ$stem_percent, 
                       method = "spearman", exact = FALSE)
hyp_test_1
ggscatter(data_univ, x = "stem_percent", y = "early_career_pay", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "stem_percent", ylab = "early_career_pay")

#The p-value here is 2.2e-16, which is less than the significance level of 0.05. We reject H0 and accept H1.
#From this it can concluded that Students having  early_career_pay contribute to STEM percentage

#### Hypothesis Statement 2:

#H0 (Null hypothesis) : stem_percent does not have a linear relation with mid_career_pay
#H1 (Alternate hypothesis) :  stem_percent has a linear relation with mid_career_pay

hyp_test_2 <- cor.test(data_univ$mid_career_pay, data_univ$stem_percent, 
                       method = "spearman", exact = FALSE)
hyp_test_2

#The p-value here is 2.2e-16, which is less than the significance level of 0.05. We reject H0 and accept H1.
#From this it can concluded that stem_percent has a linear relation with mid_career_pay

### Hypothesis Statement 3:
#H0 (Null hypothesis) : Student who contribute to make_world_better_percent are not from stem percent
#H1 (Alternate hypothesis) : Student who contribute to make_world_better_percent are not from stem percent

hyp_test_3 <- cor.test(data_univ$stem_percent, data_univ$make_world_better_percent, 
                       method = "spearman", exact = FALSE)
hyp_test_3

#The p-value here is 2.2e-16, which is less than the significance level of 0.05. We reject H0 and accept H1.
#From this it can concluded that Student who contribute to make_world_better_percent are not from stem percent

### Hypothesis Statement 4:
#H0 (Null hypothesis) : Students from who make_world_better_percent have no early_career_pay
#H1 (Alternate hypothesis) : Students from who make_world_better_percent have early_career_pay
hyp_test_4 <- cor.test(data_univ$early_career_pay, data_univ$make_world_better_percent, 
                       method = "spearman", exact = FALSE)
hyp_test_4

#Insight : The p-value here is 2.2e-16, which is less than the significance level of 0.05. We reject H0 and accept H1.
#From this it can concluded that Students from who make_world_better_percent have early_career_pay

### Which hypothesis do you find the most plausible? Which sources are there supporting it?
#Hypothesis 3 seems to be more plausible. This can be from a general and domain knowledge that students who engage themselves more in STEM are the one who contribute more in making the world a better place.To illustrate, Engineering and Technology has been high evolving and emerging which contributes more to human advancement.

### Which features do you lack in this dataset, which would have helped you determine whether your hypothesis is likely true?
#Employmeynt or placement data of the Alumini could have been provided in the dataset, which might have contributed for hypothesis likely to be  true.

## Problem 6: Correlation 2
### Explain why you think a combination will be useful before you create them.  

#Combining features or calculating new variables will give additional data points for prediction. Additionally it will make the predectors more stronger which can contribute to the 
#Calculating n-men from total enrollment and n_women

#Create new attributes by combining different features with each other.
data_univ <- data_univ %>% 
  mutate(n_men = total_enrollment - n_women)%>% 
  mutate(n_native_fee = n_native * in_state_tuition) %>% 
  mutate(n_nonresident_fee = n_nonresident * out_of_state_tuition)

#Number of men is calculated form the available data. Other possible new feature could be from n_native fee and n_nonresident_fee which might be beneficial for the organizational growth.

### Check their correlation with the out_of_state_total in comparison with the other features from before. Show the correlations with the target feature in a descending order. 

cor_data <- data_univ %>%
  select(c(3,5,10,12,14,15,28)) #[c(3,5,10,12,14,15,28,23)]
correlation = cor(cor_data,data_univ$out_of_state_tuition, method = "spearman")
names = rownames(correlation)
abs_cor = abs(correlation)
data_cor = data.frame(X_var = names,abs_cor = abs_cor,cor = correlation)
data_cor[order(data_cor$abs_cor,decreasing = TRUE),]

### Do your newly created features have higher correlation with the target feature?
#Correlation plot for the target feature with the n-Women and n-men show that there is no significant difference, which means it does not show a strong correlation with the target variable. 

### Take a look at the data from tuition_income dataset and decide which features or combinations of features you think will also be beneficial for prediction. Repeat steps 1-3 for them. Note that you may need to make some extra transformations to add them.
#Note: Tution_income and universities data is already joined in problem 1 before data processing, analysis and EDA.

#correlation Table - Descending
tution_cor_data <- data_univ %>%
  select(c(3,19:22,24:28))
correlation_tut = cor(tution_cor_data,data_univ$out_of_state_tuition, method = "spearman")
names = rownames(correlation_tut)
abs_cor_tut = abs(correlation_tut)
data_cor_tut = data.frame(X_var = names,abs_cor = abs_cor_tut,cor = correlation_tut)
data_cor_tut[order(data_cor_tut$abs_cor,decreasing = TRUE),]

#It can be observed that the newly added feature net_cost has strong positive correlation with the target variable

### The 10 states you were directed to select for analysis weren't selected randomly. In fact, they comefrom a certain well-known rating, and comprise top-10, bottom-10, middle 10, second or fourth quintile of it. Find how we selected them, and support your claim with reference to the respective rating.

data_univ_gdp <- data_univ %>%
  mutate(state_gdp2019 = case_when(state == "New York" ~ 90043, state == "Massachusetts" ~ 86942, state == "Connecticut" ~ 81055, 
                                   state == "California" ~ 80563, state == "Washington" ~ 80170, state == "Delaware" ~ 78468, 
                                   state == "Alaska" ~ 76220, state == "North Dakota" ~ 75321, state == "New Jersey" ~ 73451, 
                                   state == "Maryland" ~ 71838, TRUE ~ 0))
data_univ_gdpmap <-data_univ_usmap %>%
  mutate(state_gdp2019 = case_when(state_code == "NY" ~ 90043, state_code == "MA" ~ 86942, state_code == "CT" ~ 81055, 
                                   state_code == "CA" ~ 80563, state_code == "WA" ~ 80170, state_code == "DE" ~ 78468, 
                                   state_code == "AK" ~ 76220, state_code == "ND" ~ 75321, state_code == "NJ" ~ 73451, 
                                   state_code == "MD" ~ 71838, TRUE ~ 0))

plot_ly(data_univ_gdpmap, z=data_univ_gdpmap$state_gdp2019, locations=data_univ_gdpmap$state_code, text=paste0(data_univ_gdpmap$state_code, '<br>Out_of_state_total $: ', data_univ_gdpmap$out_of_state_total), 
        type="choropleth", locationmode="USA-states", colors = 'pink', filename="stackoverflow/simple-choropleth") %>%
  layout(geo = list(scope="usa")) %>%
  layout(title = 'Top 10 U.S. States by GDP per capita with Out of state total cost', plot_bgcolor = "#e5ecf6")

### Explain the reasoning you used to find this missing information
#All the 10 states having  well reputed and expensive universities,which is related to the states economy and GDP. Moreover, it might be correlated with the target variable out_of_state_total cost

### Might this statewide score be useful at predicting out-of-state total cost? Explain why or why not. If it is useful, add it to your data and check the correlation.
#GDP is the measure of state's economy which generally has a linear relationship with education and cost of living.
#GDP score will be a useful predictor for out_of_state total cost. state_gdp2019 has been added as a new feature to the training and testing dateset in subsequent steps for analysis, correlation and modelling.

#Finding correlation for the GDP values with the target variable to decide if it has to be added to your dataset as a new feature.
tution_cor_data <- data_univ_gdp %>%
  select(c(32))
correlation_tut = cor(tution_cor_data,data_univ_gdp$out_of_state_tuition, method = "spearman")
names = rownames(correlation_tut)
abs_cor_tut = abs(correlation_tut)
data_cor_tut = data.frame(X_var = names,abs_cor = abs_cor_tut,cor = correlation_tut)
data_cor_tut[order(data_cor_tut$abs_cor,decreasing = TRUE),]

# The variables state_gdp2019 and out_of_state_tuition exhibits a positive correlation of 0.2803806 which is significantly good.

#Adding GDP to the train dataset
train_data <- train_data %>%
  mutate(state_gdp2019 = case_when(state == "New York" ~ 90043, state == "Massachusetts" ~ 86942, state == "Connecticut" ~ 81055, 
                                   state == "California" ~ 80563, state == "Washington" ~ 80170, state == "Delaware" ~ 78468, 
                                   state == "Alaska" ~ 76220, state == "North Dakota" ~ 75321, state == "New Jersey" ~ 73451, 
                                   state == "Maryland" ~ 71838, TRUE ~ 0))

### Do any of your new features shine the new light on possible determinants of what makes students feel they are making the world the better place? Explain your insights.
#GDP is one predominant factor which makes students to enrolled and receive high standard education from the top ranked universities which makes high qualified scholars. This also is diecrly proportional to STEM percent.

## Problem 7: Data cleaning
#Note: Missing values were already handled in problem 3 (EDA and Data preprocessing), since it is a good approach to clean the data before performing EDA and visualization. This will help for better interpretation and getting more insights from the data.

### Handle the missing data
### Handle the categorical data with one-hot-encoding

#A one-hot encoding is an approach to handle categorical variables. This approach assigns a unique value for each possible observation.
#define one-hot encoding function
#Selecting only required variables for Modelling ML:
train_data <- train_data %>% 
  select(-c (X,name,state, state_code,in_state_tuition,in_state_total,out_of_state_tuition, net_cost))
colnames(train_data)
#Scaling
train_data[c(1:12,15,17:21)] <- scale(train_data[c(1:12,15,17:21)])
#One-hot encoding
encoding_train <- dummyVars(~ ., data = train_data, fullRank = TRUE)
train_data <- data.frame(predict(encoding_train, newdata = train_data))

## Problem 8: Feature scaling
### Find the feature importance with a quick Random Forest and show them in a plot. What insights do you get out of it?
set.seed(4543)
data_imp_rf <- randomForest(out_of_state_total ~ ., data=train_data, ntree=1000, keep.forest=FALSE,
                            importance=TRUE)
varImpPlot(data_imp_rf)


## Problem 9: Test data

# Data cleaning for test dataset

#Step1:
#Adding GDP to the test dataset
test_data <- test_data %>%
  mutate(state_gdp2019 = case_when(state == "New York" ~ 90043, state == "Massachusetts" ~ 86942, state == "Connecticut" ~ 81055, 
                                   state == "California" ~ 80563, state == "Washington" ~ 80170, state == "Delaware" ~ 78468, 
                                   state == "Alaska" ~ 76220, state == "North Dakota" ~ 75321, state == "New Jersey" ~ 73451, 
                                   state == "Maryland" ~ 71838, TRUE ~ 0))

#Step2: Handling missing values:
apply(is.na(test_data),2, sum)
#Filling missing value for room_and_board with stratified mean imputer
test_data <- test_data %>% group_by(state_code) %>%
  mutate(room_and_board=ifelse(is.na(room_and_board),mean(room_and_board,na.rm=TRUE),room_and_board))%>%
  group_by() #removing groupby
#Filling missing value for net_cost with Mean
test_data <- test_data %>% mutate_at(28, mean_imputer) #for net_cost
#Filling missing value with zero for early_career_pay, mid_career_pay, make_world_better_percent, stem_percent 
test_data[is.na(test_data)] <- 0 #early_career_pay, mid_career_pay, make_world_better_percent, stem_percent 
apply(is.na(test_data),2, sum)

#Step3: Selecting only required variables for Modelling ML:
test_data<- test_data%>% 
  select(-c (X,name,state, state_code,in_state_tuition,in_state_total,out_of_state_tuition, net_cost))
colnames(train_data)
colnames(test_data)

#Step4: Checking Near Zero Variance for the test dataset
test_data_nzv <- nearZeroVar(test_data)
print(test_data_nzv)

#Step5: Scaling the test data
test_data[c(1:12,15,17:21)] <- scale(test_data[c(1:12,15,17:21)])

#Step6: Perform one hot encoding
encoding_test  <- dummyVars(~ ., data = test_data , fullRank = TRUE)
test_data  <- data.frame(predict(encoding_test , newdata = test_data ))

## Problem 10: Regression
### Select and train the following regression models on the training set. Linear model, support vector regression, and random forest.

#Linear model:
set.seed(123)
fitControl <- trainControl(method = "none")
lm_reg_model <- train(out_of_state_total ~., 
                      data = train_data,
                      method = "lm",
                      trControl = fitControl)
pred_lm_reg_model = predict(lm_reg_model, test_data)
summary(lm_reg_model)

#support vector regression:
set.seed(123)
svm_reg_model = svm(out_of_state_total ~ ., data = train_data)
print(svm_reg_model)
pred_svm_reg_model = predict(svm_reg_model, test_data)
summary(pred_svm_reg_model)

#Random forest:
set.seed(123)
randomcontrol_reg <- trainControl(
  method = "repeatedcv", 
  number = 4, 
  repeats = 5,
  search = "random")
set.seed(123)
rf_reg_model <- train(out_of_state_total ~ .,  data = train_data,
                      method = "rpart",
                      trControl = randomcontrol_reg,
                      tuneLength = 4)
rf_reg_model
pred_rf_reg_model <- predict(rf_reg_model, test_data)

### Evaluate the three regression models on the test set. Which model performs best?
#Creating matrix
table<- matrix(0,3,1) # entry, rows, columns
colnames(table) <- c("RMSE")
row.names(table) <- c("Linear_Regression","SVM","Random Forest")
## Computing RMSE
rmse = function(observed, predicted) {
  sqrt(mean((observed- predicted)**2))
}
rmse_lm_reg <- rmse(test_data$out_of_state_total, pred_lm_reg_model)
rmse_svm_reg <- rmse(test_data$out_of_state_total, pred_svm_reg_model)
rmse_ranfor_reg <- rmse(test_data$out_of_state_total, pred_rf_reg_model)
table[1,1] <- rmse_lm_reg
table[2,1] <- rmse_svm_reg
table[3,1] <- rmse_ranfor_reg
table <- as.data.frame(table)
print(table)

## Problem 11: Classification

### Categorize the target variable (out_of_state_total) into five categories and build a classification model for the above pre-processed data.

#Categorization:
train_data_classifier <- mutate (train_data
                                 , out_of_state_total = case_when(train_data$out_of_state_total >= 0 & train_data$out_of_state_total <= 15000 ~ 1
                                                                  , train_data$out_of_state_total > 15000 &  train_data$out_of_state_total <= 30000 ~ 2
                                                                  , train_data$out_of_state_total > 30000 &  train_data$out_of_state_total <= 45000 ~ 3
                                                                  , train_data$out_of_state_total > 45000 &  train_data$out_of_state_total <= 60000 ~ 4
                                                                  , train_data$out_of_state_total > 60000 ~ 5 ))
test_data_classifier <- mutate (test_data
                                , out_of_state_total = case_when(test_data$out_of_state_total >= 0 & test_data$out_of_state_total <= 15000 ~ 1
                                                                 , test_data$out_of_state_total > 15000 &  test_data$out_of_state_total <= 30000 ~ 2
                                                                 , test_data$out_of_state_total > 30000 &  test_data$out_of_state_total <= 45000 ~ 3
                                                                 , test_data$out_of_state_total > 45000 &  test_data$out_of_state_total <= 60000 ~ 4
                                                                 , test_data$out_of_state_total > 60000 ~ 5 ))
# Encoding the target feature as factor
train_data_classifier$out_of_state_total = factor(train_data_classifier$out_of_state_total, levels = c(1,2,3,4,5))
# Encoding the target feature as factor
test_data_classifier$out_of_state_total = factor(test_data_classifier$out_of_state_total, levels = c(1,2,3,4,5))

### Train the following classification models on the training set for classification and evaluate the models on the test set: SVM, k-NN, and Random Forest.

#SVM:
# Fitting SVM to the Training set
set.seed(123)
svm_class_model = svm(formula = out_of_state_total ~ .,
                      data = train_data_classifier,
                      type = 'C-classification',
                      kernel = 'linear')
# Predicting the Test set results
pred_svm_class_model = predict(svm_class_model, newdata = test_data_classifier[-17])
# Making the Confusion Matrix
pred_svm_class_model_conf_matrix = table(test_data_classifier[, 17], pred_svm_class_model)
#Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy_svm <- accuracy(pred_svm_class_model_conf_matrix)

#KNN:
set.seed(123)
knn_class_model <- knn(train = train_data_classifier,
                       test = test_data_classifier,
                       cl = train_data_classifier$out_of_state_total,
                       k = 3)
# Confusion Matrix
knn_class_model_conf_matrix <- table(test_data_classifier$out_of_state_total, knn_class_model)
#Model Evaluation - Choosing K
misClassError <- mean(knn_class_model != test_data_classifier$out_of_state_total)

#Random Forest:
# Create a Random Forest model with default parameters
set.seed(123)
rf_class_model <- randomForest(out_of_state_total~., data=train_data_classifier, proximity=TRUE) 
pred_rf_class_model <- predict(rf_class_model, test_data_classifier)
rf_class_model_conf_matrix <- confusionMatrix(pred_rf_class_model, test_data_classifier$out_of_state_total)
plot(rf_class_model)

#2. Create a confusion matrix for each model. Interpret the results and compare them with each other.

#Confusion Matrix for SVM:
pred_svm_class_model_conf_matrix
#Accuracy Score:
accuracy_svm
#Insight: SVM provides an accuracy of 68% which is relatively low compared to other models.

#Confusion Matrix for KNN:
knn_class_model_conf_matrix
#Accuracy score:
print(paste('Accuracy =', 1-misClassError))
#Insight: KNN provides an accuracy of 90% which is relatively low compared to other models.

#Confusion Matrix for Random Forest:
rf_class_model_conf_matrix
#Insight : Random Forest provides an accuracy of 67% which is relatively low compared to other models.
















