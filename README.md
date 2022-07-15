# The United States University Education System - Cost Analysis

Executive Summary

Introduction:
The inherent benefits of the United States University Education System is attracting students from across the continents and cultures. A system which is largely independent from the Federal Government regulations gives them a free hand to offer diversified courses and research opportunities. Moreover, the employment opportunities during/post education are captivating especially for students from developing countries. However, affordability is a big interrogation in front of all aspiring students across the continents. This Data Analysis and Machine Learning modeling research project analyzes the historical data from 10 US state universities with university basic data and course type, student’s demographic data, tuition and boarding fee and pay details from the university Alumini and predict the cost for a prospective out of state/international student.

![image](https://user-images.githubusercontent.com/63796480/179210449-a169ec5f-8fbc-4b9a-91df-e548baa1bc6f.png)

Purpose:
Knowing the cost of university education is vital for any student to plan for their education. This question becomes more obvious for any out of state student who wants to pursue education in any of the US universities. This paper addresses the question in a more scientific way using the historical data for the out of state students. The secondary objective of this analytics project is for the University to be aware of the cost of education in their institution for them to optimize and keep it at a relatively competitive level for them to attract best talents and at the same time keep their financial side healthy.

Methodology:

Data set:
There are two datasets provided for study and analysis. 1) Universities data : It has the recent year data of all universities which includes University ranking and demographics, Student demographics, tuition and room/boarding cost for both in_state & out_of_state students and alumni data like pay after graduation and how much percentage of their students make a difference in the world. We take universities data from the given 10 states (New York, Massachusetts, Connecticut, California, Washington, Delaware, Alaska, North Dakota, New Jersey, Maryland) for analysis. 2) tuition income data : This dataset has historical price and cost data for different income levels. net_cost available in this the data set provides the actual cost spent by the student after the scholarship/award. Net average actual cost for recent years will be taken from this data set and added to the university data set for extracting insights and prediction.

Analysis Methodology:
Data Analysis is done in a structured way starting from extracting dataset, data pre-processing, data cleaning, Exploratory Data Analysis, visualization to machine learning modeling. Data is extraced and tranformation is performed by selecting only 10 States. Data summary has been created to get a sense of the given data set end extract more insights from it. Insignificant data points from the data set are removed. Missing values are handled as appropriately either by stratified mean imputer or by assigning it with a value based on the domain knowledge of the particular variable. Addionally, the pattern in which the data was missing is also considering for handling missing values. Exploratory Data Analysis is done by applying statistical methods. Data visualization and graphical representation of the given data set is performed to get additional inference from the data like trends and relationships. Related data points are considered for statistical hypotheses to assess the plausibility of the assumptions. Finally, regression and classification Machine learning modeling is done to predict the out_of_state_Cost. Modelling accuracy and residuals are documented and the best fit model has been recommended for use and implementation.

Analysis:
Early career pay and stem percent is thriving factor to increase make world better percent.
High expensive universities has high numebr of women being enrolled with them.
Net cost is 15 to 25 percent less than the out_of_state_total cost due to scholarships and awards. This factor encourages students to apply for merit based scholarships and improves student enrollment.
University rating and its alumni status are the key factors for expensive universities.

Recommendations:
Room/Boarding is a significant factor and it is essential to be kept at an optimal level to encourage the students
Net_Cost of 15 to 25% less than the Out_of_state total, Keep the Net_cost at optimal to keep the Universities financial position healthy

Conclusion:
Finding admission in a reputed university with relatively less cost is time consuming tedious research. This Model helps the students to predict and plan for their study and at the same time university can use this Machine Learning model to keep thier cost at an optimal level.
