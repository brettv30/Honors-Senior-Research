# Honors-Senior-Research

Here's a repository containing my R project that I worked on for my CSC 498 course "Honors Senior Research".
The goal of this course is to augment hands-on Artificial Intelligence programming and build upon the analytics skills I learned in the classroom in my CSC 260 course, "Analysis of Algorithms". The work I completed in that course can be found here: https://github.com/brettv30/Analysis-of-Algorithms.

Initially, we began with datasets containing National Crime Rate and State Crime Rate data. These can be found in the "Section 1 Data" folder. We retreived these datasets from the FBI Crime Stats website which can be semi-recreated from here: https://ucrdatatool.gov/Search/Crime/Crime.cfm. 

The script file is broken up into a total of 8 different sections. Each one indicating a different milestone in this class.

# Libraries
- Here's a list of all libraries that were used in this project. 
- moments, ggplot2, ggcorrplot, corrplot, ggrepel, Rmisc, amap, factoextra, cluster, gridExtra, party, partykit, ROCR, boot, randomForest, dplyr, plyr, caret, rpart.plot

# Section 1: Exploratory Data Analysis
  - Here we preformed extensive EDA specifically looking at correlations between the crimes in each individual state. Through this analysis we learned that the data is not normally distributed and that it is futile to try to make assumptions about the entire population when looking at one state's crimes.  

# Section 2: Regional Correlations, Clustering, and Median Household Income
  - Here we split that data into regions according to the split here: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf and we included Median Household Income data for the entire nation as well as for each state. This information was obtained from here: https://www.census.gov/content/dam/Census/library/publications/2019/acs/acsbr18-01.pdf.
  - We measured the correlations between each crime based on region and then used K-means clustering to see if we could accurately create clusters within each region. This proved unsuccessful as the clusters had major overlaps and usually did not exceed 3 clusters per group. Based off of these clusters we can accurately tell that the FBI should not be analyzing crime based on regional clusters because it is too difficult to accurately distinguish the differences in the data.

# Section 3: Division Correlations and Clustering by Crime Rate
- Here we split the data into divisons according the map here: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf. This sections is essentially the same as Section 2 except that the 4 regions are broken up into 9 divisions. 
- We did this in an effort to see if it was easier to have distinct clusters based on each division as opposed to each region. 
- We found the same results as Section 2: The clusters have major overlap and are unable to accurately separate the data from each other. This tells us that the FBI should not be analyzing crime based on division clusters because it is too difficult to accurately distinguish the differences in the data. 

# Section 4: Division Clustering by Crime Rate and Median Household Income (MHI) 
- In this section we decided to cluster based on all crime rates and Median Household Income (MHI) to see if the introduction of an economic metric was able to introduce some separation between the Division clusters. 
- Our results proved unsuccessful yet again. The clusters from each division were arguably worse than when we only observed crime rates. This indicated to us that we should introduce another economic metric: poverty rate. Poverty rate was chosen arbitrarily yet it proved highly successful when we analyze it in Seciton 5 and beyond. 

# Section 5: Clustering by Economic Metrics
- We received our poverty rate data from the link here: https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-people.html. We also attempted to implement the lowest quintile's income distribution but we didn't have enough time to dive into this data: https://www.taxpolicycenter.org/statistics/household-income-quintiles.
- In this section we create a dataframe that holds both poverty rate and median household income that we then use to create various clusters (2, 4, and 9). Our findings indicated that using only economic metrics and no crime rates were a much better way to distinguish clusters from one another. We used both euclidean and manhattan distances to ensure the distances between the clusters were true. 
- Through this section we learned that using both poverty rate and median household income were great measures to create clusters related to the four regions and nine divisions in the united states. 
- Towards the bottom of the section we created the DF csv file that included all economic and crime rate variables in the project. We then opened this file in excel and performed a t-test of correlation significance on all of the data to see which data correlated best with poverty rate. This could have been preformed in R but we wanted to use Excel to gain experience using different analysis tools. We found that Homicide had the highest correlation with Poverty Rate at 0.4008, this is what pushed us to attempt to classify Normal and Low homicide rates for the remainder of the project. 

# Section 6: Decision Trees classifying Normal and Low Homicide Rate
- In this section we begin by creating a data frame consisting of only homicide and poverty rate. We then created two clusters of the data and determined that the cluster with the higher homicide rate was categorized as NORMAL and the one with the lower homicide rate was categorized as LOW. We then added this cluster column back onto the large data frame containing all other data (state, year, population, etc...). After a breif analysis, we discovered that Washington DC's homicide rate was the only section where homicide rates were NORMAL, every other reading was LOW, this indicated that we had a massive outlier in Washington DC so we removed it entirely from the data. 
- After preforming the same process as above on the new data frame, we were able to create multiple decision trees that used population, poverty rate, median household income, rape, robbery, aggravated assault, burglary, larceny, and vehicle theft as features to try and accurately classify the homicide class variable. 
- We then saved each of these trees as .png files and they are located in the "Decision Tree Photos" folder for future observation. 

# Section 7: Random Forest classifying Normal and Low Homicide Rate
- Here we created a random forest model on the exact same data we used to create the decision trees on except we bootstrapped the data so that way our model would produce a higher classification accuracy. 
- The number of bootstrapped samples was 33 and the number of observations per sample was 450. We then had the model create 400 trees with an mtry of 3 because we had 8 features. 
- Our findings indicated that poverty rate had the highest weight on determining homicide classificaiton, which makes sense because the two variables were highly correlated in Section 5. The next highest features were respectively robbery, aggravated assault, and then burglary. However, poverty rate still had the highest weight on the outcome by a long shot. 
- Our model was 100% accurate at predicting our Validate homicide classification yet when running the model on our actual validate data, we found it to have an OOB estimate of error rate at 0.07%. 

# Section 8: Gradient Boosted Model clasifying Normal and Low Homicide Rate
- In this section we used the caret library to create a gradient boosted model on the exact same data as the previous random forest and decision tree models. We created a decision tree, random forest, and gradient boosted model in this section because we wanted to compare the accuracy of each model and prove that the gradient boosted model was the best performer out of the group. 
- We found that the average accuracy of the decision tree was 0.92006, the random forest was 0.95481, and the non-tuned gradient boosted model was 0.95547. After tuning the hyperparameters of the gradient boosted model we were able to improve the accuracy to 0.95849, thus proving that the tuned gradient boosted model was the best performer out of the group when attempting to classify homicide rates between NORMAL and LOW. 

# Moving Forward
- We transitioned from using R to using the DataRobot software as a means to use neural networks to analyze the data. This was done in an effort to get me comfortable with creating artifical neural networks as well as learning about the operations within a recursive neural network. 
