# Honors-Senior-Research

Here's a repository containing my R project that I worked on for my CSC 498 course "Honors Senior Research".
The goal of this course is to augment hands-on Artificial Intelligence programming and build upon the analytics skills I learned in the classroom in my CSC 260 course "Analysis of Algorithms". The work I completed in that course can be found here: https://github.com/brettv30/Analysis-of-Algorithms.

Initially we began with datasets containing National Crime Rate and State Crime Rate data. These can be found in the "Section 1 Data" folder. We retreived these datasets from the FBI Crime Stats website which can be semi-recreated from here: https://ucrdatatool.gov/Search/Crime/Crime.cfm. 

The script file is broken up into a total of 8 different sections. Each one indicating a different milestone in this class.  

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

# Section 7: Random Forest classifying Normal and Low Homicide Rate

# Section 8: Gradient Boosted Model clasifying Normal and Low Homicide Rate
