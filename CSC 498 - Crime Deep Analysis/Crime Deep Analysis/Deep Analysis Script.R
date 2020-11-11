# Brett Vogelsang
# Deep Analysis of Crime Statistics from the FBI and Economic Statistics from the Census Bureau

##### Libraries #####
library(moments)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(ggrepel)
library(Rmisc)
library(amap)
library(factoextra)
library(cluster)
library(gridExtra)
library(party)
library(partykit) 
library(ROCR)
library(boot)
library(randomForest)
library(dplyr)
library(plyr)
library(caret)
library(rpart.plot)

##### Section 1: Exploratory Data Analysis #####

# Read in the Data

# National Crime Rate
NCR = read.csv('National Crime - Rates copy.csv')

# State Crime Rate
SCR = read.csv('State Crime - Rates copy.csv')

# View a summary of each set
summary(NCR)

# From the National Crime - Rates set we see the larceny as the crime with the highest rate overall. 
# Here we notice that aggravated asault has a median higher than the average, again signifying that there are some years where aggravated assault occurs at a noticeably higher rate. 
# We also see that motor vehicle theft has a lower rate of occurance than burglary, we'll need to research this further. 

summary(SCR)

# From the State Crime - Rates set we first notice that the vehicle theft column is absent. 
# Next we become aware of the large jumps from the 2rd Quartile to the Max values again. 
# Like the previous set, it would be much better to view a summary of each State's columns instead of all the data combined together. 

# Now we are going to introduce the some different libraries in order to better understand 
# the skewness, kurtosis, normalcy, and correlations within the data
library(moments)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(ggrepel)

# first by looking at the entire state crime rate data set we are going to see if the data are 
# symmetrical and normal or if we are going to need to make some changes along the way

skewness(SCR[, c(4:10)])

# We observe homicide and robbery as being highly skewed because they are both greater than a value
# of 3.

kurtosis(SCR[, c(4:10)])

# Every single feature has extreme outliers, this leads us to belive that the data is non-normal.

# Lets take a look at the homicide, robbery, and larceny distributions. 

ggplot(SCR, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Entire 50 state homicide rate density curve",
       x = 'Rate of homicides per year')

# Here we see an extremely positively skewed distribution with two major peaks near the rates of 5 and 6 per year. 

ggplot(SCR, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Entire 50 state robbery rate density curve",
       x = 'Rate of robberies per year')

# Again we find this to be an extremely positively skewed distribution with a major peak near the rate of 125 per year. 

ggplot(SCR, aes(larceny)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Entire 50 state larceny rate density curve",
       x = 'Rate of larcenies per year')

# We decided to view the larceny distribution because it had the lowest skewness and kurtosis measurements. This graph
# is drastically different from the previous two in that it is more of a rounded out curve with no extreme peak or valley. 

# Next we are going to view two different heatmaps of the correlations between each variable in the set. 
SCRcor = cor(SCR[4:10])

ggcorrplot(SCRcor, type='lower', title='Entire 50 state Crime Rate Heatmap', hc.order = TRUE)

corrplot(SCRcor,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Entire 50 state Crime Rate Correlation Plot')

# So we can automatically tell that rape has lower correlations with every other crime compared to everything else. 
# Although the correlation between rape and aggravated assault is at 42%, there are higher correlations in the group. 
# The strongest correlations are between homicide and robbery(0.82) and larceny and burglary(0.76).
# After looking at this graph, we can make the assumption that over all 50 states rape will have lower correlations with
# every other variable besides aggravated assault and some of the strongest correlated variables will be between burglary and larceny and
# robbery and homicide. 

# Lets look more in depth at some of the direct correlations between the variables.

ggplot(SCR, aes(aggravated_assault, rape)) +
  geom_point() +
  labs(title = "Entire 50 state correlation between rape and aggravated assault",
       x = "Aggravated assaults per year", y = "Rapes per year")

# This is a visualization of a 42% correlation between rape and aggravated assault. Its interesting that there seems to be
# two different paths that the correlations take. This could be broken up in to three or four different clusters. 

ggplot(SCR, aes(homicide, robbery)) +
  geom_point() +
  labs(title = "Entire 50 state correlation between homicide and robbery",
       x = 'Homicides per year', y = "Robberies per year")

# This is a visualization of an 82% correlation between two variables. Again we see these two paths that the data transitions
# into. Could this be because of the lack of normal distributions amongst the data? We can observe at least three different 
# clusters of data. 

ggplot(SCR, aes(larceny, burglary)) +
  geom_point() +
  labs(title = "Entire 50 state correlation between larceny and burglary",
       x = "Larcenies per year", y = "Burglaries per year")

# Here we have a visualization of a 76% correlation between two variables. This looks like a normal positive correlation graph. 
# We can easily identify the positive relationship between burglaries and larcenies. 

ggplot(SCR, aes(rape, robbery)) +
  geom_point() +
  labs(title = "Entire 50 state correlation between rape and robbery",
       x = "Rapes per year", y = "Robberies per year")

# Here we have a visualization of a 20% correlation between two variables. The lack of correlation is easily identifiable because of
# the space between the different paths that the data takes. Although the large group of data in the bottom left corner of the graph
# may elude to a stronger correlation, the outliers are what determines the correlation percentage. 

# Before we take a deep dive into each individual sate, lets look at the normalcy of the National Crime Rate graphs to see
# if we have any nornal distributions. 

skewness(NCR[, c(2:9)])

# On the bright side we see that the only moderately skewed variable is rape (negatively). The rest of the data are symmetrical
# This could lead to normal distributions amongst some of the variables. 

kurtosis(NCR[, c(2:9)])

# All of the data are platykurtic, meaning they need some more outliers to round out the distribution. Lets investigate 
# the distributions of homicide, robbery, and larceny. 

ggplot(NCR, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "National homicide rate density curve",
       x = 'Rate of homicides per year')

# Here we see an almost normal distribuiton.We have two solid curves near the rates of 5 and 9 per year. The downfall of
# this variable is that the range is too small, If we were to extend out to a rate of 12 per year, the data would look more normalized. 

ggplot(NCR, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "National robbery rate density curve",
       x = 'Rate of robberies per year')

# Here we observe a virtually normal distribution. We need to ask further questions to better interpret this graph. 

ggplot(NCR, aes(larceny)) +
  geom_density(kernel="gaussian") + 
  labs(title = "National larceny rate density curve",
       x = 'Rate of larcenies per year')

# This would seem like its a moderately negatively skewed distribution, however because the data doesn't go below a rate of 
# 1000 per year we are able to keep it only slightly skewed. 

# Next we are going to observe the correlations between each variable in the set

NCRcor = cor(NCR[2:9])

ggcorrplot(NCRcor, type='lower', title='National Crime Rate Heatmap', hc.order = TRUE)

corrplot(NCRcor,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='National Crime Rate Correlation Plot')

# In the above graphs we observe some slightly different correlations than in the previous heatmaps. Here we see an
# extremely strong correlation between rape and aggravated assault(0.87). Some of the other strong correlations occur 
# between: larceny and robbery(0.94), homicide and burglary(0.92), robbery and motor vehicle theft(0.92), robbery and
# homicide(0.90), and motor vehicle theft and larceny(0.89). 
# WE are going to make the conclusion that this is a better observation of the entire nation compared to the previous
# data set. This begs the question, why do they differ in correlation strength? Could one be more accurate than the other?
# Also why is the data in this set better distributed than the data in the previous set. 

# Lets took at the correlations between: rape and aggravated assault, larceny and robbery, rape and burglary, and 
# homicide and burglary. 

ggplot(NCR, aes(aggravated_assault, rape)) +
  geom_point() +
  labs(title = "National correlation between rape and aggravated assault",
       x = "Aggravated assaults per year", y = "Rapes per year")  +
  geom_label_repel(aes(label=year), box.padding=0.25,
                   point.padding=0.5)

# Here we have a visualization of an 87% correlation between two variables. We can automatically tell that rape and 
# aggravated assault are extremely correlated because of the positive trend in the data. There are a few outliers around the
# rate of 40 rapes per year and 250 aggravated assaults per year.

ggplot(NCR, aes(larceny, robbery)) +
  geom_point() +
  labs(title = "National correlation between larceny and robbery",
       x = "larcenies per year", y = "Robberies per year")

# Here we have a visualization of a 94% correlation between two variables. We observe that the correlation between the variables
# is positive and it slightly bends downwards towards the middle of the curve. There seems to be about four different clusters
# of data here. 

ggplot(NCR, aes(burglary, rape)) +
  geom_point() +
  labs(title = "National correlation between rape and burglary",
       x = "Burglaries per year", y = "Rapes per year") +
  geom_label_repel(aes(label=year), box.padding=0.25,
                   point.padding=0.5)

# Here we have a visualization of a 27% correlation between two variables. We see something different here. There looks
# to be two different paths in the positive direction, yet there's also two different paths in the negative direction. This
# grpah helps us better understand why there is only a 27% correlation between rape and burglary.

ggplot(NCR, aes(homicide, burglary)) +
  geom_col() +
  labs(title = "National correlation between homicide and burglary",
       x = "Homicides per year", y = "Burglaries per year")

# Here we have a visualization of a 92% correlation between two variables. It may be a little difficult to view the data because 
# homicide is a diSCRete variable. We can somewhat see a positive trend in the data which is proof of the 92% correlation. 

# Now we are going to dig deeper and look at every single state's data to better understand the crime rates and distributions
# in each of the 50 states. 

# We are going to dig a little deeper and see each state's correlations to maybe find some 
# connections in this mess. We may need to find some different data for this research to be viable though.
# Not 100% sure yet. We should also pay attention to relationships that involve homicide and robbery. These 
# could have normal distribution issues. 

# Next we are going to do the same thing but with the State Crime - Rates data set.

AKRdata = subset(SCR, SCR$state == 'AK')
ALRdata = subset(SCR, SCR$state == 'AL')
ARRdata = subset(SCR, SCR$state == 'AR')
AZRdata = subset(SCR, SCR$state == 'AZ')
CARdata = subset(SCR, SCR$state == 'CA')
CORdata = subset(SCR, SCR$state == 'CO')
CTRdata = subset(SCR, SCR$state == 'CT')
DCRdata = subset(SCR, SCR$state == 'DC')
DERdata = subset(SCR, SCR$state == 'DE')
FLRdata = subset(SCR, SCR$state == 'FL')
GARdata = subset(SCR, SCR$state == 'GA')
HIRdata = subset(SCR, SCR$state == 'HI')
IARdata = subset(SCR, SCR$state == 'IA')
IDRdata = subset(SCR, SCR$state == 'ID')
ILRdata = subset(SCR, SCR$state == 'IL')
INRdata = subset(SCR, SCR$state == 'IN')
KSRdata = subset(SCR, SCR$state == 'KS')
KYRdata = subset(SCR, SCR$state == 'KY')
LARdata = subset(SCR, SCR$state == 'LA')
MARdata = subset(SCR, SCR$state == 'MA')
MDRdata = subset(SCR, SCR$state == 'MD')
MERdata = subset(SCR, SCR$state == 'ME')
MIRdata = subset(SCR, SCR$state == 'MI')
MNRdata = subset(SCR, SCR$state == 'MN')
MORdata = subset(SCR, SCR$state == 'MO')
MSRdata = subset(SCR, SCR$state == 'MS')
MTRdata = subset(SCR, SCR$state == 'MT')
NCRdata = subset(SCR, SCR$state == 'NC')
NDRdata = subset(SCR, SCR$state == 'ND')
NERdata = subset(SCR, SCR$state == 'NE')
NHRdata = subset(SCR, SCR$state == 'NH')
NJRdata = subset(SCR, SCR$state == 'NJ')
NMRdata = subset(SCR, SCR$state == 'NM')
NVRdata = subset(SCR, SCR$state == 'NV')
NYRdata = subset(SCR, SCR$state == 'NY')
OHRdata = subset(SCR, SCR$state == 'OH')
OKRdata = subset(SCR, SCR$state == 'OK')
ORRdata = subset(SCR, SCR$state == 'OR')
PARdata = subset(SCR, SCR$state == 'PA')
RIRdata = subset(SCR, SCR$state == 'RI')
SCRdata = subset(SCR, SCR$state == 'SC')
SDRdata = subset(SCR, SCR$state == 'SD')
TNRdata = subset(SCR, SCR$state == 'TN')
TXRdata = subset(SCR, SCR$state == 'TX')
UTRdata = subset(SCR, SCR$state == 'UT')
VARdata = subset(SCR, SCR$state == 'VA')
VTRdata = subset(SCR, SCR$state == 'VT')
WARdata = subset(SCR, SCR$state == 'WA')
WIRdata = subset(SCR, SCR$state == 'WI')
WVRdata = subset(SCR, SCR$state == 'WV')
WYRdata = subset(SCR, SCR$state == 'WY')

# Here we are going to review the skewness and kurtosis of all numerical variables in each State's data set
# to better understand if the data is normally distributed. Then we will look at the correlations occuring in 
# the data to see if we can find any trends/relationships. 

### Alaska

skewness(AKRdata[4:10])

kurtosis(AKRdata[4:10])

# First we create a vector of the correlation numbers between Alaskan Crime Rates.
corAKR = cor(AKRdata[4:10])

# Next we create a heatmap and a correlation plot to find connections between this data
ggcorrplot(corAKR, type='lower', title='Alaska Crime Rate Heatmap', hc.order = TRUE)

corrplot(corAKR,method='number', order = "AOE", type = 'lower', 
         diag = FALSE, tl.srt=0, tl.cex=.8,tl.col = 'black', main='Alaska Crime Rate Correlation Plot')

# In the above graphs we see major connections between burglary, larceny, and vehicle theft. We see a slight
# correlation between homicide and burglary. We see very little correlation between aggravated assault and burglary. 
# The second graph contains the correlation numerical values. 

### Alabama

skewness(ALRdata[4:10])

kurtosis(ALRdata[4:10])

corALR = cor(ALRdata[4:10])

ggcorrplot(corALR, type='lower', title='Alabama Crime Rate Heatmap', hc.order = TRUE)

corrplot(corALR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Alabama Crime Rate Correlation Plot')

# Next for Alabama we see a major correlation between robbery and larceny at 0.86 and also a strong correlation
# between robbery and vehicle theft at 0.82. In Alabama these are connected by saying that someone who commits a
# robbery is more likely to also commit either vehicle theft or larceny as well. 

### Arkansas

skewness(ARRdata[4:10])

kurtosis(ARRdata[4:10])

corARR = cor(ARRdata[4:10])

ggcorrplot(corARR, type='lower', title='Alabama Crime Rate Heatmap', hc.order = TRUE)

corrplot(corARR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Alabama Crime Rate Correlation Plot')

# For Arkansas we see strong correlations between larceny and robbery at 0.85, larceny and vehicle theft at 0.86,
# and robbery and vehicle theft at 0.82. There could be a connection because Alabama had relatively the same correlations
# we'll have to look out for this and see a trend.
# There are weak correlations between burglary and rape at -0.21 and rape and homicide at -0.16. Which means that 
# the people who rape usually only desire to commit that crime. However we see there is a 0.72% chance of someone
# committing rape and aggravated assault, these two offenses may be connected somehow. 

### Arizona

skewness(AZRdata[4:10])

kurtosis(AZRdata[4:10])

corAZR = cor(AZRdata[4:10])

ggcorrplot(corAZR, type='lower', title='Arizona Crime Rate Heatmap', hc.order = TRUE)

corrplot(corAZR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Arizona Crime Rate Correlation Plot')

# Arizona has strong connections between burglary and larceny and robbery and homicide which are 
# both at 0.9. This means that if youre getting robbed in Arizona, you're either going to die or 
# have your property stolen. Also Larceny and Aggravated assault are strongly correlated at 0.8 so
# be careful of that as well. 
# On the flip side of things if you are being raped there is a low chance of any other crime being committed.
# Also burglary and vehicle theft have a low correlation so if you're being robbed your car is probably okay. 

### California

skewness(CARdata[4:10])

kurtosis(CARdata[4:10])

corCAR = cor(CARdata[4:10])

ggcorrplot(corCAR, type='lower', title='California Crime Rate Heatmap', hc.order = TRUE)

corrplot(corCAR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='California Crime Rate Correlation Plot')

# Right off the bat we can see that all crimes in California are slightly correlated in some way.
# Could this be because of the large amount of people who call that state their home?
# Some of the strongest correlations occur between: burglary and larceny(0.97), robbery and homicide(0.98),
# homicide and larceny(0.95), robbery and larceny(0.94), aggravated assault and vehicle theft(0.92), 
# and homicide and burglary(0.9). 

### Colorado

skewness(CORdata[4:10])

Kurtosis(CORdata[4:10])

corCOR = cor(CORdata[4:10])

ggcorrplot(corCOR, type='lower', title='Colorado Crime Rate Heatmap', hc.order = TRUE)

corrplot(corCOR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Colorado Crime Rate Correlation Plot')

# Based on the graphs we can see that there are stong correlations between: burglary and larceny(0.94),
# robbery and burglary(0.95), robbery and homicide(0.91), robbery and larceny(0.91), homicide and burglary(0.88),
# and homicide and larceny(0.89). SO it looks like you don't want to get robbed in Colorado or else its highly 
# likely a different crime will be committed as well. 
# Again pay attention to the rape section, if you are being raped it its highly unlikely for a second crime to be 
# committed. 

### Conneticut

skewness(CTRdata[4:10])

kurtosis(CTRdata[4:10])

corCTR = cor(CTRdata[4:10])

ggcorrplot(corCTR, type='lower', title='Conneticut Crime Rate Heatmap', hc.order = TRUE)

corrplot(corCTR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Conneticut Crime Rate Correlation Plot')

# Again the conneticut numbers remind me a lot of the California correlations, except california was a bit 
# more extreme. Here we have strong correlations between: robbery and larceny(0.96), larceny and burglary (0.96),
# robbery and burglary(0.95), robbery and vehicle theft(0.92), larceny and vehicle theft(0.92), burglary and
# vehicle theft(0.87). This takes away from my statement that California's high correlations were because 
# of its large population. 
# Notice how in conneticut you are much more likely to have a second crime committed at the scene of a rape,
# could this be because of geographical location?

### District of Columbia

skewness(DCRdata[4:10])

kurtosis(DCRdata[4:10])

corDCR = cor(DCRdata[4:10])

ggcorrplot(corDCR, type='lower', title='District of Columbia Crime Rate Heatmap', hc.order = TRUE)

corrplot(corDCR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='District of Columbia Crime Rate Correlation Plot')

# In the above graphs we can see a strong correlation between: robbery and burglary (0.92) and aggravated assault
# and homicide(0.90). Next we can see some moderately strong correlations between: larceny and burglary(0.77),
# larceny and robbery(0.77), vehicle theft and homicide(0.76), and vehicle theft and aggravated assault(0.75).
# Again we see that rape has some fairly weak correaltions with other crimes. Also, except for homicie and
# aggravated assault, vehicle theft has some fairly low correlations with other crimes. 

##### Deleware

skewness(DERdata[4:10])

kurtosis(DERdata[4:10])

# After viewing the Kurtosis value for Burglary lets dive deeper take a look at the density curve of 
# Deleware's Burglary Crime Rate to see where the outliers are. This could help explain the Kurtosis value. 

ggplot(DERdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Delaware burglary rate density curve",
       x = 'Rate of burglaries per year')

# We can find a large amount of the data around the rate of 800 burglaries per year. There are a small amount 
# of data around the 1600 mark, which is probably why the kurtosis is greater than 2. We may need to 
# 'remove' these points somehow. 

corDER = cor(DERdata[4:10])

ggcorrplot(corDER, type='lower', title='Delaware Crime Rate Heatmap', hc.order = TRUE)

corrplot(corDER,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Delaware Crime Rate Correlation Plot')

# From the above graphs we can interpret the data to have one very strong correlation between: burglary
# and larceny(0.92). There is one other moderately strong correlation between: robbery and aggravated assault(0.81).
# For the most part, Delaware seems to be a state where its not likely a second crime will be committed.
# This is interesting, Homicide is a crime that has the least amount of correlation with any other crime,
# usually this is rape. Pay attention to this because this may be important. 

### Florida

skewness(FLRdata[4:10])

kurtosis(FLRdata[4:10])

corFLR = cor(FLRdata[4:10])

ggcorrplot(corFLR, type='lower', title='Florida Crime Rate Heatmap', hc.order = TRUE)

corrplot(corFLR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Florida Crime Rate Correlation Plot')

# This data looks a LOT like the California data. It seems like almost all crimes have at least a 67% chance 
# of happening together besides: aggravated assault and homicide(0.47) and vehicle theft and homicide(.40).
# The crimes that are most likely to happen together are: vehicle theft and aggravated assault(0.98), 
# robbery and larceny(0.96), burglary and larceny(0.94), burglary and robbery(0.94), homicide and burglary(0.91),
# and larceny and aggravated assault(0.90). 
# These crime rates could have something to do with geographical location, popular vacation spots,
# population, and state laws. 

### Georgia

skewness(GARdata[4:10])

kurtosis(GARdata[4:10])

ggplot(GARdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Georgia homicide rate density curve",
       x = 'Rate of homicides per year')

# Immediately we notice a large amount of the homicides are around the 7.5 per year rate. But the data
# are leptokurtic based on values that are over a rate of 14 per year. We may need to remove these to 
# normalize the data. 

corGAR = cor(GARdata[4:10])

ggcorrplot(corGAR, type='lower', title='Georgia Crime Rate Heatmap', hc.order = TRUE)

corrplot(corGAR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Georgia Crime Rate Correlation Plot')

# This data is not as correlated as Florida's or California's bu there are some extremely strong correaltions
# that are worth exploring: aggravated assault and larceny(0.95), vehicle theft and larceny(0.91), 
# robbery and larceny(0.90), robbery and aggravated assault(0.90), vehicle theft and aggravated assault(0.89),
# and homicide and burglary(0.88). The lowest correlation is surprisingly homicide and vehicle theft(0.24)
# usually rape has the lowest correlation out of the crimes so this is intriguing to see. 

### Hawaii

skewness(HIRdata[4:10])

kurtosis(HIRdata[4:10])

ggplot(HIRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Hawaii homicide rate density curve",
       x = 'Rate of homicides per year')

# We can see that the data are skewed because of the values that are grater than a rate of 6 homicides
# per year. There is a large amount of homicides near the 2.5 per year rate. 

ggplot(HIRdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Hawaii rape rate density curve",
       x = 'Rate of rapes per year')

# We find here that there are outliers on both ends of spectrum. Because a large amount of rapes occur 
# at a rate near 20 per year, the values that are below 25 and above 25 skew the data.

ggplot(HIRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Hawaii robbery rate density curve",
       x = 'Rate of robberies per year')

# We can see that a large amount of the data are located near the rate of 100 robberies per year. The 
# values that exceed a rate of 120 per year are the reason the robbery data are so skewed. 

corHIR = cor(HIRdata[4:10])

ggcorrplot(corHIR, type='lower', title='Hawaii Crime Rate Heatmap', hc.order = TRUE)

corrplot(corHIR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Hawaii Crime Rate Correlation Plot')

# The first thing we notice from the graphs is that rape is not alone in its lack of second crimes, 
# aggravated assault in Hawaii is extremely not correlated with other crimes. Actually its less correlated
# than rape, which is interesting. 
# the most impactful crimes are between: burglary and larceny(0.85), robbery and burglary(0.84), and 
# burglary and homicide(0.81). Hawaii seems to be a state where its highly unlikely a second crime is committed
# I wonder why this is the case. Could it be because of permanent population numbers, vacation population
# numbers, state laws, or something completely different?

### Iowa

skewness(IARdata[4:10])

kurtosis(IARdata[4:10])

ggplot(IARdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Iowa homicide rate density curve",
       x = 'Rate of homicides per year')

# We see that the curve actually does look fairly symmetrical bacause of the outliers on both
# extremes. If our data had values no less than 1.5 and no grater than 2.5 we would see our 
# homicide data normalize. 

ggplot(IARdata, aes(aggravated_assault)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Iowa aggravated assault rate density curve",
       x = 'Rate of aggravated assaults per year')

# Well this curve explains why the data isn't symmetrical and has outliers. We find outliers
# at both extremes so it would be fair to say if our values were no less than 175 agg. assaults per year
# and no greater than 225 agg. assaults per year, the entire set would then normalize. 

ggplot(IARdata, aes(vehicle_theft)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Iowa vehicle theft rate density curve",
       x = 'Rate of vehicle thefts per year')

# We see a large amount of vehicle thefts occur around the rate of 160 per year so if we removed any
# values that were greater than 200 per year the data would then begin to normalize. 

corIAR = cor(IARdata[4:10])

ggcorrplot(corIAR, type='lower', title='Iowa Crime Rate Heatmap', hc.order = TRUE)

corrplot(corIAR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Iowa Crime Rate Correlation Plot')

# Immediately we can see that burglary and larceny(0.92) are the most likely to occur with one another.
# Something interesting though, aggravated assault and rape have a 49% chance of occuring with every other 
# correlation in each variable being either 0 or negative. Like Hawaii, Iowa has low rates of aggravated assault,
# being correlated with anything else. What are these two state's doing that is different from the other
# state's we've investigated?

#### Idaho

skewness(IDRdata[4:10])

kurtosis(IDRdata[4:10])

ggplot(IDRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Idaho robbery rate density curve",
       x = 'Rate of robberies per year')

# Here we find most of the values just below a rate of 20 per year with outliers that are greater
# than 40. If we remove these insane outliers that are above a rate of 25, we will have normal data. 
# You can see that this graph is positively skewed. 

corIDR = cor(IDRdata[4:10])

ggcorrplot(corIDR, type='lower', title='Idaho Crime Rate Heatmap', hc.order = TRUE)

corrplot(corIDR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Idaho Crime Rate Correlation Plot')

# In the above graphs we see that there are two correlations that stick out the most: burglary and larceny(0.87)
# and burglary and robbery(0.85). The rest of the data are fairly similar in a correlation ranging from 0.5 - 0.77
# Here we also notice that every single correlation between rape and other crimes is negative. This is
# common among most of the sets analyzed so far. There seems to be a trend here. 

### Illinois

skewness(ILRdata[4:10])

kurtosis(ILRdata[4:10])

ggplot(ILRdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Illinois rape rate density curve",
       x = 'Rate of rapes per year')

# As expected, the data has outliers on both ends of the spectrum that are increasing the kurtosis value. 
# If our values were no less than 20 and no greater than 40 I am sure we would have normalized data. 

corILR = cor(ILRdata[4:10])

ggcorrplot(corILR, type='lower', title='Illinois Crime Rate Heatmap', hc.order = TRUE)

corrplot(corILR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Illinois Crime Rate Correlation Plot')

# First thing we notice when lookng at the graph is again the low correlation between rape and every
# other crime in the set. On the flip side of that, there are some strong correlations in the data as well:
# larceny and vehicle theft(0.98), robbery and vehicle theft(0.94), larceny and robbery(0.94), burglary and
# vehicle theft(0.92), larceny and burglary(0.90), homicide and robbery(0.89), and robbery and burglary(0.88).
# I would be curious to see these numbers if we remove the city of Chicago from illionois, its known that
# that city is filled with crime. 
# Also notice that its highly likely for a second crime to be commited during a vehicle theft in illinois. 

#### Indiana

skewness(INRdata[4:10])

kurtosis(INRdata[4:10])

ggplot(INRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Indiana burglary rate density curve",
       x = 'Rate of burglaries per year')

# This curve reminds me a lot of the illinois rape density curve. There is a large concentration around
# 750 burglaries per year and another smaller concentration around 850 burglaries per year and then the extremes
# are around 400 per year and over 1200 hundered per year. If we keep these values between 600 and 1000
# then the data will begin to normalize. 

corINR = cor(INRdata[4:10])

ggcorrplot(corINR, type='lower', title='Indiana Crime Rate Heatmap', hc.order = TRUE)

corrplot(corINR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Indiana Crime Rate Correlation Plot')

# In the above graphs we see rape with slightly higher correlations than normal, and a fairly strong 
# relationship between: rape and aggravated assault(0.72), this isn't the first time we've seen this
# pairing so this may be worth examining further. The other strong correlations out of the data are:
# vehicle theft and larceny(0.91) and larceny and burglary(0.79). The only two negative correlations
# in the group are between: burglary and aggravated assault(-0.06) and burglary and rape (-0.09).

### Kansas

skewness(KSRdata[4:10])

kurtosis(KSRdata[4:10])

corKSR = cor(KSRdata[4:10])

ggcorrplot(corKSR, type='lower', title='Kansas Crime Rate Heatmap', hc.order = TRUE)

corrplot(corKSR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Kansas Crime Rate Correlation Plot')

# In the graphs above we notice that most of the crimes in kansas have lower rates of occuring with
# each other. The entire vehicle theft variable has low rates with its highest being 0.55 with both 
# aggravated assault and robbery. The highest correlations in the set are between: robbery and larceny(0.82),
# robbery and burglary(0.82), and larceny and burglary(0.80). Another correlation to note is that of
# aggravated assault and rape(0.65). This is a recurring theme amongst most states. It's worth researching further. 

### Kentucky

skewness(KYRdata[4:10])

kurtosis(KYRdata[4:10])

ggplot(KYRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Kentucky Robbery rate density curve",
       x = 'Rate of robberies per year')

# Immediately we can tell that this curve is slightly negatively skewed. The kurtosis value is also 
# leptokurtic because of the robbery rate below 70 per year. If this data was removed then the 
# variable would be normalized. 

ggplot(KYRdata, aes(aggravated_assault)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Kentucky aggravated assault rate density curve",
       x = 'Rate of aggravated assaults per year')

# We can tell that this graph is extremely positively skewed. If all of the data greater than a rate of
# 250 was removed then we would have a normailzed aggravated assault set. 

ggplot(KYRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Kentucky burglary rate density curve",
       x = 'Rate of burglaries per year')

# Its easy to see why this graph is symmetrical, there are outliers at both extremes which play into 
# the non-normalcy of the set. If our data was no less than a rate of 550 and no greater than a rate of 
# 850 we would have normalized data. 

ggplot(KYRdata, aes(larceny)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Kentucky larceny rate density curve",
       x = 'Rate of larcenies per year')

# We can tell that this graph is negatively skewed. Most of the data surrounds the rate of 1800 per year
# so if our data went no smaller than a rate of 1550 per year, it would normalize. 

corKYR = cor(KYRdata[4:10])

ggcorrplot(corKYR, type='lower', title='Kentucky Crime Rate Heatmap', hc.order = TRUE)

corrplot(corKYR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Kentucky Crime Rate Correlation Plot')

# All of the correlations with the rape variable are negative, which is common among most states currently
# analyzed. Some of the stronger correlations in this set are: larceny and burglary(0.78), homicide and burglary(0.72), 
# and robbery and larceny(0.67). There are no extremely correlated crimes in Kentucky. Could this be
# because of the amount of open land the state has? Maybe there's state legislation in place that prevents
# certain crimes from being commited together..

### Louisiana

skewness(LARdata[4:10])

kurtosis(LARdata[4:10])

corLAR = cor(LARdata[4:10])

ggcorrplot(corLAR, type='lower', title='Louisiana Crime Rate Heatmap', hc.order = TRUE)

corrplot(corLAR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Louisiana Crime Rate Correlation Plot')

# Notice the increased amounts of correlation between rape and every other crime. Louisiana is a standout
# state in this area. Some other heavily correlated crimes are: larceny and vehicle theft(0.95), robbery and larceny(0.90),
# and vehicle theft and robbery(0.88). Something that's worth noting, aggravated assault and rape have the 
# smallest correlation out of the entire LA data set, 0.28.

### Massachusetts

skewness(MARdata[4:10])

kurtosis(MARdata[4:10])

ggplot(MARdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Massachusetts homicide rate density curve",
       x = 'Rate of homicides per year')

# We can see that the rate of homicides per year ranges from a rate of 2.0 to 4.0. This is 
# a very good example of why a few outliers would benefit the set, it could expand the range
# and normalize the data. 

ggplot(MARdata, aes(vehicle_theft)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Massachusetts vehicle theft rate density curve",
       x = 'Rate of vehicle thefts per year')

# There  are two major groups in this graph, one around a rate of 250 and another around a rate of
# 900. The data could benefit from an increased amount of ratings near the 550-650 range instead of some outliers.
# We can see that there are outliers at both extremes when we look at the data. 

corMAR = cor(MARdata[4:10])

ggcorrplot(corMAR, type='lower', title='Massachusetts Crime Rate Heatmap', hc.order = TRUE)

corrplot(corMAR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Massachusetts Crime Rate Correlation Plot')

# First thing we notice is the low correlations between rape and every other crime in the data set. Although
# the correlation between rape and aggravated assault is the strongest out of rapes correlations (0.24).
# We are also seeing extremely strong relationships between: larceny and vehicle theft(0.96), robbery and
# burglary(0.96), larceny and burglary(0.95), larceny and robbery(0.94), vehicle theft and burglary(0.94),
# and vehicle theft and robbery(0.92). You do not want your vehicle to be stolen in Massachusetts, there's an extremely
# high chance of another crime being committed at the same time. 

### Maryland

skewness(MDRdata[4:10])

kurtosis(MDRdata[4:10])

corMDR = cor(MDRdata[4:10])

ggcorrplot(corMDR, type='lower', title='Maryland Crime Rate Heatmap', hc.order = TRUE)

corrplot(corMDR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Maryland Crime Rate Correlation Plot')

# All of Maryland's crimes are positively correlated, only a few select states have this as well. Could
# the reason for this be because of the problematic city of Baltimore? The strongest correlations in the
# gorup lie with: larceny and robbery(0.94), aggravated assault and vehicle theft(0.90), and burglary and
# larceny(0.90). Pay attention to the extremely high correlations that rape has with every other crime. No other
# state has had correlations like this before. This is worth exploring as well. 

### Maine

skewness(MERdata[4:10])

kurtosis(MERdata[4:10])

ggplot(MERdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Maine homicide rate density curve",
       x = 'Rate of homicides per year')

# WOW, this curve couldn't be better. it is almost perfectly symmetrical without any variation
# on either side. This is the first completely normal set we've seen in the entire SCR data set. 

ggplot(MERdata, aes(aggravated_assault)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Maine aggravated assault rate density curve",
       x = 'Rate of aggravated assaults per year')

# Obviously, this is a positively skewed variable. We can see that the graph is not normalized because
# of the ratings greater than 100 per year. We may need to remove a lot of the outliers in this set. 

corMER = cor(MERdata[4:10])

ggcorrplot(corMER, type='lower', title='Maine Crime Rate Heatmap', hc.order = TRUE)

corrplot(corMER,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Maine Crime Rate Correlation Plot')

# Some key takeaways from these graphs are that rape has negative correlations with all other crimes in
# the data set. Also note that this is the first time we've seen robbery and homicide 
# have lower correlations with every other crime. There are also some strong correlations between: 
# burglary and larceny(0.94), larceny and vehicle theft(0.92), burglary and vehicle theft(0.92),
# burglary and aggravated assault(0.89), and aggravated assault and vehicle theft(0.86). Burglary and
# vehicle theft are two crimes that seem to have partners when being committed in maine. 

### Michigan

skewness(MIRdata[4:10])

kurtosis(MIRdata[4:10])

corMIR = cor(MIRdata[4:10])

ggcorrplot(corMIR, type='lower', title='Michigan Crime Rate Heatmap', hc.order = TRUE)

corrplot(corMIR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Michigan Crime Rate Correlation Plot')

# Take notice that compared to all other crimes rape has the lowest correlations, but all except for one are
# positive so there's still likelihood of a second crime being committed at the scene of a rape. 
# Some of the strongest correlations are: robbery and larceny(0.94), robbery and burglary(0.92), homicide
# and robbery(0.92), vehicle theft and larceny(0.91), larceny and burglary(0.91), and homicide and larceny(0.90). 
# Homicide in Michigan is actually fairly correlated with every other crime except for rape, hopefully
# this trend begins to decrease in future years. 

### Minnesota

skewness(MNRdata[4:10])

kurtosis(MNRdata[4:10])

ggplot(MNRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Minnesota homicide rate density curve",
       x = 'Rate of homicides per year')

# So we notice that there are two large humps in the graph. The reason the data aren't completely 
# normal is because the hump at a rate of 2 per year is slightly smaller than the hump at a rate of 2
# per year. Otherwise this graph looks very nice. 

corMNR = cor(MNRdata[4:10])

ggcorrplot(corMNR, type='lower', title='Minnesota Crime Rate Heatmap', hc.order = TRUE)

corrplot(corMNR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Minnesota Crime Rate Correlation Plot')

# Again appears the low correlations between rape and every other crime besides aggravated assault(0.65).
# Some of the stronger correlations out of this data are: vehicle theft and larceny(0.87), robbery and 
# larceny(0.86), and larceny and burglary(0.84), Theres and extremely negative correlation between 
# burglary and rape(-0.44), this could be something to explore further. 

### Missouri

skewness(MORdata[4:10])

kurtosis(MORdata[4:10])

ggplot(MORdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Missouri rape rate density curve",
       x = 'Rate of rapes per year')

# We can automatically tell that this graph is highly positively skewed. When we look at the data
# we can see that there are a few measurements above the rate of 40, these are what is taking
# the normalcy away from the distribution. 

ggplot(MORdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Missouri burglary rate density curve",
       x = 'Rate of burglaries per year')

# This is a very interesting graph, we can sort of tell that it has a slight positive skew because of 
# how far the tail goes. We can also tell that the data isn't normal because of the differentiation 
# in the height of the two humps in the distribution. 

corMOR = cor(MORdata[4:10])

ggcorrplot(corMOR, type='lower', title='Missouri Crime Rate Heatmap', hc.order = TRUE)

corrplot(corMOR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Missouri Crime Rate Correlation Plot')

# The first thing we noticed was the fairly high correlation between rape and homicide(0.54), this 
# is uncommon and should be investigated further. Every other correlation with rape is fairly small. 
# Some of the strongest correlations in the data are: robbery and burglary(0.82) and larceny and vehicle theft(0.80).
# One negative correlation that stood out was burglary and aggravated assault(-0.12). This should be 
# investigated further as well. 

### Mississippi

skewness(MSRdata[4:10])

kurtosis(MSRdata[4:10])

ggplot(MSRdata, aes(aggravated_assault)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Mississippi aggravated assault rate density curve",
       x = 'Rate of aggravated assaults per year')

# This curve looks a lot like the Missouri rape density curve. It is highly positively skewed and 
# has outliers that pull the tail too far out. If we wanted to normalize the data we would have measurements
# no more than a rate of 250 per year. 

corMSR = cor(MSRdata[4:10])

ggcorrplot(corMSR, type='lower', title='Mississippi Crime Rate Heatmap', hc.order = TRUE)

corrplot(corMSR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Mississippi Crime Rate Correlation Plot')

# The first thing we notice is that homicide has the lowest correlations out of the data set instead of
# rape. Some relationships to note are: rape and robbery(0.68), rape and larceny(0.69), 
# and rape and vehicle theft(0.66). These are measurements we haven't seen before and are worth 
# further investigation. 
# The strongest correlations out of the set are between: larceny and vehicle theft(0.88), robbery and vehicle theft(0.86),
# and robbery and larceny(0.84). Burglary also had very low correlations with the rest of the set.

### Montana

skewness(MTRdata[4:10])

kurtosis(MTRdata[4:10])

ggplot(MTRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Montana homicide rate density curve",
       x = 'Rate of homicides per year')

# So although we have what looks like fairly symmetrical data, we can see there are outliers greater
# than a rate of 5 per year that pull the tail of this curve in the positive direction. 
# If we ditched those outliers then this curve could appear normal.

ggplot(MTRdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Montana rape rate density curve",
       x = 'Rate of rapes per year')

# Immediately we can tell this is a positively skewed curve. The outliers greater than 40 per year are
# the reason for this non-normal curve. We should replace these with data that are more grouped together.

corMTR = cor(MTRdata[4:10])

ggcorrplot(corMTR, type='lower', title='Montana Crime Rate Heatmap', hc.order = TRUE)

corrplot(corMTR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Montana Crime Rate Correlation Plot')

# In the above graphs we see the correlation between rape and aggravated assault(0.65) is fairly strong. 
# Yet for both rape and aggravated assault every other correlation is negative. This is intriguing. The
# only strong correltion worth noting is between larceny and burglary(0.80). Another unique correlation
# is between robbery and homicide(-0.02), we should investigate this further. 

### North Carolina

skewness(NCRdata[4:10])

kurtosis(NCRdata[4:10])

ggplot(NCRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "North Carolina burglary rate density curve",
       x = 'Rate of burglaries per year')

# There's a HUGE spike of burglaries around the rate of 1240 per year. What is causing the non-normalcy 
# is the outliers less than a rate of 800 per year. If we remove those then our data should be normalized.

corNCR = cor(NCRdata[4:10])

ggcorrplot(corNCR, type='lower', title='North Carolina Crime Rate Heatmap', hc.order = TRUE)

corrplot(corNCR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='North Carolina Crime Rate Correlation Plot')

# Surprisingly rape has very 'strong' correlations with all other crimes. Specifically aggravated assault(0.77)
# and robbery(0.77), this is worth researching further. Other strong relationships worth mentioning are
# burglary and larceny(0.89), aggravated assault and larceny(0.86), robbery and vehicle theft(0.87), 
# agggravated assault and homicide(0.86), larceny and vehicle theft(0.84), and burglary and aggravated assault(0.84).
# Something interesting is the relationship between robbery and homicide(0.26), this is fairly low compared
# to other states.

### North Dakota

skewness(NDRdata[4:10])

kurtosis(NDRdata[4:10])

ggplot(NDRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "North Dakota homicide rate density curve",
       x = 'Rate of homicides per year')

# At first glance this graph looks like it should be more normalized than it is. The reason for this 
# lack of normalcy is because comparatively the number of rates or 2 and 4 are drastically less than the
# number of rates of 1 and 2. If we supply more of 2 and 4 then we will find normalcy.

ggplot(NDRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "North Dakota robbery rate density curve",
       x = 'Rate of robberies per year')

# We can see why the distribution is positively skewed. We need to remove the outliers with values greater
# than a rate of 15 in order for the distribution to be normalized. 

ggplot(NDRdata, aes(vehicle_theft)) +
  geom_density(kernel="gaussian") + 
  labs(title = "North Dakota vehicle theft rate density curve",
       x = 'Rate of vehicle thefts per year')

# This curve looks funky. Because of the disproportionate values of rates near 150 and rates near 250
# the distribution suffers. If we were to change some of the rates near 150 to rates near 250, this 
# problem would be solved. 

corNDR = cor(NDRdata[4:10])

ggcorrplot(corNDR, type='lower', title='North Dakota Crime Rate Heatmap', hc.order = TRUE)

corrplot(corNDR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='North Dakota Crime Rate Correlation Plot')

# These correlations are interesting. We have never seen larceny have a negative correlation with so 
# many other crimes before. We should investigate this further. Also note the strong correlations between
# rape and aggravated assault(0.90) and rape and robbery(0.78). Another strong correlation is between
# robbery and aggravated assault(0.85).

### Nebraska

skewness(NERdata[4:10])

kurtosis(NERdata[4:10])

ggplot(NERdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Nebraska rape rate density curve",
       x = 'Rate of rapes per year')

# Immediately we can see that the outliers occur in the values greater than a rate of 40 per year. 
# We will need to remove these to normalize the data. 

ggplot(NERdata, aes(aggravated_assault)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Nebraska aggravated assault rate density curve",
       x = 'Rate of aggravated assaults per year')

# Its very easy to see how this distribution is moderately skewed. There are a couple outliers greater than a rate
# of 200 per year and these are what is dragging the tail in the positive direction. If we remove these
# measurements then we will normalize the data. 

corNER = cor(NERdata[4:10])

ggcorrplot(corNER, type='lower', title='Nebraska Crime Rate Heatmap', hc.order = TRUE)

corrplot(corNER,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Nebraska Crime Rate Correlation Plot')

# Right off the bat we see that rape has negative correlations with every other variable in the set. 
# This is fairly common and needs to be researched further. The strongest correlation is between: larceny and
# burglary( 0.78). The rest of the crimes have fairly lower correlation values. One of the negative 
# correlations that stands out is: burglary and vehicle theft(-0.22).

### New Hampshire

skewness(NHRdata[4:10])

kurtosis(NHRdata[4:10])

ggplot(NHRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "New Hampshire homicide rate density curve",
       x = 'Rate of homicides per year')

# This distribution may be more normalized without the 1991 homicide measurement. It would still be
# non-normal because of the high number of 1s and 2s but at least it would have no real outliers.
# Also pretty cool to note that New Hampshire has a very low homicide rate over the years. 

ggplot(NHRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "New Hampshire burglary rate density curve",
       x = 'Rate of burglaries per year')

# We see a large amount of robberies located around 400 per year. This is skewed to the right 
# because of the measurements greater than 1000. 

ggplot(NHRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "New Hampshire robbery rate density curve",
       x = 'Rate of robberies per year')

# This is a fairly interesting graph to look at. At first glance it doesnt seem too symmetrical and it looks
# like it is positively skewed. However, because of the low amount of values greater than 40 we are able
# to observe an almost normal distribution. 

corNHR = cor(NHRdata[4:10])

ggcorrplot(corNHR, type='lower', title='New Hampshire Crime Rate Heatmap', hc.order = TRUE)

corrplot(corNHR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='New Hampshire Crime Rate Correlation Plot')

# There are two major things that are happening when looking at these graphs. We notice a sort of triad
# between: rape and robbery(0.29), rape and aggravated assault(0.28), and aggravated assault and robbery(0.54).
# Then we also see a triad between: burglary and larceny(0.92), burglary and vehicle theft (0.87), and
# larceny and vehicle theft (0.86). Also note that robbery and aggravated have negative relationships with
# every other variable in the set. These correlations are worth looking further in to. 

### New Jersey

skewness(NJRdata[4:10])

kurtosis(NJRdata[4:10])

ggplot(NJRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "New Jersey homicide rate density curve",
       x = 'Rate of homicides per year')

# Looking at this distribution we can see why it was only moderately skewed. When we look at the data
# we see that there are three different measurements at a rate of 7 per year and two different measurements
# at a rate of 6 per year. If we were to remove the measurements at 7, the data would more than likely 
# normalize or be extremely close to normal. 

ggplot(NJRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "New Jersey burglary rate density curve",
       x = 'Rate of burglaries per year')

# We immediately notice the two humps around the rates of 500 per year and 1000 per year. Had the data 
# taken measurements until a rate of 1500 per year, the distribution would be much more normalized than it is. 

corNJR = cor(NJRdata[4:10])

ggcorrplot(corNJR, type='lower', title='New Jersey Crime Rate Heatmap', hc.order = TRUE)

corrplot(corNJR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='New Jersey Crime Rate Correlation Plot')

# We can tell from the above graphs that many of the crimes committed in New Jersey are correlated. Its
# uncommon to see homicide have the lowest correlation values out of the set. The smallest correlation
# value is between homicide and aggravated assault (0.52). Based off of these readings its safe to say
# that New Jersey is a dangerous state. The variable with the strongest correlations is larceny. its
# almost 90% gauranteed that if larceny is committed, there is a secondary crime being committed as well. 

### New Mexico

skewness(NMRdata[4:10])

kurtosis(NMRdata[4:10])

ggplot(NMRdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "New Mexico rape rate density curve",
       x = 'Rate of rapes per year')

# We see outliers above the rate of 65 per year. A large amount of the data are located around the rate of 
# 52 per year. Its easy to understand why the data are non-normally distributed. 

ggplot(NMRdata, aes(vehicle_theft)) +
  geom_density(kernel="gaussian") + 
  labs(title = "New Mexico vehicle theft rate density curve",
       x = 'Rate of vehicle thefts per year')

# This graph looks a lot like the previous distribution except that the tail is further pulled in the positive
# direction. We know that if we were to remove the outliers greater than a rate of 550 per year, the data
# would begin to behave normally. 

corNMR = cor(NMRdata[4:10])

ggcorrplot(corNMR, type='lower', title='New Mexico Crime Rate Heatmap', hc.order = TRUE)

corrplot(corNMR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='New Mexico Crime Rate Correlation Plot')

# We see a strong correlation betwen: burglary and larceny (0.89) and robbery and vehicle theft(0.75). 
# Also note the low and negative correlations between rape and every other category. One other negtive
# correlation to note is between burglary and vehicle theft (-0.16). 

# Something to consider when viewing the New Mexico data is the large amount of Native American reservation
# land where crimes aren't necessarily reported to the FBI. This needs to be taken into account when
# making observations about the state's data.

### Nevada

skewness(NVRdata[4:10])

kurtosis(NVRdata[4:10])

ggplot(NVRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Nevada homicide rate density curve",
       x = 'Rate of homicides per year')

# Here we see the a positively highly skewed distribution with outliers greater than a rate of 
# 15 per year. 

ggplot(NVRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Nevada robbery rate density curve",
       x = 'Rate of robberies per year')

# Its easy to see why this variable is only moderately skewed, the scale of the rate per year isn't that
# extreme however there are some outliers on both extremes that cause the distribution to be non-normal.

ggplot(NVRdata, aes(aggravated_assault)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Nevada aggravated assault rate density curve",
       x = 'Rate of aggravated assaults per year')

# We recognize theat the data does not have a normal distribution because of the outliers greater than 
# a rate of 500 per year. 

ggplot(NVRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Nevada burglary rate density curve",
       x = 'Rate of burglaries per year')

# The range between the minimum and maximum is the larest we've seen out of all fifty states. Its not
# surprising that Nevada has a large amount of burglaries, considering that both Las Vegas and Reno 
# are in Nevada. Two cities that rely heavily on gambling.

corNVR = cor(NVRdata[4:10])

ggcorrplot(corNVR, type='lower', title='Nevada Crime Rate Heatmap', hc.order = TRUE)

corrplot(corNVR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Nevada Crime Rate Correlation Plot')

# These graphs display something very uncommon. Both vehicle theft and aggravated assault have extremely low
# correlations with every other variable in the set. This is the first time we've seen something like this
# and is worth researching further. 
# Also note the high correlations between: burglary and larceny(0.89), burglary and homicide(0.88), 
# burglary and robbery(0.87), lerceny and homicide(0.84), larceny and robbery(0.86), and homicide and
# robbery (0.88). Why are these four crimes the only ones with extremely high correlations in Nevada?

### New York 

skewness(NYRdata[4:10])

kurtosis(NYRdata[4:10])

corNYR = cor(NYRdata[4:10])

ggcorrplot(corNYR, type='lower', title='New York Crime Rate Heatmap', hc.order = TRUE)

corrplot(corNYR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='New York Crime Rate Correlation Plot')

# Although rape does have the lowest correlations out of the entire set, there's still a 50% chance of 
# a second crime being committed at the scene of a rape. Could these insanely large correlations be because
# of the economic disparity thats prevalent in NYC? It would be fun to take the NYC measurements out
# of this data set and then viewing the changes.
# The only correlation that is below 0.86 besides all of the rape correlations is: burglary and aggravated assault(0.66).
# Every other correlation is 0.86 and above. 

### Ohio

skewness(OHRdata[4:10])

kurtosis(OHRdata[4:10])

ggplot(OHRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Ohio robbery rate density curve",
       x = 'Rate of robberies per year')

# This distribution actually looks extremely normalized. We can observe a slight curve change around the 
# rate of 180 per year, this is what seems to be throwing off the kurtosis value. Other than that, the
# data looks normalized and is a decent example to use. 

ggplot(OHRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Ohio burglary rate density curve",
       x = 'Rate of burglaries per year')

# After viewing the graph we understand why the data is symmetrical. The only difference are the slight 
# humps near the 1200 rate per year and 1500 rate per year. If we were able to have a range of data that
# was closer together, then it would be normalized. 

corOHR = cor(OHRdata[4:10])

ggcorrplot(corOHR, type='lower', title='Ohio Crime Rate Heatmap', hc.order = TRUE)

corrplot(corOHR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Ohio Crime Rate Correlation Plot')

# Recognize that the correlations between rape and every other variable are very low again. This is 
# a trend that has showed up in a majority of the analysis. The strongest correlation in Ohio is between:
# larceny and vehicle theft (0.92). Some other correlations worth noting are: aggravated assault and homicide (0.82),
# robbery and larceny(0.84), and burglary and rovvery(0.80). 

### Oklahoma

skewness(OKRdata[4:10])

kurtosis(OKRdata[4:10])

ggplot(OKRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Oklahoma homicide rate density curve",
       x = 'Rate of homicides per year')

# After viewing the graph and the data we see that the outlier is with the two measurements of 
# 11 and 12. Without these measurements the data would be much more symmetrical and normalized. 

corOKR = cor(OKRdata[4:10])

ggcorrplot(corOKR, type='lower', title='Oklahoma Crime Rate Heatmap', hc.order = TRUE)

corrplot(corOKR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Oklahoma Crime Rate Correlation Plot')

# In these graphs we can see again that rape has extremely low correlations with every other variable in 
# the data set. We also find aggravated assault to have low correlations with every other variable. This
# is something worth further investigation. 
# Oklahoma seems like a fairly toned down state based off of the overall lower correlations between crimes. 
# The strongest correlation is between vehicle theft and burglary(0.86)

### Oregon 

skewness(ORRdata[4:10])

kurtosis(ORRdata[4:10])

ggplot(ORRdata, aes(aggravated_assault)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Oregon aggravated assault rate density curve",
       x = 'Rate of aggravated assaults per year')

# So its easy to see why the data is extremely symmetrical, but comparatively the range from the minimum value
# to the maximum value isn't enough to warrant a normal distribution. This seems very weird after viewing the graph.

corORR = cor(ORRdata[4:10])

ggcorrplot(corORR, type='lower', title='Oregon Crime Rate Heatmap', hc.order = TRUE)

corrplot(corORR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Oregon Crime Rate Correlation Plot')

# This is uncommon: vehicle theft's correlations are on average lower than the correlations between
# rape and every other variable in the set. The strongest correlations in the set are between: robbery and
# burglary(0.96), homicide and robbery(0.90), larceny and aggravated assault(0.85), and larceny and robbery (0.85). 

### Pennsylvania

skewness(PARdata[4:10])

kurtosis(PARdata[4:10])

ggplot(PARdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Pennsylvania rape rate density curve",
       x = 'Rate of rapes per year')

# Rape in Pennsylvania is interesting because it has a symmetrical distribution. The problem comes with
# the outliers near the minimum and maximum. If we remove one outlier toward the max, the data should be
# extremely close to a normal distribution. 

ggplot(PARdata, aes(larceny)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Pennsylvania larceny rate density curve",
       x = 'Rate of larcenies per year')

# Here we see a negative moderately skewed distribution. The outliers lie towards the minimum value
# instead of the normal maximum value. This graph is fairly unique out of the entire data set. 

corPAR = cor(PARdata[4:10])

ggcorrplot(corPAR, type='lower', title='Pennsylvania Crime Rate Heatmap', hc.order = TRUE)

corrplot(corPAR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Pennsylvania Crime Rate Correlation Plot')

# Here we view the extremely low and negative correlations between rape and every other variable in
# the set. Aggravated assault has very low correlations with every other variable as well. A surprise, 
# homicide's correlations are also very low with every other variable in the set. 
# The strongest correlations are between robbery and larceny(0.89) and robbery and vehicle theft(0.82).

# Lets take a look at the correlation between robbery and larceny to better understand the relationship.

ggplot(PARdata, aes(robbery,larceny)) +
  geom_point()


### Rhode Island

skewness(RIRdata[4:10])

kurtosis(RIRdata[4:10])

ggplot(RIRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Rhode Island robbery rate density curve",
       x = 'Rate of robberies per year')

# We can see that although the graph is symmetrical, there arent enough outliers on either of the extrema to 
# make the graph a completely normal distribution. 

corRIR = cor(RIRdata[4:10])

ggcorrplot(corRIR, type='lower', title='PRhode Island Crime Rate Heatmap', hc.order = TRUE)

corrplot(corRIR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Rhode Island Crime Rate Correlation Plot')

# As we can see, rape has negative correlations with every other variable in the set. Every other variable has 
# at least a 60% correlation with one another. The strongest correlations are between: vehicle theft and robbery(0.94),
# vehicle theft and larceny(0.94), robbery and larceny(0.94), robbery and burglary(0.92), larceny and burglary(0.92),
# vehicle theft and burglary(0.92). Lets take a look at the distribution between rape and burglary(-0.77).

ggplot(RIRdata, aes(rape,burglary)) +
  geom_point()  +
  geom_label_repel(aes(label=year), box.padding=0.25,
                   point.padding=0.5)

# From this graph we observe that with less burglaries comes higher rape rates and with higher burglaries comes lower
# rape rates. 

### South Carolina

skewness(SCRdata[4:10])

kurtosis(SCRdata[4:10])

ggplot(SCRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "South Carolina homicide rate density curve",
       x = 'Rate of homicides per year')

# Here we see the graph with a slight positive skewness. However, the data is mesokurtic which means that the outliers are healthy
# and allow for what would be a normalized distribution if the max outlier were to be subtracted by a rate of 1 per year. 

corSCR = cor(SCRdata[4:10])

ggcorrplot(corSCR, type='lower', title='South Carolina Crime Rate Heatmap', hc.order = TRUE)

corrplot(corSCR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='South Carolina Crime Rate Correlation Plot')

# In the above graphs we observe that aside from homicide's correlation with burglary(0.76), it has low correlations
# with every other variable in the set. Also note that the correlation between burglary and vehicle theft is very low (0.08).
# Some of the larger correlations in the group occur between: larceny and aggravated assault(0.96), robbery and aggravated assault(0.96),
# and robbery and larceny(0.92). Aggravated assault seems to be the crime most likely to have a partner when being committed. 

### South Dakota

skewness(SDRdata[4:10])

kurtosis(SDRdata[4:10])

ggplot(SDRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "South Dakota robbery rate density curve",
       x = 'Rate of robberies per year')

# This graph is a little difficult to see why the data are leptokurtic. Comparatively to other robbery density graphs, the
# range of the per year rate isn't as large as others. After observing the graph it is easy to see why the skewness and
# kurtosis are their respective measurements. If we removed the spike of robberies around the rate of 20 per year the graph 
# would begin to normalize. 

ggplot(SDRdata, aes(aggravated_assault)) +
  geom_density(kernel="gaussian") + 
  labs(title = "South dakota aggravated assault rate density curve",
       x = 'Rate of aggravated assaults per year')

# Its easy to tell that this graph is positively highly skewed because of the minimum and maximum rates per year. 
# If the spike near a rate of 100 per year was slightly less dense, the rest of the graph would normalize. 

ggplot(SDRdata, aes(vehicle_theft)) +
  geom_density(kernel="gaussian") + 
  labs(title = "South Dakota vehicle theft rate density curve",
       x = 'Rate of vehicle thefts per year')

# This graph isn't normal due to the small spikes near the rates of 140 and 170 per year. If these were missing from the set,
# the graph would begin to normalize. 

corSDR = cor(SDRdata[4:10])

ggcorrplot(corSDR, type='lower', title='South Dakota Crime Rate Heatmap', hc.order = TRUE)

corrplot(corSDR,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='South Dakota Crime Rate Correlation Plot')

# Something really weird is going on here. We observe larceny and burglary to have extremely negative correlations
# with every other variable. Except between larceny and buglary(0.90) which is the strongest out the the entire set. 
# Rape's correlation with aggravated assault(0.75) is worth noting as abnormally high.
# Lets dive deeper into the relationships between larceny and burglary and larceny and rape. 

ggplot(SDRdata, aes(larceny, burglary)) +
  geom_point()

# We already begin to see three different clusters forming when looking at this distribution. There is a cluster near
# the low amount of burglaries and low amount of larcenies. A cluster forms near the mid to higher larceny/burglary rate.
# Then a final cluster forms at the rate of high larcenies and high burglaries. 

ggplot(SDRdata, aes(larceny, rape)) +
  geom_point()

# We see a slight downward trend in the data, but it's very hard to accurately interpret the distribution. Hence the reason
# why rape and larceny have a negative correlation. 

### Tennessee

skewness(TNRdata[4:10])

kurtosis(TNRdata[4:10])

ggplot(TNRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Tennessee burglary rate density curve",
       x = 'Rate of burglaries per year')

# So as we already knew, the data is negatively skewed. We observed that the data are platykurtic because of the large
# amount of measurements near the rate of 1000 per year and the small amount of measurements near the rate of 625 per year. 

corTNR = cor(TNRdata[4:10])

ggcorrplot(corTNR, type='lower', title='Tennessee Crime Rate Heatmap', hc.order = TRUE)

corrplot(corTNR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Tennessee Crime Rate Correlation Plot')

# Here we see some abnormal behavior. Aggravated assault has some of the lowest correlations out of the entire set except
# when it comes to larceny(0.70). There is also an abnormally large correlation between rape and vehicle theft(0.72). The
# strongest correlation is between robbery and vehicle theft(0.86). 
# Some correlations worth exploring further are between aggravated assault and larceny and homicide and larceny. 

ggplot(TNRdata, aes(aggravated_assault, larceny)) +
  geom_point()

# Here we see that aggravated assault and larceny have a strong positive correlation that breaks up in to what looks like 
# three different clusters. One cluster where both variables are low. One cluster where both variables are high. And one
# cluster where aggravated assault is high and larceny is low.

ggplot(TNRdata, aes(homicide, larceny)) +
  geom_point()

# This graph is a little difficult to interpret because homicide can be observed as a diSCRete variable. however, there 
# seems to be no real rhyme or reason to the distribution so the negative correlation value is understood. 

### Texas

skewness(TXRdata[4:10])

kurtosis(TXRdata[4:10])

corTXR = cor(TXRdata[4:10])

ggcorrplot(corTXR, type='lower', title='Texas Crime Rate Heatmap', hc.order = TRUE)

corrplot(corTXR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Texas Crime Rate Correlation Plot')

# Here we observe some fairly high correlations between rape and every other variable. Some of the strongest correaltions
# in the set are between: robbery and vehicle theft(0.96), vehicle theft and larceny(0.96), robbery and larceny(0.91), and
# homicide and burglary(0.90). The correlation between rape and aggravated assault (0.29) is actually the lowest out of rape's correlattions
# which is extremely abnormal. 
# Lets view the correlation between robbery and vehicle theft. 

ggplot(TXRdata, aes(robbery, vehicle_theft)) +
  geom_point()

# Here we can see either three or four clusters separating the data. We have a cluster near the low vehicle theft rate and low
# robbery rate. WE have a cluster near the high robbery rate and high vehicle theft rate and we have 1-2 clusters near the middle
# of bother vehicle theft and robbery. 

### Utah

skewness(UTRdata[4:10])

kurtosis(UTRdata[4:10])

ggplot(UTRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Utah homicide rate density curve",
       x = 'Rate of homicides per year')

# The graph we observe looks actually fairly symmetrical but the non-normalcy comes from the slightly large number of
# homicides at the rates of 4 and 5 per year. 

ggplot(UTRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Utah robbery rate density curve",
       x = 'Rate of robberies per year')
 
# Here we see that the data's distribution isn't normal because of the slight increase in measurements near the rate of 
# 80 per year. If these were out of the data set, we would see a more normalized distribution. 

ggplot(UTRdata, aes(vehicle_theft)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Utah vehicle theft rate density curve",
       x = 'Rate of vehicle thefts per year')

# Again we see the positive skewness caused by high outliers. Even though this graph doesn't seem that symmetrical
# the outliers are the cause of this. 

corUTR = cor(UTRdata[4:10])

ggcorrplot(corUTR, type='lower', title='Utah Crime Rate Heatmap', hc.order = TRUE)

corrplot(corUTR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Utah Crime Rate Correlation Plot')

# Here we observe rape having negative correlations with every other variable besides vehicle theft(0.22). The highest
# correlations in the group are: larceny and aggravated assault(0.87) and robbery and burglary(0.87).

### Virginia

skewness(VARdata[4:10])

kurtosis(VARdata[4:10])

ggplot(VARdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Virginia rape rate density curve",
       x = 'Rate of rapes per year')

# Immediately we can tell why the data are symmetrical. The slightly leptokurtic value comes from the ever so slight hump 
# near the rate of 20 per year. Everything else about the data is normalized. 

corVAR = cor(VARdata[4:10])

ggcorrplot(corVAR, type='lower', title='Virginia Crime Rate Heatmap', hc.order = TRUE)

corrplot(corVAR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Virginia Crime Rate Correlation Plot')

# In the above graphs we obseve that again, rape has very low correlations with every other variable. We can also tell that 
# overall, larceny has the highest rate of happening at the same time as a separate crime. However the strongest correlation
# out of the group is a tie between: aggravated assault and vehicle theft(0.92) and robbery and larceny(0.92). Other strong 
# correlations are between larceny and homicide(0.88), aggravated assault and robbery(0.87), and burglary and larceny(0.85).
# Lets investigate the rrelationship between homicide and rape(0.40).

ggplot(VARdata, aes(homicide, rape)) +
  geom_col()

# Here we can see two major clusters between the data. One where homicides are low and rapes are low and one where rapes are high
# and homicides are above a rate of 4 per year. This helps prove that only 40% of the time a rape and a homicide happen together in
# virginia. 

### Vermont

skewness(VTRdata[4:10])

kurtosis(VTRdata[4:10])

ggplot(VTRdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Vermont rape rate density curve",
       x = 'Rate of rapes per year')

# We see here That the data is leptokurtic and positively skewed because of the large amount of rapes near the rate of 40 per year
# and because of the outliers greater than 25 per year. 

ggplot(VTRdata, aes(robbery)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Vermont robbery rate density curve",
       x = 'Rate of robberies per year')

# Its very easy to tell that this graph isn't normally distributed. We see that the outlier at the rate of 29 per year is what 
# makes the data both leptokurtic and highly skewed. 

ggplot(VTRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Vermont burglary rate density curve",
       x = 'Rate of burglaries per year')

# This graph looks fairly normalized except for the slight right skewness near the rate of 1400. If the data were to stop
# at 1400 then it would normalize even better. 

corVTR = cor(VTRdata[4:10])

ggcorrplot(corVTR, type='lower', title='Vermont Crime Rate Heatmap', hc.order = TRUE)

corrplot(corVTR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Vermont Crime Rate Correlation Plot')

# Here we see aggravated assault have very low correlations with every other varible in the set. We also observe rape
# with low correlations but aggravated assault on average is lower. The strongest correlations out of the group belong to 
# larceny and burglary(0.94), larceny and vehicle theft(0.90), and burglary and vehicle theft(0.94). A correlation worth noting
# is between aggravated assault and homicide(-0.22). 
# Lets investigate this more. 

ggplot(VTRdata, aes(homicide, aggravated_assault)) +
  geom_point() +
  geom_label_repel(aes(label=year), box.padding=0.25,
                   point.padding=0.5)

# Here we can tell that the negative correlation comes from the large number of aggravated assaults that correlate with two 
# homicides per year and then the low number of aggravated assaults that correlate with 4 homicides per year. 

### Washington

skewness(WARdata[4:10])

kurtosis(WARdata[4:10])

ggplot(WARdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Washington rape rate density curve",
       x = 'Rate of rapes per year')

# After reviewing this graph we can see that it is moderately positively skewed and that it looks almost normal. If the
# slight increase near the rate of 60 per year were gone, then we could have a normalized distribution. 

corWAR = cor(WARdata[4:10])

ggcorrplot(corWAR, type='lower', title='Washington Crime Rate Heatmap', hc.order = TRUE)

corrplot(corWAR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Washington Crime Rate Correlation Plot')

# This is very unexpected. All the correlations between vehicle theft and every other variable are negative. Rape actually
# has some extremely large correlations: rape and aggravated assault(0.91), rape and robbery(0.84), and rape and larceny(0.78).
# SOme of the other strong correlations are between: larceny and robbery(0.94), aggravated assault and robbery(0.90), and
# aggravated assault and larceny(0.86), larceny and homicide(0.86), and robbery and homicide(0.86). Lets explore the relationships 
# between rape and aggravated assault.

ggplot(WARdata, aes(rape, aggravated_assault)) +
  geom_point()

# So its easy to see a very tight knit positive correlation between rape and aggravated assault. We begin to slightly see
# 2-4 different clusters on this graph. 

### Wisconsin

skewness(WIRdata[4:10])

kurtosis(WIRdata[4:10])

ggplot(WIRdata, aes(homicide)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Wisconsin homicide rate density curve",
       x = 'Rate of homicides per year')

# Although this graph looks very normalized, the only reason it is not considered normal is because of the lower amount 
# of homicides at a rate of 4 per year and the slight decline near the rate of 5 per year. 

ggplot(WARdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Wisconsin rape rate density curve",
       x = 'Rate of rapes per year')

# Here we can easily tell that the distribution is not normal and it is positively skewed. This is because of the 
# outliers greater than a rate of 60 per year. 

corWIR = cor(WIRdata[4:10])

ggcorrplot(corWIR, type='lower', title='Wisconsin Crime Rate Heatmap', hc.order = TRUE)

corrplot(corWIR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='WisconsinCrime Rate Correlation Plot')

# We can immediately tell that both rape and aggravated assault (0.75) have very low correlations with every other variable
# except each other. The strongest correlation in the set is between burglary and larceny(0.92). The rest of the data are
# standard measurements that occur many times throughout the previous analysis. 

### West Virginia

skewness(WVRdata[4:10])

kurtosis(WVRdata[4:10])

ggplot(WVRdata, aes(rape)) +
  geom_density(kernel="gaussian") + 
  labs(title = "West Virginia rape rate density curve",
       x = 'Rate of rapes per year')

# As we can see, there is an extremely high amount of rapes near the rate of 20 per year. The heavy outliers are around 
# a rate of 44 per year and 28 per year. If we remove these values then the data will begin to normalize. 

ggplot(WVRdata, aes(burglary)) +
  geom_density(kernel="gaussian") + 
  labs(title = "West Virginia burglary rate density curve",
       x = 'Rate of burglaries per year')

# Here we see a negatively skewed variable with a very high amount of burglaries near a rate of 600 per year. 
# The minimum outliers are near a rate of 200 per year. 

corWVR = cor(WVRdata[4:10])

ggcorrplot(corWVR, type='lower', title='West Virginia Crime Rate Heatmap', hc.order = TRUE)

corrplot(corWVR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='West Virginia Crime Rate Correlation Plot')

# This data looks a lot like the Wisconsin data as well. Rape and aggravated assault have very low correlations with every
# other variable except each other at (0.54). The highest correlation in the entire set is between burglary and robbery(0.61).
# Through these graphs we begin to wonder why there are a lack of strong correlations between the data.

### Wyoming

skewness(WYRdata[4:10])

kurtosis(WYRdata[4:10])

ggplot(WYRdata, aes(vehicle_theft)) +
  geom_density(kernel="gaussian") + 
  labs(title = "Wyoming vehicle theft rate density curve",
       x = 'Rate of vehicle thefts per year')

# Although the range of the rate of vehicle thefts per year isn't too bad, there are so many oscillations in the graph
# that cause this non-normalcy. This is an extremely unique distribution. 

corWYR = cor(WYRdata[4:10])

ggcorrplot(corWYR, type='lower', title='Wyoming Crime Rate Heatmap', hc.order = TRUE)

corrplot(corWYR, method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Wyoming Crime Rate Correlation Plot')

# Here we observe rape to have a negative correlation with every other variable in the data set. Every other crime has
# at least a 52% chance of happening with each other. The strongest correlations are between: vehicle theft and robbery(0.92)
# and burglary and larceny(0.91). I wonder if the data is so heavily correlated because most of the distributions aren't normal.




##### Section 2: Regional Correlations, Clustering, and Median Household Income #####

# Here we are going to organize the states by region and then later by division 
# to see if we can find any strong correlations amongst the crime rate data. 

# We determined each region off of the information here: https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf

NortheastSCR = subset(SCR, SCR$state == 'CT' | SCR$state == 'ME' | SCR$state == 'MA' | SCR$state == 'NH' | 
                     SCR$state == 'RI' | SCR$state == 'VT' | SCR$state == 'NJ' | SCR$state == 'NY' | SCR$state == 'PA')

MidwestSCR = subset(SCR, SCR$state == 'IN' | SCR$state == 'IL' | SCR$state == 'MI' | SCR$state == 'OH' | 
                     SCR$state == 'WI' | SCR$state == 'IA' | SCR$state == 'KS' | SCR$state == 'MN' | SCR$state == 'MO' | 
                   SCR$state == 'NE' | SCR$state == 'ND' | SCR$state == 'SD')

SouthSCR = subset(SCR, SCR$state == 'DE' | SCR$state == 'DC' | SCR$state == 'FL' | SCR$state == 'GA' | 
                     SCR$state == 'MD' | SCR$state == 'NC' | SCR$state == 'SC' | SCR$state == 'VA' | SCR$state == 'WV' | 
                 SCR$state == 'AL' | SCR$state == 'KY' | SCR$state == 'MS' | SCR$state == 'TN' | SCR$state == 'AR' | SCR$state == 'LA'
               | SCR$state == 'OK' | SCR$state == 'TX')

WestSCR = subset(SCR, SCR$state == 'AZ' | SCR$state == 'CO' | SCR$state == 'ID' | SCR$state == 'NM' | 
                     SCR$state == 'MT' | SCR$state == 'UT' | SCR$state == 'NV' | SCR$state == 'WY' | SCR$state == 'AK' | SCR$state == 'CA'
                 | SCR$state == 'HI' | SCR$state == 'OR' | SCR$state == 'WA')

# Next lets read in the median household income data for each state and for the entire US population. 
# This data was retreived from https://www.census.gov/content/dam/Census/library/publications/2019/acs/acsbr18-01.pdf
# and ranges from 1984-2018.

# State Median Household Income
SMHI = read.csv('State household income copy.csv')

# National Median Household Income
NMHI = read.csv('US Median Household Income copy.csv')

# Lets shrink the NCR variable to have the same years as the NMHI

# National Crime Rate Subset
NCR_sub = subset(NCR, NCR$year > 1982)

# Lets observe a graph showing the distribution of the US Median Household Income then compare
# that graph with the curve from burglary, robbery, larceny, motor vehicle theft, and rape. In
# order to see if there's a visual connection amongst the data. 

# Median Income GG
mi_gg = ggplot(NMHI, aes(year, median.income, group = 1)) + 
  geom_line() + geom_point() + 
  labs(title = "US median household income density curve",
       x = 'Year', y = "Median Household Income")

mi_gg

# Burglary GG
bg_gg = ggplot(NCR_sub, aes(year, burglary, group = 1)) + 
  geom_line() + geom_point() + 
  labs(title = "US burglary density curve",
       x = 'Year', y = "Burglaries")
bg_gg

# Robbery GG
rb_gg = ggplot(NCR_sub, aes(year, robbery, group = 1)) + 
  geom_line() + geom_point() + 
  labs(title = "US robbery density curve",
       x = 'Year', y = "Robberies")

rb_gg

# Larceny GG
ly_gg = ggplot(NCR_sub, aes(year, larceny, group = 1)) + 
  geom_line() + geom_point() + 
  labs(title = "US larceny density curve",
       x = 'Year', y = "Larcenies")

ly_gg

# Motor Vehicle Theft GG
mvt_gg = ggplot(NCR_sub, aes(year, motor_vehicle_theft, group = 1)) + 
  geom_line() + geom_point() + 
  labs(title = "US vehicle theft density curve",
       x = 'Year', y = "Vehicle Thefts")

mvt_gg

# Rape GG
rp_gg = ggplot(NCR_sub, aes(year, rape, group = 1)) +
  geom_line() + geom_point() +
  labs(title = "US rape density curve",
       x = "Year", Y = "Rape") +
  geom_label_repel(aes(label=year), box.padding=0.45,
point.padding=0.6)

# LEts observe this graph alone first before we throw it in with the other 5. 

rp_gg

# Notice the large jump from 2012-2013. This can be explained by the FBI's change in how they defined
# rape. Before 2013 the FBI had the term "forcible" in the definition of Rape, after they removed that term 
# and modified the definition the way the data was recorded needed to change as well. 

# Call the Rmisc library to put all the National Graphs together

library(Rmisc)

multiplot(mi_gg, bg_gg, rb_gg, ly_gg, mvt_gg, rp_gg, cols = 2)

# Next we are going to convert the state median household income into region data frames

NortheastSMHI = subset(SMHI, SMHI$state == 'CT' | SMHI$state == 'ME' | SMHI$state == 'MA' | SMHI$state == 'NH' | 
                         SMHI$state == 'RI' | SMHI$state == 'VT' | SMHI$state == 'NJ' | SMHI$state == 'NY' | SMHI$state == 'PA')

MidwestSMHI = subset(SMHI, SMHI$state == 'IN' | SMHI$state == 'IL' | SMHI$state == 'MI' | SMHI$state == 'OH' | 
                       SMHI$state == 'WI' | SMHI$state == 'IA' | SMHI$state == 'KS' | SMHI$state == 'MN' | SMHI$state == 'MO' | 
                       SMHI$state == 'NE' | SMHI$state == 'ND' | SMHI$state == 'SD')

SouthSMHI = subset(SMHI, SMHI$state == 'DE' | SMHI$state == 'DC' | SMHI$state == 'FL' | SMHI$state == 'GA' | 
                     SMHI$state == 'MD' | SMHI$state == 'NC' | SMHI$state == 'SC' | SMHI$state == 'VA' | SMHI$state == 'WV' | 
                     SMHI$state == 'AL' | SMHI$state == 'KY' | SMHI$state == 'MS' | SMHI$state == 'TN' | SMHI$state == 'AR' | SMHI$state == 'LA'
                   | SMHI$state == 'OK' | SMHI$state == 'TX')

WestSMHI = subset(SMHI, SMHI$state == 'AZ' | SMHI$state == 'CO' | SMHI$state == 'ID' | SMHI$state == 'NM' | 
                    SMHI$state == 'MT' | SMHI$state == 'UT' | SMHI$state == 'NV' | SMHI$state == 'WY' | SMHI$state == 'AK' | SMHI$state == 'CA'
                  | SMHI$state == 'HI' | SMHI$state == 'OR' | SMHI$state == 'WA')

# Before we begin looking at density curves we must subset the data some more to only be looking at 
# between the years of 2005 and 2018 due to the MHI data.

NortheastSCR_sub = subset(NortheastSCR, NortheastSCR$year >1983)

MidwestSCR_sub = subset(MidwestSCR, MidwestSCR$year >1983)

SouthSCR_sub = subset(SouthSCR, SouthSCR$year >1983)

WestSCR_sub = subset(WestSCR, WestSCR$year >1983)

# Now lets look at the correlations between the crimes in each region, we also are going to add
# the median household income and margin of error variables to the data to view these 
# correlations as well. 

# Joint Northeast Data
jointNEdata = merge(NortheastSCR_sub, NortheastSMHI, by = c('state', 'year'), all =TRUE)

skewness(jointNEdata[4:11])

kurtosis(jointNEdata[4:11])

# We can see from the above measurements that the distributions are normal for median household income. 

corNE = cor(jointNEdata[4:11])

ggcorrplot(corNE, type='lower', title='Northeast Region Correlation Heatmap', hc.order = TRUE)

corrplot(corNE,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Northeast Region Correlation Plot')

# In these graphs we see that in the Northeast, MHI actually has a negative correlation
# with burglary, larceny and homicide. MHI has a low correlation with everything else as well 

# Now lets explore some of the different correlations some more to get a feel for how they look. 

ggplot(jointNEdata, aes(larceny, burglary, group = state,
                        color = state)) + geom_line() + geom_point() + 
  labs(title = "Northeast larceny vs burglary between 84' - 18'", 
       x = 'Larceny', y = 'Burglary') 

ggplot(jointNEdata, aes(homicide, robbery, group = state,
                        color = state)) + geom_line() + geom_point() + 
  labs(title = "Northeast homicide vs robbery betwen 84' - 18'", 
       x = 'Homicider', y = 'Robbery')

ggplot(jointNEdata, aes(robbery, rape, group = state,
                        color = state)) + geom_line() + geom_point() + 
  labs(title = "Northeast robbery vs rape between 84' - 18'", 
       x = 'Robbery', y = 'Rape')


# Lets further investigate the midwest region 

jointMWdata = merge(MidwestSCR_sub, MidwestSMHI, by = c('state', 'year'), all =TRUE)

skewness(jointMWdata[4:11])

kurtosis(jointMWdata[4:11])

# We can see from the above measurements that trobbery and vehicle_theft are extremely non
# normal. And Rape, burglary, aggravated assault, & median income are non normal as well. 

corMW = cor(jointMWdata[4:11])

ggcorrplot(corMW, type='lower', title='Midwest Region Correlation Heatmap', hc.order = TRUE)

corrplot(corMW,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='Midwest Region Correlation Plot')

# In the midwest we have found that the median household income has negative relationships with
# burglary and larceny but it has slightly positive relationsships with every other variable. 

ggplot(jointMWdata, aes(homicide, robbery, group = state,
                        color = state)) + geom_line() + geom_point() + 
  labs(title = "Midwest homicide vs robbery between 84' - 18'", 
       x = 'Homicide', y = 'Robbery') 

ggplot(jointMWdata, aes(robbery, vehicle_theft, group = state,
                        color = state)) + geom_line() + geom_point() + 
  labs(title = "Midwest robbery vs vehicle theft betwen 84' - 18'", 
       x = 'Robbery', y = 'Vehicle Theft')

ggplot(jointMWdata, aes(median.income, rape, group = state,
                        color = state)) + geom_line() + geom_point() + 
  labs(title = "Midwest median household income vs rape between 84' - 18'", 
       x = 'Median household income', y = 'Rape')

# Now lets look at the South

jointSodata = merge(SouthSCR_sub, SouthSMHI, by = c('state', 'year'), all =TRUE)

skewness(jointSodata[4:11])

kurtosis(jointSodata[4:11])

# Here we observe some non-normal data for the MHI variable. We also notice that
# homicide, robbery, and vehicle theft are extrememly non-normal.

corSo = cor(jointSodata[4:11])

ggcorrplot(corSo, type='lower', title='South Region Correlation Heatmap', hc.order = TRUE)

corrplot(corSo,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='South Region Correlation Plot')

# We observe that MHI has an extremely negative correlation with burglary and actually a 
# 21% correlation with robbery. Robbery and homicide are heavily correlated. 
# Lets visualize some of these correlations we just observed.

ggplot(jointSodata, aes(robbery, homicide, group = state,
                        color = state, label = year)) + geom_line() + geom_point() + 
  labs(title = "South robbery vs homicide between 84' - 18'", 
       x = 'Robbery', y = 'Homicide') 

ggplot(jointSodata, aes(median.income, robbery, group = state,
                        color = state)) + geom_line() + geom_point() + 
  labs(title = "South median household income vs robbery betwen 84' - 18'", 
       x = 'Median Household Income', y = 'Robbery')

ggplot(jointSodata, aes(rape, larceny, group = state,
                        color = state)) + geom_line() + geom_point() + 
  labs(title = "South rape vs larceny between 84' - 18'", 
       x = 'Rape', y = 'Larceny')

# Lets move on to the West 

jointWdata = merge(WestSCR_sub, WestSMHI, by = c('state', 'year'), all =TRUE)

skewness(jointWdata[4:11])

kurtosis(jointWdata[4:11])

# We can see from the above values that rape, robbery, and vehicle theft are all leptokurtic. 
# Every other value is normally distributed

corW = cor(jointWdata[4:11])

ggcorrplot(corW, type='lower', title='West Region Correlation Heatmap', hc.order = TRUE)

corrplot(corW,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='West Region Correlation Plot')

# Here we observe that MHI has four negative correlations and a 22% correlation with rape. Some
# other strong correlations are between homicide and aggravated assault, homicide and robbery, and
# vehicle_theft and robbery. 

ggplot(jointWdata, aes(homicide, aggravated_assault, group = state,
                       color = state, label = year)) + geom_line() + geom_point() + 
  labs(title = "West homicide vs aggravated assault between 84' - 18'", 
       x = 'Homicide', y = 'Aggravated Assault ') 

ggplot(jointWdata, aes(robbery, vehicle_theft, group = state,
                       color = state)) + geom_line() + geom_point() + 
  labs(title = "West robbery vs vehicle theft betwen 84' - 18'", 
       x = 'Robbery', y = 'vehicle theft')

ggplot(jointWdata, aes(rape, median.income, group = state,
                       color = state)) + geom_line() + geom_point() + 
  labs(title = "West rape vs median household income between 84' - 18'", 
       x = 'Rape', y = 'Median Household Income')


# What we are doing below is created a graph for each region that displays the distribution 
# of the median household income between the years 1984 and 2018. We then show them 
# all together at the same time. 

Neast_gg = ggplot(NortheastSMHI, aes(year, median.income, group = state, 
                                     color = state)) +
  geom_line() + geom_point() +
  labs(title = "Northeast Median Household Income", 
       x = 'Year', y = 'Median Household Income')

Mwest_gg = ggplot(MidwestSMHI, aes(year, median.income, group = state, 
                                   color = state)) +
  geom_line() + geom_point() +
  labs(title = "Midwest Median Household Income", 
       x = 'Year', y = 'Median Household Income')

South_gg = ggplot(SouthSMHI, aes(year, median.income, group = state, 
                                 color = state)) +
  geom_line() + geom_point() +
  labs(title = "South Median Household Income", 
       x = 'Year', y = 'Median Household Income')

West_gg = ggplot(WestSMHI, aes(year, median.income, group = state, 
                               color = state)) +
  geom_line() + geom_point() +
  labs(title = "West Median Household Income", 
       x = 'Year', y = 'Median Household Income')

# Use the Rmisc library so we can use the multiplot function

multiplot(Neast_gg, Mwest_gg, South_gg, West_gg, cols=2)

## Now we are going to begin clustering the regional data. 

# begin with calling the amap, cluster, and factoextra libraries.

library(amap)
library(factoextra)
library(cluster)

# begin with setting the random number generator seed

set.seed(30)

# We need to create a data frame for each region containing only numbers

# Northeast
NE_num = jointNEdata[4:11]

# Midwest
MW_num = jointMWdata[4:11]

# South
S_num = jointSodata[4:11]

# West
W_num = jointWdata[4:11]

# Next we will use the WSS, silhouette, and gap methods to determine the optimal number of 
# clusters for each different regions joint data
set.seed(30)

fviz_nbclust(NE_num, kmeans, method = 'wss')
fviz_nbclust(NE_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(NE_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# For the Northeast, the optimal number of clusters looks to be two according to all graphs. 
set.seed(30)

fviz_nbclust(MW_num, kmeans, method = 'wss')
fviz_nbclust(MW_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(MW_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# For the Midwest, the optimal number of clusters looks to be either three or one. 
set.seed(30)

fviz_nbclust(S_num, kmeans, method = 'wss')
fviz_nbclust(S_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(S_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# For the South, the optimal number of clusters looks to be either around two clusters or five clusters according to 
# the gap stat graph. We'll investigate that range. 
set.seed(30)

fviz_nbclust(W_num, kmeans, method = 'wss')
fviz_nbclust(W_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(W_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# For the West, the optimal number of clusters looks to be two according to wss and silhouette but when
# looking at the Gap we see it choose a cluster of one. 

# Now we are going to begin creating the clusters for the NE region. We will use both the euclidian method and 
# the manhattan method as measures of distance. We will look at up to 4 clusters just to prove that having 2 makes the
# most sense. According to our optimal cluster tests, the optimal number of clusters is two all the way areound. 
set.seed(30)

NECluster2 = kmeans(NE_num, 2, nstart=20)
print(NECluster2)
NECluster2 = kmeans(NE_num, 2, nstart=20)
print(NECluster2)
NECluster4 = kmeans(NE_num, 4, nstart=20)
print(NECluster4)

set.seed(30)

NECluster2_m = Kmeans(NE_num, 2, nstart=20, method='manhattan')
print(NECluster2_m)
NECluster2_m = Kmeans(NE_num, 2, nstart=20, method='manhattan')
print(NECluster2_m)
NECluster4_m = Kmeans(NE_num, 4, nstart=20, method='manhattan')
print(NECluster4_m)

# After reviewing each of the cluster assignments, we found that the tests were correct in clustering the data into 
# two main clusters for the NE region between 05' and 18'.

# Now we are going to begin creating the clusters for the MW region. We are going to observe between 2 and 4 clusters because
# that is the range determined by our three optimal cluster tests. 
set.seed(30)

MWCluster2 = kmeans(MW_num, 2, nstart=20)
print(MWCluster2)
MWCluster2 = kmeans(MW_num, 2, nstart=20)
print(MWCluster2)
MWCluster4 = kmeans(MW_num, 4, nstart=20)
print(MWCluster4)

set.seed(30)

MWCluster2_m = Kmeans(MW_num, 2, nstart=20, method='manhattan')
print(MWCluster2_m)
MWCluster2_m = Kmeans(MW_num, 2, nstart=20, method='manhattan')
print(MWCluster2_m)
MWCluster4_m = Kmeans(MW_num, 4, nstart=20, method='manhattan')
print(MWCluster4_m)

# After reviewing each of the cluster assignments, we found that two different clusters provides the best
# distribution of the data.

# Next we will run the Kmeans function on the South Region. 
set.seed(30)

SCluster2 = kmeans(S_num, 2, nstart=20)
print(SCluster2)
SCluster2 = kmeans(S_num, 2, nstart=20)
print(SCluster2)
SCluster4 = kmeans(S_num, 4, nstart=20)
print(SCluster4)

set.seed(30)

SCluster2_m = Kmeans(S_num, 2, nstart=20, method='manhattan')
print(SCluster2_m)
SCluster2_m = Kmeans(S_num, 2, nstart=20, method='manhattan')
print(SCluster2_m)
SCluster4_m = Kmeans(S_num, 4, nstart=20, method='manhattan')
print(SCluster4_m)

# After reviewing each of the cluster assignments, we found that four different clusters provides the best
# distribution of the data. It also boasts a 92.7% separation of clusters. 

# Next we will look at the West region now, pay attention becase this range is much different than the range from previous
# tests. Here we check for 2, 7, and 10 clusters in the data because of the optimal cluster tests we performed earlier. 
set.seed(30)

WCluster2 = kmeans(W_num, 2, nstart=20)
print(Wcluster2)
WCluster2 = kmeans(W_num, 2, nstart=20)
print(WCluster2)
WCluster4 = kmeans(W_num, 4, nstart=20)
print(WCluster4)

set.seed(30)

WCluster2_m = Kmeans(W_num, 2, nstart=20, method='manhattan')
print(WCluster2_m)
WCluster2_m = Kmeans(W_num, 2, nstart=20, method='manhattan')
print(WCluster2_m)
WCluster4_m = Kmeans(W_num, 4, nstart=20, method='manhattan')
print(WCluster4_m)

# After reviewing each of the cluster assignments, we found that four different clusters provides the best
# distribution of the data. Three of the clusters are around a size of 40 and the separation is at 
# 92.9%. 

# Now we will create visualizations of the optimal clusters for each of the four US regions. 
set.seed(30)

NEC2 = fviz_cluster(NECluster2, data=NE_num) + ggtitle('2 clusters')
NEC2 = fviz_cluster(NECluster2, data=NE_num) + ggtitle('2 clusters')
NEC4 = fviz_cluster(NECluster4, data=NE_num) + ggtitle('4 clusters')

set.seed(30)

NEC2_m = fviz_cluster(NECluster2_m, data=NE_num) + ggtitle('2 clusters')
NEC2_m = fviz_cluster(NECluster2_m, data=NE_num) + ggtitle('2 clusters')
NEC4_m = fviz_cluster(NECluster4_m, data=NE_num) + ggtitle('4 clusters')

# we call the gridExtra library so that way we can arrange the cluster visualizations accordingly

library(gridExtra)

grid.arrange(NEC2, NEC2, NEC4, nrow=2)

grid.arrange(NEC2_m, NEC2_m, NEC4_m, nrow=2)

# We can clearly see that things begin becoming too convoluded anywhere after two clusters. So as proven before
# we are going to make the claim that there are two different clusters of communities/areas in the Northeast region
# of the US.

# We are now going to check out the Midwest
set.seed(30)

MWC2 = fviz_cluster(MWCluster2, data=MW_num) + ggtitle('2 clusters')
MWC2 = fviz_cluster(MWCluster2, data=MW_num) + ggtitle('2 clusters')
MWC4 = fviz_cluster(MWCluster4, data=MW_num) + ggtitle('4 clusters')

grid.arrange(MWC2, MWC2, MWC4, nrow=2)

set.seed(30)

MWC2_m = fviz_cluster(MWCluster2_m, data=MW_num) + ggtitle('2 clusters')
MWC2_m = fviz_cluster(MWCluster2_m, data=MW_num) + ggtitle('2 clusters')
MWC4_m = fviz_cluster(MWCluster4_m, data=MW_num) + ggtitle('4 clusters')

grid.arrange(MWC2_m, MWC2_m, MWC4_m, nrow=2)

# As we expected from the wss and silhouette observations, the optimal number of clusters for the Midwest is
# two. We see the least amount of overlap in this visualization as well. 

# We are moving towards the South now
set.seed(30)

SC2 = fviz_cluster(SCluster2, data=S_num) + ggtitle('2 clusters')
SC2 = fviz_cluster(SCluster2, data=S_num) + ggtitle('2 clusters')
SC4 = fviz_cluster(SCluster4, data=S_num) + ggtitle('4 clusters')

grid.arrange(SC2, SC2, SC4, nrow=2)

set.seed(30)

SC2_m = fviz_cluster(SCluster2_m, data=S_num) + ggtitle('2 clusters')
SC2_m = fviz_cluster(SCluster2_m, data=S_num) + ggtitle('2 clusters')
SC4_m = fviz_cluster(SCluster4_m, data=S_num) + ggtitle('4 clusters')

grid.arrange(SC2_m, SC2_m, SC4_m, nrow=2)

# As expected from our earlier observations, the optimal number of clusters for the South is two. This graph
# has the least overlap. 

# Finally we look at the West
set.seed(30)

WC2 = fviz_cluster(WCluster2, data=W_num) + ggtitle('2 clusters')
WC2 = fviz_cluster(WCluster2, data=W_num) + ggtitle('2 clusters')
WC4 = fviz_cluster(WCluster4, data=W_num) + ggtitle('4 clusters')

grid.arrange(WC2, WC2, WC4, nrow=2)

set.seed(30)

WC2_m = fviz_cluster(WCluster2_m, data=W_num) + ggtitle('2 clusters')
WC2_m = fviz_cluster(WCluster2_m, data=W_num) + ggtitle('2 clusters')
WC4_m = fviz_cluster(WCluster4_m, data=W_num) + ggtitle('4 clusters')

grid.arrange(WC2_m, WC2_m, WC4_m, nrow=2)

# Next steps. potentially break the data down into divisions because regions seem to be too large
# to make any accurate observations.  Maybe make DC into its own group because the robberies stand out so much.

# We have just proved that you are unable to accurately analyze crime by region so therefore, the FBI should not have any
# regional headquarters. Now we are going to dive in and see if it would be wise to have an FBI headquarters in each division
# in the US. 

##### Section 3: Division Correlations and clustering by crime rates ####

# Now we are going to subset the data by division instead of region to attempt and properly analyze
# crime & median household income this way

NwEngSMHI = subset(SMHI, SMHI$state == 'CT' | SMHI$state == 'ME' | SMHI$state == 'MA' | 
                      SMHI$state == 'NH' | SMHI$state == 'RI' | SMHI$state == 'VT')

MidAtlSMHI = subset(SMHI, SMHI$state == 'NJ' | SMHI$state == 'NY' | SMHI$state == 'PA')

EastNCSMHI = subset(SMHI, SMHI$state == 'IN' | SMHI$state == 'IL' | SMHI$state == 'MI'
                | SMHI$state == 'OH' | SMHI$state == 'WI')

WestNCSMHI = subset(SMHI, SMHI$state == 'IA' | SMHI$state == 'KS' | SMHI$state == 'NE' | 
                      SMHI$state == 'ND' | SMHI$state == 'MN' | SMHI$state == 'MO' | 
                  SMHI$state == 'SD')

SouthAtlSMHI = subset(SMHI, SMHI$state == 'DE' | SMHI$state == 'DC' | SMHI$state == 'FL' | 
                  SMHI$state == 'GA' | SMHI$state == 'MD' | SMHI$state == 'NC' | 
                  SMHI$state == 'SC' | SMHI$state == 'VA' | SMHI$state == 'WV')

EastSCSMHI = subset(SMHI, SMHI$state == 'AL' | SMHI$state == 'KY' | SMHI$state == 'MS' | SMHI$state == 'TN')

WestSCSMHI = subset(SMHI, SMHI$state == 'AR' | SMHI$state == 'LA' | SMHI$state == 'OK' | SMHI$state == 'TX')

MtnSMHI = subset(SMHI, SMHI$state == 'AZ' | SMHI$state == 'MT' | SMHI$state == 'CO' | 
                    SMHI$state == 'UT' | SMHI$state == 'ID' | SMHI$state == 'NV' | 
                    SMHI$state == 'NM' | SMHI$state == 'WY')

PcfSMHI = subset(SMHI, SMHI$state == 'AL' | SMHI$state == 'CA' | SMHI$state == 'HI'
                | SMHI$state == 'OR' | SMHI$state == 'WA')

# Next we are going to create subsets for each division's Crime Rates

NwEngSCR = subset(SCR, SCR$state == 'CT' | SCR$state == 'ME' | SCR$state == 'MA' | SCR$state == 'NH'
                  | SCR$state == 'RI' | SCR$state == 'VT')

MidAtlSCR = subset(SCR, SCR$state == 'NJ' | SCR$state == 'NY' | SCR$state == 'PA')

EastNCSCR = subset(SCR, SCR$state == 'IN' | SCR$state == 'IL' | SCR$state == 'MI' | SCR$state == 'OH'
                   | SCR$state == 'WI')

WestNCSCR = subset(SCR, SCR$state == 'IA' | SCR$state == 'NE' | SCR$state == 'KS' | SCR$state == 'ND'
                   | SCR$state == 'MN' | SCR$state == 'SD' | SCR$state == 'MO')

SouthAtlSCR = subset(SCR, SCR$state == 'DE' | SCR$state == 'DC' | SCR$state == 'FL' | SCR$state == 'GA'
                     | SCR$state == 'MD' | SCR$state == 'NC' | SCR$state == 'SC' | SCR$state == 'VA' 
                     | SCR$state == 'WV')

EastSCSCR = subset(SCR, SCR$state == 'AL' | SCR$state == 'KY' | SCR$state == 'MS' | SCR$state == 'TN')

WestSCSCR = subset(SCR, SCR$state == 'AR' | SCR$state == 'LA' | SCR$state == 'OK' | SCR$state == 'TX')

MtnSCR = subset(SCR, SCR$state == 'AZ' | SCR$state == 'MT' | SCR$state == 'CO' | SCR$state == 'UT'
                     | SCR$state == 'ID' | SCR$state == 'NV' | SCR$state == 'NM' | SCR$state == 'WY')

PcfSCR = subset(SCR, SCR$state == 'AL' | SCR$state == 'CA' | SCR$state == 'HI' | SCR$state == 'OR'
                   | SCR$state == 'WA')

# Now we are going to limit each division's Crime Rate data to data after 1982.

NwEngSCR_sub = subset(NwEngSCR, NwEngSCR$year > 1983)

MidAtlSCR_sub = subset(MidAtlSCR, MidAtlSCR$year > 1983)

EastNCSCR_sub = subset(EastNCSCR, EastNCSCR$year > 1983)

WestNCSCR_sub = subset(WestNCSCR, WestNCSCR$year > 1983)

NwEngSCR_sub = subset(NwEngSCR, NwEngSCR$year > 1983)

SouthAtlSCR_sub = subset(SouthAtlSCR, SouthAtlSCR$year > 1983)

EastSCSCR_sub = subset(EastSCSCR, EastSCSCR$year > 1983)

WestSCSCR_sub = subset(WestSCSCR, WestSCSCR$year > 1983)

MtnSCR_sub = subset(MtnSCR, MtnSCR$year > 1983)

PcfSCR_sub = subset(PcfSCR, PcfSCR$year > 1983)

# Now we are going to join the two data sets of ...SCR_sub and SMHI together at the year
# and state columns

jointNwEngdata = merge(NwEngSCR_sub, NwEngSMHI, by = c('state', 'year'), all =TRUE)

jointMidAtldata = merge(MidAtlSCR_sub, MidAtlSMHI, by = c('state', 'year'), all =TRUE)

jointEastNCdata = merge(EastNCSCR_sub, EastNCSMHI, by = c('state', 'year'), all =TRUE)

jointWestNCdata = merge(WestNCSCR_sub, WestNCSMHI, by = c('state', 'year'), all =TRUE)

jointSouthAtldata = merge(SouthAtlSCR_sub, SouthAtlSMHI, by = c('state', 'year'), all =TRUE)

jointEastSCdata = merge(EastSCSCR_sub, EastSCSMHI, by = c('state', 'year'), all =TRUE)

jointWestSCdata = merge(WestSCSCR_sub, WestSCSMHI, by = c('state', 'year'), all =TRUE)

jointMtndata = merge(MtnSCR_sub, MtnSMHI, by = c('state', 'year'), all =TRUE)

jointPcfdata = merge(PcfSCR_sub, PcfSMHI, by = c('state', 'year'), all =TRUE)

# Now that we have the joint data frames needed to complete the correlations and clustering 
# Lets begin by looking at heatmaps of each divisions correlations and test if some of the strong 
# correlations are siginificat via a T-test of correlation significance. 

skewness(jointNwEngdata[4:11])

kurtosis(jointNwEngdata[4:11])

# We can see from the above measurements that the distributions are normal for median household income. 

corNwEng = cor(jointNwEngdata[4:11])

ggcorrplot(corNwEng, type='lower', title='New England Division Correlation Heatmap', hc.order = TRUE)

corrplot(corNwEng,method='number', order = "AOE", type = 'lower',
         diag = FALSE, tl.srt=0, tl.cex=.8, tl.col = 'black', main='New England Division Correlation Plot')

# Lets preform a two sided t-test of correlation significance on some of the variables to see if the 
# values are actually significant. 

t.test(jointNwEngdata$burglary, jointNwEngdata$larceny, alternative = 'two.sided')

# We observe that we fail to reject the alternate hypothesis after viewing the p-value to be less than 
# 0.05. There is sufficient evidence to conclude that there is a significant linear relationship between burglary
# and larceny in the New England district. 

t.test(jointNwEngdata$median.income, jointNwEngdata$robbery, alternative = 'two.sided')

# We fail to reject the alternate hypothesis. There is sufficient evidence to conclude that there is a 
# significant linear relationshp  betwwen MHI and robbery. 

t.test(jointNwEngdata$median.income, jointNwEngdata$larceny, alternative = 'two.sided')

# We fail to reject the alternate hypothesis. There is sufficient evidence to suggest that there is 
# a significant linear relationship between MHI and Larceny. 


# Since we are well aware of how to analyze correlations and heatmaps, we are going to jump in to 
# some unsupervised learning by preforming the Kmeans clustering on each division to see if we can 
# cluster them based on three topics: only crime, only median household income, and both crime and MHI. 

# Set the seed so the randomize functions produce the same results every iteration

set.seed(30)

# create data frames consisting of only numbers for each division

NwEng_num = subset(jointNwEngdata[4:10])

MidAtl_num = subset(jointMidAtldata[4:10])

EastNC_num = subset(jointEastNCdata[4:10])

WestNC_num = subset(jointWestNCdata[4:10])

SouthAtl_num = subset(jointSouthAtldata[4:10])

EastSC_num = subset(jointEastSCdata[4:10])

WestSC_num = subset(jointWestSCdata[4:10])

Mtn_num = subset(jointMtndata[4:10])

Pcf_num = subset(jointPcfdata[4:10])

# We will begin by finding the optimal number of clusters for each division by only crime using the wss, silhouette, and gap methods.


#### New England 
set.seed(30)

fviz_nbclust(NwEng_num, kmeans, method = 'wss')
fviz_nbclust(NwEng_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(NwEng_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# It seems like the optimal number of clusters according to the gap method is four and according to the other two methods 
# is two. 
set.seed(30)

NwEngCluster2 = kmeans(NwEng_num, 2, nstart=20)
print(NwEngCluster2)
NwEngCluster4 = kmeans(NwEng_num, 4, nstart=20)
print(NwEngCluster4)

# Notice that the cluster of four seems to have a relatively even distribution in each 
# cluster besides the first one. That one has very high rates of vehicle theft, burglary,
# aggravated assault, and robbery. Lets observe these on a graph.
set.seed(30)

NwEngC2 = fviz_cluster(NwEngCluster2, data=NwEng_num) + ggtitle('2 clusters')
NwEngC4 = fviz_cluster(NwEngCluster4, data=NwEng_num) + ggtitle('4 clusters')

grid.arrange(NwEngC2, NwEngC4, nrow = 2)


#### Mid Atlantic
set.seed(30)

fviz_nbclust(MidAtl_num, kmeans, method = 'wss')
fviz_nbclust(MidAtl_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(MidAtl_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# The first two graphs seem to belive that two clusters is optimal and the gap
# method believes that three clusters is optimal. Lets take a closer look...
set.seed(30)

MidAtlCluster2 = kmeans(MidAtl_num, 2, nstart=20)
print(MidAtlCluster2)
MidAtlCluster3 = kmeans(MidAtl_num, 3, nstart=20)
print(MidAtlCluster3)

MidAtlC2 = fviz_cluster(MidAtlCluster2, data=MidAtl_num) + ggtitle('2 clusters')
MidAtlC3 = fviz_cluster(MidAtlCluster3, data=MidAtl_num) + ggtitle('3 clusters')

grid.arrange(MidAtlC2, MidAtlC3, nrow = 1)


# East North Central
set.seed(30)

fviz_nbclust(EastNC_num, kmeans, method = 'wss')
fviz_nbclust(EastNC_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(EastNC_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# The methods seem to mainy agree that two is the optimal number of clusters. 
# However on the wss graph it also looks like the line elbows off at three clusters
# so lets look at  that as well. 

EastNCCluster2 = kmeans(EastNC_num, 2, nstart=20)
print(EastNCCluster2)
EastNCCluster3 = kmeans(EastNC_num, 3, nstart=20)
print(EastNCCluster3)

EastNCC2 = fviz_cluster(EastNCCluster2, data=EastNC_num) + ggtitle('2 clusters')
EastNCC3 = fviz_cluster(EastNCCluster3, data=EastNC_num) + ggtitle('3 clusters')

grid.arrange(EastNCC2, EastNCC3, nrow = 2)


### West North Central
set.seed(30)

fviz_nbclust(WestNC_num, kmeans, method = 'wss')
fviz_nbclust(WestNC_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(WestNC_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Here we see the first two methods agree that the optimal number of clusters
# is two and we see the gap method choose three clusters. Lets look at both of these. 

WestNCCluster2 = kmeans(WestNC_num, 2, nstart=20)
print(WestNCCluster2)
WestNCCluster3 = kmeans(WestNC_num, 3, nstart=20)
print(WestNCCluster3)

WestNCC2 = fviz_cluster(WestNCCluster2, data=WestNC_num) + ggtitle('2 clusters')
WestNCC3 = fviz_cluster(WestNCCluster3, data=WestNC_num) + ggtitle('3 clusters')

grid.arrange(WestNCC2, WestNCC3, nrow = 2)


### South Atlantic
set.seed(30)

fviz_nbclust(SouthAtl_num, kmeans, method = 'wss')
fviz_nbclust(SouthAtl_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(SouthAtl_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Here we see both of the first mthods agree that the optimal number of clusters
# should be three and we see the gap method choose one as the optimal number. 
# We are going to investigate the cluster of three to see if this choice is correct. 

SouthAtlCluster2 = kmeans(SouthAtl_num, 2, nstart=20)
print(SouthAtlCluster2)

SouthAtlC2 = fviz_cluster(SouthAtlCluster2, data=SouthAtl_num) + ggtitle('2 clusters')

SouthAtlC2


### East South Central
set.seed(30)

fviz_nbclust(EastSC_num, kmeans, method = 'wss')
fviz_nbclust(EastSC_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(EastSC_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Well this is odd, both of the first two tests indicate that two clusters is optimal 
# and the gap method indicates that eleven clusters is optimal. Lets investigate the choice of two because
# we do observe a high jump in the gap method from 1-2

EastSCCluster2 = kmeans(EastSC_num, 2, nstart=20)
print(EastSCCluster2)

EastSCC2 = fviz_cluster(EastSCCluster2, data=EastSC_num) + ggtitle('2 clusters')

EastSCC2


### West South Central
set.seed(30) 

fviz_nbclust(WestSC_num, kmeans, method = 'wss')
fviz_nbclust(WestSC_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(WestSC_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# The first two methods agree that the optimal number of clusters is two and
# the gap method believes that the optimal number of clusters is one. We are going 
# to investigate two clusters further. 

WestSCCluster2 = kmeans(WestSC_num, 2, nstart=20)
print(WestSCCluster2)

WestSCC2 = fviz_cluster(WestSCCluster2, data=WestSC_num) + ggtitle('2 clusters')

WestSCC2


### Mountain 
set.seed(30)

fviz_nbclust(Mtn_num, kmeans, method = 'wss')
fviz_nbclust(Mtn_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(Mtn_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# The first two tests conclude that two clusters is the optimal amount and 
# the gap method believes that one cluster is the optimal amount. Lets investigate
# two and three clusters to see which one better segments the data. 

MtnCluster2 = kmeans(Mtn_num, 2, nstart=20)
print(MtnCluster2)
MtnCluster3 = kmeans(Mtn_num, 3, nstart=20)
print(MtnCluster3)

MtnC2 = fviz_cluster(MtnCluster2, data=Mtn_num) + ggtitle('2 clusters')
MtnC3 = fviz_cluster(MtnCluster3, data=Mtn_num) + ggtitle('3 clusters')

grid.arrange(MtnC2, MtnC3, nrow=2)


### Pacific
set.seed(30)

fviz_nbclust(Pcf_num, kmeans, method = 'wss')
fviz_nbclust(Pcf_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(Pcf_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Here we see all three methods agree that two clusters is the optimal amount needed

PcfCluster2 = kmeans(Pcf_num, 2, nstart=20)
print(PcfCluster2)

PcfC2 = fviz_cluster(PcfCluster2, data=Pcf_num) + ggtitle('2 clusters')

PcfC2



##### Section 4: Division Clustering by crime and MHI #####

# We are now going to investigate and cluster each division based on Median Household Income and crime to 
# see if we can properly cluster & analyze the data in this format. 

# create data frames consisting of both  median household income and crime numbers for each division

NwEng_num = subset(jointNwEngdata[4:11])

MidAtl_num = subset(jointMidAtldata[4:11])

EastNC_num = subset(jointEastNCdata[4:11])

WestNC_num = subset(jointWestNCdata[4:11])

SouthAtl_num = subset(jointSouthAtldata[4:11])

EastSC_num = subset(jointEastSCdata[4:11])

WestSC_num = subset(jointWestSCdata[4:11])

Mtn_num = subset(jointMtndata[4:11])

Pcf_num = subset(jointPcfdata[4:11])


### New England
set.seed(30)

fviz_nbclust(NwEng_num, kmeans, method = 'wss')
fviz_nbclust(NwEng_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(NwEng_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Looks like the first two methods agree that there should be two clusters
# and the gap method believes there should be one cluster.

NwEngCluster2 = kmeans(NwEng_num, 2, nstart=20)
print(NwEngCluster2)
NwEngCluster3 = kmeans(NwEng_num, 3, nstart=20)
print(NwEngCluster3)

NwEngC2 = fviz_cluster(NwEngCluster2, data=NwEng_num) + ggtitle('2 clusters')
NwEngC3 = fviz_cluster(NwEngCluster3, data=NwEng_num) + ggtitle('3 clusters')

grid.arrange(NwEngC2, NwEngC3, nrow = 1)


### Mid Atlantic
set.seed(30)

fviz_nbclust(MidAtl_num, kmeans, method = 'wss')
fviz_nbclust(MidAtl_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(MidAtl_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# As we can observe from the graphs, all three methods agree that the optimal number
# of clusters is two so we are going to investigate that further. 

MidAtlCluster2 = kmeans(MidAtl_num, 2, nstart=20)
print(MidAtlCluster2)

MidAtlC2 = fviz_cluster(MidAtlCluster2, data=MidAtl_num) + ggtitle('2 clusters')

MidAtlC2


### East North Central 
set.seed(30)

fviz_nbclust(EastNC_num, kmeans, method = 'wss')
fviz_nbclust(EastNC_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(EastNC_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Each of the graphs has a different choice for the East North Central division. The gap 
# method thinks that only one cluster will work and the silhouette method thinks that only two 
# clusters will work and the wss method looks to be either two or three. Lets investigate
# both two and three. 

EastNCCluster2 = kmeans(EastNC_num, 2, nstart=20)
print(EastNCCluster2)
EastNCCluster3 = kmeans(EastNC_num, 3, nstart=20)
print(EastNCCluster3)

EastNCC2 = fviz_cluster(EastNCCluster2, data=EastNC_num) + ggtitle('2 clusters')
EastNCC3 = fviz_cluster(EastNCCluster3, data=EastNC_num) + ggtitle('3 clusters')

grid.arrange(EastNCC2, EastNCC3, nrow = 1)


### West North Central
set.seed(30)

fviz_nbclust(WestNC_num, kmeans, method = 'wss')
fviz_nbclust(WestNC_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(WestNC_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# The first two methods agree that three clusters is the optimal amount and
# the gap method believes that one cluster is the optimal value. Lets investigate
# three clusters and see what we can find. 

WestNCCluster2 = kmeans(WestNC_num, 2, nstart=20)
print(WestNCCluster2)

WestNCC2 = fviz_cluster(WestNCCluster2, data=WestNC_num) + ggtitle('2 clusters')

WestNCC2


###South Atlantic
set.seed(30)

fviz_nbclust(SouthAtl_num, kmeans, method = 'wss')
fviz_nbclust(SouthAtl_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(SouthAtl_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Here we see the first two methods agree that two clusters is the optimal number 
# and the gap method thinks that one cluster is the optimal number. Lets investigate
# two clusters to see if this is accurate. 

SouthAtlCluster2 = kmeans(SouthAtl_num, 2, nstart=20)
print(SouthAtlCluster2)

SouthAtlC2 = fviz_cluster(SouthAtlCluster2, data=SouthAtl_num) + ggtitle('2 clusters')

SouthAtlC2


### East South Central 
set.seed(30)

fviz_nbclust(EastSC_num, kmeans, method = 'wss')
fviz_nbclust(EastSC_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(EastSC_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# The first two methods believe that two is the optimal number of clusters and the third method
# believes that one is the optimal cluster. Lets investigate two clusters to see what we find.

EastSCCluster2 = kmeans(EastSC_num, 2, nstart=20)
print(EastSCCluster2)

EastSCC2 = fviz_cluster(EastSCCluster2, data=EastSC_num) + ggtitle('2 clusters')

EastSCC2


### West South Central 
set.seed(30)

fviz_nbclust(WestSC_num, kmeans, method = 'wss')
fviz_nbclust(WestSC_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(WestSC_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Here we observe that the first two graphs think that two clusters are optimal and the gap
# method believes that one cluster is optimal. 

WestSCCluster2 = kmeans(WestSC_num, 2, nstart=20)
print(WestSCCluster2)

WestSCC2 = fviz_cluster(WestSCCluster2, data=WestSC_num) + ggtitle('2 clusters')

WestSCC2


### Mountain
set.seed(30)

fviz_nbclust(Mtn_num, kmeans, method = 'wss')
fviz_nbclust(Mtn_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(Mtn_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Here we see the first two agree that three is the optimal number of clusters and the gap method
# show that the optimal number of clusters is one. Lets investigate three. 

MtnCluster2 = kmeans(Mtn_num, 2, nstart=20)
print(MtnCluster2)

MtnC2 = fviz_cluster(MtnCluster2, data=Mtn_num) + ggtitle('2 clusters')

MtnC2


### Pacific
set.seed(30)

fviz_nbclust(Pcf_num, kmeans, method = 'wss')
fviz_nbclust(Pcf_num, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(Pcf_num, FUN=kmeans, nstart = 20, B=50, K.max=12))

# Each method disagrees. The wss looks like theres three optimal clusters. The silhouette says the 
# optimal number of clusters is two. The gap method says that the optimal numebr is one. Lets
# look at two and three. 

PcfCluster2 = kmeans(Pcf_num, 2, nstart=20)
print(PcfCluster2)
PcfCluster3 = kmeans(Pcf_num, 3, nstart=20)
print(PcfCluster3)

PcfC2 = fviz_cluster(PcfCluster2, data=Pcf_num) + ggtitle('2 clusters')
PcfC3 = fviz_cluster(PcfCluster3, data=Pcf_num) + ggtitle('3 clusters')

grid.arrange(PcfC2, PcfC3, nrow=1)


# In this section we have discovered that analyzing divison by only crime and by crime and MHI are 
# terrible metrics to use to cluster data. We therefore conclude that the FBI should not analyze crime data
# based on division. 

# Lets cluster the entire State Crime Rate data frame and see what we find 

SCR_sub = SCR[2:9]

clusterSCR = kmeans(SCR_sub, 9, nstart=12)

print(clusterSCR)

SCR = cbind(SCR, cluster = clusterSCR$cluster)

View(SCR_sub)

set.seed(30)

fviz_nbclust(SCR_sub, kmeans, method = 'wss')
fviz_nbclust(SCR_sub, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(SCR_sub, FUN=kmeans, nstart = 20, B=50, K.max=12))

SCR9 = fviz_cluster(clusterSCR, data=SCR_sub) + ggtitle('9 clusters')

SCR9

# Terrible clustering option. We can tell from this that there is no way the State Crime Rates
# can accurately delineate each state. This is weird because generally we would think that crime
# can easily distinguish one place from another.

# The FBI should not be analyzing/clustering crime based on state because the data is too difficult to sift
# through and there are no proper separations amongst the clusters. 

View(SCR)

write.csv(SCR,"SCR_total.csv")


##### Section 5: Clustering based on economic metrics ####

# Here we are going to read in some files regarding state poverty rates and the lower 20% income
# of people across the nation

# You can find the poverty number by state file at this link: https://www.census.gov/data/tables/time-series/demo/income-poverty/historical-poverty-people.html
# It is Table 21 towards the end of the page. 
state.poverty.rate = read.csv('poverty number by state copy.csv')

# You can find the bottom 20 income distribution file at this link: https://www.taxpolicycenter.org/statistics/household-income-quintiles
bottom.20 = read.csv('bottom 20 income distribution copy.csv')

View(bottom.20)

View(state.poverty.rate)

# Subset the state poverty rate data so we can combine with the median household income data

spr_sub = subset(state.poverty.rate, state.poverty.rate$year > 1983)

jointSdata = merge(spr_sub, SMHI, by = c('state', 'year'), all = TRUE)

jointSdata$margin.of.error = NULL

jointSdata$poverty.rate = NULL

# jointSdata$cluster = NULL

# Change the name of the percent column because that represents the actualy poverty rate. 
colnames(jointSdata)[colnames(jointSdata) == 'percent'] = 'poverty.rate'

# create another subset containing population, MHI, and Poverty rate percentage

State_econ_data = jointSdata[2:5]

# Now lets cluster the entire data set

set.seed(30)

fviz_nbclust(State_econ_data, kmeans, method = 'wss')
fviz_nbclust(State_econ_data, kmeans, method = 'silhouette')
fviz_gap_stat(clusGap(State_econ_data, FUN=kmeans, nstart = 20, B=50, K.max=12))

set.seed(30)

econ_clus = kmeans(State_econ_data, 2, nstart=12)

print(econ_clus)

EC2 = fviz_cluster(econ_clus, data=State_econ_data) + ggtitle('2 clusters')

# Add the clusters onto the end of a new data frame containing the jointSdata and the clusters

JointSdata = cbind(jointSdata, cluster = econ_clus$cluster)

# Lets look at the clusters without the population this time

State_econ_data = jointSdata[4:5]

set.seed(30)
s_econ_clus9 = kmeans(State_econ_data, 9, nstart=12)

print(s_econ_clus9)


SE9 = fviz_cluster(s_econ_clus9, data=State_econ_data) + ggtitle('9 clusters')

SE9

# This proves that we may be able to cluster by division when looking
# at economic metrics

set.seed(30)
s_econ_clus4 = kmeans(State_econ_data, 4, nstart=12)

SE4 = fviz_cluster(s_econ_clus4, data=State_econ_data) + ggtitle('4 clusters')

SE4

# This proves that we may be able to cluster by region too 

# Lets observe the manhattan distance measurements to see if we find any differences
set.seed(30)
s_econ_clusM = Kmeans(State_econ_data, 4, nstart=12, method = 'manhattan')

print(s_econ_clusM)

SE4M = fviz_cluster(s_econ_clusM, data=State_econ_data) + ggtitle('4 clusters')

SE4M

set.seed(30)
s_econ_clusM = Kmeans(State_econ_data, 9, nstart=12, method = 'manhattan')

print(s_econ_clusM)

SE9M = fviz_cluster(s_econ_clusM, data=State_econ_data) + ggtitle('9 clusters')

SE9M

# We find that there are slight differences in the cluster choices but overall they still split near the same points.

# Now lets add the 9 clusters to the file 

jointSdata = cbind(jointSdata, cluster = s_econ_clus9$cluster)

# Now we head on over to excel and tableau to analyze the new data

# write.csv(jointSdata, "state econ data.csv")


# now we are going to group the data by each cluster and incorporate the according crime rates from the other
# data frame in order to analyze the averages of all the variables and see if we can make predictions on the crime
# rates in these clusters. 

clus1data = subset(jointSdata, jointSdata$cluster == 1)

clus1Data = merge(clus1data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

# Reorganize the data for easier manipulation

clus1Data = clus1Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

# We are going to create a cluster_means data frame that holds all of the averages of each
# cluster. 

cluster_means = as.data.frame(sapply(clus1Data[,5:13], mean))


clus2data = subset(jointSdata, jointSdata$cluster == 2)

clus2Data = merge(clus2data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

clus2Data = clus2Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

cluster_means = cbind(cluster_means, cluster.2 = sapply(clus2Data[,5:13], mean))



clus3data = subset(jointSdata, jointSdata$cluster == 3)

clus3Data = merge(clus3data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

clus3Data = clus3Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

cluster_means = cbind(cluster_means, cluster.3 = sapply(clus3Data[,5:13], mean))



clus4data = subset(jointSdata, jointSdata$cluster == 4)

clus4Data = merge(clus4data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

clus4Data = clus4Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

cluster_means = cbind(cluster_means, cluster.4 = sapply(clus4Data[,5:13], mean))



clus5data = subset(jointSdata, jointSdata$cluster == 5)

clus5Data = merge(clus5data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

clus5Data = clus5Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

cluster_means = cbind(cluster_means, cluster.5 = sapply(clus5Data[,5:13], mean))



clus6data = subset(jointSdata, jointSdata$cluster == 6)

clus6Data = merge(clus6data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

clus6Data = clus6Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

cluster_means = cbind(cluster_means, cluster.6 = sapply(clus6Data[,5:13], mean))



clus7data = subset(jointSdata, jointSdata$cluster == 7)

clus7Data = merge(clus7data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

clus7Data = clus7Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

cluster_means = cbind(cluster_means, cluster.7 = sapply(clus7Data[,5:13], mean))



clus8data = subset(jointSdata, jointSdata$cluster == 8)

clus8Data = merge(clus8data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

clus8Data = clus8Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

cluster_means = cbind(cluster_means, cluster.8 = sapply(clus8Data[,5:13], mean))



clus9data = subset(jointSdata, jointSdata$cluster == 9)

clus9Data = merge(clus9data, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

clus9Data = clus9Data[c(1,2,3,6,4,5,7,8,9,10,11,12,13)]

cluster_means = cbind(cluster_means, cluster.9 = sapply(clus9Data[,5:13], mean))

names(cluster_means)[1] = 'cluster.1'

# write the new cluster means data frame to a csv to save for analysis later on

write.csv(cluster_means, "state econ cluster means.csv")



# Now lets add the crime rates to the large data frame containing each state's cluster

allClusData = merge(jointSdata, SCR, by.x = c('state','year', 'population'), by.y = c('state', 'year', 'population'))

View(allClusData)

allClusData$cluster.x = NULL
allClusData$cluster.y = NULL

# allClusData = allClusData[c(1,2,3,5,6,4,7,8,9,10,11,12)]

# Create a quick data frame containing only the numerical values we need to create clusters
# based on all of the data. This was used for an in class session between myself and Dr. Berg.

DF = allClusData[4:12]

View(DF)

write.csv(DF, 'df.csv')

# Now lets write this to a csv

write.csv(allClusData, "all state data.csv")

# Now lets look at the national level 

NCR_sub = subset(NCR, NCR$year > 1966)

#joint National Data
jointNdata = merge(NCR_sub, bottom.20, by = c('year'), all = TRUE)

# Now lets try to cluster everything together 
set.seed(30)
national_clus = kmeans(jointNdata, 9, nstart=12)

print(national_clus)

set.seed(30)
NC9 = fviz_cluster(national_clus, data=jointNdata) + ggtitle('9 clusters')

NC9

# We see some great separation with both graphs

jointNdata = cbind(jointNdata, cluster = national_clus$cluster)

View(jointNdata)


# Now lets do the same thing with all the state data

SCR_sub = subset(SCR, SCR$year > 1983)

# edit the spr_sub data frame for the future merge
spr_sub$poverty.rate = NULL

colnames(spr_sub)[colnames(spr_sub)=='percent'] = 'poverty.rate'

# joint State data for crime rate and median household income
jointSdata = merge(SCR_sub, SMHI, by = c('state', 'year'), all = TRUE)

jointSdata$cluster = NULL

jointSdata$margin.of.error = NULL

# joint State data for 
jointStatedata = merge(jointSdata, spr_sub, by = c('state', 'year'), all = TRUE)

jointStatedata$population.y = NULL

jointStatedata = jointStatedata[c(1,2,3,11,12,4,5,6,7,8,9,10)]

colnames(jointStatedata)[colnames(jointStatedata)=='population.x'] = 'population'

jointStatedata_sub = jointStatedata[4:12]

# Cluster the data by 2, 4, and 9 because of the regions and divisions in the unites states. Use 2 because it 
# has been common with this data to do better with a lower amount of clusters.

all_state_clus = kmeans(jointStatedata_sub, 2, nstart=12)

print(all_state_clus)

SC2 = fviz_cluster(all_state_clus, data = jointStatedata_sub) + ggtitle('2 clusters')

SC2

all_state_clus = kmeans(jointStatedata_sub, 4, nstart=12)

SC4 = fviz_cluster(all_state_clus, data = jointStatedata_sub) + ggtitle('2 clusters')

SC4

all_state_clus = kmeans(jointStatedata_sub, 9, nstart=12)

SC9 = fviz_cluster(all_state_clus, data = jointStatedata_sub) + ggtitle('2 clusters')

SC9

# As expected, we can tell a lot by clustering the data, however when looking at visualizations of each cluster
# we find extreme overlap in every aspect


##### Section 6: Decision Trees classifying High and Low Murder Rate #####

# Here we are going to begin using decision trees as a classification method to findout the factors it takes to determine if a 
# state will have a high or low homicide rate.

set.seed(30)

hom_clus = jointStatedata[5:6]

homicide_clusters = kmeans(hom_clus, 2, nstart=25)

print(homicide_clusters)

fullData = cbind(jointStatedata, cluster = homicide_clusters$cluster)

View(fullData) 

fullData$cluster = factor(fullData$cluster, levels = c(1,2), labels = c("LOW", "HIGH"), ordered = TRUE)   

names(fullData)[13] = "homicide_class"

# We are going to take Washington DC out of the data set for now because it is a MAJOR outlier in terms of homicide rate in the United States
# We will analyze that data set as well, we just were curious about how the results were going to change after removal of that area. 

no_dc_data = fullData[!(fullData$state=='DC'),]

no_dc_data$homicide_class = NULL

no_dc_clus = no_dc_data[5:6]

no_dc_hom = kmeans(no_dc_clus, 2, nstart=25)   

print(no_dc_hom)

no_dc_data = cbind(no_dc_data, cluster = no_dc_hom$cluster)

View(no_dc_data)

no_dc_data$cluster = factor(no_dc_data$cluster, levels = c(1,2), labels = c("LOW", "NORMAL"), ordered = TRUE)   

names(no_dc_data)[13] = "homicide_class"

# Next we are going to begin by setting the seed and grabbing a random 80% from each data set
set.seed(30)
samplesizef = 0.7 * nrow(fullData)

set.seed(30)
samplesizedc = 0.7 * nrow(no_dc_data)

# Here we take a random sample from the fullData of size samplesizef
set.seed(30)
indexf = sample( seq_len(nrow(fullData)), size = samplesizef)

# Here we take a random sample from the no_dc_data of size samplesizedc
set.seed(30)
indexdc = sample( seq_len(nrow(no_dc_data)), size = samplesizedc)

# Here we create our training and test data sets for the fullData
set.seed(30)
traindataf = fullData[indexf, ]

set.seed(30)
testdataf = fullData[-indexf, ]  

# Here we create our training and test data sets for the no_dc_data
set.seed(30)
traindatadc = no_dc_data[indexdc, ]

set.seed(30)
testdatadc = no_dc_data[-indexdc, ] 

# call the party and partykit libraries because we will need some commands from them
library(party)
library(partykit) 

# Here we are going to look at the effects of every variable on the fullData homicide_class variable when thrown into 
# a decision tree
set.seed(30)
MyTrainTreeModelf=ctree(homicide_class~ population + poverty.rate + median.income
                        + rape + robbery + aggravated_assault + burglary + larceny + vehicle_theft, data=traindataf)


png(file = "decision_tree_trainf.png", res=200, height=2000, width=5000)
class(MyTrainTreeModelf) 
plot(MyTrainTreeModelf, type="simple",         
     inner_panel=node_inner(MyTrainTreeModelf, abbreviate = FALSE, pval = FALSE, id= TRUE)) 
dev.off()

plot(MyTrainTreeModelf)


# Here we are going to look at the effects of every variable on the no_dc_data homicide_class variable when thrown into 
# a decision tree
set.seed(30)
MyTrainTreeModeldc=ctree(homicide_class~ population + poverty.rate + median.income
                       + rape + robbery + aggravated_assault + burglary + larceny + vehicle_theft, data=traindatadc)


png(file = "decision_tree_traindc.png", res=200, height=2000, width=5500)
class(MyTrainTreeModeldc) 
plot(MyTrainTreeModeldc, type = "simple",         
     inner_panel=node_inner(MyTrainTreeModeldc, abbreviate = FALSE, pval = FALSE, id= TRUE)) 
dev.off()

plot(MyTrainTreeModeldc)

# NExt we are going to create a couple pruned models of the no_dc_data because the fullData is already only at two branches.
# The max depths of 4 and 5 are going to be used. 
set.seed(30)
PrunedTrainModeldc4=ctree(homicide_class~ population + poverty.rate + median.income
                        + rape + robbery + aggravated_assault + burglary + larceny + vehicle_theft, data=traindatadc, maxdepth=4)

png(file = "pruned_4_decision_tree_traindc.png", res=200, height=2000, width=5500)
class(PrunedTrainModeldc4) 
plot(PrunedTrainModeldc4, type="simple",         
     inner_panel=node_inner(PrunedTrainModeldc4, abbreviate = FALSE, pval = TRUE, id= TRUE)) 
dev.off()

# In the pruned model with only a max depth of four we see that the poverty rate plays an extremely high role in determining HIGH or LOW homicide
# classification. 
set.seed(30)
PrunedTrainModeldc5=ctree(homicide_class~ population + poverty.rate + median.income
                          + rape + robbery + aggravated_assault + burglary + larceny + vehicle_theft, data=traindatadc, maxdepth=5)

png(file = "pruned_5_decision_tree_traindc.png", res=200, height=2000, width=5500)
class(PrunedTrainModeldc5) 
plot(PrunedTrainModeldc5, type="simple",         
     inner_panel=node_inner(PrunedTrainModeldc5, abbreviate = FALSE, pval = TRUE, id= TRUE)) 
dev.off()

# Lets see how well the first training model for the no_dc_data is good at predicting HIGH or LOW Homicide rate
set.seed(30)
myPred = predict(MyTrainTreeModeldc, testdatadc[,-13])
table(observed=testdatadc[,13],predicted=myPred)

library(ROCR)
Colors  = c("Red","Blue")

# Calculate the probability of new observations belonging to each class
ROC_Predictions= predict(MyTrainTreeModeldc,testdatadc[-13],type="prob")
HomicideClass = levels(testdatadc$homicide_class)

# For each type of degree
for (i in 1:2)
{
  # Define which observations belong to that homicide class
  true_values = ifelse(testdatadc[,13]==HomicideClass[i],1,0)
  
  # Assess the performance of model for that homicide class
  pred = prediction(ROC_Predictions[,i],true_values)
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for each type of murder rate on the full dc training model") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  # Find the Area Under the Curve (AUC) and show it for each EducationClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}

# This model does acceptionally well at predicting the HIGH and LOW homicide rates! Lets check and see how well each 
# pruned model does. 

# Lets see how strong the PrunedTrainModeldc4 is when using it to predict Normal or LOW murder rate in the testdatadc

myPred = predict(PrunedTrainModeldc4, testdatadc[,-13])
table(observed=testdatadc[,13],predicted=myPred)

# Calculate the probability of new observations belonging to each class
ROC_Predictions= predict(PrunedTrainModeldc4,testdatadc[-13],type="prob")
HomicideClass = levels(testdatadc$homicide_class)

# For each type of degree
for (i in 1:2)
{
  # Define which observations belong to that homicide class
  true_values = ifelse(testdatadc[,13]==HomicideClass[i],1,0)
  
  # Assess the performance of model for that homicide class
  pred = prediction(ROC_Predictions[,i],true_values)
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for each type of murder rate with a maxdepth of 4") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  # Find the Area Under the Curve (AUC) and show it for each EducationClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}



# Lets see how strong the PrunedTrainModeldc5 is when using it to predict HIGH or LOW murder rate in the testdatadc
myPred = predict(PrunedTrainModeldc5, testdatadc[,-13])
table(observed=testdatadc[,13],predicted=myPred)

# Calculate the probability of new observations belonging to each class
ROC_Predictions= predict(PrunedTrainModeldc5,testdatadc[-13],type="prob")
HomicideClass = levels(testdatadc$homicide_class)


# For each type of degree
for (i in 1:2)
{
  # Define which observations belong to that homicide class
  true_values = ifelse(testdatadc[,13]==HomicideClass[i],1,0)
  
  # Assess the performance of model for that homicide class
  pred = prediction(ROC_Predictions[,i],true_values)
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for each type of murder rate wth a max depth of 5") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  
  # Find the Area Under the Curve (AUC) and show it for each EducationClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}


# Lets see how strong the MyTrainTreeModelf is when using it to predict HIGH or LOW murder rate in the testdataf
myPred = predict(MyTrainTreeModelf, testdataf[,-13])
table(observed=testdataf[,13],predicted=myPred)

# Calculate the probability of new observations belonging to each class
ROC_Predictions= predict(MyTrainTreeModelf,testdataf[-13],type="prob")
HomicideClass = levels(testdataf$homicide_class)


# For each type of degree
for (i in 1:2)
{
  # Define which observations belong to that homicide class
  true_values = ifelse(testdataf[,13]==HomicideClass[i],1,0)
  
  # Assess the performance of model for that homicide class
  pred = prediction(ROC_Predictions[,i],true_values)
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for each type of murder rate including Washington DC") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  
  # Find the Area Under the Curve (AUC) and show it for each EducationClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}



# Now lets get in to building and evaluating our test model 
set.seed(30)
MyTestTreeModelf=ctree(homicide_class~ population + poverty.rate + median.income
                        + rape + robbery + aggravated_assault + burglary + larceny + vehicle_theft, data=testdataf)

print(MyTestTreeModelf)

png(file = "decision_tree_testf.png", res=200, height=2000, width=5000)
class(MyTestTreeModelf) 
plot(MyTestTreeModelf, type="simple",         
     inner_panel=node_inner(MyTestTreeModelf, abbreviate = FALSE, pval = FALSE, id= TRUE)) 
dev.off()

plot(MyTestTreeModelf)


# Here we are going to look at the effects of every variable on the no_dc_data homicide_class variable when thrown into 
# a decision tree
set.seed(30)
MyTestTreeModeldc=ctree(homicide_class~ population + poverty.rate + median.income
                         + rape + robbery + aggravated_assault + burglary + larceny + vehicle_theft, data=testdatadc)

print(MyTestTreeModeldc)

png(file = "decision_tree_testdc.png", res=200, height=2000, width=5500)
class(MyTestTreeModeldc) 
plot(MyTestTreeModeldc, type = "simple",         
     inner_panel=node_inner(MyTestTreeModeldc, abbreviate = FALSE, pval = FALSE, id= TRUE)) 
dev.off()

plot(MyTestTreeModeldc)

# Lets see if we can observe node 4 and better understand why it has a 50% accuracy

ggplot(testdatadc, aes(poverty.rate, robbery, group = homicide_class, color = homicide_class)) + 
  geom_point() + geom_vline(aes(xintercept=12.87)) + geom_hline(aes(yintercept=270.45)) +
  labs(title = "Poverty Rate vs Robbery Rate by Homicide Class",
       x = 'Poverty Rate', y = "Robbery Rate")

# Now lets see if we can investigate North Carolina's Homicide Rate Calssification just for fun

nc_only = fullData[(fullData$state == 'NC'),]

View(nc_only)

nc_only$homicide_class = NULL

nc_clus_data = nc_only[5:6]

nc_clus = kmeans(nc_clus_data, 2, nstart=25)   

print(nc_clus)

nc_only= cbind(nc_only, cluster = nc_clus$cluster)

View(nc_only)

nc_only$cluster = factor(nc_only$cluster, levels = c(1,2), labels = c("NORMAL", "LOW"), ordered = TRUE)   

names(nc_only)[13] = "homicide_class"

# Next we are going to use bootstrapping to increase the sample size, that way we can have some reliable
# models

library(boot)

# We can now set the parameters for our bootstrap sampling
n = 12  # Number of boostrap samples
B = 10   # Number of observations in each bootstap sample

nc_samples = nc_only[sample(NROW(nc_only), B * n, replace = TRUE),]
View(nc_samples)

# set the sample size to be greater than 35 so that way we can accurately predict the entire population
Samples = sample(1:120, size = 96)

# Now we assign the Samples to Build, and the 'leftovers' to Validate datasets
Build = nc_samples[Samples,]
Validate = nc_samples[-Samples,]

# Now lets build our models
set.seed(30)
MyTrainNCModel=ctree(homicide_class~ poverty.rate + median.income
                       + rape + robbery + aggravated_assault + burglary + larceny + vehicle_theft, data=Build)

print(MyTrainNCModel)

png(file = "NC_train_model.png", res=200, height=2000, width=5000)
class(MyTrainNCModel) 
plot(MyTrainNCModel, type="simple",         
     inner_panel=node_inner(MyTrainNCModel, abbreviate = FALSE, pval = FALSE, id= TRUE)) 
dev.off()

plot(MyTrainNCModel)

ggplot(Build, aes(aggravated_assault, median.income, group = homicide_class, color = homicide_class)) + 
  geom_point() + geom_vline(aes(xintercept=291)) + geom_hline(aes(yintercept=53840)) +
  labs(title = "Agg. Assault vs Median Income by Homicide Class",
       x = 'Agg. Assault', y = "Median Income")

# Lets see how strong the NC Build model is when using it to predict HIGH or LOW murder rate in the Validate data

myPred = predict(MyTrainNCModel, Validate[,-13])
table(observed=Validate[,13],predicted=myPred)

# Calculate the probability of new observations belonging to each class
ROC_Predictions= predict(MyTrainNCModel,Validate[-13],type="prob")
HomicideClass = levels(Validate$homicide_class)


# For each type of degree
for (i in 1:2)
{
  # Define which observations belong to that homicide class
  true_values = ifelse(Validate[,13]==HomicideClass[i],1,0)
  
  # Assess the performance of model for that homicide class
  pred = prediction(ROC_Predictions[,i],true_values)
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for each type of murder classification in NC") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  
  # Find the Area Under the Curve (AUC) and show it for each EducationClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}

# Now lets use the validation data and see if our results match!
set.seed(30)
MyTestNCModel=ctree(homicide_class~ poverty.rate + median.income
                     + rape + robbery + aggravated_assault + burglary + larceny + vehicle_theft, data=Validate)

print(MyTestNCModel)

png(file = "NC_test_model.png", res=200, height=2000, width=5000)
class(MyTestNCModel) 
plot(MyTestNCModel, type="simple",         
     inner_panel=node_inner(MyTestNCModel, abbreviate = FALSE, pval = FALSE, id= TRUE)) 
dev.off()

plot(MyTestNCModel)

ggplot(Validate, aes(sample=aggravated_assault, color = homicide_class)) + 
  geom_qq() + geom_hline(aes(yintercept=290.99)) +
  labs(title = "Agg. Assault by Homicide Class in NC",
       y = 'Agg. Assault Rate')


##### Section 7: Random Forest classifying Low and Normal Murder Rate ######

# In this section we are going to create a random forest model on the exact same data
# we used for the decision tree except we are going to bootstrap the samples. This is done in 
# an effort to create a model that is better at predicting when there is going to be a low 
# homicide rate. 

set.seed(30)

# We can now set the parameters for our bootstrap sampling
n = 33  # Number of boostrap samples
B = 450   # Number of observations in each bootstap sample

RF_samples = no_dc_data[sample(NROW(no_dc_data), B * n, replace = TRUE),]
View(RF_samples)

na.omit(RF_samples)

# set the sample size to be greater than 35 so that way we can accurately predict the entire population
Samples = sample(1:15000, size = 7425)

# Now we assign the Samples to Build, and the 'leftovers' to Validate datasets
set.seed(30)
Build = RF_samples[Samples,]

Build = na.omit(Build)

Validate = RF_samples[-Samples,]

Validate = na.omit(Validate)

# Now we will begin creating the random forests of the data 

library(randomForest)

set.seed(30)
MyModel = randomForest(homicide_class ~ poverty.rate + median.income+ rape + robbery + aggravated_assault + burglary 
                       + larceny + vehicle_theft, data=Build, ntree=400, mtry=3, importance=TRUE)

MyModel

# Since we set importance = TRUE, we can see what 
# importance our model has assigned to each feature 
# using the varImpPlot() command
varImpPlot(MyModel)

set.seed(30)
MyPredictions = predict(MyModel, Validate[,-13])
table(observed=Validate[,13],predicted=MyPredictions)

# Calculate the probability of new observations belonging to each class
ROC_Predictions= predict(MyModel,Validate[,-13],type="prob")

# Adding line colours and specifying EducationClass based on unique variety
Colors  = c("Red","Blue")
HomicideClass = levels(Validate$homicide_class)

# For each type of degree
for (i in 1:2)
{
  # Define which observations belong to that education class
  true_values = ifelse(Validate[,13]==HomicideClass[i],1,0)
  
  # Assess the performance of model for that education class
  pred = prediction(ROC_Predictions[,i],true_values)
  perf = performance(pred, "tpr", "fpr")
  
  if (i==1)
  {
    plot(perf, col=Colors[i], main="ROC Curve for each type of murder class") 
  }
  else
  {
    plot(perf, main="ROC Curve", col=Colors[i], add=TRUE) 
  }
  # Find the Area Under the Curve (AUC) and show it for each EducationClass
  AUC = performance(pred, measure = "auc")
  print(AUC@y.values)
}

set.seed(30)
MyTestModel = randomForest(homicide_class ~ poverty.rate + median.income+ rape + robbery + aggravated_assault + burglary 
                       + larceny + vehicle_theft, data=Validate, ntree=400, mtry=3, importance=TRUE)

MyTestModel


# Since we set importance = TRUE, we can see what 
# importance our model has assigned to each feature 
# using the varImpPlot() command
varImpPlot(MyTestModel)

MyTestModel$confusion



##### Section 8: Gradient Boosted RF Model #####

# In this section we are going to create a gradient boosted model on the exact same data
# as the decision tree and random forest in an attempt to create a better model. This is the 
# first seciton of this semester where we are doing something I have no previous experience with.

library(caret)

# First we are going to create the training control variable that will allow each model to
# have 5 different folds and 5 repeats
trainctrl = trainControl(method = "repeatedcv", number = 5, repeats = 5)

# Next lets randomize the data 
set.seed(30)
random_value = sample(1:nrow(no_dc_data), nrow(no_dc_data))
random_build = no_dc_data[random_value, ]

# Next lets create our train and holdout samples. 
set.seed(30)
index = createDataPartition(random_build$homicide_class, p = 0.7, list = FALSE)
Build = random_build[index, ]

na.omit(Build)

Validate = random_build[-index, ]

na.omit(Validate)

# First lets recreate the singular decision tree
set.seed(30)
dec_tree = train(homicide_class ~ poverty.rate + median.income+ rape + robbery + aggravated_assault + burglary 
                 + larceny + vehicle_theft, 
                 data=Build,
                 method = "rpart",
                 trControl = trainctrl)

dec_tree

# Second lets create the entire random forest using an mtry of 3 because that is what our previous rf model used
rfGrid = expand.grid(mtry = c(3))

set.seed(30)
rf_tree = train(homicide_class ~ poverty.rate + median.income+ rape + robbery + aggravated_assault + burglary 
                + larceny + vehicle_theft, 
                data=Build,
                method = "rf",
                trControl = trainctrl,
                tuneGrid = rfGrid)

rf_tree

# Third lets create a stochastic gradient boosted model
set.seed(30)
gbm_tree = train(homicide_class ~ poverty.rate + median.income+ rape + robbery + aggravated_assault + burglary 
                 + larceny + vehicle_theft, 
                 data=Build,
                 method = "gbm",
                 trControl = trainctrl,
                 verbose = FALSE)

gbm_tree

# Now lets throw all three of these results into a list and see which one produced the best results
tree_performances = resamples(list(decision_tree = dec_tree,
                                   random_forest = rf_tree,
                                   gradient_boosted_machine = gbm_tree))

summary(tree_performances)

# We can then use each of these models to make predictions on the validate data
set.seed(30)
dec_conf_mat = confusionMatrix(
  data = predict(dec_tree, Validate),
  reference = Validate$homicide_class
)

dec_conf_mat

set.seed(30)
rf_conf_mat = confusionMatrix(
  data = predict(rf_tree, Validate),
  reference = Validate$homicide_class
)

rf_conf_mat

set.seed(30)
gbm_conf_mat = confusionMatrix(
  data = predict(gbm_tree, Validate),
  reference = Validate$homicide_class
)

gbm_conf_mat

# By the looks of it, our Random Forest model is slightly better than our generic stochastic gradient boosted model.

# Next, lets optimize the results of the gbm by using tuning the hyperparameters via a grid, instead of only a few hyperparameters we'll be
# able to use around 81 in model creation.
tuneGrid = expand.grid(n.trees = c(300, 400, 500),
                       interaction.depth = c(5, 7, 10),
                       shrinkage = c(0.1, 0.15, 0.2),
                       n.minobsinnode = c(5, 10, 15))

# Always check the number of rows in the grid
nrow(tuneGrid)

# Now lets create a tuned gbm model to see if we can create a better model than the random Forest. 
set.seed(30)
gbm_tune_tree = train(homicide_class ~ poverty.rate + median.income+ rape + robbery + aggravated_assault + burglary 
                      + larceny + vehicle_theft, 
                      data=Build,
                      method = "gbm",
                      trControl = trainctrl,
                      verbose = FALSE,
                      tuneGrid = tuneGrid)

gbm_tune_tree

# Now lets check our best tuned model
gbm_tune_tree$bestTune

# n.trees interaction.depth shrinkage n.minobsinnode
#     500               10      0.15              5

# Now we are going to create a tuned model based on these four hyperparameters and we'll see
# if it preforms better than the random forest model 
newGrid = gbm_tune_tree$bestTune

set.seed(30)
gbm_final_tree = train(homicide_class ~ poverty.rate + median.income+ rape + robbery + aggravated_assault + burglary 
                       + larceny + vehicle_theft, 
                       data=Build,
                       method = "gbm",
                       trControl = trainctrl,
                       verbose = FALSE,
                       tuneGrid = newGrid)

gbm_final_tree

set.seed(30)
tree_performances = resamples(list(decision_tree = dec_tree,
                                   random_forest = rf_tree,
                                   gradient_boosted_machine = gbm_tree,
                                   gbm_final_model = gbm_final_tree))

summary(tree_performances)

# Lets look at some plots of each model's performance
dotplot(tree_performances)
bwplot(tree_performances)

# Now create a confusion matrix for the final gbm model
set.seed(30)
gbmf_conf_mat = confusionMatrix(
  data = predict(gbm_final_tree, Validate),
  reference = Validate$homicide_class
)

gbmf_conf_mat
