#God is good all the times

#libraries
library(RColorBrewer)
library(dplyr)
library(FNN)
library(caret)


#Loading the Data
inc = read.csv("C:/Users/tonyk/Documents/DSP 34/Task/5 KNN/DataSet/adult.csv",
               stringsAsFactors = TRUE)

#Checking the dimensions of the Data
dim(inc)
'48842    15'

#Checking the missing value
"Note that NULL is different from the other two. NULL means that there is no 
value, while NA and NaN mean that there is some value, although one that is 
perhaps not usable. Here's an illustration of the difference:"
sapply(inc, function(x) sum(is.na(x)))
sapply(inc, function(x) sum(is.null(x)))
'No missing values'

str(inc)
'observed some ? symbols as factors'

#Checking the factors of all catergorical variables, to find any suspected
#factors are there
names(which(sapply(inc, class)=='factor'))
[1] "workclass"      "education"      "marital.status"
[4] "occupation"     "relationship"   "race"          
[7] "gender"         "native.country" "income"   

for (i in names(which(sapply(inc, class)=='factor'))){
    print(list(unique(inc[i])))
}
'
Work class variable having a value ?
  Occupation variable having a value ?
  native.country variable having a value ?'

#Exploring the ? symbol
'Indexing 1 being symbol comes first in order'
table(inc$workclass)[1]
'   ? 
  2799 '

table(inc$occupation)[1]
'   ? 
  2809 '

table(inc$native.country)[1]
'  ? 
  857'

#Being occupation is having highest no of ? and compare to total data they are
#less in number so removing ? from the data
inc  = inc[inc$occupation!='?',]

table(inc$workclass)[1]
'? 
  0 '

table(inc$occupation)[1]
'? 
  0 '
'Even though there is no obs with that factor, still the factor label is 
existing, need to correct this'

table(inc$native.country)[1]
'  ? 
  811'

#Removing ? from native.country
inc = inc[inc$native.country!='?',]

#Updating the data type of factors so that factor labels with 0 values 
#will be removed
inc$workclass = factor(inc$workclass)
table(inc$workclass)

inc$occupation = factor(inc$occupation)
table(inc$occupation)

inc$native.country = factor(inc$native.country)
table(inc$native.country)

'Noted that ? symbol is removed from the data'

#Dimension of the data
dim(inc)
'45222    15'
'48842-45222 = 3620 obs removed ie 3620/48842 = 0.074 % of the data'

#Names of the variables
colnames(inc)
'
[1] "age"             "workclass"       "fnlwgt"          "education"      
[5] "educational.num" "marital.status"  "occupation"      "relationship"   
[9] "race"            "gender"          "capital.gain"    "capital.loss"   
[13] "hours.per.week"  "native.country"  "income"  '

#_______________________________Target Variable -Income
str(inc$income)
'Factor w/ 2 levels "<=50K",">50K": 1 1 2 2 1 1 1 2 1 1 ...'

table(inc$income)
'<=50K  >50K 
34014 11208  '

#Barplot
barplot(table(inc$income),
        col = c('forestgreen','firebrick'),
        main = 'Individual Annual Income',
        ylim = c(0,35000), las=1)

#Dividing the data as per the income factor less than & above 50K
inc_les50k = inc[inc$income=='<=50K',]
inc_abv50k = inc[inc$income=='>50K',]

#_______________________________AGE - Age of an individual
str(inc$age)
'int [1:45222] 25 38 28 44 34 63 24 55 65 36 ...'

summary(inc$age)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
17.00   28.00   37.00   38.55   47.00   90.00 '

#Histogram of Age of an individual
hist(inc$age,
     main = 'Age of an individual',
     xlab = 'Age',
     col = brewer.pal(8,'Accent'),las=1)

par(mfrow = c(1,2))

#Histogram of Age of an individual
hist(inc_les50k$age,
     main = 'Income <=50k',
     xlab = 'Age',
     ylim = c(0,6000),
     col = brewer.pal(8,'Accent'), las=1)

hist(inc_abv50k$age,
     main = 'Income >50k',
     xlab = 'Age',
     ylim = c(0,6000),
     col = brewer.pal(8,'Accent'),las=1)

par(mfrow = c(1,1))

#Boxplot
boxplot(inc$age,
        horizontal = TRUE,
        col = 'springgreen',
        main = 'Boxplot of Age')

#Checking the outliers from complete data
age_ub = quantile(inc$age, 0.75)+1.5*IQR(inc$age)
length(inc$age[inc$age>age_ub])
'269'

for (i in seq(age_ub,max(inc$age),2)){
  j = length(inc$age[inc$age > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}

'[1] "No of outliers with ub as 76 is 269"
[1] "No of outliers with ub as 78 is 184"
[1] "No of outliers with ub as 80 is 143"
[1] "No of outliers with ub as 82 is 87"
[1] "No of outliers with ub as 84 is 68"
[1] "No of outliers with ub as 86 is 54"
[1] "No of outliers with ub as 88 is 52"
[1] "No of outliers with ub as 90 is 46"'

'Even though KNN is senstitive to outliers, in classification, it uses mode to 
predict, mode is not sensitve to outliers so only removing extreme outliers '

#Removing extreme outliers keeping cut off at 80
inc = inc[inc$age<80,]

dim(inc)
'45079    15'
'45222-45079 = 143 Removed'

#Updatind sub data's
inc_les50k = inc[inc$income=='<=50K',]
inc_abv50k = inc[inc$income=='>50K',]

#_______________________________workclass
'Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, 
Without-pay, Never-worked.'
str(inc$workclass)
'Factor w/ 7 levels "Federal-gov",..: 3 3 2 3 3 5 3 3 3 1 ...'

#Barplot
table(inc$workclass)
'     Federal-gov        Local-gov          Private     Self-emp-inc 
            1404             3090            33224             1634 
Self-emp-not-inc        State-gov      Without-pay 
            3761             1945               21 '

barplot(table(inc$workclass),
        col = brewer.pal(8,'Spectral'),
        ylim = c(0,35000),
        main = 'Barplot of Work Class', las=2)

par(mfrow=c(1,2))

barplot(table(inc_les50k$workclass),
        col = brewer.pal(8,'Spectral'),
        main = 'Work Class Income <=50k',las=2)

barplot(table(inc_abv50k$workclass),
        col = brewer.pal(8,'Spectral'),
        main = 'Work Class Income >50k',las=2,
        ylim = c(0,25000))

par(mfrow=c(1,1))

#_______________________________fnlwgt
'The weights on the CPS files are controlled to independent estimates of the 
civilian noninstitutional population of the US. These are prepared monthly 
for us by Population Division here at the Census Bureau.'

str(inc$fnlwgt)
' int [1:45079] 226802 89814 336951 160323 198693 104626 369667 104996 184454 
212465 ...'

summary(inc$fnlwgt)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  13492  117444  178353  189824  237994 1490400 '

#For easy readable & convenience converting the numbers in 1000's
inc$fnlwgt = inc$fnlwgt/1000

str(inc$fnlwgt)
'num [1:45079] 226.8 89.8 337 160.3 198.7 ...'

summary(inc$fnlwgt)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  13.49  117.44  178.35  189.82  237.99 1490.40 '

#Convertind the divided data into 1000's
inc_abv50k$fnlwgt = inc_abv50k$fnlwgt/1000
inc_les50k$fnlwgt = inc_les50k$fnlwgt/1000

#Histogram
hist(inc$fnlwgt,
     col = brewer.pal(9,'Paired'),
     main = 'Histogram of Final Weight',
     xlab = 'Weight',las=1)

par(mfrow=c(1,2))
hist(inc_les50k$fnlwgt,
     col = brewer.pal(9,'Paired'),
     main = 'Final Weight - Income <=50',
     xlab = "",las=1)

hist(inc_abv50k$fnlwgt,
     col = brewer.pal(9,'Paired'),
     main = 'Final Weight - Income >50',
     ylim = c(0,15000),
     xlab = "",las=1)
par(mfrow=c(1,1))

#Boxplot
boxplot(inc$fnlwgt,
        horizontal = TRUE,
        col = 'seagreen3',
        main = 'Boxplot of Final Weight')

par(mfrow=c(2,1))
boxplot(inc_les50k$fnlwgt,
        col = 'plum3',
        main = 'Final Weight - Income <=50',
        horizontal = TRUE)

boxplot(inc_abv50k$fnlwgt,
        col = 'sienna3',
        main = 'Final Weight - Income >50',
        horizontal = TRUE)
par(mfrow=c(1,1))

#Checking the outliers from complete data
fw_ub = quantile(inc$fnlwgt, 0.75)+1.5*IQR(inc$fnlwgt)
length(inc$fnlwgt[inc$fnlwgt>fw_ub])
'1330'

for (i in seq(fw_ub,max(inc$fnlwgt),100)){
  j = length(inc$fnlwgt[inc$fnlwgt > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}

'[1] "No of outliers with ub as 419 is 1330"
[1] "No of outliers with ub as 519 is 416"
[1] "No of outliers with ub as 619 is 164"
[1] "No of outliers with ub as 719 is 81"
[1] "No of outliers with ub as 819 is 36"
[1] "No of outliers with ub as 919 is 21"
[1] "No of outliers with ub as 1019 is 17"
[1] "No of outliers with ub as 1119 is 10"
[1] "No of outliers with ub as 1219 is 6"
[1] "No of outliers with ub as 1319 is 4"
[1] "No of outliers with ub as 1419 is 3"'

#Removing outliers taking ub as 519
inc = inc[inc$fnlwgt<=519,]

dim(inc)
'44663    15'
'45079-44663 = 416 Removed'

#Updating sub data's
inc_les50k = inc[inc$income=='<=50K',]
inc_abv50k = inc[inc$income=='>50K',]

boxplot(inc$fnlwgt,
        horizontal = TRUE,
        col = 'seagreen3',
        main = 'Boxplot of Final Weight')

#_______________________________Education
'Bachelors, Some-college, 11th, HS-grad, Prof-school, Assocacdm, Assoc-voc, 
9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
'
str(inc$education)
'Factor w/ 16 levels "10th","11th",..: 2 12 8 16 1 15 16 6 12 10 ...'

table(inc$education)
'        10th         11th         12th      1st-4th      5th-6th 
        1202         1600          565          213          439 
     7th-8th          9th   Assoc-acdm    Assoc-voc    Bachelors 
         799          665         1498         1947         7474 
   Doctorate      HS-grad      Masters    Preschool  Prof-school 
         541        14602         2483           70          773 
Some-college 
        9792'

#Barplot
barplot(table(inc$education),
        col = brewer.pal(11,'RdYlGn'),
        ylim = c(0,16000), las=2,
        main = 'Barplot of Education')

par(mfrow = c(1,2))
barplot(table(inc_les50k$education),
        col = brewer.pal(11,'RdYlBu'),
        ylim = c(0,12000), las=2,
        main = 'Education - Income <=50')

barplot(table(inc_abv50k$education),
        col = brewer.pal(11,'RdYlBu'),
        ylim = c(0,12000), las=2,
        main = 'Education - Income >50')
par(mfrow=c(1,1))

#_______________________________educational.num

#Creating data frame to understand the educational.num & education
edu_index = data.frame(unique(inc$education),unique(inc$educational.num))
edu_index[order(edu_index$unique.inc.educational.num.),]

str(inc$educational.num)
'int [1:44663] 7 9 12 10 6 15 10 4 9 13 ...'

summary(inc$educational.num)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00    9.00   10.00   10.12   13.00   16.00 '

#Barplot
barplot(table(inc$educational.num),
        col = brewer.pal(11,'Dark2'),
        ylim = c(0,16000), las=2,
        main = 'Barplot of Education')

par(mfrow = c(1,2))
barplot(table(inc_les50k$educational.num),
        col = brewer.pal(8,'Dark2'),
        ylim = c(0,12000), las=2,
        main = 'Education - Income <=50')

barplot(table(inc_abv50k$educational.num),
        col = brewer.pal(8,'Dark2'),
        ylim = c(0,12000), las=2,
        main = 'Education - Income >50')
par(mfrow=c(1,1))  

'Compare to previous variable this variable plots are in order'

#_______________________________Marital Status
'Married-civ-spouse, Divorced, Never-married, Separated, Widowed, 
Married-spouse-absent, Married-AF-spouse.'
str(inc$marital.status)
' Factor w/ 7 levels "Divorced","Married-AF-spouse",..: 5 3 3 3 5 3 5 3 3 3 ...'

table(inc$marital.status)
'             Divorced     Married-AF-spouse    Married-civ-spouse 
                 6246                    31                 20806 
Married-spouse-absent         Never-married             Separated 
                  541                 14424                  1389 
              Widowed 
                 1226'

#Barplot
barplot(table(inc$marital.status),
        col = brewer.pal(11,'PRGn'),
        ylim = c(0,22000), las=2,
        main = 'Barplot of Martial Status')

par(mfrow = c(1,2))
barplot(table(inc_les50k$marital.status),
        col = brewer.pal(11,'PRGn'),
        ylim = c(0,14000), las=2,
        main = 'Martial Status - Income <=50')

barplot(table(inc_abv50k$marital.status),
        col = brewer.pal(11,'PRGn'),
        ylim = c(0,14000), las=2,
        main = 'Martial Status - Income >50')
par(mfrow=c(1,1))  

#_______________________________Occupation
'Tech-support, Craft-repair, Other-service, Sales, Execmanagerial, 
Prof-specialty, Handlers-cleaners, Machine-opinspct, Adm-clerical, 
Farming-fishing, Transport-moving, Privhouse-serv, Protective-serv, 
Armed-Forces.'

str(inc$occupation)
'Factor w/ 14 levels "Adm-clerical",..: 7 5 11 7 8 10 8 3 7 1 ...'

table(inc$occupation)
'     Adm-clerical      Armed-Forces      Craft-repair   Exec-managerial 
             5468                14              5947              5906 
  Farming-fishing Handlers-cleaners Machine-op-inspct     Other-service 
             1448              2021              2944              4739 
  Priv-house-serv    Prof-specialty   Protective-serv             Sales 
              227              5947               965              5348 
     Tech-support  Transport-moving 
             1406              2283 '

#Barplot
barplot(table(inc$occupation),
        col = brewer.pal(9,'Set1'),
        las=2,
        main = 'Barplot of Occupation')

par(mfrow = c(1,2))
barplot(table(inc_les50k$occupation),
        col = brewer.pal(9,'Set1'),
        ylim = c(0,5000), las=2,
        main = 'Occupation - Income <=50')

barplot(table(inc_abv50k$occupation),
        col = brewer.pal(9,'Set1'),
        ylim = c(0,5000), las=2,
        main = 'Occupation - Income >50')
par(mfrow=c(1,1))  

#_______________________________Relationship
'Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried'

str(inc$relationship)
'Factor w/ 6 levels "Husband","Not-in-family",..: 4 1 1 1 2 1 5 1 1 1 ...'

table(inc$relationship)
'       Husband  Not-in-family Other-relative      Own-child      Unmarried 
         18441          11546           1327           6561           4718 
          Wife 
          2070'

#Barplot
barplot(table(inc$relationship),
        col = brewer.pal(11,'BrBG'),
        ylim = c(0,20000),las=2,
        main = 'Barplot of relationship')

par(mfrow = c(1,2))
barplot(table(inc_les50k$relationship),
        col = brewer.pal(11,'BrBG'),
        ylim = c(0,10000), las=2,
        main = 'relationship - Income <=50')

barplot(table(inc_abv50k$relationship),
        col = brewer.pal(11,'BrBG'),
        ylim = c(0,10000), las=2,
        main = 'relationship - Income >50')
par(mfrow=c(1,1))  

#_______________________________RACE
'White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.'
str(inc$race)
'Factor w/ 5 levels "Amer-Indian-Eskimo",..: 3 5 5 3 5 5 5 5 5 5 ...'

table(inc$race)
'Amer-Indian-Eskimo Asian-Pac-Islander              Black              Other 
               433               1295               4091                352 
             White 
             38492 '

#Barplot
barplot(table(inc$race),
        col = brewer.pal(11,'PiYG'),
        ylim = c(0,40000),las=2,
        main = 'Barplot of race')

par(mfrow = c(1,2))
barplot(table(inc_les50k$race),
        col = brewer.pal(11,'PiYG'),
        ylim = c(0,30000), las=2,
        main = 'race - Income <=50')

barplot(table(inc_abv50k$race),
        col = brewer.pal(11,'PiYG'),
        ylim = c(0,30000), las=2,
        main = 'race - Income >50')
par(mfrow=c(1,1))  

#_______________________________Gender
'Female, Male.'
str(inc$gender)
'Factor w/ 2 levels "Female","Male": 2 2 2 2 2 2 1 2 2 2 ...'

table(inc$gender)
'Female   Male 
 14524  30139'

#Barplot
barplot(table(inc$gender),
        col = c('purple3', 'peru'),
        ylim = c(0,30000),las=1,
        main = 'Barplot of gender')

par(mfrow = c(1,2))
barplot(table(inc_les50k$gender),
        col = c('purple3', 'peru'),
        ylim = c(0,20000), las=1,
        main = 'gender - Income <=50')

barplot(table(inc_abv50k$gender),
        col = c('purple3', 'peru'),
        ylim = c(0,20000), las=1,
        main = 'gender - Income >50')
par(mfrow=c(1,1))  

#_______________________________Capital Gain
str(inc$capital.gain)
'int [1:44663] 0 0 0 7688 0 3103 0 0 6418 0 ...'

summary(inc$capital.gain)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0       0       0    1101       0   99999 '

table(inc$capital.gain)

#Historgram
hist(inc$capital.gain,
     col = brewer.pal(11,'RdYlGn'), las=1,
     main = 'Histogram of Capital Gains',
     xlab = 'Capital Gain')

#Historgram
par(mfrow=c(1,2))
hist(inc_les50k$capital.gain,
     col = brewer.pal(11,'RdYlGn'), las=2,
     main = 'Capital Gain - Income <=50',
     ylim = c(0,35000))

hist(inc_abv50k$capital.gain,
     col = brewer.pal(11,'RdYlGn'), las=2,
     main = 'Capital Gain - Income >50',
     ylim = c(0,35000))

par(mfrow=c(1,1))

#Boxplot
boxplot(inc$capital.gain,
        horizontal = TRUE,
        col = 'coral3',
        main = 'Boxplot of Capital Gain', las=2)

#Checking the outliers from complete data
cg_ub = quantile(inc$capital.gain, 0.75)+1.5*IQR(inc$capital.gain)
length(inc$capital.gain[inc$capital.gain>cg_ub])
'3731'

for (i in seq(cg_ub,max(inc$capital.gain),10000)){
  j = length(inc$capital.gain[inc$capital.gain > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}

'
[1] "No of outliers with ub as 0 is 3731"
[1] "No of outliers with ub as 10000 is 1069"
[1] "No of outliers with ub as 20000 is 347"
[1] "No of outliers with ub as 30000 is 234"
[1] "No of outliers with ub as 40000 is 230"
[1] "No of outliers with ub as 50000 is 227"
[1] "No of outliers with ub as 60000 is 227"
[1] "No of outliers with ub as 70000 is 227"
[1] "No of outliers with ub as 80000 is 227"
[1] "No of outliers with ub as 90000 is 227"'

#Removing extreme outliers @ cut off 10000
inc = inc[inc$capital.gain<10000,]

dim(inc)
'43594 15'
'44663-43594 = 1069 removed'

#Updating sub data's
inc_les50k = inc[inc$income=='<=50K',]
inc_abv50k = inc[inc$income=='>50K',]

#Boxplot
boxplot(inc$capital.gain,
        horizontal = TRUE,
        col = 'coral3',
        main = 'Boxplot of Capital Gain', las=2)

#_______________________________Capital Loss
str(inc$capital.loss)
'int [1:43594] 0 0 0 0 0 0 0 0 0 0 ...'

summary(inc$capital.loss)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.00    0.00    0.00   90.74    0.00 3900.00 '

table(inc$capital.loss)

#Historgram
hist(inc$capital.loss,
     col = brewer.pal(11,'Spectral'), las=1,
     main = 'Histogram of Capital Loss',
     xlab = 'Capital Loss')

#Historgram
par(mfrow=c(1,2))
hist(inc_les50k$capital.loss,
     col = brewer.pal(11,'Spectral'), las=2,
     main = 'Capital Loss - Income <=50',
     xlab = "",
     ylim = c(0,35000))

hist(inc_abv50k$capital.loss,
     col = brewer.pal(11,'Spectral'), las=2,
     main = 'Capital Loss - Income >50',
     xlab = "",
     ylim = c(0,35000))

par(mfrow=c(1,1))

#Boxplot
boxplot(inc$capital.loss,
        horizontal = TRUE,
        col = 'coral3',
        main = 'Boxplot of Capital Loss', las=2)

#Checking the outliers from complete data
cl_ub = quantile(inc$capital.loss, 0.75)+1.5*IQR(inc$capital.loss)
length(inc$capital.loss[inc$capital.loss>cl_ub])
'2115'

for (i in seq(cg_ub,max(inc$capital.loss),500)){
  j = length(inc$capital.loss[inc$capital.loss > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}

'[1] "No of outliers with ub as 0 is 2115"
[1] "No of outliers with ub as 500 is 2103"
[1] "No of outliers with ub as 1000 is 2072"
[1] "No of outliers with ub as 1500 is 1928"
[1] "No of outliers with ub as 2000 is 442"
[1] "No of outliers with ub as 2500 is 54"
[1] "No of outliers with ub as 3000 is 14"
[1] "No of outliers with ub as 3500 is 8"'

#Removing extreme outliers @ cut off 2000
inc = inc[inc$capital.loss<2000,]

dim(inc)
'43152    15'
'43594-43152 = 442 Removed'

#Updating sub data's
inc_les50k = inc[inc$income=='<=50K',]
inc_abv50k = inc[inc$income=='>50K',]

#Boxplot
boxplot(inc$capital.loss,
        horizontal = TRUE,
        col = 'coral3',
        main = 'Boxplot of Capital Loss', las=2)

#_______________________________Hours Per Week
str(inc$hours.per.week)
'int [1:43152] 40 50 40 40 30 32 40 10 40 40 ...'

summary(inc$hours.per.week)
'   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   1.00   40.00   40.00   40.78   45.00   99.00 '

#Histogram
hist(inc$hours.per.week,
     col = brewer.pal(11,'RdYlGn'), las=1,
     main = 'Histogram of Hours Per Week',
     xlab = 'Hours Per Week')

#Historgram
par(mfrow=c(1,2))
hist(inc_les50k$hours.per.week,
     col = brewer.pal(11,'RdYlGn'), las=2,
     main = 'Hours Per Week - Income <=50',
     ylim = c(0,20000),
     xlab = '')

hist(inc_abv50k$hours.per.week,
     col = brewer.pal(11,'RdYlGn'), las=2,
     main = 'Hours Per Week - Income >50',
     xlab = "", ylim = c(0,20000))

par(mfrow=c(1,1))

#Boxplot
boxplot(inc$hours.per.week,
        horizontal = TRUE,
        col = 'coral3',
        main = 'Boxplot of Hours Per Week', las=2)

#Checking the outliers from complete data
hpw_ub = quantile(inc$hours.per.week, 0.75)+1.5*IQR(inc$hours.per.week)
length(inc$hours.per.week[inc$hours.per.week>hpw_ub])
'4567'

for (i in seq(hpw_ub,max(inc$hours.per.week),5)){
  j = length(inc$hours.per.week[inc$hours.per.week > i])
  print(paste('No of outliers with ub as',round(i,0), 'is',j))
}

'
1] "No of outliers with ub as 52 is 4567"
[1] "No of outliers with ub as 58 is 3394"
[1] "No of outliers with ub as 62 is 1438"
[1] "No of outliers with ub as 68 is 1067"
[1] "No of outliers with ub as 72 is 573"
[1] "No of outliers with ub as 78 is 462"
[1] "No of outliers with ub as 82 is 276"
[1] "No of outliers with ub as 88 is 193"
[1] "No of outliers with ub as 92 is 142"
[1] "No of outliers with ub as 98 is 128"'

#Removing extreme outliers @ cut off 72hrs per week; 12 hrs per day/ 6 days
inc = inc[inc$hours.per.week<72,]

dim(inc)
'42491    15'
'43152-42491 = 661 removed'

#Updating sub data's
inc_les50k = inc[inc$income=='<=50K',]
inc_abv50k = inc[inc$income=='>50K',]

#Checking the outliers from complete data
hpw_lb = quantile(inc$hours.per.week, 0.25)-1.5*IQR(inc$hours.per.week)
length(inc$hours.per.week[inc$hours.per.week<hpw_lb])
'6681'

for (i in seq(hpw_lb,min(inc$hours.per.week),-3)){
  j = length(inc$hours.per.week[inc$hours.per.week < i])
  print(paste('No of outliers with lb as',round(i,0), 'is',j))
}

'
[1] "No of outliers with lb as 32 is 6681"
[1] "No of outliers with lb as 30 is 4894"
[1] "No of outliers with lb as 26 is 4726"
[1] "No of outliers with lb as 24 is 3603"
[1] "No of outliers with lb as 20 is 3473"
[1] "No of outliers with lb as 18 is 1797"
[1] "No of outliers with lb as 14 is 1011"
[1] "No of outliers with lb as 12 is 756"
[1] "No of outliers with lb as 8 is 397"
[1] "No of outliers with lb as 6 is 163"
[1] "No of outliers with lb as 2 is 32"'

#Removing extreme outliers @ cut off 18hrs per week; 3 hrs per day/ 6 days
inc = inc[inc$hours.per.week>=18,]

dim(inc)
'40694    15'
'42491-40694 = 1797 removed'

#Updating sub data's
inc_les50k = inc[inc$income=='<=50K',]
inc_abv50k = inc[inc$income=='>50K',]

#Boxplot
boxplot(inc$hours.per.week,
        horizontal = TRUE,
        col = 'coral3',
        main = 'Boxplot of Hours Per Week', las=2)

#_______________________________Native Country
str(inc$native.country)
'Factor w/ 41 levels "Cambodia","Canada",..: 39 39 39 39 39 39 39 39 39 39 ...'

table(inc$native.country)

#Barplot
barplot(table(inc$native.country),
        col = brewer.pal(8, 'Set1'),
        las=2,
        main = 'Barplot of Native Country')

par(mfrow = c(2,1))
barplot(table(inc_les50k$native.country),
        col = brewer.pal(8, 'Set1'),
        las=2,
        main = 'Native Country - Income <=50')

barplot(table(inc_abv50k$native.country),
        col = brewer.pal(8, 'Set1'),
        las=2,
        main = 'Native Country - Income >50')
par(mfrow=c(1,1))

#Note that because k-NN involves calculating distances between datapoints, 
#we must use numeric variables only. This only applies to the predictor 
#variables. The outcome variable for k-NN classification should remain a 
#factor variable.

'
1. Scaling the numerical data
2. Converting the categoricial data which are in characters to numeric,
data type remains the same
3.Spliting the data
4. Building the model'

#Scaling the numeric data
#Copying the data as a backup
inc1 = inc

str(inc1)

#Selectint the numeric variables
numeric_variables = names(which(sapply(inc1, class)=='integer'))
print(numeric_variables)
'[1] "age"             "educational.num" "capital.gain"   
[4] "capital.loss"    "hours.per.week" '
#educational.num is index of education, there is order, so not scaling this 
#variable - Scaling the numeric varibles
inc1[,numeric_variables[-2]] = scale(inc1[,numeric_variables[-2]])
inc1[,numeric_variables]

#Converting data to numbers
factor_variables = names(which(sapply(inc1, class)=='factor'))
print(factor_variables)

'[1] "workclass"      "education"      "marital.status" "occupation"    
[5] "relationship"   "race"           "gender"         "native.country"
[9] "income" '

inc1 = as.data.frame(lapply(inc1, as.numeric))
str(inc1)

#Splitting the data into train & test
set.seed(121)
select_rows_80 = sample(1:nrow(inc1), round(0.8*nrow(inc1)),replace = FALSE)         
inc_train = inc1[select_rows_80,]
inc_test = inc1[-select_rows_80,]

#________________________________Build KNN Model - kdtree
model = knn(train = inc_train, test = inc_test, 
            cl = inc_train$income, k = 5, algorithm = 'kd_tree')

pred_tab = table(model, inc_test$income)
print(pred_tab)
'model    1    2
    1 5724  864
    2  498 1053'

#Accuracy
sum(diag(pred_tab))/sum(pred_tab)
'0.8326576'

library(FNN)

detach("package:FNN", unload = TRUE)
library(class)

#________________________________Build KNN Model Cover tree
model1 = knn(train = inc_train, test = inc_test, 
            cl = inc_train$income, k = 5, algorithm = 'cover_tree')

pred_tab1 = table(model1, inc_test$income)
print(pred_tab1)
'model1    1    2
     1 5724  865
     2  498 1052'

#Accuracy
sum(diag(pred_tab1))/sum(pred_tab1)
'0.8325347'


#________________________________Build KNN Model - brute
model2 = knn(train = inc_train, test = inc_test, 
            cl = inc_train$income, k = 5, algorithm = 'brute')

pred_tab2 = table(model2, inc_test$income)
print(pred_tab2)
'model2    1    2
     1 5724  864
     2  498 1053'

#Accuracy
sum(diag(pred_tab2))/sum(pred_tab2)
'0.8326576'

#To find the optimal value of K
Accuracy = NULL;
for (i in seq(1,25,2)){
  model = knn(train = inc_train, test = inc_test, 
              cl = inc_train$income, k = i)
  pred_tab = table(model, inc_test$income)
  y = round(sum(diag(pred_tab))/sum(pred_tab),4)
  Accuracy = rbind(Accuracy,y)
  print(paste('for K is',i,'accuracy is',y))
}

plot(x = seq(1,25,2), y = Accuracy, 
     xlab='K Values', ylab = 'Accuracy', 
     col='red', type='b', pch=19,
     main='Accuracy vs K Values',xaxt="none", mgp=c(3,1,0), 
     panel.first = grid(), las=1)
axis(1,seq(1,25,2))

#xaxt="none" to remove x axis
#mgp=c(3,1,0) to adjust the labels and axis numbers
#pch = 19 to fill the bubbles
