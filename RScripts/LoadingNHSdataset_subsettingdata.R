# Title: Loading NHSRdatasets

# Script and data information: 

#View, explore and tabulate the NHS England accident and emergency attendances and admissions (ae_attendances) data set 

#A subset of the variables needed was selected. 
#It was partitioned the data subset into training and testing data and save them to your working ‘Data’ folder for downstream exploratory analysis

# Data consists of date, numerical data and character data from NHSRdatasets package.
# Copyright statement: This script is the product of University of Edinburgh.


# Background
#The data are from the NHSRdatasets package. 

# It contains the dataset selected for this exploratory analysis:

#NHS England accident and emergency attendances and admissions (ae_attendances). 
#Reported attendances, four-hour breaches and admissions for all A&E departments in England 
#for the years 2016/17 through 2018/19 (Apr-Mar).

## Load packages and data
#We have to load the packages and data needed for this script.
library(NHSRdatasets)
library(tidyverse) #The tidyverse is a collection of R packages designed for data science
library(here) # enables easy file referencing in project-oriented workflows
library(knitr) # integrates code into text documents. 
library(scales) #provides the internal scaling infrastructure to ggplot2 and its functions allow programmers to customise the transformations, breaks, guides and palettes used in visualisations. 
library(lubridate) #provides tools that make it easier to manipulate dates in R.
library(caret) #a set of functions that attempt to streamline the process for creating predictive model

#Here is the code to load the NHSRdatasets from the NHSRdatasets R package.

#Load the NHS England accident and emergency attendances and admissions (ae_attendances) data.
data(ae_attendances)

##  have a look at the ae_attendances data
data(ae_attendances)
ae<-ae_attendances
class(ae)

#The tbl_df class is a subclass of data.frame. 

ae

#An overview of your data is returned when a tibble is printed to the RStudio console. 
#You can see the ae_attendances tibble consists of 12,765 rows of data and six columns with different classes. 
#Dataset contains 6 variables: one date variable, period, two character variables (or factors), org_code and type, and three numeric (double precision) variables, attendances, breaches and admissions. 

#The dataset contains:     

#period: the month that this activity relates to, stored as a date (1st of each month).  

#org_code: the Organisation data service (ODS) code for the organisation. 
#The ODS code is a unique code created by the Organisation data service within NHS Digital,
#and used to identify organisations across health and social care. 
#ODS codes are required in order to gain access to national systems like NHSmail and the Data Security and Protection Toolkit. 
#If you want to know the organisation associated with a particular ODS code, 
#you can look it up from the following address: <https://odsportal.digital.nhs.uk/Organisation/Search>. 
#For example, the organisation associated with the ODS code 'AF003' is
#Parkway health centre <https://odsportal.digital.nhs.uk/Organisation/OrganisationDetails?organisationId=132839&showOpenChildredOnly=True>.      

#type: the Department Type for this activity, either  
#1: Emergency departments are a consultant led 24 hour service with full resuscitation facilities and designated accommodation for the reception of accident and emergency patients,  
# 2: Consultant led mono specialty accident and emergency service (e.g. ophthalmology, dental) with designated accommodation for the reception of patients, or  
# other: Other type of A&E/minor injury activity with designated accommodation for the reception of accident and emergency patients. The department may be doctor led or nurse led and treats at least minor injuries and illnesses and can be routinely accessed without appointment. A service mainly or entirely appointment based (for example a GP Practice or Out-patient clinic) is excluded even though it may treat a number of patients with minor illness or injury. Excludes NHS walk-in centres.[(National Health Service, 2020)](https://eu01.alma.exlibrisgroup.com/leganto/public/44UOE_INST/citation/37459630310002466?auth=SAML) 

#attendances: the number of attendances for this department type at this organisation for this month.    

#breaches: the number of attendances that breached the four hour target.   

#admissions: the number of attendances that resulted in an admission to the hospital.(Chris Mainey, 2021)

## Let's view at the ae_attendances data
#The `glimpse()` function is from tibble package and is great to view the columns/variables in a data frame. 
#It also shows data type and some of the data in the data frame in each row. 
#The tibble package provides utilities for handling tibbles. 
#The tibble package is loaded by the tidyverse package, as one of its core components. 

glimpse(ae) 

#Here is the output of the `glimpse()' function. It starts off with the number of rows and columns and each column in separate rows.

#The `head()` function let's you get a look at top n rows of a data frame. By default it shows the first 6 rows in a data frame.
head(ae)


## Missing data
#reducing the representativeness of the selected sample. We therefore need to check for missing data in the ae_attendances data.  which can significantly impact any attempt to gain useful insight from data.  

## Calculate how many NAs there are in each variable
ae %>% 
  map(is.na) %>%
  map(sum)


# Let's add an index link column to ae_attendances data  
#Separating data into training and testing sets is vital for evaluating data collection and analysis tools. 

#To develop and evaluate your data capture tool, you will need to split the raw data into training and testing sets. 
#We, therefore, need to add an index column to the raw data so we can link the partitioned data sets to the raw data 
#We will use the `rowid_to_column()` function to convert row identities to a column named index.

ae <- rowid_to_column(ae, "index")


## Let's tablulate the raw data for your report

ae %>%
  mutate_at(vars(period), format, "%b-%y") %>% 
  mutate_at(vars(attendances, breaches, admissions), comma) %>%
  head(10) %>%
  kable()


# Let's save the raw ae_attendances data to your 'RawData' folder
#We will use the here package to build a path relative to the top-level directory to write the 
#raw ae_attendances data to your 'RawData' folder. The goal of the here package is to enable easy file referencing in 
#project-oriented workflows. In contrast to using `setwd()` function, which is fragile and dependent on the way you organise your files, 
#here uses the top-level directory of a project to easily build paths to files.
write_csv(ae, here("RawData", "ae_attendances.csv"))

# Selecting variables for your data capture tool
#It really important point to note, is that you are not going to need the full ae_attendances dataset develop your data collection tool. 
#For example if you are interested in exploring four hours performance for England as a whole you will only need the variables: index, 
#period, attendances, breaches. Likewise, you may wish examine performance for the different types of department, 
#we would also need the type variable. For the purpose of this lesson, we will focus on  England as a whole.


## Let's select the ae_attendances data subset for further exploratory analysis
#Based on this brief probe of the of the ae_attendances data, 
#we have decide we wish to explore this data further for the development and evaluation your data capture tool. 
#Let's calculate monthly four hour waiting time target performance for England as a whole
ae<- ae %>%
  mutate(performance = round(1- breaches / attendances,2))

### Let's tabulate the subsetted ae_attendances data for your report
ae %>%
  mutate_at(vars(period), format, "%b-%y") %>% 
  mutate_at(vars(attendances, breaches), comma) %>%
  head(10) %>%
  kable()


### Let's save provisional subsetted ae_attendances data to the 'RawData' folder
#Of note, when naming folders and files, it is important that you do so in a consistent, logical and predictable way means that information may be located, identified and retrieved by your and your colleagues, as quickly and easily as possible. With this in mind let's name this file "ae_attendances_ENG_4hr_perfom", and write it to the raw data folder.
glimpse(ae)

#chosen variables (6)
#Index, period, org_ code, attendance, breaches and performance

#filter type 1 departments and select variables for the data capture tool
ae2<-ae %>%
  filter(type=="1") %>%
  select(-type, -admissions) 

ae2 %>%
  mutate_at(vars(period), format, "%b-%y") %>% 
  mutate_at(vars(attendances, breaches), comma) %>%
  head(10) %>%
  kable()

write_csv(ae2, here("RawData", "ae_type1_performance.full.csv"))


## Separating provisional ae_attendances_ENG_4hr_perfom data into training and testing sets
#To develop and evaluate your data capture tool, you will need to splint the raw data into test and training data sets.
#We have added an index column earlier, so we can link the partitioned data sets to 
#the raw ae_attendances and ae_attendances_ENG_4hr_perfom if required in the future. 


### How many rows are in the ae_attendances_ENG_4hr_perfom dataset?
#The ae_attendances_ENG_4hr_perfom dataset is large with 
nrow(ae2) #rows of data


#We do not want you to spend hours inputting rows and rows of data into your data capture tool. 
#A test data set of 10-15 records to capture with and evaluate your data capture tool is sufficient for the purpose of the course assignment. 
#Here is how to work out the proportion (`prop`) of the raw data to assign to the training data:
prop<-(1-(15/nrow(ae2)))

#The proportion of the raw that needs to be assigned to the training data to ensure there is only 10 to 15 records in the test data is: 
print(prop)

#We will use the createDataPartition() function from the caret package to splint our raw data into test and training data sets.
#The 'set.seed()' function is a random number generator, which is useful for creating random objects that can be reproduced. 
#This will make sure that every time we run this script, we will partition the raw data into the same test and training data.
set.seed(333)

#Partitioning the raw data into the test and training data.
trainIndex <- createDataPartition(ae2$index, p = prop, 
                                  list = FALSE, 
                                  times = 1)

head(trainIndex)

# All records that are in the trainIndex are assigned to the training data.
ae2Train <- ae2[ trainIndex,]

nrow(ae2Train)
#There are 12,753 records in your training data. That is a large dataset!

### Let's tabulate ae_attendances_ENG_4hr_perfom training data for your report
ae2Train %>%
  mutate_at(vars(period), format, "%b-%y") %>% 
  mutate_at(vars(attendances, breaches), comma) %>%
  head(10) %>%
  kable()


### Our next task, it to save ae_attendances_ENG_4hr_perfom training data to your working data folder 'Data'
write_csv(ae2Train, here("Data", "ae_type1_performance_train_full.csv"))


### Let's extract the ae_attendances_ENG_4hr_perfom test data
#All records that are not in the trainIndex (`-trainIndex`) are assigned to the test data.
ae2Test  <- ae2[-trainIndex,]

nrow(ae2Test)
#There are 12 records in your test data. Perfect! 

#You now need to set aside the first record from the ae_attendances_ENG_4hr_perfom test data so that your markers 
#can test and evaluate your data-capture tool.
ae2TestMarker  <- ae2Test[1,]

#### Let's tabulate ae_attendances_ENG_4hr_perfom marker test data for your report
ae2TestMarker  %>%
  mutate_at(vars(period), format, "%b-%y") %>% 
  mutate_at(vars(attendances, breaches), comma) %>%
  head(10) %>%
  kable()

### Our next task, it to save our ae_attendances_ENG_4hr_perfom marker test data to our working data folder 'Data'
write_csv(ae2TestMarker, here("Data", "ae_type1_performance_test_marker_full.csv"))

### We then need to set aside the remaining records for you to test (or collect with your) your data-capture tool.
ae2Test  <- ae2Test[2:nrow(ae2Test),]


#### Let's tabulate ae_attendances_ENG_4hr_perfom test data for your report
ae2Test  %>%
  mutate_at(vars(period), format, "%b-%y") %>% 
  mutate_at(vars(attendances, breaches), comma) %>%
  head(10) %>%
  kable()


### Our final task, is to save our ae_attendances_ENG_4hr_perfom test data to our working data folder 'Data'
write_csv(ae2Test, here("Data", "ae_type1_performance_test_full.csv"))
