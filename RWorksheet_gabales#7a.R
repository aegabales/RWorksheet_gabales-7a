library(Hmisc)
library(pastecs)


#1. Create a data frame for the table below 

df <- data.frame(Student = seq(1:10),
           PreTest = c(55,54,47,57,51,61,57,54,63,58),
           PostTest = c(61,60,56,63,56,63,59,56,62,61))
df

#1.a Compute the descriptive statistics using different packages (Hmisc and pastecs).
#Write the codes and its result.

library(Hmisc)
library(pastecs)

describe(df)

stat.desc(df)

#2. The Department of Agriculture was studying the effects of several levels of a
#fertilizer on the growth of a plant. For some analyses, it might be useful to convert
#the fertilizer levels to an ordered factor.

DepartmentofAgriculture <- c(10,10,10,20,20,50,10,
                             20,10,50,20,50,20,10)

#2.a Write the codes and describe the result.

ordered_factor <- sort(DepartmentofAgriculture, decreasing = FALSE)
ordered_factor

#3. Abdul Hassan, president of Floor Coverings Unlimited, has asked you to study
#the exercise levels undertaken by 10 subjects were “l”, “n”, “n”, “i”, “l” ,
#“l”, “n”, “n”, “i”, “l” ; n=none, l=light, i=intense

#3.a What is the best way to represent this in R?

#DATAFRAME
Subjects <- c("l","n","n","i","l","l","n","n","i","l")

df <- data.frame(Subjects)
df


#4.Sample of 30 tax accountants from all the states and territories of Australia and
#their individual state of origin is specified by a character vector of state mnemonics
#as:

state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld",
           "vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt",
           "wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw",
           "vic", "vic", "act")

#4.a Apply the factor function and factor level. Describe the results.

fcFunction <- factor(state)
fcFunction 


#5. From #4 - continuation:

#• Suppose we have the incomes of the same tax accountants in another vector (in
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54,
             62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48,
             65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)

#5.a Calculate the sample mean income for each state we can now use the special
#function tapply():

incmeans <- tapply(incomes, state, mean)
incmeans

#6.Calculate the standard errors of the state income means (refer again to number 3)
#6.a What is the standard error? Write the codes.

stdError <- function(x) sqrt(var(x)/length(x))
incster <- tapply(incomes, state, stdError)
incster

#7. Use the titanic dataset.

data("Titanic")
dfTitanic<- data.frame(Titanic)

#a. subset the titatic dataset of those who survived and not survived. Show the
#codes and its result.

headSubset <- subset(dfTitanic, select = "Survived")
headSubset


#8. The data sets are about the breast cancer Wisconsin. 
#The samples arrive periodically as Dr. Wolberg reports his clinical cases. 
#The database therefore reflects this chronological grouping of the data. You can create this dataset in Microsoft Excel.

#8.b Import the data from MS Excel. Copy the codes.

library(readxl)
Breast_Cancer <- read_excel("C:/Users/asus/Desktop/GABALES_BSIT2A/Breast_Cancer.xlsx")
View(Breast_Cancer)

#8.c Compute the descriptive statistics using different packages. Find the values of:
#c.1 Standard error of the mean for clump thickness.

stdCLth <- stdError(Breast_Cancer$`CL. thickness`)
stdCLth

#c.2 Coefficient of variability for Marginal Adhesion.

COV <- sd(Breast_Cancer$`Marg. Adhesion`) / mean(Breast_Cancer$`Marg. Adhesion`)* 100
COV


#c.3 Number of null values of Bare Nuclei.

Null_Values <- subset(Breast_Cancer[ , 7], `Bare. Nuclei` == "NA")
Null_Values

#c.4 Mean and standard deviation for Bland Chromatin

mean(Breast_Cancer$`Bl. Cromatin`)
sd(Breast_Cancer$`Bl. Cromatin`)

#c.5 Confidence interval of the mean for Uniformity of Cell Shape
#Calculate the mean

CalMean<- mean(Breast_Cancer$`Cell Shape`)
CalMean

#d. How many attributes?

length(attributes(Breast_Cancer))

#e. Find the percentage of respondents who are malignant. Interpret the results.
P_respond <- subset(Breast_Cancer, Class == "maligant")
P_respond

library(scales)
percent_respo <- 17  / 49
percent(percent_respo)


#9. Export the data abalone to the Microsoft excel file. Copy the codes.


library("AppliedPredictiveModeling")
data("abalone")
head(abalone)
summary(abalone)

dfabalone <- data.frame(abalone)

#Exporting the data abalone to the Microsoft excel file

library(xlsx)
write.xlsx(dfabalone, "C:/Users/asus/Desktop/GABALES_BSIT2A/Abalone.xlsx", col.names = TRUE)
