###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse)
library(dplyr) #for mutations
library(reshape2)
library(Rcpp)

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(mydata, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

#melt sub2 columns with disease so the data can be wrangled
bysex_data <- mydata%>%group_by(Sex) %>% summarize(Septicima = sum(Septicemia..A40.A41.), Malignant.Neoplasm= sum(Malignant.neoplasms..C00.C97.),
                                                   Diabetes=sum(Diabetes.mellitus..E10.E14.), Alzheimer=sum(Alzheimer.disease..G30.),
                                                   Flu=sum(Influenza.and.pneumonia..J09.J18.),
                                                   Chromic.low.resp=sum(Chronic.lower.respiratory.diseases..J40.J47.),
                                                   Other.resp=sum(Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98.),
                                                   Nephritis=sum(Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27.),
                                                   Abnormal=sum(Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99.),
                                                   Heart.diseases=sum(Diseases.of.heart..I00.I09.I11.I13.I20.I51.),
                                                   Cerebrovascular=sum(Cerebrovascular.diseases..I60.I69.),
                                                   Covid.multiple=sum(COVID.19..U071..Multiple.Cause.of.Death.), 
                                                   Covid.underlying.cause=sum(COVID.19..U071..Underlying.Cause.of.Death.))
#Melt this data so it can be used in ggplot. Make sure Rcpp and reshape2 packages are installed
melt_bysex <- melt(bysex_data, id.vars=("Sex"))

#Observe causes of death by sex using a bar chart.
p2 <- melt_bysex %>% ggplot(aes(fill = Sex, x = variable, y = value)) + 
  geom_bar(position="dodge", stat="identity") +
  xlab("Cause of Death") + ylab("Number of Deaths") +
  theme(axis.text.x = element_text(angle = 45))
plot(p2)
              
#make a pie chart bar chart of the data for diabetes by age group
sub1 <- mydata[, c(3,12)]


p1 <- sub1 %>% ggplot(aes(x = Age.group, y = Diabetes.mellitus..E10.E14.)) + 
  geom_bar(position="dodge", stat="identity", fill = "#FF6666") + 
  xlab("Age Group")+
  ylab("Deaths by Diabetes")+
  ggtitle("Deaths by Diabetes by Age")
  

#look at figure
plot(p1)

#save figures
figure_file = here("results","resultfigure1.png")
ggsave(filename = figure_file, plot=p1) 

figure_file = here("results","resultfigure2.png")
ggsave(filename = figure_file, plot=p2) 

######################################
#Data fitting/statistical analysis
######################################

# fit linear model
#lmfit <- lm(Weight ~ Height, mydata)  

# place results from fit into a data frame with the tidy function
#lmtable <- broom::tidy(lmfit)

#look at fit results
#print(lmtable)

# save fit results table  
#table_file = here("results", "resulttable.rds")
#saveRDS(lmtable, file = table_file)

  