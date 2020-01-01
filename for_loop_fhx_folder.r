## FHX to sample features table
## Taylor Edwards, Bianca Gonzalez


#### install and load packages #### 
install.packages('dplyr')
library(burnr)
library(vctrs)
library(stringr)
library(dplyr)
library(tidyverse)

## download fhx to work with 
#code from https://stackoverflow.com/questions/11433432/how-to-import-multiple-csv-files-at-once

setwd("C:/Users/bgonzalez/Desktop/R_Analysis/jemez.final.fhx.files")
temp = list.files(pattern="*.fhx")


#assign the fhx value to the name of the file - iteration to the read_fhx function
for (i in 1:length(temp)) assign(temp[i], read_fhx(temp[i]))


#use list of files already made and consolidate
bind_rows(temp)


?read.delim
#### Initalize Data with fire yrs ####

#for usb get event years // only needs to be done once to get all event years
event.df <- (get_event_years(usb))

#### Looping through all the dataframes to add beg_record_fire years ####

#get the total # sampleIDs to iterate through later
df_unique<- unique(usb$series)

#create dataframe for "begin recording fire" feature - to be bound to original fhx df
df_bob<-data.frame(year=NA,series=NA,rec_type=NA)

for(i in seq_along(df_unique)){
  df_bob[i,1]<- min(event.df[[i]]) #function for min yr of each SampleID
  df_bob[i,2]<- names(event.df[i]) #function for variable names
  df_bob[i,3]<-"beg_fire_yr_fs" #Name the min year beginning fire year
}

#bind bob to original FHX
bbind<- rbind(df_bob, usb)

#construct the -table-
# pipeline
new_usb <- bbind %>%
  rename(SampleID=series) %>%
  rename(Year=year) %>%
  rename(Feature=rec_type) %>%
  filter(Feature != "recorder_year") %>%
  mutate(FireYear = ifelse(Feature %in% grep("_fs", new_usb$Feature,value =T) == T, Year, "NA")) %>%
  mutate(Position = ifelse(Feature %in% grep("_fs", new_usb$Feature, value = T) == T, gsub("_fs","", new_usb$Feature), "NA")) %>%
  mutate(Position = recode(Position, 
                           latewd = "latewood", latelw = "late latewood", falldormant = "fall dormant", 
                           earlylw = "early latewood", early = "early earlywood"))


#Change the column order so we can paste into the DB directly
new_usb2<-new_usb[,c(2,1,4,5,3)]

#have to set absolute path of filename
write.csv(new_usb2,file="C:/Users/bgonzalez/Desktop/R_Analysis/tricky_challenge/data.csv")
getwd()