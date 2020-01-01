## FHX to sample features table
#### install and load packages #### 

library(burnr)
library(vctrs)
library(stringr)
library(dplyr)
library(tidyverse)
library(arsenal)

#setwd("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/")
## download fhx to work with 
usb <- read_fhx("~/data_mgmt/FHX_data/vgr.fhx")
fils <- list.files("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/FHX_data", pattern=NULL, full.names = TRUE, recursive = TRUE)

## reading multiple in at a time using the read_fhx function - bind rows
tbl <- sapply(fils, read_fhx, simplify=FALSE) %>% bind_rows()

# when confirming if all series are read - remember file name may be different than 
# location acronym used in file -- IE GP101.fhx is MCN location.
substr(tbl$series, start = 1, stop = 4) %>% unique()

#use unique series in tbl to run the rest of the code below # 

#### Initalize Data with fire yrs ####

# for usb get event years // only needs to be done once to get all event years
# looks for scar events - default TRUE // injury event - default false
event.df <- (get_event_years(usb))

#### Looping through all the dataframes to add beginning fire years ####

# get the total sampleIDs to iterate through later
df_unique<- unique(usb$series)

# create dataframe for "begin recording fire" feature - to be bound to original fhx df
df_gina<-data.frame(year=NA,series=NA,Feature=NA)

for(i in seq_along(df_unique)){
  df_gina[i,1]<- min(event.df[[i]]) #function for min yr of each SampleID
  df_gina[i,2]<- names(event.df[i]) #function for variable names
  df_gina[i,3]<-"Begin recording fire" #Name the min year beginning fire year
}
#sampleID, year, fire_year, position, feature 
#abr01, 2019, 2019, dormant/begin, fire scar

# reformat columns to DB format
df_gina <- df_gina %>%
  mutate(FireYear = "") %>% # fire year should be empty for begin recording 
  mutate(Position = "Not Applicable") %>%  # new col
  rename(SampleID = series) %>% 
  rename(Year = year)

# rearrange 
df_gina <- df_gina[,c(2,1,4,5,3)]

# to bind when data has been recoded below
df_gina

#### Pipeline to get Feature and Position Columns ####

# Pipeline to rename and filter
new_usb <- usb %>%
  rename(SampleID=series) %>%
  rename(Year=year) %>%
  rename(Feature=rec_type) %>%
  dplyr::filter(Feature != "recorder_year") # will be filled out when we generate a FHX in DB

# what variables do we need to consider when coding hard values below?
# summary(usb$rec_type)

# creates position col, fire year, and edits feature column contents 
new_usb <-new_usb %>%
  mutate(FireYear = ifelse(Feature %in% grep("_fs", new_usb$Feature,value =T) == T, Year, NA)) %>%
  mutate(Position = ifelse(Feature %in% grep("_fs", new_usb$Feature, value = T) == T, gsub("_fs","", new_usb$Feature), 
                           ifelse(Feature %in% grep("_fi", new_usb$Feature, value = T) == T, gsub("_fi", "", new_usb$Feature), "Not Applicable"))) %>%
  
  mutate(Feature = ifelse(Feature %in% grep("_fi", new_usb$Feature, value = T) == T, "Undetermined scar", 
                          ifelse(Feature %in% grep("_fs", new_usb$Feature, value = T) == T,"Fire scar",
                                 ifelse(Feature %in% grep("inner", new_usb$Feature, value = T) ==T, "Innermost ring", 
                                        ifelse(Feature %in% grep("outer", new_usb$Feature, value = T) ==T, "Outermost ring", 
                                               ifelse(Feature %in% grep("pith", new_usb$Feature, value = T) ==T, "pith", 
                                                      ifelse(Feature %in% grep("bark", new_usb$Feature, value = T) ==T, "Bark", Feature))))))) %>% 
  mutate(Position = recode(Position, 
                           latewd = "latewood", falldormant = "fall dormant", 
                           early = "early earlywood", late = "late earlywood",
                           middle = "Middle earlywood")) 

# rearrange to DB format
new_usb<-new_usb[,c(2,1,4,5,3)]

# check this puppy out
head(new_usb)

# bind beginning years to df
new_usb<-rbind(new_usb,df_gina)

# Can only assume one sample per tree // Add A to every sample in the SampleID
new_usb$SampleID <- paste(new_usb$SampleID, "A", sep="")

####presto!####
write.csv(new_usb,file="C:/Users/bgonzalez/Desktop/R_Analysis/tricky_challenge/ssn.csv", na = "")

# ideally with every FHX from same site I make a for loop to run through the files and export a CSV that has results bound from loop

# for now however.... 
# with multiple FHX that belong to one site, paste into the master csv file
write.table(new_usb, "C:/Users/bgonzalez/Desktop/R_Analysis/tricky_challenge/gp_master.csv", sep = ",",
            col.names = !file.exists("C:/Users/bgonzalez/Desktop/R_Analysis/tricky_challenge/gp_master.csv"), append = T)


#### generate site membership ####
site_mem <-as.data.frame(unique(usb$series))
write.csv(site_mem, file="C:/Users/bgonzalez/Desktop/R_Analysis/tricky_challenge/site_members.csv", na = "")


#### Q/C ####

a <- read_fhx("C:/Users/bgonzalez/Desktop/Bianca_2019/Access_databases/Jemez.database.merge/efk_dbb.fhx")
b <- read_fhx("C:/Users/bgonzalez/Desktop/R_Analysis/jemez.final.fhx.files/efk.FHX")

# find unique trees from each FHX from one site and compare trees to ensure no duplication of trees 
new_usb %>% group_by(SampleID) %>% tally()

#rearrange to compare dataframes 
a<-a %>% 
  arrange(year,series,rec_type)

b<-b %>% 
  arrange(year,series,rec_type)

all_equal(a,b, convert = TRUE)

# ERROR: different number of rows - ok which one has greater length?
lapply(list(a,b), dim)

# actually, let's look at the records in b that are not in a 
bj<-anti_join(b, a, by=c('year','series', 'rec_type'))

# let's look at the records in a that are not in b
aj<-anti_join(a, b, by=c('year','series', 'rec_type'))

unique(aj$series)

# so it looks like B(old) stops at 1896 for recorder year and doesn't continue to 1901 like A (DB) does

#### other Q/C operations ####
#compare unique series
U_A<-as.data.frame(unique(a$series)) %>%rename(id=`unique(a$series)`) %>%  arrange(id)
U_B<-as.data.frame(unique(b$series)) %>% rename(id=`unique(b$series)`) %>%  arrange(id)

all_equal(U_A,U_B, convert = TRUE)

# what values are missing? Is it after dimension ends in A?
b_adim<-b[1:3577,]
b_adim<-b_adim %>% 
  arrange(year,series,rec_type)

all_equal(a,b_adim, convert = TRUE)

cbind(U_A,U_B)
# latewd_fs in A and late_fs in B (original db) 

# list of FHX files I've generated : 

# multiple FHX files - can add all of them in but how do I do quality control on these if I cannot compare individual objects? -
# would we have to do it one at a time and delete the old sample features for that site? 

# FRM - database fhx has 10 extra entries - # so it looks like B(old) stops at 1896 for recorder year and doesn't continue to 1901 like A (DB) does
# the oldest one were not created with notepad++


# references: 
# http://zevross.com/blog/2014/08/05/using-the-r-function-anti_join-to-find-unmatched-records/ 

