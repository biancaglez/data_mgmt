######################################################
# Purpose: Script manipulating legacy FHX fire data files using the burnR package. 
# Converts FHX objects to CSV output with the necessary format for USGS access databases. 
# Once data is imported from csv output to access, a new database generated FHX file is made, 
# where FHX objects are then compared for Q/C.

# Developped by: Bianca Gonzalez - contribution from Taylor Edwards
# 01/01/2019, bianca_gonzalez@partner.nps.gov
######################################################
#### install and load packages #### 

library(burnr)
library(vctrs)
library(stringr)
library(dplyr)
library(tidyverse)
library(arsenal)

#setwd("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/")

fils <- list.files("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/FHX_data", pattern=NULL, full.names = TRUE, recursive = TRUE)

## reading multiple files using the read_fhx function - bind rows
tbl <- sapply(fils, read_fhx, simplify=FALSE) %>% bind_rows()

# when confirming if all series are read - remember file name may be different than 
# location acronym used in file -- IE GP101.fhx is MCN location.
unq<-substr(tbl$series, start = 1, stop = 3) %>% unique()

### add only when a letter from alphabet- how to grab all of them when there are 3-5 chars in series name?

# for multiple FHX files (unique sites) - run below code
for(i in seq_along(unq)){
  
  # if running code on single FHX file - comment out for loop and bring in single file & run code
  # temp <- read_fhx("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/FHX_data/vgr.fhx")
  
  # make a temp of unique series
  t<-tbl %>% mutate(srs_stat = ifelse(series %in% grep(unq[i], tbl$series, value = T)
                                      ==T, "TRUE", "FALSE")) %>% filter(srs_stat=="TRUE")
  temp<- as_fhx(t[1:3]) #make FHX and drop last col
  
  # use temp to run code and dump into master file
  event.df<- (get_event_years(temp))
  
  # get the total sampleIDs to iterate through later
  df_unique<- unique(temp$series)
  
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
  
  # rearrange - to bind when data has been recoded below
  df_gina <- df_gina[,c(2,1,4,5,3)]

  #### Pipeline to get Feature and Position Columns ####
  new_temp <- temp %>%
    rename(SampleID=series) %>%
    rename(Year=year) %>%
    rename(Feature=rec_type) %>%
    dplyr::filter(Feature != "recorder_year") # will be filled out when we generate a FHX in DB

  # creates position col, fire year, and edits feature column contents 
  new_temp <-new_temp %>%
    mutate(FireYear = ifelse(Feature %in% grep("_fs", new_temp$Feature,value =T) == T, Year, NA)) %>%
    mutate(Position = ifelse(Feature %in% grep("_fs", new_temp$Feature, value = T) == T, gsub("_fs","", new_temp$Feature), 
                             ifelse(Feature %in% grep("_fi", new_temp$Feature, value = T) == T, gsub("_fi", "", new_temp$Feature), "Not Applicable"))) %>%
    
    mutate(Feature = ifelse(Feature %in% grep("_fi", new_temp$Feature, value = T) == T, "Undetermined scar", 
                            ifelse(Feature %in% grep("_fs", new_temp$Feature, value = T) == T,"Fire scar",
                                   ifelse(Feature %in% grep("inner", new_temp$Feature, value = T) ==T, "Innermost ring", 
                                          ifelse(Feature %in% grep("outer", new_temp$Feature, value = T) ==T, "Outermost ring", 
                                                 ifelse(Feature %in% grep("pith", new_temp$Feature, value = T) ==T, "pith", 
                                                        ifelse(Feature %in% grep("bark", new_temp$Feature, value = T) ==T, "Bark", Feature))))))) %>% 
    mutate(Position = recode(Position, 
                             latewd = "latewood", falldormant = "fall dormant", 
                             early = "early earlywood", late = "late earlywood",
                             middle = "Middle earlywood")) 
  
  # rearrange to DB format
  new_temp<-new_temp[,c(2,1,4,5,3)]

  # bind beginning years to df
  new_temp<-rbind(new_temp,df_gina)
  
  # Can only assume one sample per tree // Add A to every sample in the SampleID
  new_temp$SampleID <- paste(new_temp$SampleID, "A", sep="")
 
  # with multiple FHX that belong to one site, paste into the master csv file
  write.table(new_temp, "/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/new_temp.csv", sep = ",",
              col.names = !file.exists("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/new_temp.csv"), append = T)
  
  #### generate site membership ####
  
  # generates site membership for single or multiple FHX to go in the site memberships table
  site_mem <-as.data.frame(unique(temp$series)) %>% rename(site_id =`unique(temp$series)`)
  write.table(site_mem, file="/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/site_members.csv", sep = ",",
              col.names = !file.exists("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/site_members.csv"), append = T)
  
  
}

#### Q/C ####

a <- read_fhx("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/FHX_data/vgr.fhx")
b <- read_fhx("/Users/BiancaGonzalez/Desktop/Bandelier USGS/R Analysis/data_mgmt/data_mgmt/FHX_data/EFK.FHX")

# find unique trees from each FHX from one site and compare trees to ensure no duplication of trees 
new_temp %>% group_by(SampleID) %>% tally()

fhxs<-list(a,b)
#rearrange to compare dataframes 
# lapply(fhxs, arrange) - 

a<-a %>% 
  arrange(year,series,rec_type)

b<-b %>% 
  arrange(year,series,rec_type)

all_equal(a,b, convert = TRUE)

# why are they differing in number of rows - ok which one has greater length?
lapply(list(a,b), dim)

# actually, let's look at the records in b that are not in a 
bj<-anti_join(b, a, by=c('year','series', 'rec_type'))

# let's look at the records in a that are not in b
aj<-anti_join(a, b, by=c('year','series', 'rec_type'))

unique(aj$series)

# so it looks like B(old) stops at 1896 for recorder year and doesn't continue to 1901 like A (DB) does


# make a list of FHX files that have the same site code as other FHX files -- will have to run the script without the for loop for these

#### other Q/C ####
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
