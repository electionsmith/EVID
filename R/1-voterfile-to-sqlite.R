library(tidyverse)
library(lubridate)
library(stringr)
library(RSQLite)


# Create file.sqlite3 -----------------------------------------------

my_db <- src_sqlite("../Data/evid-db.sqlite3", create = TRUE)              # create src


# Create voter history table -----------------------------------------------

nms_h <- c("CountyCode", "VoterID", "ElectionDate", "ElectionType", "HistoryCode")
history <- read_delim("../Data/Parsed/voterfile-history-jan-2016.txt", delim = "\t", col_names = nms_h, col_types = cols("c", "c", "c", "c", "c"), quote = "") # read 2016 history
history <- history %>% separate(ElectionDate, sep = "-", into = c("y", "m", "d")) %>% unite_("ElectionDate", c("m", "d", "y"), sep = "/")
copy_to( my_db, history, "history", temporary = FALSE, indexes = list("VoterID"))                 # create table


files  <- list.files("../Data/Parsed/VoterFile-Dec-2014/VoterHistory",full.names =  T)

for (i in 1:length(files)){
  print(files[i])
  file <-  read_delim(files[i], delim = "\t", col_names = nms_h, col_types = cols("c", "c", "c", "c", "c"), quote = "")
  db_insert_into( con = my_db$con, table = "history", values = file)  #Insert all files into db
  print("done")
}


# Create extract table -----------------------------------------------

nms<-c("CountyCode","VoterID","NameLast","NameSuffix","NameFirst","NameMiddle","Requestedpublicrecordsexemption","ResidenceAddressLine1","ResidenceAddressLine2","ResidenceCity(USPS)","ResidenceState","ResidenceZipcode","MailingAddressLine1","MailingAddressLine2","MailingAddressLine3","MailingCity","MailingState","MailingZipcode","MailingCountry","Gender","Race","BirthDate","RegistrationDate","PartyAffiliation","Precinct","PrecinctGroup","PrecinctSplit","PrecinctSuffix","VoterStatus","CongressionalDistrict","HouseDistrict","SenateDistrict","CountyCommissionDistrict","SchoolBoardDistrict","DaytimeAreaCode","DaytimePhoneNumber","DaytimePhoneExtension","Emailaddress")


tps <- cols("c","c","c","c","c","c","c","c","c","c","c","i","c","c","c","c","c","c","c","c","i","c","c","c","c","i","c","c","c","i","i","i","i","i","i","i","c","c")
extract <- read_delim("../Data/Parsed/voterfile-extract-jan-2017.txt", delim = "\t", col_names = nms, na = c("NA", "", " ", "*"), col_types = tps , quote = "")
extract <- extract %>% 
  select(VoterID, NameFirst, NameMiddle, NameLast,CountyCode, Gender, Race, BirthDate, RegistrationDate, PartyAffiliation, VoterStatus, ResidenceZipcode, Precinct, PrecinctSplit) %>%
  mutate(ResidenceZipcode = str_sub(ResidenceZipcode,1,5))
names(extract) <- tolower(names(extract))
copy_to( my_db, extract, "extract", temporary = FALSE, indexes = list("voterid"))   

