library(rvest)
library(tidyverse)
library(stringr)


web <- read_html("http://www.voterfocus.com/ws/pfinder/pl_County.php?op=showall&county=broward&name_and_zip=Y")
tbl <- web %>% html_table(header = T, fill = T)
df <- tbl[[2]]
df <- df[,1:2]

colspec <- cols(
  RegNum = col_character(),
  Precinct = col_character(),
  VoteDate = col_character()
)


broward <- read_delim("../Data/Parsed/BrowardEarlyVotingEVID2016General-parsed.txt", delim = "\t",  col_types = colspec)

broward <- broward %>% 
  separate(VoteDate, sep = " ", into = c("date", "time")) %>%
  mutate(time = str_replace(time, "Z", ""), 
         Precinct = str_sub(Precinct, 1, 4)) %>%
  left_join(df, by = "Precinct")

broward <- broward %>% 
  rename(voterid = RegNum,
         location = Location,
         precinct = Precinct) %>%
  select(location, date, time, precinct, voterid)

write.table(broward,"../Data/Parsed/BrowardEarlyVotingEVID2016General-parsed.txt", sep = ";", row.names = F)
