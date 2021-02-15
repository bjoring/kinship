library(tidyverse)

# Convert records into long format
blong <- births %>% 
  gather(v, value, Subject.Last:WitnessB.Location) %>% 
  separate(v, c("var", "col")) %>% 
  arrange(ActID) %>% 
  spread(col, value) %>%
  select(-Priest) %>%
  mutate_if(is.character, list(~na_if(., "NA"))) %>%
  filter(!is.na(First)|!is.na(Last))
dlong <- deaths %>% 
  gather(v, value, Subject.Last:WitnessB.Location) %>% 
  separate(v, c("var", "col")) %>% arrange(ActID) %>% 
  spread(col, value) %>%
  select(-c(OtherWitnesses,Priest)) %>%
  mutate_if(is.character, list(~na_if(., "NA"))) %>%
  filter(!is.na(First)|!is.na(Last))
mlong <- marry %>% 
  gather(v, value, SubjectA.Last:WitnessD.Location) %>% 
  separate(v, c("var", "col")) %>% 
  arrange(ActID) %>% 
  spread(col, value) %>%
  select(-c(OtherWitnesses,Priest)) %>%
  mutate_if(is.character, list(~na_if(., "NA"))) %>%
  filter(!is.na(First)|!is.na(Last))

# Bind records together
records <- bind_rows(blong,dlong,mlong)
records <- left_join(records, affilclean, by = c("Affil"="Subject.Affil"))
records <- records %>% select(ParishID, ActID, Act, ActDate, EventDate, var, Last, First, Clean, Affil, Location, Sex, Age)

