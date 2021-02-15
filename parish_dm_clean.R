library(tidyverse)
library(lubridate)

# Read in all registries
registries <- list.files("Registers", "parish.+cards.csv")

# Clean death records
deathlist <- lapply(registries, clean_deaths)
deaths <- do.call("rbind", deathlist)
cols_to_be_rectified <- names(deaths)[vapply(deaths, is.character, logical(1))]
deaths[,cols_to_be_rectified] <- lapply(deaths[,cols_to_be_rectified], trimws)
deaths <- deaths %>% mutate_if(is.character, list(~na_if(., "NA")))

# Clean marriage records
marrylist <- lapply(registries, clean_marry)
marry <- do.call("rbind", marrylist)
cols_to_be_rectified <- names(marry)[vapply(marry, is.character, logical(1))]
marry[,cols_to_be_rectified] <- lapply(marry[,cols_to_be_rectified], trimws)
marry <- marry %>% mutate_if(is.character, list(~na_if(., "NA")))

clean_deaths <- function(registry) {
  # Read in registry data
  reg <- read.csv(paste0("Registers/",registry), header = FALSE, sep = ",", col.names = paste0("V",seq_len(300)), fill = TRUE, na.strings="")
  
  # Get parish ID
  pid <- str_split(registry,"_",3,simplify=TRUE)[2]
  
  # Filter for affiliation
  reg <- reg %>% 
    filter(!is.na(V8)) %>% 
    mutate(V3 = parse_date_time(V3, "%Y%m%d", truncated=2), V4 = parse_date_time(V4, "%Y%m%d", truncated=2)) %>%
    mutate(V1 = sapply(str_split(V1,"/"), tail, 1)) %>%
    distinct()
  
  # Filter for burials
  deaths <- reg %>% 
    filter(V2 == "s")
  
  # Only include burials of people who have Indigenous affiliations
  deaths <- subset(deaths, deaths$V8 %in% affilclean$Subject.Affil[!is.na(affilclean$Clean)])
  
  # Gather subject demographic data
  deaths <- deaths %>% 
    mutate(demog = paste(V10,V11,V12,V13, sep=" "),
           V11 = ifelse(str_detect(demog, "m\\."), "M", NA),
           V11 = ifelse(str_detect(demog, "f\\."), "F", V11),
           V12 = str_extract(demog, "\\d+"),
           V12 = as.numeric(V12),
           V12 = ifelse(!is.na(V12) & str_detect(demog, "\\d+(?=m)"), V12/12.0, V12),
           V12 = ifelse(!is.na(V12) & str_detect(demog, "\\d+(?=j)"), V12/12.0/30.0, V12),
           surname = ifelse(!is.na(V6), trimws(V5, which="both"), NA),
           V6 = ifelse(is.na(V6), trimws(V5, which="both"), trimws(V6, which="both")),
           V5 = surname) %>%
    unite("mergeinfo", V13:last_col(2),sep = " / ") %>%
    mutate(ParishID = as.numeric(pid)) %>%
    select(ParishID,V1:V6,V8,V9,V11,V12,mergeinfo)
  
  # Apply better column names
  
  colnames(deaths) <- c("ParishID","ActID", "Act", "ActDate", "EventDate", "Subject.Last", "Subject.First", "Subject.Affil", "Subject.Location", "Subject.Sex", "Subject.Age", "Info")
  
  # Capture and separate father and mother info
  deaths <- deaths %>%
    separate(Info, c("Info.A", "Info.B", "Info.C", "Info.D", "Info.E", "Info.F", "Info.G", "Info.H"), sep = "(/\\sNA\\s?){3,}", extra = "merge", fill = "right") %>%
    mutate(Father.First = ifelse(str_detect(Info.A, "Father"), Info.A, 
                                 ifelse(str_detect(Info.B, "Father"), Info.B, 
                                        ifelse(str_detect(Info.C, "Father"), Info.C, NA))),
           Mother.First = ifelse(str_detect(Info.A, "Mother"), Info.A,
                                 ifelse(str_detect(Info.B, "Mother"), Info.B,
                                        ifelse(str_detect(Info.C, "Mother"), Info.C, NA))),
           Spouse.First = ifelse(str_detect(Info.A, "Spouse"), Info.A,
                                 ifelse(str_detect(Info.B, "Spouse"), Info.B,
                                        ifelse(str_detect(Info.C, "Spouse"), Info.C, NA))),
           Info.A = ifelse(str_detect(Info.A, "[A-Z][a-z]+"), NA, Info.A),
           Info.B = ifelse(str_detect(Info.B, "[A-Z][a-z]+"), NA, Info.B),
           Info.C = ifelse(str_detect(Info.C, "[A-Z][a-z]+"), NA, Info.C)) %>%
    unite("Info", Info.A:Info.H, sep=",") %>%
    mutate(Info = str_remove(Info, "^/?\\s?(NA,)+"),
           Info = str_remove(Info, "(NA,)+"),
           Info = str_remove(Info, "(,+?NA)+$"),
           Info = ifelse(Info == "NA", NA, Info)) %>%
    separate(Info, c("Info.A", "Info.B", "Info.C", "Info.D","Info.E","Info.F", "Info.G", "Info.H"), sep = ",", extra = "merge", fill = "left") %>%
    mutate(Priest = ifelse(str_count(Info.H, "/") != 4, Info.H, NA),
           Info.H = ifelse(str_count(Info.H, "/") != 4, NA, Info.H)) %>%
    unite("Info", Info.A:Info.H, sep=",") %>%
    mutate(Info = str_remove(Info, ",NA$"),
           Info = str_remove(Info, "(NA,)+"),
           Info = ifelse(Info == "NA", NA, Info)) %>%
    separate(Info, c("WitnessA.First","WitnessB.First","OtherWitnesses"), sep=",", extra="merge", fill="right")
  
  # Gather and clean mother, father, and spouse data
  deaths <- deaths %>%
    mutate(Father.Affil = ifelse(!is.na(Father.First), str_extract(Father.First, "(?<=Father).+$"), NA),
           Mother.Affil = ifelse(!is.na(Mother.First), str_extract(Mother.First, "(?<=Mother).+$"), NA),
           Spouse.Affil = ifelse(!is.na(Spouse.First), str_extract(Spouse.First, "(?<=Spouse).+$"), NA),
           Father.First = ifelse(!is.na(Father.First), str_extract(str_remove(Father.First, "^[\\s/]+"), "^.+(?=Father)"), NA),
           Mother.First = ifelse(!is.na(Mother.First), str_extract(str_remove(Mother.First, "^[\\s/]+"), "^.+(?=Mother)"), NA),
           Spouse.First = ifelse(!is.na(Spouse.First), str_extract(str_remove(Spouse.First, "^[\\s/]+"), "^.+(?=Spouse)"), NA),
           Father.First = ifelse(!is.na(Father.First), str_remove(Father.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           Mother.First = ifelse(!is.na(Mother.First), str_remove(Mother.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           Spouse.First = ifelse(!is.na(Spouse.First), str_remove(Spouse.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           Father.Affil = ifelse(!is.na(Father.Affil), str_remove(Father.Affil, "^\\s?/\\s"), NA),
           Mother.Affil = ifelse(!is.na(Mother.Affil), str_remove(Mother.Affil, "^\\s?/\\s"), NA),
           Spouse.Affil = ifelse(!is.na(Spouse.Affil), str_remove(Spouse.Affil, "^\\s?/\\s"), NA)) %>%
    separate(Father.First, c("Father.Last","Father.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(Mother.First, c("Mother.Last","Mother.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(Spouse.First, c("Spouse.Last","Spouse.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(Father.Affil, c("Father.Affil","Father.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(Mother.Affil, c("Mother.Affil","Mother.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(Spouse.Affil, c("Spouse.Affil","Spouse.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right")
  
  # Gather and clean information for other witnesses
  deaths <- deaths %>%
    mutate(WitnessA.First = ifelse(!is.na(WitnessA.First), str_remove(WitnessA.First, "^\\s?/\\s"), NA),
           WitnessB.First = ifelse(!is.na(WitnessB.First), str_remove(WitnessB.First, "^\\s?/\\s"), NA),
           WitnessA.Affil = ifelse(str_count(WitnessA.First, "/") >= 3, str_remove(WitnessA.First, "(?:.*?/){3}"), NA),
           WitnessA.First = ifelse(str_count(WitnessA.First, "/") >=3, str_extract(WitnessA.First, "(?:.*?/){1}[\\s\\w-]+"), WitnessA.First),
           WitnessB.Affil = ifelse(str_count(WitnessB.First, "/") >= 3, str_remove(WitnessB.First, "(?:.*?/){3}"), NA),
           WitnessB.First = ifelse(str_count(WitnessB.First, "/") >=3, str_extract(WitnessB.First, "(?:.*?/){1}[\\s\\w-]+"), WitnessB.First)) %>%
    separate(WitnessA.Affil, c("WitnessA.Affil","WitnessA.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(WitnessB.Affil, c("WitnessB.Affil","WitnessB.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(WitnessA.First, c("WitnessA.Last","WitnessA.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(WitnessB.First, c("WitnessB.Last","WitnessB.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    # Reorganize columns
    relocate(any_of(c("Father.Last","Father.First","Father.Affil","Father.Location")), .after = Subject.Age) %>%
    relocate(any_of(c("Mother.Last","Mother.First","Mother.Affil","Mother.Location")), .after = Father.Location) %>%
    relocate(any_of(c("Spouse.Last","Spouse.First","Spouse.Affil","Spouse.Location")), .after = Mother.Location) %>%
    relocate(any_of(c("WitnessA.Last","WitnessA.First","WitnessA.Affil","WitnessA.Location")), .after = Spouse.Location) %>%
    relocate(any_of(c("WitnessB.Last","WitnessB.First","WitnessB.Affil","WitnessB.Location")), .after = WitnessA.Location) %>%
    # Get rid of blank and "NA" columns
    mutate_if(is.character, list(~na_if(., ""))) %>%
    mutate_if(is.character, list(~na_if(., " "))) %>%
    mutate_if(is.character, list(~na_if(., "NA")))
  
  return(deaths)
}

clean_marry <- function(registry) {
  reg <- read.csv(paste0("Registers/",registry), header = FALSE, sep = ",", col.names = paste0("V",seq_len(300)), fill = TRUE, na.strings="")
  pid <- str_split(registry,"_",3,simplify=TRUE)[2]
  reg <- reg %>% 
    filter(!is.na(V8)) %>% 
    mutate(V3 = parse_date_time(V3, "%Y%m%d", truncated=2), V4 = parse_date_time(V4, "%Y%m%d", truncated=2)) %>%
    mutate(V1 = sapply(str_split(V1,"/"), tail, 1)) %>%
    distinct()
  
  marry <- reg %>% 
    filter(V2 == "m")
  
  marry <- subset(marry, marry$V8 %in% affilclean$Subject.Affil[!is.na(affilclean$Clean)])
  
  marry <- marry %>% 
    mutate(demog = paste(V10,V11,V12,V13, sep=" "),
           V11 = "M",
           V12 = str_extract(demog, "\\d+"),
           V12 = as.numeric(V12),
           V12 = ifelse(!is.na(V12) & str_detect(demog, "\\d+(?=m)"), V12/12.0, V12),
           V12 = ifelse(!is.na(V12) & str_detect(demog, "\\d+(?=j)"), V12/12.0/30.0, V12),
           surname = ifelse(!is.na(V6), trimws(V5, which="both"), NA),
           V6 = ifelse(is.na(V6), trimws(V5, which="both"), trimws(V6, which="both")),
           V5 = surname) %>%
    unite("mergeinfo", V13:last_col(2),sep = " / ") %>%
    mutate(ParishID = as.numeric(pid)) %>%
    select(ParishID,V1:V6,V8,V9,V11,V12,mergeinfo)
  
  colnames(marry) <- c("ParishID","ActID", "Act", "ActDate", "EventDate", "SubjectA.Last", "SubjectA.First", "SubjectA.Affil", "SubjectA.Location", "SubjectA.Sex", "SubjectA.Age", "Info")
  
  marry <- marry %>%
    mutate(SubjectB = ifelse(str_detect(Info, "\\d"), str_extract(Info, "^.+\\d+"), 
                             ifelse(str_detect(Info, "min"), str_extract(Info, "^.+min"), 
                                    ifelse(str_detect(Info, "maj"), str_extract(Info, "^.+maj"), str_extract(Info, "^.+?(?=(/\\sNA\\s){3,})")))),
           Info = ifelse(str_detect(Info, "\\d"), str_remove(Info, "^.+\\d+"), 
                         ifelse(str_detect(Info, "min"), str_remove(Info, "^.+min"), 
                                ifelse(str_detect(Info, "maj"), str_remove(Info, "^.+maj"), str_remove(Info, "^.+?(/\\sNA\\s){3,}"))))) %>%
    separate(Info, c("Info.A", "Info.B", "Info.C", "Info.D", "Info.E", "Info.F", "Info.G", "Info.H", "Info.I", "Info.J", "Info.K", "Info.L", "Info.M", "Info.N"), sep = "(/\\sNA\\s?){3,}", extra = "merge", fill = "right") %>%
    mutate(FatherA.First = ifelse(str_detect(Info.A, "Father of the groom"), Info.A,
                                  ifelse(str_detect(Info.B, "Father of the groom"), Info.B,
                                         ifelse(str_detect(Info.C, "Father of the groom"), Info.C,
                                                ifelse(str_detect(Info.D, "Father of the groom"), Info.D,
                                                       ifelse(str_detect(Info.E, "Father of the groom"), Info.E, NA))))),
           MotherA.First = ifelse(str_detect(Info.A, "Mother of the groom"), Info.A,
                                  ifelse(str_detect(Info.B, "Mother of the groom"), Info.B,
                                         ifelse(str_detect(Info.C, "Mother of the groom"), Info.C,
                                                ifelse(str_detect(Info.D, "Mother of the groom"), Info.D,
                                                       ifelse(str_detect(Info.E, "Mother of the groom"), Info.E, NA))))),
           FatherB.First = ifelse(str_detect(Info.A, "Father of the bride"), Info.A,
                                  ifelse(str_detect(Info.B, "Father of the bride"), Info.B,
                                         ifelse(str_detect(Info.C, "Father of the bride"), Info.C,
                                                ifelse(str_detect(Info.D, "Father of the bride"), Info.D,
                                                       ifelse(str_detect(Info.E, "Father of the bride"), Info.E, NA))))),
           MotherB.First = ifelse(str_detect(Info.A, "Mother of the bride"), Info.A,
                                  ifelse(str_detect(Info.B, "Mother of the bride"), Info.B,
                                         ifelse(str_detect(Info.C, "Mother of the bride"), Info.C,
                                                ifelse(str_detect(Info.D, "Mother of the bride"), Info.D,
                                                       ifelse(str_detect(Info.E, "Mother of the bride"), Info.E, NA))))),
           SpouseA.First = ifelse(str_detect(Info.A, "Spouse of the groom"), Info.A,
                                  ifelse(str_detect(Info.B, "Spouse of the groom"), Info.B,
                                         ifelse(str_detect(Info.C, "Spouse of the groom"), Info.C,
                                                ifelse(str_detect(Info.D, "Spouse of the groom"), Info.D,
                                                       ifelse(str_detect(Info.E, "Spouse of the groom"), Info.E, NA))))),
           SpouseB.First = ifelse(str_detect(Info.A, "Spouse of the bride"), Info.A,
                                  ifelse(str_detect(Info.B, "Spouse of the bride"), Info.B,
                                         ifelse(str_detect(Info.C, "Spouse of the bride"), Info.C,
                                                ifelse(str_detect(Info.D, "Spouse of the bride"), Info.D,
                                                       ifelse(str_detect(Info.E, "Spouse of the bride"), Info.E, NA))))),
           Info.A = ifelse(str_detect(Info.A, "[A-Z][a-z]+"), NA, Info.A),
           Info.B = ifelse(str_detect(Info.B, "[A-Z][a-z]+"), NA, Info.B),
           Info.C = ifelse(str_detect(Info.C, "[A-Z][a-z]+"), NA, Info.C),
           Info.D = ifelse(str_detect(Info.D, "[A-Z][a-z]+"), NA, Info.D),
           Info.E = ifelse(str_detect(Info.E, "[A-Z][a-z]+"), NA, Info.E)) %>%
    unite("Info", Info.A:Info.N, sep=",") %>%
    mutate(Info = str_remove(Info, "^/?\\s?(NA,)+"),
           Info = str_remove(Info, "(NA,)+"),
           Info = str_remove(Info, "(,+?NA)+$"),
           Info = ifelse(Info == "NA", NA, Info)) %>%
    separate(Info, c("Info.A", "Info.B", "Info.C", "Info.D","Info.E","Info.F", "Info.G", "Info.H", "Info.I", "Info.J"), sep = ",", extra = "merge", fill = "left") %>%
    mutate(Priest = ifelse(str_count(Info.J, "/") != 4, Info.J, NA),
           Info.J = ifelse(str_count(Info.J, "/") != 4, NA, Info.J)) %>%
    unite("Info", Info.A:Info.J, sep=",") %>%
    mutate(Info = str_remove(Info, ",NA$"),
           Info = str_remove(Info, "(NA,)+"),
           Info = ifelse(Info == "NA", NA, Info)) %>%
    separate(Info, c("WitnessA.First","WitnessB.First","WitnessC.First", "WitnessD.First","OtherWitnesses"), sep=",", extra = "merge", fill="right") %>%
    mutate(FatherA.Affil = ifelse(!is.na(FatherA.First), str_extract(FatherA.First, "(?<=Father of the groom).+$"), NA),
           MotherA.Affil = ifelse(!is.na(MotherA.First), str_extract(MotherA.First, "(?<=Mother of the groom).+$"), NA),
           SpouseA.Affil = ifelse(!is.na(SpouseA.First), str_extract(SpouseA.First, "(?<=Spouse of the groom).+$"), NA),
           SpouseA.Sex = ifelse(!is.na(SpouseA.First), "F", NA),
           SubjectB.Affil = ifelse(!is.na(SubjectB), str_extract(SubjectB, "(?<=Subject).+$"), NA),
           SubjectB.Sex = "F",
           FatherB.Affil = ifelse(!is.na(FatherB.First), str_extract(FatherB.First, "(?<=Father of the bride).+$"), NA),
           MotherB.Affil = ifelse(!is.na(MotherB.First), str_extract(MotherB.First, "(?<=Mother of the bride).+$"), NA),
           SpouseB.Affil = ifelse(!is.na(SpouseB.First), str_extract(SpouseB.First, "(?<=Spouse of the bride).+$"), NA),
           SpouseB.Sex = ifelse(!is.na(SpouseB.First), "M", NA),
           FatherA.First = ifelse(!is.na(FatherA.First), str_extract(str_remove(FatherA.First, "^[\\s/]+"), "^.+(?=Father)"), NA),
           MotherA.First = ifelse(!is.na(MotherA.First), str_extract(str_remove(MotherA.First, "^[\\s/]+"), "^.+(?=Mother)"), NA),
           SpouseA.First = ifelse(!is.na(SpouseA.First), str_extract(str_remove(SpouseA.First, "^[\\s/]+"), "^.+(?=Spouse)"), NA),
           SubjectB = ifelse(!is.na(SubjectB), str_extract(str_remove(SubjectB, "^[\\s/]+"), "^.+(?=Subject)"), NA),
           FatherB.First = ifelse(!is.na(FatherB.First), str_extract(str_remove(FatherB.First, "^[\\s/]+"), "^.+(?=Father)"), NA),
           MotherB.First = ifelse(!is.na(MotherB.First), str_extract(str_remove(MotherB.First, "^[\\s/]+"), "^.+(?=Mother)"), NA),
           SpouseB.First = ifelse(!is.na(SpouseB.First), str_extract(str_remove(SpouseB.First, "^[\\s/]+"), "^.+(?=Spouse)"), NA),
           FatherA.First = ifelse(!is.na(FatherA.First), str_remove(FatherA.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           MotherA.First = ifelse(!is.na(MotherA.First), str_remove(MotherA.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           SpouseA.First = ifelse(!is.na(SpouseA.First), str_remove(SpouseA.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           FatherA.Affil = ifelse(!is.na(FatherA.Affil), str_remove(FatherA.Affil, "^\\s?/\\s"), NA),
           MotherA.Affil = ifelse(!is.na(MotherA.Affil), str_remove(MotherA.Affil, "^\\s?/\\s"), NA),
           SpouseA.Affil = ifelse(!is.na(SpouseA.Affil), str_remove(SpouseA.Affil, "^\\s?/\\s"), NA),
           SubjectB = ifelse(!is.na(SubjectB), str_remove(SubjectB, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           SubjectB = ifelse(!is.na(SubjectB), str_remove(SubjectB, "^(NA\\s/\\s?)+"), NA),
           FatherB.First = ifelse(!is.na(FatherB.First), str_remove(FatherB.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           MotherB.First = ifelse(!is.na(MotherB.First), str_remove(MotherB.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           SpouseB.First = ifelse(!is.na(SpouseB.First), str_remove(SpouseB.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           SubjectB.Affil = ifelse(!is.na(SubjectB.Affil), str_remove(SubjectB.Affil, "^\\s?/\\s"), NA),
           FatherB.Affil = ifelse(!is.na(FatherB.Affil), str_remove(FatherB.Affil, "^\\s?/\\s"), NA),
           MotherB.Affil = ifelse(!is.na(MotherB.Affil), str_remove(MotherB.Affil, "^\\s?/\\s"), NA),
           SpouseB.Affil = ifelse(!is.na(SpouseB.Affil), str_remove(SpouseB.Affil, "^\\s?/\\s"), NA),
           SubjectB.Age = ifelse(str_detect(SubjectB.Affil, "\\d+"), as.numeric(str_extract(SubjectB.Affil, "\\d+")), NA)) %>%
    separate(FatherA.First, c("FatherA.Last","FatherA.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(MotherA.First, c("MotherA.Last","MotherA.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(SpouseA.First, c("SpouseA.Last","SpouseA.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(SubjectB, c("SubjectB.Last", "SubjectB.First"), sep = "\\s?/\\s+", extra="drop", fill = "left") %>%
    separate(FatherB.First, c("FatherB.Last","FatherB.First"), sep = "\\s?/\\s+", extra="drop", fill = "left") %>%
    separate(MotherB.First, c("MotherB.Last","MotherB.First"), sep = "\\s?/\\s+", extra="drop", fill = "left") %>%
    separate(SpouseB.First, c("SpouseB.Last","SpouseB.First"), sep = "\\s?/\\s+", extra="drop", fill = "left") %>%
    separate(FatherA.Affil, c("FatherA.Affil","FatherA.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(MotherA.Affil, c("MotherA.Affil","MotherA.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(SpouseA.Affil, c("SpouseA.Affil","SpouseA.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(FatherB.Affil, c("FatherB.Affil","FatherB.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(MotherB.Affil, c("MotherB.Affil","MotherB.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(SpouseB.Affil, c("SpouseB.Affil","SpouseB.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(SubjectB.Affil, c("SubjectB.Affil","SubjectB.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    mutate(WitnessA.First = ifelse(!is.na(WitnessA.First), str_remove(WitnessA.First, "^\\s?/\\s"), NA),
           WitnessB.First = ifelse(!is.na(WitnessB.First), str_remove(WitnessB.First, "^\\s?/\\s"), NA),
           WitnessC.First = ifelse(!is.na(WitnessC.First), str_remove(WitnessC.First, "^\\s?/\\s"), NA),
           WitnessD.First = ifelse(!is.na(WitnessD.First), str_remove(WitnessD.First, "^\\s?/\\s"), NA),
           WitnessA.Affil = ifelse(str_count(WitnessA.First, "/") >= 3, str_remove(WitnessA.First, "(?:.*?/){3}"), NA),
           WitnessA.First = ifelse(str_count(WitnessA.First, "/") >=3, str_extract(WitnessA.First, "(?:.*?/){1}[\\s\\w-]+"), WitnessA.First),
           WitnessB.Affil = ifelse(str_count(WitnessB.First, "/") >= 3, str_remove(WitnessB.First, "(?:.*?/){3}"), NA),
           WitnessB.First = ifelse(str_count(WitnessB.First, "/") >=3, str_extract(WitnessB.First, "(?:.*?/){1}[\\s\\w-]+"), WitnessB.First),
           WitnessC.Affil = ifelse(str_count(WitnessC.First, "/") >= 3, str_remove(WitnessC.First, "(?:.*?/){3}"), NA),
           WitnessC.First = ifelse(str_count(WitnessC.First, "/") >=3, str_extract(WitnessC.First, "(?:.*?/){1}[\\s\\w-]+"), WitnessC.First),
           WitnessD.Affil = ifelse(str_count(WitnessD.First, "/") >= 3, str_remove(WitnessD.First, "(?:.*?/){3}"), NA),
           WitnessD.First = ifelse(str_count(WitnessD.First, "/") >=3, str_extract(WitnessD.First, "(?:.*?/){1}[\\s\\w-]+"), WitnessD.First)
           ) %>%
    separate(WitnessA.Affil, c("WitnessA.Affil","WitnessA.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(WitnessB.Affil, c("WitnessB.Affil","WitnessB.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(WitnessC.Affil, c("WitnessC.Affil","WitnessC.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(WitnessD.Affil, c("WitnessD.Affil","WitnessD.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(WitnessA.First, c("WitnessA.Last","WitnessA.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(WitnessB.First, c("WitnessB.Last","WitnessB.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(WitnessC.First, c("WitnessC.Last","WitnessC.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(WitnessD.First, c("WitnessD.Last","WitnessD.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    relocate(any_of(c("SubjectB.Last","SubjectB.First","SubjectB.Affil","SubjectB.Location","SubjectB.Sex","SubjectB.Age")), .after = SubjectA.Age) %>%
    relocate(any_of(c("FatherA.Last","FatherA.First","FatherA.Affil","FatherA.Location")), .after = SubjectB.Age) %>%
    relocate(any_of(c("MotherA.Last","MotherA.First","MotherA.Affil","MotherA.Location")), .after = FatherA.Location) %>%
    relocate(any_of(c("SpouseA.Last","SpouseA.First","SpouseA.Affil","SpouseA.Location","SpouseA.Sex")), .after = MotherA.Location) %>%
    relocate(any_of(c("FatherB.Last","FatherB.First","FatherB.Affil","FatherB.Location")), .after = SpouseA.Sex) %>%
    relocate(any_of(c("MotherB.Last","MotherB.First","MotherB.Affil","MotherB.Location")), .after = FatherB.Location) %>%
    relocate(any_of(c("SpouseB.Last","SpouseB.First","SpouseB.Affil","SpouseB.Location","SpouseB.Sex")), .after = MotherB.Location) %>%
    relocate(any_of(c("WitnessA.Last","WitnessA.First","WitnessA.Affil","WitnessA.Location")), .after = SpouseB.Sex) %>%
    relocate(any_of(c("WitnessB.Last","WitnessB.First","WitnessB.Affil","WitnessB.Location")), .after = WitnessA.Location) %>%
    relocate(any_of(c("WitnessC.Last","WitnessC.First","WitnessC.Affil","WitnessC.Location")), .after = WitnessB.Location) %>%
    relocate(any_of(c("WitnessD.Last","WitnessD.First","WitnessD.Affil","WitnessD.Location")), .after = WitnessC.Location) %>%
    mutate_if(is.character, list(~na_if(., ""))) %>%
    mutate_if(is.character, list(~na_if(., " "))) %>%
    mutate_if(is.character, list(~na_if(., "NA")))

  return(marry)
}
