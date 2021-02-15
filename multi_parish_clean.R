library(tidyverse)
library(lubridate)

# Functions for cleaning up NAs
col_all_na <- function(x) { return(!(all(is.na(x)))) }

empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

# Read in full list of parish cards
registries <- list.files("Registers", "parish.+cards.csv")

# Clean birth data for all parishes
birthlist <- lapply(registries, clean_births)
births <- do.call("rbind", birthlist)

# Trim white space out of start and end of string columns
cols_to_be_rectified <- names(births)[vapply(births, is.character, logical(1))]
births[,cols_to_be_rectified] <- lapply(births[,cols_to_be_rectified], trimws)
births <- births %>% mutate_if(is.character, list(~na_if(., "NA")))

clean_births <- function(registry) {
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
  
  births <- reg %>% 
    filter(V2 == "b") %>% #Filter births/baptisms
    mutate(demog = paste(V10,V11,V12,V13, sep=" "), #Join demographic data that's sometimes unevenly aligned
           V11 = ifelse(str_detect(demog, "m\\."), "M", NA), #Detect sex
           V11 = ifelse(str_detect(demog, "f\\."), "F", V11),
           V12 = str_extract(demog, "\\d+"), #Extract age if present
           V12 = as.numeric(V12),
           V12 = ifelse(!is.na(V12) & str_detect(demog, "\\d+(?=m)"), V12/12.0, V12), #Modify age if month or day
           V12 = ifelse(!is.na(V12) & str_detect(demog, "\\d+(?=j)"), V12/12.0/30.0, V12),
           surname = ifelse(!is.na(V6), trimws(V5, which="both"), NA), #Make column alignment of person with single name consistent
           V6 = ifelse(is.na(V6), trimws(V5, which="both"), trimws(V6, which="both")),
           V5 = surname,
           V15 = ifelse((!is.na(V13) | !is.na(V14)) & (!is.na(V16) | !is.na(V17)) & is.na(V15), "Father", V15), #Identify father and mother if unidentified
           V23 = ifelse((!is.na(V21) | !is.na(V22)) & (!is.na(V24) | !is.na(V25)) & is.na(V23), "Mother", V23)) %>%
    unite("mergeinfo", V13:last_col(2),sep = " / ") %>% #Merge extra information for further manipulation
    mutate(ParishID = as.numeric(pid)) %>%
    select(ParishID,V1:V6,V8,V9,V11,V12,mergeinfo)
  
  # Provide more informative column names
  colnames(births) <- c("ParishID","ActID", "Act", "ActDate", "EventDate", "Subject.Last", "Subject.First", "Subject.Affil", "Subject.Location", "Subject.Sex", "Subject.Age", "Info")
  
  # Extract father and mother names and affils, pull out priests, and split up other witness information
  births <- births %>%
    separate(Info, c("Info.A", "Info.B", "Info.C", "Info.D", "Info.E", "Info.F", "Info.G", "Info.H"), sep = "(/\\sNA\\s){3,}", extra = "merge", fill = "right") %>%
    mutate(Father.First = ifelse(str_detect(Info.A, "Father"), Info.A,
                                  ifelse(str_detect(Info.B, "Father"), Info.B, NA)),
           Mother.First = ifelse(str_detect(Info.A, "Mother"), Info.A,
                                 ifelse(str_detect(Info.B, "Mother"), Info.B, NA)),
           Info.A = ifelse(str_detect(Info.A, "[A-Z][a-z]+"), NA, Info.A),
           Info.B = ifelse(str_detect(Info.B, "[A-Z][a-z]+"), NA, Info.B)) %>%
    unite("Info", Info.A:Info.H, sep=",") %>%
    mutate(Info = str_remove(Info, "^/?\\s?(NA,)+"),
           Info = str_remove(Info, "(NA,)+"),
           Info = str_remove(Info, "(,+?NA)+$"),
           Info = ifelse(Info == "NA", NA, Info),
           Info = str_remove(Info, ",/\\s?(NA)$")) %>%
    separate(Info, c("Info.A", "Info.B", "Info.C", "Info.D","Info.E","Info.F", "Info.G", "Info.H"), sep = ",", extra = "merge", fill = "left") %>%
    mutate(Priest = ifelse(str_count(Info.H, "/") != 4, Info.H, NA),
           Info.H = ifelse(str_count(Info.H, "/") != 4, NA, Info.H)) %>%
    unite("Info", Info.A:Info.H, sep=",") %>%
    mutate(Info = str_remove(Info, ",NA$"),
           Info = str_remove(Info, "(NA,)+"),
           Info = ifelse(Info == "NA", NA, Info)) %>%
    separate(Info, c("GodparentA.First","GodparentB.First","WitnessA.First", "WitnessB.First","OtherWitnesses"), sep=",", extra = "merge", fill="right")
    
  # Separate and clean mother and father info
  births <- births %>%
    mutate(Father.Affil = ifelse(!is.na(Father.First), str_extract(Father.First, "(?<=Father).+$"), NA),
           Mother.Affil = ifelse(!is.na(Mother.First), str_extract(Mother.First, "(?<=Mother).+$"), NA),
           Father.First = ifelse(!is.na(Father.First), str_extract(str_remove(Father.First, "^[\\s/]+"), "^.+(?=Father)"), NA),
           Mother.First = ifelse(!is.na(Mother.First), str_extract(str_remove(Mother.First, "^[\\s/]+"), "^.+(?=Mother)"), NA),
           Father.First = ifelse(!is.na(Father.First), str_remove(Father.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           Mother.First = ifelse(!is.na(Mother.First), str_remove(Mother.First, "(\\s/\\sNA)?\\s/\\s?$"), NA),
           Father.Affil = ifelse(!is.na(Father.Affil), str_remove(Father.Affil, "^\\s?/\\s"), NA),
           Mother.Affil = ifelse(!is.na(Mother.Affil), str_remove(Mother.Affil, "^\\s?/\\s"), NA)) %>%
    separate(Father.First, c("Father.Last","Father.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(Mother.First, c("Mother.Last","Mother.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(Father.Affil, c("Father.Affil","Father.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(Mother.Affil, c("Mother.Affil","Mother.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right")
    
  # Separate and clean godparent and witness info
  births <- births %>%
    mutate(GodparentA.First = ifelse(!is.na(GodparentA.First), str_remove(GodparentA.First, "^\\s?/\\s"), NA),
           GodparentB.First = ifelse(!is.na(GodparentB.First), str_remove(GodparentB.First, "^\\s?/\\s"), NA),
           WitnessA.First = ifelse(!is.na(WitnessA.First), str_remove(WitnessA.First, "^\\s?/\\s"), NA),
           WitnessB.First = ifelse(!is.na(WitnessB.First), str_remove(WitnessB.First, "^\\s?/\\s"), NA),
           GodparentA.Affil = ifelse(str_count(GodparentA.First, "/") >= 3, str_remove(GodparentA.First, "(?:.*?/){3}"), NA),
           GodparentA.First = ifelse(str_count(GodparentA.First, "/") >=3, str_extract(GodparentA.First, "(?:.*?/){1}[\\s\\w-]+"), GodparentA.First),
           GodparentB.Affil = ifelse(str_count(GodparentB.First, "/") >= 3, str_remove(GodparentB.First, "(?:.*?/){3}"), NA),
           GodparentB.First = ifelse(str_count(GodparentB.First, "/") >=3, str_extract(GodparentB.First, "(?:.*?/){1}[\\s\\w-]+"), GodparentB.First),
           WitnessA.Affil = ifelse(str_count(WitnessA.First, "/") >= 3, str_remove(WitnessA.First, "(?:.*?/){3}"), NA),
           WitnessA.First = ifelse(str_count(WitnessA.First, "/") >=3, str_extract(WitnessA.First, "(?:.*?/){1}[\\s\\w-]+"), WitnessA.First),
           WitnessB.Affil = ifelse(str_count(WitnessB.First, "/") >= 3, str_remove(WitnessB.First, "(?:.*?/){3}"), NA),
           WitnessB.First = ifelse(str_count(WitnessB.First, "/") >=3, str_extract(WitnessB.First, "(?:.*?/){1}[\\s\\w-]+"), WitnessB.First)) %>%
    separate(GodparentA.Affil, c("GodparentA.Affil","GodparentA.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(GodparentB.Affil, c("GodparentB.Affil","GodparentB.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(WitnessA.Affil, c("WitnessA.Affil","WitnessA.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(WitnessB.Affil, c("WitnessB.Affil","WitnessB.Location"), sep = "\\s?/\\s+", extra="drop", fill = "right") %>%
    separate(GodparentA.First, c("GodparentA.Last","GodparentA.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(GodparentB.First, c("GodparentB.Last","GodparentB.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(WitnessA.First, c("WitnessA.Last","WitnessA.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
    separate(WitnessB.First, c("WitnessB.Last","WitnessB.First"), sep = "\\s/\\s+", extra="drop", fill = "left") %>%
      # Reorganize columns
    relocate(any_of(c("Father.Last","Father.First","Father.Affil","Father.Location")), .after = Subject.Age) %>%
    relocate(any_of(c("Mother.Last","Mother.First","Mother.Affil","Mother.Location")), .after = Father.Location) %>%
    relocate(any_of(c("GodparentA.Last","GodparentA.First","GodparentA.Affil","GodparentA.Location")), .after = Mother.Location) %>%
    relocate(any_of(c("GodparentB.Last","GodparentB.First","GodparentB.Affil","GodparentB.Location")), .after = GodparentA.Location) %>%
    relocate(any_of(c("WitnessA.Last","WitnessA.First","WitnessA.Affil","WitnessA.Location")), .after = GodparentB.Location) %>%
    relocate(any_of(c("WitnessB.Last","WitnessB.First","WitnessB.Affil","WitnessB.Location")), .after = WitnessA.Location) %>%
      # Get rid of blank and "NA" columns
    mutate_if(is.character, list(~na_if(., ""))) %>%
    mutate_if(is.character, list(~na_if(., " "))) %>%
    mutate_if(is.character, list(~na_if(., "NA")))
  
  return(births)
}




