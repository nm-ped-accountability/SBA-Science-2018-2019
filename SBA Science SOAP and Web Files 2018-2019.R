
### SBA Science 2018-2019

################################################################################
### open and process raw data file
################################################################################

rm(list = ls())
library(stringi)
library(lubridate)
library(Hmisc)
library(tidyverse)

# open files
raw <- read.csv("NMSBA1819Admin1StudentResultsUNMASKED_Updated 2019-07-16.csv",
                header = TRUE, stringsAsFactors = FALSE)
dat <- raw
nrow(dat) 
# 2019: 74320

schools <- read.csv("Master Schools 2019 V3.csv", 
                    header = TRUE, stringsAsFactors = FALSE)
# select the current school year
schools <- schools[schools$ï..SY == 2019, ]

################################################################################
## recode variables

# test_schnumb
dat$test_schnumb <- dat$DisCode * 1000 + dat$SchCode
# vendor schnumbs will be used

# STARS_schnumb
dat$STARS_schnumb <- dat$S_DISTRICT_CODE * 1000 + dat$S_LOCATION_CODE
sum(dat$test_schnumb != dat$STARS_schnumb) 
#2019: 394 records have different schnumbs

# distcode
dat$distcode <- dat$DisCode

# distname
dat$distname <- schools$distname[match(dat$distcode, schools$distcode)]
# check for missing distnames
dat[is.na(dat$distname), ] #none

# schcode
dat$schcode <- dat$SchCode

# schname
dat$schname <- schools$schname[match(dat$test_schnumb, schools$schnumb)]
# check for missing schnames
dat[is.na(dat$schname), ] #none

# stid
dat$stid <- dat$STUID

# last
dat$last <- dat$S_LASTNAME

# first
dat$fist <- dat$S_FIRSTNAME

# mi
dat$mi <- dat$S_MIDDLE_NAME
dat$mi <- toupper(dat$mi)
table(dat$mi)
dat$mi <- gsub("-", "", dat$mi)
dat$mi <- gsub("NULL", "", dat$mi)
table(dat$mi)

# dob
dat$dob <- dat$S_DOB
dat$dob <- mdy(dat$dob)
str(dat$dob)

# test grade
dat$test_grade <- dat$Grade
table(dat$test_grade)
dat$test_grade[dat$test_grade == "HS"] <- "11"
table(dat$test_grade)
# test grade will be used

# STARS grade
dat$STARS_grade <- dat$S_GRADE
table(dat$STARS_grade)
sum(dat$test_grade != dat$STARS_grade) 
#2019: 2131 records have different grades

# eth
dat$eth <- dat$S_ETNICITY
table(dat$eth)
table(dat$S_HISPANIC_INDICATOR)
dat$eth[dat$S_HISPANIC_INDICATOR == "Yes"] <- "Hispanic"
dat$eth[dat$eth == "Native Hawaiian or Other Pacific Islander"] <- "Asian"
dat$eth[dat$eth == "Black or African American"] <- "African American"
dat$eth[dat$eth == "American Indian/Alaskan Native"] <- "Native American"
table(dat$eth)

# gender
dat$gender <- dat$S_GENDER
table(dat$gender)

# swd
dat$swd <- dat$S_SPECIAL_ED
table(dat$swd)
dat$swd[dat$swd == "Y"] <- "Students with Disabilities"
dat$swd[dat$swd == "N"] <- "Non SWD"
table(dat$swd)

# frl
table(dat$S_FRLP)
dat$frl[dat$S_FRLP == "F"] <- "Economically Disadvantaged"
dat$frl[dat$S_FRLP == "R"] <- "Economically Disadvantaged"
dat$frl[dat$S_FRLP == "N"] <- "Non ED"
table(dat$frl)

# ell
dat$ell[dat$S_ELL_STATUS == "Y"] <- "English Learners"
dat$ell[dat$S_ELL_STATUS == "N"] <- "Non EL"
table(dat$ell)

# migrant
dat$migrant[dat$S_MIGRANT == "Y"] <- "Migrants"
dat$migrant[dat$S_MIGRANT == "N"] <- "Non Migrant"
dat$migrant[dat$S_MIGRANT == "NULL"] <- "Non Migrant"
table(dat$migrant)

# military
# active, national guard, researve
table(dat$S_MILITARY)
dat$military[dat$S_MILITARY == "Active"] <- "Military"
dat$military[dat$S_MILITARY == "National Guard"] <- "Military"
dat$military[dat$S_MILITARY == "Reserve"] <- "Non Military"
dat$military[dat$S_MILITARY == "NULL"] <- "Non Military"
table(dat$military)

# homeless
dat$homeless <- dat$S_HOMELESS
table(dat$homeless)
dat$homeless <- gsub("NULL", "Not Homeless", dat$homeless)
table(dat$homeless)

# foster
table(dat$S_FOSTER)
dat$foster[dat$S_FOSTER == "Y"] <- "Foster Care"
dat$foster[dat$S_FOSTER == "NULL"] <- "Not Foster Care"
table(dat$foster)

# test name
dat$testname <- "SBASCI"

# subtest
dat$subtest <- "SCI"

# test language
dat$testlang <- dat$SciTestLanguage
table(dat$testlang)

# accommodation
dat$accommodation[dat$ELLSciAccom20 == 1] <- 1
dat$accommodation[dat$ELLSciAccom21 == 1] <- 1
dat$accommodation[dat$ELLSciAccom22 == 1] <- 1
dat$accommodation[dat$ELLSciAccom23 == 1] <- 1
dat$accommodation[dat$ELLSciAccom24 == 1] <- 1
dat$accommodation[dat$ELLSciAccom25 == 1] <- 1
dat$accommodation[dat$ELLSciAccom26 == 1] <- 1
dat$accommodation[dat$ELLSciAccom27 == 1] <- 1
dat$accommodation[dat$ELLSciAccom28 == 1] <- 1
dat$accommodation[dat$SWDSciAccom01 == 1] <- 1
dat$accommodation[dat$SWDSciAccom02 == 1] <- 1
dat$accommodation[dat$SWDSciAccom03 == 1] <- 1
dat$accommodation[dat$SWDSciAccom04 == 1] <- 1
dat$accommodation[dat$SWDSciAccom05 == 1] <- 1
dat$accommodation[dat$SWDSciAccom06 == 1] <- 1
dat$accommodation[dat$SWDSciAccom07 == 1] <- 1
dat$accommodation[dat$SWDSciAccom08 == 1] <- 1
dat$accommodation[dat$SWDSciAccom09 == 1] <- 1
dat$accommodation[dat$SWDSciAccom10 == 1] <- 1
dat$accommodation[dat$SWDSciAccom11 == 1] <- 1
dat$accommodation[dat$SWDSciAccom12 == 1] <- 1
dat$accommodation[dat$SWDSciAccom13 == 1] <- 1
dat$accommodation[dat$SWDSciAccom14 == 1] <- 1
dat$accommodation[dat$SWDSciAccom15 == 1] <- 1
dat$accommodation[dat$SciAccomLineReader == 1] <- 1
dat$accommodation[dat$SciAccomMaskingAnswer == 1] <- 1
dat$accommodation[dat$SciAccomMaskingCustom == 1] <- 1
dat$accommodation[dat$SciAccomNativeLanguage == 1] <- 1
dat$accommodation[dat$SciAccomReverseContrast == 1] <- 1
dat$accommodation[(is.na(dat$accommodation))] <- 0
table(dat$accommodation)

# cbt
dat$cbt[dat$SciCBT == 1] <- "PBT only"
dat$cbt[dat$SciCBT == 2] <- "CBT only"
dat$cbt[dat$SciCBT == 3] <- "CBT and PBT"
table(dat$cbt)

# testbookid
dat$testbookid <- dat$BookletID

# scale scores
dat$SciScaleScore <- as.character(dat$SciScaleScore)
dat$SS <- stri_sub(dat$SciScaleScore, -2, -1) #keep only the last 2 digits
table(dat$SciScaleScore)
table(dat$SS)

# proficiency levels
table(dat$SciPerformanceLevel)
dat$PL <- dat$SciPerformanceLevel
# 2018-2019: 2255 invalid scores

# proficient
dat$proficient[dat$PL == 1] <- 0
dat$proficient[dat$PL == 2] <- 0
dat$proficient[dat$PL == 3] <- 1
dat$proficient[dat$PL == 4] <- 1

# test completion code
dat$TC <- dat$SciTC

# snapshot date
dat$status <- dat$STATUS
table(dat$status) 
#2019: 4 records not from the current year, 4 manual corrections

################################################################################
## remove invalid records and save file
table(dat$TC[dat$PL != 5]) #all valid records have TC 0
table(dat$SS[dat$PL == 5])
table(dat$SS[dat$PL != 5])

# remove invalid records
dat <- dat[dat$PL != 5, ]

# remove extra columns
names(dat)
dat <- dat[c(395:427)]
names(dat)

# save file
current_date <- Sys.Date()
file_name <- paste0("SBA Science Spring 2018-2019 Cleaned ", current_date, ".csv")
write.csv(dat, file = file_name, row.names = FALSE)
nrow(dat) 
# 2019: 72065

################################################################################
### calculate rates for SOAP and web files
################################################################################
dat$allstudents <- "All Students"
dat$statecode <- 999

groups <- c("allstudents", "gender", "eth", "swd", "frl", 
            "ell", "migrant", "homeless", "military", "foster")

dat$level1[dat$PL == 1] <- 1
dat$level1[is.na(dat$level1)] <- 0
dat$level2[dat$PL == 2] <- 1
dat$level2[is.na(dat$level2)] <- 0
dat$level3[dat$PL == 3] <- 1
dat$level3[is.na(dat$level3)] <- 0
dat$level4[dat$PL == 4] <- 1
dat$level4[is.na(dat$level4)] <- 0


rate <- function(dataset, code) {
    Rates <- data.frame()
    
    for (group in groups) {
        GroupRate <- dataset %>%
            select(code, group, test_grade,
                   level1, level2, level3, level4, proficient) %>%
            group_by(dataset[[code]], dataset[[group]], test_grade) %>%
            summarise(NStudents = n(),
                      Level1 = (sum(level1) / NStudents) * 100,
                      Level2 = (sum(level2) / NStudents) * 100,
                      Level3 = (sum(level3) / NStudents) * 100,
                      Level4 = (sum(level4) / NStudents) * 100,
                      ProficiencyRate = (sum(proficient) / NStudents * 100))
        names(GroupRate) <- c("Code", "Group", "Grade", "NStudents", 
                              "Level1", "Level2", "Level3", "Level4",
                              "ProficiencyRate")
        
        GroupRate <- GroupRate[GroupRate$Code != 999999, ]
        Rates <- rbind(GroupRate, Rates)
    }
    Rates
}

# state rates
stateRates <- rate(dat, "statecode")
stateRates$schnumb <- 999999
stateRates$DistrictCode <- 999
stateRates$SchoolCode <- 999
stateRates$SORT <- 1

# district rates
districtRates <- rate(dat, "distcode")
districtRates$schnumb <- districtRates$Code * 1000
districtRates$DistrictCode <- districtRates$Code
districtRates$SchoolCode <- 0
districtRates$SORT <- 2

# school rates
schoolRates <- rate(dat, "test_schnumb")
schoolRates$schnumb <- schoolRates$Code
schoolRates$DistrictCode <- floor(schoolRates$Code / 1000)
schoolRates$SchoolCode <- schoolRates$Code - (schoolRates$DistrictCode * 1000)
schoolRates$SORT <- 3



################################################################################
### merging, formatting, masking
################################################################################
all <- rbind(stateRates, districtRates, schoolRates)
all$Grade <- as.numeric(all$Grade)

# sort codes for subgroups
table(all$Group)
all$SORTCODE[all$Group == "All Students"] <- 1
all$SORTCODE[all$Group == "Female"] <- 2
all$SORTCODE[all$Group == "Male"] <- 3
all$SORTCODE[all$Group == "Caucasian"] <- 4
all$SORTCODE[all$Group == "African American"] <- 5
all$SORTCODE[all$Group == "Hispanic"] <- 6
all$SORTCODE[all$Group == "Asian"] <- 7
all$SORTCODE[all$Group == "Native American"] <- 8
all$SORTCODE[all$Group == "Economically Disadvantaged"] <- 9
all$SORTCODE[all$Group == "Students with Disabilities"] <- 10
all$SORTCODE[all$Group == "English Learners"] <- 11
all$SORTCODE[all$Group == "Migrant"] <- 12
all$SORTCODE[all$Group == "Homeless"] <- 13
all$SORTCODE[all$Group == "Military"] <- 14
all$SORTCODE[all$Group == "Foster Care"] <- 15

table(all$SORTCODE)

# add district and school names
all$DistrictName <- schools$distname[match(all$DistrictCode, schools$distcode)]
all$SchoolName <- schools$schname[match(all$schnumb, schools$schnumb)]
all$DistrictName[all$SORT == 1] <- "Statewide"
all$SchoolName[all$SORT == 1] <- "All Students"
all$SchoolName[all$SORT == 2] <- "Districtwide"

# check for missing district and school names
all <- all[!is.na(all$schnumb), ]
nrow(all)
# 2019: 16278
all[is.na(all$DistrictName), ] #none
all[is.na(all$SchoolName), ] #none

################################################################################
# SOAP file
SOAP <- all[c("schnumb", "DistrictCode", "DistrictName", 
              "SchoolCode", "SchoolName", "Grade", "Group", "NStudents",
              "Level1", "Level2", "Level3", "Level4", "ProficiencyRate",
              "SORTCODE", "SORT")]
nrow(SOAP) 
# 2019: 20805

SOAP <- SOAP[!is.na(SOAP$SORTCODE), ]
nrow(SOAP) 
#2019: 12515

# remove district-level rates for state charter schools
SOAP <- SOAP[!(SOAP$DistrictCode > 500 & SOAP$SchoolName == "Districtwide"), ]
nrow(SOAP)
# 2019: 11729

# round to one digit
# R rounds 0.5 to even, but this function rounds 0.5 up
round2 <- function(x, digits) {
    posneg <- sign(x)
    z <- abs(x) * (10 ^ digits)
    z <- z + 0.5
    z <- as.numeric(as.character(z))
    z <- trunc(z)
    z <- z / (10 ^ digits)
    z * posneg
}

head(SOAP)
SOAP$Level1 <- round2(SOAP$Level1, digits = 1)
SOAP$Level2 <- round2(SOAP$Level2, digits = 1)
SOAP$Level3 <- round2(SOAP$Level3, digits = 1)
SOAP$Level4 <- round2(SOAP$Level4, digits = 1)
SOAP$ProficiencyRate <- round2(SOAP$ProficiencyRate, 1)
head(SOAP)

# sorting
SOAP <- SOAP[order(SOAP$SORT, SOAP$schnumb, SOAP$SORTCODE, SOAP$Grade), ]
SOAP$SORT <- NULL
SOAP$SORTCODE <- NULL

# save output
current_date <- Sys.Date()
file_name <- paste0("SBA Science UNMASKED SOAP 2018-2019 ", current_date, ".csv")
write.csv(SOAP, file = file_name, row.names = FALSE)

################################################################################
# web file
web <- all[c("schnumb", "DistrictCode", "DistrictName", 
             "SchoolCode", "SchoolName", "Grade", "Group", "NStudents",
             "Level1", "Level2", "Level3", "Level4",
             "SORTCODE", "SORT")]

web <- web[!is.na(web$SORTCODE), ]

# remove district-level rates for state charter schools
web <- web[!(web$DistrictCode > 500 & web$SchoolName == "Districtwide"), ]
nrow(web)
# 2019: 11729

# round to integers
head(web)
web$Level1 <- round2(web$Level1, digits = 0)
web$Level2 <- round2(web$Level2, digits = 0)
web$Level3 <- round2(web$Level3, digits = 0)
web$Level4 <- round2(web$Level4, digits = 0)
head(web)

# check totals
web$total <- rowSums(web[, c("Level1", "Level2", "Level3", "Level4")])
range(web$total) #99-102
web[web$total == 99, ]
web[web$total == 101, ]
web[web$total == 102, ]
web$total <- NULL


###############################################
## masking

# remove records with fewer than 10 students
nrow(web) 
#2019: 12729
web <- web[web$NStudents >= 10, ]
nrow(web) 
#2019: 6750

mask <- function(dataset, level) {
    masked <- data.frame()
    
    for (row in 1:nrow(dataset)) {
        row <- dataset[row, ]
        
        # N = 301 or higher
        if (row$NStudents > 300) {
            row$pct[row[[level]] >= 99] <- "GE 99"
            row$pct[row[[level]] <= 1] <- "LE 1"
            row$pct[row[[level]] < 99 & row[[level]] > 1] <- row[[level]]
            row$merge <- 0
        }
        
        # N = 201-300
        else if (row$NStudents > 200 & row$NStudents <= 300) {
            row$pct[row[[level]] >= 98] <- "GE 98"
            row$pct[row[[level]] <= 2] <- "LE 2"
            row$pct[row[[level]] < 98 & row[[level]] > 2] <- row[[level]]
            row$merge <- 0
        }
        
        # N = 101-200
        else if (row$NStudents > 100 & row$NStudents <= 200) {
            row$pct[row[[level]] < 3] <- "LE 2"
            row$pct[row[[level]] >= 3 & row[[level]] < 5] <- "3-4"
            row$pct[row[[level]] >= 5 & row[[level]] < 10] <- "5-9"
            row$pct[row[[level]] >= 10 & row[[level]] < 15] <- "10-14"
            row$pct[row[[level]] >= 15 & row[[level]] < 20] <- "15-19"
            row$pct[row[[level]] >= 20 & row[[level]] < 25] <- "20-24"
            row$pct[row[[level]] >= 25 & row[[level]] < 30] <- "25-29"
            row$pct[row[[level]] >= 30 & row[[level]] < 35] <- "30-34"
            row$pct[row[[level]] >= 35 & row[[level]] < 40] <- "35-39"
            row$pct[row[[level]] >= 40 & row[[level]] < 45] <- "40-44"
            row$pct[row[[level]] >= 45 & row[[level]] < 50] <- "45-49"
            row$pct[row[[level]] >= 50 & row[[level]] < 55] <- "50-54"
            row$pct[row[[level]] >= 55 & row[[level]] < 60] <- "55-59"
            row$pct[row[[level]] >= 60 & row[[level]] < 65] <- "60-64"
            row$pct[row[[level]] >= 65 & row[[level]] < 70] <- "65-69"
            row$pct[row[[level]] >= 70 & row[[level]] < 75] <- "70-74"
            row$pct[row[[level]] >= 75 & row[[level]] < 80] <- "75-79"
            row$pct[row[[level]] >= 80 & row[[level]] < 85] <- "80-84"
            row$pct[row[[level]] >= 85 & row[[level]] < 90] <- "85-89"
            row$pct[row[[level]] >= 90 & row[[level]] < 95] <- "90-94"
            row$pct[row[[level]] >= 95 & row[[level]] < 98] <- "95-97"
            row$pct[row[[level]] >= 98] <- "GE 98"
            row$merge <- 0
        }
        
        # N = 41-100
        else if (row$NStudents > 40 & row$NStudents <= 100) {
            row$pct[row[[level]] < 6] <- "LE 5"
            row$pct[row[[level]] >= 6 & row[[level]] < 10] <- "6-9"
            row$pct[row[[level]] >= 10 & row[[level]] < 15] <- "10-14"
            row$pct[row[[level]] >= 15 & row[[level]] < 20] <- "15-19"
            row$pct[row[[level]] >= 20 & row[[level]] < 25] <- "20-24"
            row$pct[row[[level]] >= 25 & row[[level]] < 30] <- "25-29"
            row$pct[row[[level]] >= 30 & row[[level]] < 35] <- "30-34"
            row$pct[row[[level]] >= 35 & row[[level]] < 40] <- "35-39"
            row$pct[row[[level]] >= 40 & row[[level]] < 45] <- "40-44"
            row$pct[row[[level]] >= 45 & row[[level]] < 50] <- "45-49"
            row$pct[row[[level]] >= 50 & row[[level]] < 55] <- "50-54"
            row$pct[row[[level]] >= 55 & row[[level]] < 60] <- "55-59"
            row$pct[row[[level]] >= 60 & row[[level]] < 65] <- "60-64"
            row$pct[row[[level]] >= 65 & row[[level]] < 70] <- "65-69"
            row$pct[row[[level]] >= 70 & row[[level]] < 75] <- "70-74"
            row$pct[row[[level]] >= 75 & row[[level]] < 80] <- "75-79"
            row$pct[row[[level]] >= 80 & row[[level]] < 85] <- "80-84"
            row$pct[row[[level]] >= 85 & row[[level]] < 90] <- "85-89"
            row$pct[row[[level]] >= 90 & row[[level]] < 95] <- "90-94"
            row$pct[row[[level]] >= 95] <- "GE 95"
            row$merge <- 0
        }
        
        # N = 21-40
        else if (row$NStudents > 20 & row$NStudents <= 40) {
            row$pct[row[[level]] < 11] <- "LE 10"
            row$pct[row[[level]] >= 11 & row[[level]] < 20] <- "11-19"
            row$pct[row[[level]] >= 20 & row[[level]] < 30] <- "20-29"
            row$pct[row[[level]] >= 30 & row[[level]] < 40] <- "30-39"
            row$pct[row[[level]] >= 40 & row[[level]] < 50] <- "40-49"
            row$pct[row[[level]] >= 50 & row[[level]] < 60] <- "50-59"
            row$pct[row[[level]] >= 60 & row[[level]] < 70] <- "60-69"
            row$pct[row[[level]] >= 70 & row[[level]] < 80] <- "70-79"
            row$pct[row[[level]] >= 80 & row[[level]] < 90] <- "80-89"
            row$pct[row[[level]] >= 90] <- "GE 90"
            row$merge <- 0
        }
        
        # N = 10-20
        else {
            if (level == "Level1" | level == "Level2") {
                row$pct <- row[["Level1"]] + row[["Level2"]]
            }
            else {
                row$pct <- row[["Level3"]] + row[["Level4"]]
            }
            row$merge <- 1
            row$pct1 <- row$pct
            row$pct[row$pct1 < 21] <- "LE 20"
            row$pct[row$pct1 >= 21 & row$pct1 < 30] <- "21-29"
            row$pct[row$pct1 >= 30 & row$pct1 < 40] <- "30-39"
            row$pct[row$pct1 >= 40 & row$pct1 < 50] <- "40-49"
            row$pct[row$pct1 >= 50 & row$pct1 < 60] <- "50-59"
            row$pct[row$pct1 >= 60 & row$pct1 < 70] <- "60-69"
            row$pct[row$pct1 >= 70 & row$pct1 < 80] <- "70-79"
            row$pct[row$pct1 >= 80] <- "GE 80"
            row$pct1 <- NULL
        }
        masked <- rbind(row, masked)    
    }
    masked <- masked[order(masked$SORT, 
                           masked$SORTCODE, 
                           masked$schnumb, 
                           masked$Grade), ]
}

level1 <- mask(web, "Level1")
colnames(level1)[15] <- "PL1"
colnames(level1)[16] <- "Merge1"

level2 <- mask(web, "Level2")
colnames(level2)[15] <- "PL2"
colnames(level2)[16] <- "Merge2"

level3 <- mask(web, "Level3")
colnames(level3)[15] <- "PL3"
colnames(level3)[16] <- "Merge3"

level4 <- mask(web, "Level4")
colnames(level4)[15] <- "PL4"
colnames(level4)[16] <- "Merge4"


# merge files
webfile <- cbind(level1, level2[c(15, 16)], level3[c(15, 16)], level4[c(15, 16)])
head(webfile)

# check if Merge1 and Merge2 are the same
all(webfile$Merge1 == webfile$Merge2) #true

# check if Merge3 and Merge4 are the same
all(webfile$Merge3 == webfile$Merge4) #true

# formatting and removing columns
webfile$PL1[webfile$Merge1 == 1] <- "^"
webfile$PL4[webfile$Merge4 == 1] <- "^"

names(webfile)
final <- webfile[c("schnumb", "DistrictName", "SchoolName", "Grade", "Group", 
                   "PL1", "PL2", "PL3", "PL4")]

final <- final[final$Group == "All Students", ]

final$Group <- NULL

# renames columns
names(final) <- c("Code", "District", "School", "Grade", 
                  "Level 1 (%)", "Level 2 (%)", "Level 3 (%)", "Level 4 (%)")
head(final)

# save output
current_date <- Sys.Date()
file_name <- paste0("SBA Science MASKED Web 2018-2019 ", current_date, ".csv")
write.csv(final, file = file_name, row.names = FALSE)


################################################################################
### the end
################################################################################
