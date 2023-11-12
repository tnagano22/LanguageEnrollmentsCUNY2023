RProcedureMLAEnrollmentSurvey2023
================
Tomonori Nagano
2023-11-12

# RProcedureACTFL2023JapaneseEnrollment

- To analyze the Modern Language Association (MLA)’s Enrollment Survey
  with a special focus on CUNY
- Some campuses are listed twice (e.g., CityTech) because of the changes
  of the institutional name that took place between 1965 and 2016.

## Setting up the environment

``` r
# clear the cache
rm(list = ls())

library(ggplot2); library(gdata); library(ggthemes); library(plyr); 
library(memisc); library(reshape2); library(devtools)

# turning off scientific notation of numbers
options(scipen=999)

#setwd("~/Desktop/")
setwd("~/Dropbox/Documents/USB/700LaGuardia/AdminModernLanguagesProgram/EnrollmentDataAnalysis/MLAEnrollmentSurveyCUNY2023/")

# change the default width
width.default <- getOption("width"); options(width=300)

# the add comma function
addComma<-function(x) {format(x, big.mark = ',', trim = TRUE, scientific = FALSE)}

# creating a notin function
`%notin%` <- Negate(`%in%`)
```

## Analyzing the MLA’s Enrollment Survey

- MLA’s Language Enrollment Database, 1958-2016
  (<http://apps.mla.org/flsurvey_search>)
- The data file “MLA_Historical_enrollments_1958-2016\_(3-13-19).xlsx”
  was obtained from the MLA’s website at
  <http://apps.mla.org/flsurvey_search>.

``` r
# importing data (Download the original data from https://apps.mla.org/flsurvey_search. Change xlsx to csv.)
thisData <- read.csv("MLA_Historical_enrollments_1958-2016_(3-13-19).csv", sep = ",")
# filling empty "UNIV_NAME_HISTORY"
thisData[thisData$UNIV_NAME_HISTORY=="",c("UNIV_NAME_HISTORY")] <- thisData[thisData$UNIV_NAME_HISTORY=="",c("UNIV")]
thisData$SRVY_YEAR <- as.factor(thisData$SRVY_YEAR)
thisData$TERM <- as.factor(thisData$TERM)
thisData$YR.TERM <- as.factor(thisData$YR.TERM)
thisData$UNIV <- as.factor(thisData$UNIV)
thisData$UNIV_NAME_HISTORY <- as.factor(thisData$UNIV_NAME_HISTORY)
thisData$CAMPUS <- as.factor(thisData$CAMPUS)
thisData$NCES_ID <- as.factor(thisData$NCES_ID)
thisData$STATE <- as.factor(thisData$STATE)
thisData$STATE_ID <- as.factor(thisData$STATE_ID)
thisData$MLA.ICLEVEL <- as.factor(thisData$MLA.ICLEVEL)
levels(thisData$MLA.ICLEVEL) = c("4 year","2 year")
thisData$LANG_CODE <- as.factor(thisData$LANG_CODE)
thisData$CITY <- as.factor(thisData$CITY)
thisData$LANGUAGE <- as.factor(thisData$LANGUAGE)
thisData$LANG_REGION <- as.factor(thisData$LANG_REGION)
thisData$OTHER_LANG <- as.factor(thisData$OTHER_LANG)
thisData$GEOGRAPHY_CODE <- as.factor(thisData$GEOGRAPHY_CODE)
thisData$N_RESP <- as.factor(thisData$N_RESP)
thisData$ZERO_ERL <- as.factor(thisData$ZERO_ERL)

# Between 1963 - 1972 many institutions did not report "UNDERGRAD_TOTAL" and "GRAD_TOTAL". We need to use "ALL_LEVEL_TOTAL" instead
thisData[is.na(thisData$UNDERGRAD_TOTAL),c("UNDERGRAD_TOTAL")] <- thisData[is.na(thisData$UNDERGRAD_TOTAL),c("ALL_LEVEL_TOTAL")]

# Retrieving all data
MLA <- drop.levels(thisData[thisData$YR.TERM %notin% c("1958 Fall", "1959 Fall", "1961 Fall", "1963 Fall", "1969 Summer", "1970 Fall", "1971 Summer", "2016 Summer"),],reorder=FALSE)

# selecting only CUNY
#MLA.CUNY <- drop.levels(MLA[grep("CUNY",MLA$UNIV),],reorder=FALSE)
MLA.CUNY <- drop.levels(MLA[MLA$UNIV %in% c("BARUCH C, CUNY","BOROUGH OF MANHATTAN COMM C, CUNY","BROOKLYN C, CUNY","C OF STATEN ISLAND, CUNY","CITY C OF NEW YORK, CUNY","HOSTOS COMM C, CUNY","HUNTER C, CUNY","JOHN JAY C OF CRIMINAL JUSTICE, CUNY","KINGSBOROUGH COMM C, CUNY","LAGUARDIA COMM C, CUNY","LEHMAN C, CUNY","QUEENS C, CUNY","MEDGAR EVERS C, CUNY","NEW YORK CITY C OF TECH, CUNY","QUEENSBOROUGH COMM C, CUNY","YORK C, CUNY"),],reorder=FALSE)

levels(MLA.CUNY$UNIV) <- c("Baruch","BMCC","Brooklyn","CSI","City","Hostos C","Hunter","John Jay","Kingsborough C","LaGuardia C","Lehman","Medger Evers","CityTech","Queens","Queensborough C","York")

MLA.CUNY.e <- tapply(MLA.CUNY$UNDERGRAD_TOTAL, list(MLA.CUNY$UNIV, MLA.CUNY$YR.TERM, MLA.CUNY$LANGUAGE),sum, na.rm=TRUE)
```

- Comparing the Fall 2009 enrollments and the Fall 2016 enrollments at
  CUNY

``` r
# The total number of enrollment in modern language courses in CUNY in Fall 2009
sum(MLA.CUNY.e[,"2009 Fall",],na.rm=TRUE)
```

    ## [1] 28662

``` r
# The total number of enrollment in modern language courses in CUNY in Fall 2016
sum(MLA.CUNY.e[,"2016 Fall",],na.rm=TRUE)
```

    ## [1] 31899

``` r
for (i in 1:length(names(MLA.CUNY.e[1,1,]))) {
  cat("\n\n")
  cat("-------------------------------------------\n")
  print(names(MLA.CUNY.e[1,1,])[i])
  cat("-------------------------------------------\n")
  cat(paste("Fall 2009:", sum(MLA.CUNY.e[,"2009 Fall",i],na.rm=TRUE)))
  cat("\n")
  cat(paste("Fall 2016:", sum(MLA.CUNY.e[,"2016 Fall",i],na.rm=TRUE)))
  cat("\n")
  cat(paste("Change from 2009 to 2016: ", format(sum(MLA.CUNY.e[,"2016 Fall",i],na.rm=TRUE)/sum(MLA.CUNY.e[,"2009 Fall",i],na.rm=TRUE)*100-100,digit=2)
),"%\n",sep="")}
```

    ## 
    ## 
    ## -------------------------------------------
    ## [1] "AMERICAN SIGN LANGUAGE (ASL)"
    ## -------------------------------------------
    ## Fall 2009: 347
    ## Fall 2016: 661
    ## Change from 2009 to 2016:  90%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "ARABIC"
    ## -------------------------------------------
    ## Fall 2009: 441
    ## Fall 2016: 1016
    ## Change from 2009 to 2016:  130%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "BENGALI/BANGLA"
    ## -------------------------------------------
    ## Fall 2009: 1
    ## Fall 2016: 15
    ## Change from 2009 to 2016:  1400%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CANTONESE"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CHINESE"
    ## -------------------------------------------
    ## Fall 2009: 1962
    ## Fall 2016: 2160
    ## Change from 2009 to 2016:  10%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CREOLE, HAITIAN"
    ## -------------------------------------------
    ## Fall 2009: 14
    ## Fall 2016: 25
    ## Change from 2009 to 2016:  79%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "DANISH"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "DUTCH"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "EGYPTIAN, ANCIENT"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 12
    ## Change from 2009 to 2016:  Inf%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "FRENCH"
    ## -------------------------------------------
    ## Fall 2009: 4048
    ## Fall 2016: 4561
    ## Change from 2009 to 2016:  13%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GERMAN"
    ## -------------------------------------------
    ## Fall 2009: 685
    ## Fall 2016: 852
    ## Change from 2009 to 2016:  24%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK"
    ## -------------------------------------------
    ## Fall 2009: 1
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, ANCIENT"
    ## -------------------------------------------
    ## Fall 2009: 225
    ## Fall 2016: 114
    ## Change from 2009 to 2016:  -49%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, MODERN"
    ## -------------------------------------------
    ## Fall 2009: 61
    ## Fall 2016: 21
    ## Change from 2009 to 2016:  -66%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW"
    ## -------------------------------------------
    ## Fall 2009: 138
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, BIBLICAL"
    ## -------------------------------------------
    ## Fall 2009: 26
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, MODERN"
    ## -------------------------------------------
    ## Fall 2009: 253
    ## Fall 2016: 477
    ## Change from 2009 to 2016:  89%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HINDI"
    ## -------------------------------------------
    ## Fall 2009: 44
    ## Fall 2016: 28
    ## Change from 2009 to 2016:  -36%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "IRISH"
    ## -------------------------------------------
    ## Fall 2009: 8
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "ITALIAN"
    ## -------------------------------------------
    ## Fall 2009: 3358
    ## Fall 2016: 3482
    ## Change from 2009 to 2016:  3.7%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "JAPANESE"
    ## -------------------------------------------
    ## Fall 2009: 1002
    ## Fall 2016: 1387
    ## Change from 2009 to 2016:  38%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "KOREAN"
    ## -------------------------------------------
    ## Fall 2009: 66
    ## Fall 2016: 183
    ## Change from 2009 to 2016:  177%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "LATIN"
    ## -------------------------------------------
    ## Fall 2009: 385
    ## Fall 2016: 312
    ## Change from 2009 to 2016:  -19%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "NORWEGIAN"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "OTHER LANGS"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "POLISH"
    ## -------------------------------------------
    ## Fall 2009: 58
    ## Fall 2016: 33
    ## Change from 2009 to 2016:  -43%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "PORTUGUESE"
    ## -------------------------------------------
    ## Fall 2009: 123
    ## Fall 2016: 296
    ## Change from 2009 to 2016:  141%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "RUSSIAN"
    ## -------------------------------------------
    ## Fall 2009: 510
    ## Fall 2016: 369
    ## Change from 2009 to 2016:  -28%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SPANISH"
    ## -------------------------------------------
    ## Fall 2009: 14906
    ## Fall 2016: 15895
    ## Change from 2009 to 2016:  6.6%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SWAHILI/KISWAHILI"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SWEDISH"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "UKRAINIAN"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "YIDDISH"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "YORUBA"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2016: 0
    ## Change from 2009 to 2016:  NaN%

- Showing the detailed ML enrollment data at CUNY

``` r
for (i in 1:length(names(MLA.CUNY.e[1,1,]))) {
  cat("\n\n")
  cat("-------------------------------------------\n")
  print(names(MLA.CUNY.e[1,1,])[i])
  cat("-------------------------------------------\n")
  print(MLA.CUNY.e[,,i])
  write.csv(MLA.CUNY.e[,,i], file= paste("data/CUNYMLEnrollmentMLAEnrollmentSurvey2023",gsub(",","-",gsub(" ","",gsub("/","-",names(MLA.CUNY.e[1,1,])[i]))),".csv",sep=""), row.names = TRUE)  
}
```

    ## 
    ## 
    ## -------------------------------------------
    ## [1] "AMERICAN SIGN LANGUAGE (ASL)"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       116       113       112       334       407
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        32        11
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        27        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       128       131       160
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        66        33
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        68        47        77        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        30        27        50
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "ARABIC"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        21        24
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       127
    ## Brooklyn               12        26        NA        32        62        28        NA        NA        NA        NA        NA        NA        42        30        26        50
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        68       111
    ## City                   NA        NA        NA        19        19        23        17        16        28        NA        22        27        42        65       151       125
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        90       138       147       173
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        13        62        59       104       133
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        90        58
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        48        30         9
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         4         3         2
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        29        34
    ## Queens                 NA        NA        NA        NA        45        26        38        27        42        70        77        61        48        68        83       101
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        91        69
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "BENGALI/BANGLA"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         1        NA        15
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CANTONESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CHINESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        76        88        85        63        63        71        94       225       168       175       215       230       282
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        26        52        63        87       154       237       282       253
    ## Brooklyn               38        60        NA       120       124       103        73       108       116       118        52        71        92       124       114       136
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        43        94       178       197
    ## City                   NA        NA        NA       147        75        39        28        15        22        38        28        25        69        83       188       151
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        77        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        37        NA       296       210       232       246       375       315       401       334       298       295       414       403       453
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        13        NA        NA        NA        12        50        83        NA        83
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        51        49        79        32
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA       131        11        24        19        97        58        60
    ## Lehman                 NA        NA        NA        16        NA        NA        NA        NA        NA        NA        NA        NA        NA         3         1         1
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        33        26        48
    ## Queens                 41        NA        NA       131       123       127        90        99       107       175        88       112       187       251       360       181
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       138       245       200       243
    ## York                   NA        NA        NA        10        NA        NA        NA        NA        NA        21         9        12        15        34        26        40
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CREOLE, HAITIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        21         0        11        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        17        13        14        12        25
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "DANISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        44        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "DUTCH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        10        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "EGYPTIAN, ANCIENT"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        12
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "FRENCH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA       152       357       298       282       312       278       289       203       350       205       227       240       243       283
    ## BMCC                  111       296        NA       271       319       325       291       361       282       425       369       386       390       564       684       869
    ## Brooklyn             2773      2268        NA      1216       943       657       411       349       359       143       227       190       235       277       238       151
    ## CSI                    NA        NA        NA        NA       102       133       145        77       116       126        NA       131        93       112       207       270
    ## City                 2613      1531      1143       546       379       238       225       236       210       208       145       127       190       329       681       663
    ## Hostos C               NA        NA        NA        73       121        85        67       463        53        45        42        69        34        43        73        55
    ## Hunter               3384      1737      1514      1233       851       916      1088       977       979       804       774       823       649       641       581       677
    ## John Jay               NA        NA        NA       100       138        76       110       136       158       175        NA       151       224       130        79        93
    ## Kingsborough C        332       316        NA       175       116       150       144       132       138       123       118       177       192       268       182       220
    ## LaGuardia C            NA        NA        NA       119        NA        30        32        22        73        NA        27        28        25       110       119       137
    ## Lehman                 NA       830       520       385       195       265       299       165       145        87        NA       143       122       176       160       166
    ## Medger Evers           NA        NA        44        32        45        16        17         8        NA        42        NA        54       102       141       198       121
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       100        94        53        69        61
    ## Queens               2570      3289      2135      1711      1046       758       592       432       436       355       177       151       233       294       342       300
    ## Queensborough C       345       476        NA       390       369       291       401       272       313       283       292       284       272       434       379       314
    ## York                   NA       191       161       230       252       138       161       122       155       267       187       148       161       236       200       181
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GERMAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        96        90        84        86        59        45        59        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                    8        81        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         0
    ## Brooklyn              720       545        NA       272       152        72        62        65        63        43        11        15        22        24        22        23
    ## CSI                    NA        NA        NA        NA        20        18        30        20        42        NA        NA        NA        NA        NA        NA        NA
    ## City                 1580       762       214       217       116        78        57        26        38        18        NA        NA        NA        60        NA        69
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA       117        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter               1219       594       587       561       375       315       308       389       397       241       341       378       301       458       392       537
    ## John Jay               NA        NA        NA       100        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        54        59
    ## Kingsborough C         NA        NA        NA        40        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA       363       631       240       118        40        71        20        23        11        NA        NA        NA         0         2         2
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens               1034       899       639       543       402       296       220       122       131       105        95        60        58        80       142       112
    ## Queensborough C        99       142        NA       182       203        68       119       113       132        67        44        46        47        63        68        50
    ## York                   NA        68        40        24        NA        12        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         1        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, ANCIENT"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               46        53        NA        31        23        NA        NA        NA        10        12        11         7        11        20        30        24
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        56        34         7        52        34        32         6        12        11        NA         4        80       138        16        18
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA         2        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 82        53        57        57        52        41        67        62        57        46        48        64        NA        67        54        63
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        12        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        15        29        33        11         5         4         4         4         3        NA        NA        NA         0         3         9
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 49        23        90        93       105        11        13         9        16        10        18         6         6        NA        11        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA         6         9         8        NA        NA        NA        NA        NA        35        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, MODERN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        30        NA        30        21        33        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        11        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA         8         8        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        23        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        62        92        86        NA        NA        NA        57        61        77        21
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA       122       137        82        90        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        57        11        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              508       555        NA       426       269       200        70        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                  179       229        NA        80        10        14        29        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                365       208        NA       190       246       283       379        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        51        NA        75        84        30        85        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA       146        NA       140        38        30        16        NA        NA        NA        NA        NA        NA         2        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                226       276        NA       735       459       278       195        NA        NA        NA        NA        NA        NA       136        NA        NA
    ## Queensborough C        NA        NA        NA       144       123        90       109        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        40        NA        31        22        13        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, BIBLICAL"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        37        NA        NA        25        40        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        72        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA         4         6        NA        NA        NA        NA        26        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA       402       351       271       285       242       111        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        93        NA        67        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        15        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA       215        49        40        99        64        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA       122        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, MODERN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        39        31        50        42        48        30        45        24
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        68        49        43        30        NA        33         6        25
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA         4        33        NA        NA        NA         0        NA        47
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        49        12        NA       210       179
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA       117        82        56        71        55       115        43        47
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        27        NA        NA        NA        NA        NA         1        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA       177       140        36        37        66        NA       117       132
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA       106        88        73       105        82        75        31        23
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HINDI"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        17         0        17        44        77        28
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "IRISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         8        11         0
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "ITALIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA       120       183       162       220       173       128       103       118       175       196       198       175       243       255
    ## BMCC                   16       158        NA        61        55        37        54        73        43        45       137        58       246       381       666       683
    ## Brooklyn              401       356        NA       436       390       294       163       189       244       191       172       141       149       118       146       103
    ## CSI                    NA        NA        NA        NA        87        91       125        88       136       126        NA       239       284       389       453       424
    ## City                  183       235       320       156       119       110       103        55        58        58        41        89       156       225       458       395
    ## Hostos C               NA        NA        NA        22        34        19        27       195        31        19        35        38        43        43        70        51
    ## Hunter                699       532       796       631       329       303       384       552       474       440       593       613       574       537       477       513
    ## John Jay               NA        NA        NA        75        54        36        40        49        58        84        55        99       138        87       146       114
    ## Kingsborough C         NA        15        NA       175       169       152       162       177       115       114       119       161       187       225       127       140
    ## LaGuardia C            NA        NA        NA        78        23        NA        NA        NA        NA        NA        NA        NA        26       119        93        93
    ## Lehman                 NA       288       480       388       227       179       203       113       120        92        NA        86       139       182       130       109
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                223       629       780       770       535       496       427       315       340       250       266       205       261       275       267       222
    ## Queensborough C        NA       312        NA       525       363       237       410       290       264       230       233       265       334       486       467       270
    ## York                   NA        65       118       121       115        62        53        35        86        58        61        62        81       116       109       110
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "JAPANESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       150       124       140       145       203       243
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        76        25        57
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        15        NA        NA        18        22        35        22        33        78        87       200       179
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        32        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       104       107       168       258       332       464
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        28        72       110
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       105       105       125
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        35        12        NA        36        47        77        80        91
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        55        10        NA        97       143       226       135       118
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "KOREAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        16        23
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        25        NA        NA        NA        66        70       160
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "LATIN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA         1        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              138       122        NA        94        97        NA        NA        NA        80        25        27        33        19        37        36        38
    ## CSI                    NA        NA        NA        NA        NA        14        13        16        NA        NA        NA        NA        NA        NA        NA        30
    ## City                  479       208        58        60        34        36        13         7        19        17        64         1        18       211        48        79
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        33        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                367       151        97       116        83        91       111       120       121       103       106       114        98       104        89        90
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        56
    ## Lehman                 NA        36        55        47        21        35        11         8         8        35        NA        13        27        22        19        19
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                118       135        22        79        44        47        30        21        27        15        23        22        21        11        27        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        14         5        12        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "NORWEGIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        12        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "OTHER LANGS"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA       226        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA       562        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA       579        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA       268        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        25        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA      1228        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        53        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "POLISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        12        NA         6        13        19        23        26        NA        NA        NA        50        40        33
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         8        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        21        11         7        NA         5        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "PORTUGUESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        20        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        23        23
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        11        NA         9        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   20        NA        NA        NA        NA        NA        NA        12         9         1        NA        14        28        45       123        69
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        66        NA        20        34        NA        NA        NA        NA        NA        NA        NA        NA        43
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        22        45        NA        82
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        15         6
    ## Lehman                 NA        NA        NA        17         6        NA        NA        NA        NA        NA        NA        NA        NA        NA         0        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 30        28        NA       202        50        77        36        16        21        15        NA        NA         5         4        39        73
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "RUSSIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        30        40        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              267       203        NA        49        70       130        96        24        92       145        38        61        62        52        32        42
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                  312       164        60        68        31        14        31        19         7        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        32        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                335       248       306       251       190       183       210       247       263       272       310       303       210       310       217       237
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        43        35        68        77        36        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        15        NA        NA
    ## Lehman                 NA        71       115        64        22        11        29        18        10        28        NA        12        21        24        35        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                215       206       200       259       151       130       107        68        81        80       168        89       100        73       125        90
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        13        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SPANISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA       788      1236       979       772       803       672       627       692       825       478       514       498       581       639
    ## BMCC                  125       616        NA      1241      1324      1200       715       856       903      1314      1648      2001      2170      2641      3073      3238
    ## Brooklyn             1926      1829        NA      1490      1328      1021       649       710       752       609       716       654       712       687       535       270
    ## CSI                    NA        NA        NA        NA       356       410       351       299       375       392        26       747       713       937       753       810
    ## City                 2237      1387      1389      1493      1166       678       446       513       431       635       450       654       806      1066      1654      2051
    ## Hostos C               NA        NA        NA       420       760       690       394       603       930      1052       214       516       402       311       340       192
    ## Hunter               1874      1861      2882      2857      2924      1598      1328      1427      1386      1253      1798      1859      1548      1630      1515      1831
    ## John Jay               NA       258        NA       800       651       567       512       549       728       726       516       902      1666       622       692       782
    ## Kingsborough C        395       579        NA       700       640       556       526       495       643       647       603       695       756      1157       780       660
    ## LaGuardia C            NA        NA        NA       455       165        42        52        66       265      1409       490       204       273       530       460       413
    ## Lehman                 NA       980      1452      1379      1178       984       853       750       760       826        NA       799       740       723       722       874
    ## Medger Evers           NA        NA       145       264       194       147        87       136        NA       454        NA       354       411       586       683       555
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       260       277       267       267       248
    ## Queens               2380      3808      2748      2696      1696      1389      1058       898      1014       980       939       925       845       995       977       978
    ## Queensborough C       430       649        NA      1161       886       645       890       797       858      1170      1026      1043      1271      1305      1419      1334
    ## York                   NA       355       335       566       603       399       364       344       448       809       730       635       801       951       869      1020
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SWAHILI/KISWAHILI"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        27        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        10        NA        17        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        36        NA        43        31        24        29        11        29        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        10        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        42        NA        20         5        11        14        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        25        NA        NA        NA        60        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        88        NA        17        15        NA        15        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SWEDISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        43        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "UKRAINIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA         8        NA         3        11         4         1        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "YIDDISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        51        35        32        22         3        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        80        NA        45        NA        NA        10        NA        NA        NA        NA        NA        NA         0        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        26        19        25        NA        15        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        24        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA       594       341       412        94        74        77        NA        NA        15        36        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA         9        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "YORUBA"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        21        NA        15        14        41        10        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA         6        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA

``` r
for (i in 1:length(names(MLA.CUNY.e[1,1,]))) {
  cat("\n")
  cat("-------------------------------------------\n")
  print(names(MLA.CUNY.e[1,1,])[i])
  cat("-------------------------------------------\n")
  print(MLA.CUNY.e[,,i])
  write.csv(MLA.CUNY.e[,,i], file= paste("data/CUNYMLEnrollmentMLAEnrollmentSurvey2023",gsub(",","-",gsub(" ","",gsub("/","-",names(MLA.CUNY.e[1,1,])[i]))),".csv",sep=""), row.names = TRUE)  
}
```

    ## 
    ## -------------------------------------------
    ## [1] "AMERICAN SIGN LANGUAGE (ASL)"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       116       113       112       334       407
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        32        11
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        27        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       128       131       160
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        66        33
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        68        47        77        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        30        27        50
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "ARABIC"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        21        24
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       127
    ## Brooklyn               12        26        NA        32        62        28        NA        NA        NA        NA        NA        NA        42        30        26        50
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        68       111
    ## City                   NA        NA        NA        19        19        23        17        16        28        NA        22        27        42        65       151       125
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        90       138       147       173
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        13        62        59       104       133
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        90        58
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        48        30         9
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         4         3         2
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        29        34
    ## Queens                 NA        NA        NA        NA        45        26        38        27        42        70        77        61        48        68        83       101
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        91        69
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "BENGALI/BANGLA"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         1        NA        15
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "CANTONESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "CHINESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        76        88        85        63        63        71        94       225       168       175       215       230       282
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        26        52        63        87       154       237       282       253
    ## Brooklyn               38        60        NA       120       124       103        73       108       116       118        52        71        92       124       114       136
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        43        94       178       197
    ## City                   NA        NA        NA       147        75        39        28        15        22        38        28        25        69        83       188       151
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        77        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        37        NA       296       210       232       246       375       315       401       334       298       295       414       403       453
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        13        NA        NA        NA        12        50        83        NA        83
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        51        49        79        32
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA       131        11        24        19        97        58        60
    ## Lehman                 NA        NA        NA        16        NA        NA        NA        NA        NA        NA        NA        NA        NA         3         1         1
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        33        26        48
    ## Queens                 41        NA        NA       131       123       127        90        99       107       175        88       112       187       251       360       181
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       138       245       200       243
    ## York                   NA        NA        NA        10        NA        NA        NA        NA        NA        21         9        12        15        34        26        40
    ## 
    ## -------------------------------------------
    ## [1] "CREOLE, HAITIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        21         0        11        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        17        13        14        12        25
    ## 
    ## -------------------------------------------
    ## [1] "DANISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        44        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "DUTCH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        10        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "EGYPTIAN, ANCIENT"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        12
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "FRENCH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA       152       357       298       282       312       278       289       203       350       205       227       240       243       283
    ## BMCC                  111       296        NA       271       319       325       291       361       282       425       369       386       390       564       684       869
    ## Brooklyn             2773      2268        NA      1216       943       657       411       349       359       143       227       190       235       277       238       151
    ## CSI                    NA        NA        NA        NA       102       133       145        77       116       126        NA       131        93       112       207       270
    ## City                 2613      1531      1143       546       379       238       225       236       210       208       145       127       190       329       681       663
    ## Hostos C               NA        NA        NA        73       121        85        67       463        53        45        42        69        34        43        73        55
    ## Hunter               3384      1737      1514      1233       851       916      1088       977       979       804       774       823       649       641       581       677
    ## John Jay               NA        NA        NA       100       138        76       110       136       158       175        NA       151       224       130        79        93
    ## Kingsborough C        332       316        NA       175       116       150       144       132       138       123       118       177       192       268       182       220
    ## LaGuardia C            NA        NA        NA       119        NA        30        32        22        73        NA        27        28        25       110       119       137
    ## Lehman                 NA       830       520       385       195       265       299       165       145        87        NA       143       122       176       160       166
    ## Medger Evers           NA        NA        44        32        45        16        17         8        NA        42        NA        54       102       141       198       121
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       100        94        53        69        61
    ## Queens               2570      3289      2135      1711      1046       758       592       432       436       355       177       151       233       294       342       300
    ## Queensborough C       345       476        NA       390       369       291       401       272       313       283       292       284       272       434       379       314
    ## York                   NA       191       161       230       252       138       161       122       155       267       187       148       161       236       200       181
    ## 
    ## -------------------------------------------
    ## [1] "GERMAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        96        90        84        86        59        45        59        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                    8        81        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         0
    ## Brooklyn              720       545        NA       272       152        72        62        65        63        43        11        15        22        24        22        23
    ## CSI                    NA        NA        NA        NA        20        18        30        20        42        NA        NA        NA        NA        NA        NA        NA
    ## City                 1580       762       214       217       116        78        57        26        38        18        NA        NA        NA        60        NA        69
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA       117        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter               1219       594       587       561       375       315       308       389       397       241       341       378       301       458       392       537
    ## John Jay               NA        NA        NA       100        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        54        59
    ## Kingsborough C         NA        NA        NA        40        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA       363       631       240       118        40        71        20        23        11        NA        NA        NA         0         2         2
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens               1034       899       639       543       402       296       220       122       131       105        95        60        58        80       142       112
    ## Queensborough C        99       142        NA       182       203        68       119       113       132        67        44        46        47        63        68        50
    ## York                   NA        68        40        24        NA        12        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "GREEK"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         1        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, ANCIENT"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               46        53        NA        31        23        NA        NA        NA        10        12        11         7        11        20        30        24
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        56        34         7        52        34        32         6        12        11        NA         4        80       138        16        18
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA         2        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 82        53        57        57        52        41        67        62        57        46        48        64        NA        67        54        63
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        12        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        15        29        33        11         5         4         4         4         3        NA        NA        NA         0         3         9
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 49        23        90        93       105        11        13         9        16        10        18         6         6        NA        11        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA         6         9         8        NA        NA        NA        NA        NA        35        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, MODERN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        30        NA        30        21        33        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        11        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA         8         8        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        23        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        62        92        86        NA        NA        NA        57        61        77        21
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA       122       137        82        90        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        57        11        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              508       555        NA       426       269       200        70        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                  179       229        NA        80        10        14        29        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                365       208        NA       190       246       283       379        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        51        NA        75        84        30        85        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA       146        NA       140        38        30        16        NA        NA        NA        NA        NA        NA         2        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                226       276        NA       735       459       278       195        NA        NA        NA        NA        NA        NA       136        NA        NA
    ## Queensborough C        NA        NA        NA       144       123        90       109        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        40        NA        31        22        13        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, BIBLICAL"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        37        NA        NA        25        40        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        72        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA         4         6        NA        NA        NA        NA        26        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA       402       351       271       285       242       111        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        93        NA        67        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        15        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA       215        49        40        99        64        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA       122        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, MODERN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        39        31        50        42        48        30        45        24
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        68        49        43        30        NA        33         6        25
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA         4        33        NA        NA        NA         0        NA        47
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        49        12        NA       210       179
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA       117        82        56        71        55       115        43        47
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        27        NA        NA        NA        NA        NA         1        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA       177       140        36        37        66        NA       117       132
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA       106        88        73       105        82        75        31        23
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "HINDI"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        17         0        17        44        77        28
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "IRISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         8        11         0
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "ITALIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA       120       183       162       220       173       128       103       118       175       196       198       175       243       255
    ## BMCC                   16       158        NA        61        55        37        54        73        43        45       137        58       246       381       666       683
    ## Brooklyn              401       356        NA       436       390       294       163       189       244       191       172       141       149       118       146       103
    ## CSI                    NA        NA        NA        NA        87        91       125        88       136       126        NA       239       284       389       453       424
    ## City                  183       235       320       156       119       110       103        55        58        58        41        89       156       225       458       395
    ## Hostos C               NA        NA        NA        22        34        19        27       195        31        19        35        38        43        43        70        51
    ## Hunter                699       532       796       631       329       303       384       552       474       440       593       613       574       537       477       513
    ## John Jay               NA        NA        NA        75        54        36        40        49        58        84        55        99       138        87       146       114
    ## Kingsborough C         NA        15        NA       175       169       152       162       177       115       114       119       161       187       225       127       140
    ## LaGuardia C            NA        NA        NA        78        23        NA        NA        NA        NA        NA        NA        NA        26       119        93        93
    ## Lehman                 NA       288       480       388       227       179       203       113       120        92        NA        86       139       182       130       109
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                223       629       780       770       535       496       427       315       340       250       266       205       261       275       267       222
    ## Queensborough C        NA       312        NA       525       363       237       410       290       264       230       233       265       334       486       467       270
    ## York                   NA        65       118       121       115        62        53        35        86        58        61        62        81       116       109       110
    ## 
    ## -------------------------------------------
    ## [1] "JAPANESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       150       124       140       145       203       243
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        76        25        57
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        15        NA        NA        18        22        35        22        33        78        87       200       179
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        32        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       104       107       168       258       332       464
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        28        72       110
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       105       105       125
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        35        12        NA        36        47        77        80        91
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        55        10        NA        97       143       226       135       118
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "KOREAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        16        23
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        25        NA        NA        NA        66        70       160
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "LATIN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA         1        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              138       122        NA        94        97        NA        NA        NA        80        25        27        33        19        37        36        38
    ## CSI                    NA        NA        NA        NA        NA        14        13        16        NA        NA        NA        NA        NA        NA        NA        30
    ## City                  479       208        58        60        34        36        13         7        19        17        64         1        18       211        48        79
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        33        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                367       151        97       116        83        91       111       120       121       103       106       114        98       104        89        90
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        56
    ## Lehman                 NA        36        55        47        21        35        11         8         8        35        NA        13        27        22        19        19
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                118       135        22        79        44        47        30        21        27        15        23        22        21        11        27        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        14         5        12        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "NORWEGIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        12        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "OTHER LANGS"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA       226        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA       562        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA       579        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA       268        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        25        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA      1228        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        53        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "POLISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        12        NA         6        13        19        23        26        NA        NA        NA        50        40        33
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         8        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        21        11         7        NA         5        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "PORTUGUESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        20        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        23        23
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        11        NA         9        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   20        NA        NA        NA        NA        NA        NA        12         9         1        NA        14        28        45       123        69
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        66        NA        20        34        NA        NA        NA        NA        NA        NA        NA        NA        43
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        22        45        NA        82
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        15         6
    ## Lehman                 NA        NA        NA        17         6        NA        NA        NA        NA        NA        NA        NA        NA        NA         0        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 30        28        NA       202        50        77        36        16        21        15        NA        NA         5         4        39        73
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "RUSSIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        30        40        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              267       203        NA        49        70       130        96        24        92       145        38        61        62        52        32        42
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                  312       164        60        68        31        14        31        19         7        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        32        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                335       248       306       251       190       183       210       247       263       272       310       303       210       310       217       237
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        43        35        68        77        36        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        15        NA        NA
    ## Lehman                 NA        71       115        64        22        11        29        18        10        28        NA        12        21        24        35        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                215       206       200       259       151       130       107        68        81        80       168        89       100        73       125        90
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        13        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "SPANISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA       788      1236       979       772       803       672       627       692       825       478       514       498       581       639
    ## BMCC                  125       616        NA      1241      1324      1200       715       856       903      1314      1648      2001      2170      2641      3073      3238
    ## Brooklyn             1926      1829        NA      1490      1328      1021       649       710       752       609       716       654       712       687       535       270
    ## CSI                    NA        NA        NA        NA       356       410       351       299       375       392        26       747       713       937       753       810
    ## City                 2237      1387      1389      1493      1166       678       446       513       431       635       450       654       806      1066      1654      2051
    ## Hostos C               NA        NA        NA       420       760       690       394       603       930      1052       214       516       402       311       340       192
    ## Hunter               1874      1861      2882      2857      2924      1598      1328      1427      1386      1253      1798      1859      1548      1630      1515      1831
    ## John Jay               NA       258        NA       800       651       567       512       549       728       726       516       902      1666       622       692       782
    ## Kingsborough C        395       579        NA       700       640       556       526       495       643       647       603       695       756      1157       780       660
    ## LaGuardia C            NA        NA        NA       455       165        42        52        66       265      1409       490       204       273       530       460       413
    ## Lehman                 NA       980      1452      1379      1178       984       853       750       760       826        NA       799       740       723       722       874
    ## Medger Evers           NA        NA       145       264       194       147        87       136        NA       454        NA       354       411       586       683       555
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       260       277       267       267       248
    ## Queens               2380      3808      2748      2696      1696      1389      1058       898      1014       980       939       925       845       995       977       978
    ## Queensborough C       430       649        NA      1161       886       645       890       797       858      1170      1026      1043      1271      1305      1419      1334
    ## York                   NA       355       335       566       603       399       364       344       448       809       730       635       801       951       869      1020
    ## 
    ## -------------------------------------------
    ## [1] "SWAHILI/KISWAHILI"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        27        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        10        NA        17        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        36        NA        43        31        24        29        11        29        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        10        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        42        NA        20         5        11        14        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        25        NA        NA        NA        60        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        88        NA        17        15        NA        15        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "SWEDISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        43        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "UKRAINIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA         8        NA         3        11         4         1        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "YIDDISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        51        35        32        22         3        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        80        NA        45        NA        NA        10        NA        NA        NA        NA        NA        NA         0        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        26        19        25        NA        15        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        24        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA       594       341       412        94        74        77        NA        NA        15        36        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA         9        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## -------------------------------------------
    ## [1] "YORUBA"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1972 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        21        NA        15        14        41        10        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA         6        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
