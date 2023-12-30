RProcedureMLAEnrollmentSurveyCUNY2023
================
Tomonori Nagano
2023-11-15

# RProcedureMLAEnrollmentSurveyCUNY2023

- To analyze the Modern Language Association (MLA)’s Enrollment Survey
  with a special focus on CUNY
- Some campuses are listed twice (e.g., CityTech) because of the changes
  of the institutional name that took place between 1965 and 2021.

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

- MLA’s Language Enrollment Database, 1958-2021
  (<http://apps.mla.org/flsurvey_search>)
- The data file “Historical-language-enrollments-1958-2021.xlsx” was
  obtained from the MLA’s website at
  <http://apps.mla.org/flsurvey_search>.

``` r
# importing data (Download the original data from https://apps.mla.org/flsurvey_search. Change xlsx to csv.)
thisData <- read.csv("Historical-language-enrollments-1958-2021.csv", sep = ",")
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
# NOTE: From the 2021 data, the ICLEVEL is a three-level factor: 4Y, 2Y, and 2Y with some 4Y degrees
levels(thisData$MLA.ICLEVEL) = c("4 year","2 year","2 year")
thisData$LANG_CODE <- as.factor(thisData$LANG_CODE)
thisData$CITY <- as.factor(thisData$CITY)
thisData$LANGUAGE <- as.factor(thisData$LANGUAGE)
thisData$LANG_REGION <- as.factor(thisData$LANG_REGION)
thisData$OTHER_LANG <- as.factor(thisData$OTHER_LANG)
thisData$GEOGRAPHY_CODE <- as.factor(thisData$GEOGRAPHY_CODE)
thisData$N_RESP <- as.factor(thisData$N_RESP)
thisData$ZERO_ERL <- as.factor(thisData$ZERO_ERL)

# Between 1963 - 1972 many institutions did not report "UNDERGRAD_TOTAL" and "GRAD_TOTAL". We need to use "ALL_LEVEL_TOTAL" instead
# Note: From the 2021 data, UNDERGRAD_TOTAL is renamed  to UG.TOTAL (similar changes happened in other column names)
thisData[is.na(thisData$UG.TOTAL),c("UG.TOTAL")] <- thisData[is.na(thisData$UG.TOTAL),c("ALL.LEVEL.TOTAL")]

# Retrieving all data (for some reason, there is no enrollment data for some community colleges in 1972)
MLA <- drop.levels(thisData[thisData$YR.TERM %notin% c("1958 Fall", "1959 Fall", "1961 Fall", "1963 Fall", "1969 Summer", "1970 Fall", "1971 Summer", "1972 Fall", "2016 Summer", "2020 Fall"),],reorder=FALSE)

# selecting only CUNY
#MLA.CUNY <- drop.levels(MLA[grep("CUNY",MLA$UNIV),],reorder=FALSE)
MLA.CUNY <- drop.levels(MLA[MLA$UNIV %in% c("BARUCH C, CUNY","BOROUGH OF MANHATTAN COMM C, CUNY","BRONX COMM C, CUNY", "BROOKLYN C, CUNY", "C OF STATEN ISLAND, CUNY", "CITY C OF NEW YORK, CUNY", "HOSTOS COMM C, CUNY", "HUNTER C, CUNY", "JOHN JAY C OF CRIMINAL JUSTICE, CUNY", "KINGSBOROUGH COMM C, CUNY", "LAGUARDIA COMM C, CUNY", "LEHMAN C, CUNY", "MEDGAR EVERS C, CUNY", "NEW YORK CITY C OF TECH, CUNY", "QUEENS C, CUNY", "QUEENSBOROUGH COMM C, CUNY","YORK C, CUNY"),],reorder=FALSE)

levels(MLA.CUNY$UNIV) <- c("Baruch","BMCC", "BCC", "Brooklyn","CSI","City","Hostos C","Hunter","John Jay","Kingsborough C","LaGuardia C","Lehman","Medger Evers","CityTech","Queens","Queensborough C","York")

MLA.CUNY.e <- tapply(MLA.CUNY$UG.TOTAL, list(MLA.CUNY$UNIV, MLA.CUNY$YR.TERM, MLA.CUNY$LANGUAGE),sum, na.rm=TRUE)
MLA.CUNY.e3 <- tapply(MLA.CUNY$UG.TOTAL, list(MLA.CUNY$UNIV, MLA.CUNY$YR.TERM),sum, na.rm=TRUE)
```

- Comparing the Fall 2009 enrollments and the Fall 2016 enrollments at
  CUNY

``` r
# The total number of enrollment in modern language courses in CUNY in Fall 2009
sum(MLA.CUNY.e[,"2009 Fall",],na.rm=TRUE)
```

    ## [1] 30115

``` r
# The total number of enrollment in modern language courses in CUNY in Fall 2016
sum(MLA.CUNY.e[,"2016 Fall",],na.rm=TRUE)
```

    ## [1] 33023

``` r
# The total number of enrollment in modern language courses in CUNY in Fall 2021
sum(MLA.CUNY.e[,"2021 Fall",],na.rm=TRUE)
```

    ## [1] 25459

``` r
for (i in 1:length(names(MLA.CUNY.e[1,1,]))) {
  cat("\n\n")
  cat("-------------------------------------------\n")
  print(names(MLA.CUNY.e[1,1,])[i])
  cat("-------------------------------------------\n")
  cat(paste("Fall 2009:", sum(MLA.CUNY.e[,"2009 Fall",i],na.rm=TRUE)))
  cat("\n")
  cat(paste("Fall 2021:", sum(MLA.CUNY.e[,"2021 Fall",i],na.rm=TRUE)))
  cat("\n")
  cat(paste("Change from 2009 to 2021: ", format(sum(MLA.CUNY.e[,"2021 Fall",i],na.rm=TRUE)/sum(MLA.CUNY.e[,"2009 Fall",i],na.rm=TRUE)*100-100,digit=2)
),"%\n",sep="")}
```

    ## 
    ## 
    ## -------------------------------------------
    ## [1] "AMERICAN SIGN LANGUAGE (ASL)"
    ## -------------------------------------------
    ## Fall 2009: 347
    ## Fall 2021: 762
    ## Change from 2009 to 2021:  120%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "ARABIC"
    ## -------------------------------------------
    ## Fall 2009: 441
    ## Fall 2021: 847
    ## Change from 2009 to 2021:  92%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "BENGALI/BANGLA"
    ## -------------------------------------------
    ## Fall 2009: 1
    ## Fall 2021: 28
    ## Change from 2009 to 2021:  2700%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CANTONESE"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CHINESE"
    ## -------------------------------------------
    ## Fall 2009: 1962
    ## Fall 2021: 1293
    ## Change from 2009 to 2021:  -34%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CREOLE, HAITIAN"
    ## -------------------------------------------
    ## Fall 2009: 14
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "DANISH"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "DUTCH"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "EGYPTIAN, ANCIENT"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "FRENCH"
    ## -------------------------------------------
    ## Fall 2009: 4284
    ## Fall 2021: 3474
    ## Change from 2009 to 2021:  -19%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GERMAN"
    ## -------------------------------------------
    ## Fall 2009: 685
    ## Fall 2021: 595
    ## Change from 2009 to 2021:  -13%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK"
    ## -------------------------------------------
    ## Fall 2009: 1
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, ANCIENT"
    ## -------------------------------------------
    ## Fall 2009: 225
    ## Fall 2021: 93
    ## Change from 2009 to 2021:  -59%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, MODERN"
    ## -------------------------------------------
    ## Fall 2009: 61
    ## Fall 2021: 32
    ## Change from 2009 to 2021:  -48%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW"
    ## -------------------------------------------
    ## Fall 2009: 138
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, BIBLICAL"
    ## -------------------------------------------
    ## Fall 2009: 26
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, MODERN"
    ## -------------------------------------------
    ## Fall 2009: 253
    ## Fall 2021: 270
    ## Change from 2009 to 2021:  6.7%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HINDI"
    ## -------------------------------------------
    ## Fall 2009: 44
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  -100%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "IRISH"
    ## -------------------------------------------
    ## Fall 2009: 8
    ## Fall 2021: 24
    ## Change from 2009 to 2021:  200%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "ITALIAN"
    ## -------------------------------------------
    ## Fall 2009: 3534
    ## Fall 2021: 2707
    ## Change from 2009 to 2021:  -23%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "JAPANESE"
    ## -------------------------------------------
    ## Fall 2009: 1002
    ## Fall 2021: 1436
    ## Change from 2009 to 2021:  43%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "KOREAN"
    ## -------------------------------------------
    ## Fall 2009: 66
    ## Fall 2021: 191
    ## Change from 2009 to 2021:  189%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "LATIN"
    ## -------------------------------------------
    ## Fall 2009: 385
    ## Fall 2021: 263
    ## Change from 2009 to 2021:  -32%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "MANDARIN"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 723
    ## Change from 2009 to 2021:  Inf%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "NAHUATL LANGS"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 1
    ## Change from 2009 to 2021:  Inf%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "NORWEGIAN"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "POLISH"
    ## -------------------------------------------
    ## Fall 2009: 58
    ## Fall 2021: 55
    ## Change from 2009 to 2021:  -5.2%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "PORTUGUESE"
    ## -------------------------------------------
    ## Fall 2009: 179
    ## Fall 2021: 328
    ## Change from 2009 to 2021:  83%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "RUSSIAN"
    ## -------------------------------------------
    ## Fall 2009: 510
    ## Fall 2021: 252
    ## Change from 2009 to 2021:  -51%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SPANISH"
    ## -------------------------------------------
    ## Fall 2009: 15891
    ## Fall 2021: 12085
    ## Change from 2009 to 2021:  -24%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SWAHILI/KISWAHILI"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SWEDISH"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "UKRAINIAN"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "YIDDISH"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "YORUBA"
    ## -------------------------------------------
    ## Fall 2009: 0
    ## Fall 2021: 0
    ## Change from 2009 to 2021:  NaN%

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
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       105
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       116       113       112       334       407       393
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        32        11        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        27        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       128       131       160       161
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        66        33        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        68        47        77        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        30        27        50        24
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        50
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "ARABIC"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        21        24        24
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       127        97
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        28        28         9
    ## Brooklyn               12        26        32        62        28        NA        NA        NA        NA        NA        NA        42        30        26        50        87
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        68       111        92
    ## City                   NA        NA        19        19        23        17        16        28        NA        22        27        42        65       151       125        99
    ## Hostos C               NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        90       138       147       173       115
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        13        62        59       104       133        87
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        90        58        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        48        30         9        22
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         4         3         2         7
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        29        34        21
    ## Queens                 NA        NA        NA        45        26        38        27        42        70        77        61        48        68        83       101       101
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        91        69        39
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        47
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "BENGALI/BANGLA"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         1        NA        15        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        28
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CANTONESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA        NA
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
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        76        88        85        63        63        71        94       225       168       175       215       230       282       294
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        26        52        63        87       154       237       282       253       136
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               38        60       120       124       103        73       108       116       118        52        71        92       124       114       136       187
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        43        94       178       197       127
    ## City                   NA        NA       147        75        39        28        15        22        38        28        25        69        83       188       151        98
    ## Hostos C               NA        NA        NA        NA        NA        NA        77        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        37       296       210       232       246       375       315       401       334       298       295       414       403       453        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        13        NA        NA        NA        12        50        83        NA        83        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        51        49        79        32        30
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA       131        11        24        19        97        58        60        17
    ## Lehman                 NA        NA        16        NA        NA        NA        NA        NA        NA        NA        NA        NA         3         1         1        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        33        26        48        15
    ## Queens                 41        NA       131       123       127        90        99       107       175        88       112       187       251       360       181       220
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       138       245       200       243       103
    ## York                   NA        NA        10        NA        NA        NA        NA        NA        21         9        12        15        34        26        40        66
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "CREOLE, HAITIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        21         0        11        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         0
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        29        17        13        14        12        25        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "DANISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
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
    ## Queens                 NA        NA        44        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "DUTCH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
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
    ## Queens                 NA        NA        NA        10        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "EGYPTIAN, ANCIENT"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        12        NA
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
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA       357       298       282       312       278       289       203       350       205       227       240       243       283       184
    ## BMCC                  111       296       271       319       325       291       361       282       425       369       386       390       564       684       869       303
    ## BCC                   522       455       264       181       114        68       117       127       131       111       106       153       236       264       274       135
    ## Brooklyn             2773      2268      1216       943       657       411       349       359       143       227       190       235       277       238       151       195
    ## CSI                    NA        NA        NA       102       133       145        77       116       126        NA       131        93       112       207       270       240
    ## City                 2613      1531       546       379       238       225       236       210       208       145       127       190       329       681       663       250
    ## Hostos C               NA        NA        73       121        85        67       463        53        45        42        69        34        43        73        55        32
    ## Hunter               3384      1737      1233       851       916      1088       977       979       804       774       823       649       641       581       677       792
    ## John Jay               NA        NA       100       138        76       110       136       158       175        NA       151       224       130        79        93        85
    ## Kingsborough C        332       316       175       116       150       144       132       138       123       118       177       192       268       182       220       147
    ## LaGuardia C            NA        NA       119        NA        30        32        22        73        NA        27        28        25       110       119       137        41
    ## Lehman                 NA       830       385       195       265       299       165       145        87        NA       143       122       176       160       166       146
    ## Medger Evers           NA        NA        32        45        16        17         8        NA        42        NA        54       102       141       198       121       159
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       100        94        53        69        61        60
    ## Queens               2570      3289      1711      1046       758       592       432       436       355       177       151       233       294       342       300       258
    ## Queensborough C       345       476       390       369       291       401       272       313       283       292       284       272       434       379       314       141
    ## York                   NA       191       230       252       138       161       122       155       267       187       148       161       236       200       181       306
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GERMAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        90        84        86        59        45        59        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                    8        81        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         0        NA
    ## BCC                   163       163       108        79        18        13        15        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              720       545       272       152        72        62        65        63        43        11        15        22        24        22        23        34
    ## CSI                    NA        NA        NA        20        18        30        20        42        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                 1580       762       217       116        78        57        26        38        18        NA        NA        NA        60        NA        69        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA       117        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter               1219       594       561       375       315       308       389       397       241       341       378       301       458       392       537       354
    ## John Jay               NA        NA       100        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        54        59        53
    ## Kingsborough C         NA        NA        40        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA       363       240       118        40        71        20        23        11        NA        NA        NA         0         2         2        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens               1034       899       543       402       296       220       122       131       105        95        60        58        80       142       112       100
    ## Queensborough C        99       142       182       203        68       119       113       132        67        44        46        47        63        68        50        54
    ## York                   NA        68        24        NA        12        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         1        NA        NA        NA
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
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               46        53        31        23        NA        NA        NA        10        12        11         7        11        20        30        24        34
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        56         7        52        34        32         6        12        11        NA         4        80       138        16        18        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA         2        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 82        53        57        52        41        67        62        57        46        48        64        NA        67        54        63        45
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        12        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        15        33        11         5         4         4         4         3        NA        NA        NA         0         3         9        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 49        23        93       105        11        13         9        16        10        18         6         6        NA        11        NA        14
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA         6         8        NA        NA        NA        NA        NA        35        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "GREEK, MODERN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        30        NA        30        21        33        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        11        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA         8         8        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        23        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        62        92        86        NA        NA        NA        57        61        77        21        32
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA       122       137        82        90        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        57        11        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        17        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              508       555       426       269       200        70        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                  179       229        80        10        14        29        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                365       208       190       246       283       379        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        51        75        84        30        85        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA       146       140        38        30        16        NA        NA        NA        NA        NA        NA         2        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                226       276       735       459       278       195        NA        NA        NA        NA        NA        NA       136        NA        NA        NA
    ## Queensborough C        NA        NA       144       123        90       109        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        40        31        22        13        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, BIBLICAL"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        37        NA        NA        25        40        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        72        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA         4         6        NA        NA        NA        NA        26        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA       402       351       271       285       242       111        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        93        NA        67        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        15        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA       215        49        40        99        64        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA       122        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HEBREW, MODERN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        39        31        50        42        48        30        45        24         0
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        68        49        43        30        NA        33         6        25        34
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA         4        33        NA        NA        NA         0        NA        47        16
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        49        12        NA       210       179       100
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA       117        82        56        71        55       115        43        47        21
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        27        NA        NA        NA        NA        NA         1        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA       177       140        36        37        66        NA       117       132        99
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA       106        88        73       105        82        75        31        23        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "HINDI"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        17         0        17        44        77        28        NA
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
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         8        11         0        24
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
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA       183       162       220       173       128       103       118       175       196       198       175       243       255       220
    ## BMCC                   16       158        61        55        37        54        73        43        45       137        58       246       381       666       683       251
    ## BCC                    NA       161        89        36        40        17        18        25        47        37        52        68       176       247       203       136
    ## Brooklyn              401       356       436       390       294       163       189       244       191       172       141       149       118       146       103       118
    ## CSI                    NA        NA        NA        87        91       125        88       136       126        NA       239       284       389       453       424       487
    ## City                  183       235       156       119       110       103        55        58        58        41        89       156       225       458       395       198
    ## Hostos C               NA        NA        22        34        19        27       195        31        19        35        38        43        43        70        51        14
    ## Hunter                699       532       631       329       303       384       552       474       440       593       613       574       537       477       513       503
    ## John Jay               NA        NA        75        54        36        40        49        58        84        55        99       138        87       146       114       157
    ## Kingsborough C         NA        15       175       169       152       162       177       115       114       119       161       187       225       127       140        66
    ## LaGuardia C            NA        NA        78        23        NA        NA        NA        NA        NA        NA        NA        26       119        93        93        22
    ## Lehman                 NA       288       388       227       179       203       113       120        92        NA        86       139       182       130       109       119
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                223       629       770       535       496       427       315       340       250       266       205       261       275       267       222       225
    ## Queensborough C        NA       312       525       363       237       410       290       264       230       233       265       334       486       467       270       104
    ## York                   NA        65       121       115        62        53        35        86        58        61        62        81       116       109       110        87
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "JAPANESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA       150       124       140       145       203       243       198
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         9
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        14        NA        NA        NA        NA        76        25        57        87
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        15        NA        NA        18        22        35        22        33        78        87       200       179       127
    ## Hostos C               NA        NA        NA        NA        NA        NA        32        NA        NA        NA        NA        NA        NA        NA        NA        50
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA       104       107       168       258       332       464       504
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        28        72       110       150
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       105       105       125        87
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        35        12        NA        36        47        77        80        91        70
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        55        10        NA        97       143       226       135       118       154
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "KOREAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        16        23        35
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        25        NA        NA        NA        66        70       160       156
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "LATIN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA         1        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              138       122        94        97        NA        NA        NA        80        25        27        33        19        37        36        38        70
    ## CSI                    NA        NA        NA        NA        14        13        16        NA        NA        NA        NA        NA        NA        NA        30         0
    ## City                  479       208        60        34        36        13         7        19        17        64         1        18       211        48        79        28
    ## Hostos C               NA        NA        NA        NA        NA        NA        33        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                367       151       116        83        91       111       120       121       103       106       114        98       104        89        90       143
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         0        NA
    ## Lehman                 NA        36        47        21        35        11         8         8        35        NA        13        27        22        19        19        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                118       135        79        44        47        30        21        27        15        23        22        21        11        27        NA        22
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        14        12        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "MANDARIN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       661
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        60
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         2
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "NAHUATL LANGS"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         1
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "NORWEGIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
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
    ## [1] "POLISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        12        NA         6        13        19        23        26        NA        NA        NA        50        40        33        55
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA         8        NA        NA        NA
    ## Lehman                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        21        11         7        NA         5        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "PORTUGUESE"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        20        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        23        23        37
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        56        58        66        35
    ## Brooklyn               NA        11         9        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   20        NA        NA        NA        NA        NA        12         9         1        NA        14        28        45       123        69        64
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        66        NA        20        34        NA        NA        NA        NA        NA        NA        NA        NA        43        36
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        22        45        NA        82        73
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        29        15         6        NA
    ## Lehman                 NA        NA        17         6        NA        NA        NA        NA        NA        NA        NA        NA        NA         0        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 30        28       202        50        77        36        16        21        15        NA        NA         5         4        39        73        83
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "RUSSIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        40        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    32        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn              267       203        49        70       130        96        24        92       145        38        61        62        52        32        42        49
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                  312       164        68        31        14        31        19         7        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        32        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                335       248       251       190       183       210       247       263       272       310       303       210       310       217       237       137
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        43        35        68        77        36        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        15        NA        NA        NA
    ## Lehman                 NA        71        64        22        11        29        18        10        28        NA        12        21        24        35        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                215       206       259       151       130       107        68        81        80       168        89       100        73       125        90        66
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        13        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SPANISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA      1236       979       772       803       672       627       692       825       478       514       498       581       639       463
    ## BMCC                  125       616      1241      1324      1200       715       856       903      1314      1648      2001      2170      2641      3073      3238      1291
    ## BCC                   694       620      1815      1373       754       382       627       598      1034       740       650       660       985       725       609       440
    ## Brooklyn             1926      1829      1490      1328      1021       649       710       752       609       716       654       712       687       535       270       394
    ## CSI                    NA        NA        NA       356       410       351       299       375       392        26       747       713       937       753       810      1074
    ## City                 2237      1387      1493      1166       678       446       513       431       635       450       654       806      1066      1654      2051       897
    ## Hostos C               NA        NA       420       760       690       394       603       930      1052       214       516       402       311       340       192       115
    ## Hunter               1874      1861      2857      2924      1598      1328      1427      1386      1253      1798      1859      1548      1630      1515      1831      2112
    ## John Jay               NA       258       800       651       567       512       549       728       726       516       902      1666       622       692       782       682
    ## Kingsborough C        395       579       700       640       556       526       495       643       647       603       695       756      1157       780       660       449
    ## LaGuardia C            NA        NA       455       165        42        52        66       265      1409       490       204       273       530       460       413       234
    ## Lehman                 NA       980      1379      1178       984       853       750       760       826        NA       799       740       723       722       874       819
    ## Medger Evers           NA        NA       264       194       147        87       136        NA       454        NA       354       411       586       683       555       554
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA       260       277       267       267       248       185
    ## Queens               2380      3808      2696      1696      1389      1058       898      1014       980       939       925       845       995       977       978       971
    ## Queensborough C       430       649      1161       886       645       890       797       858      1170      1026      1043      1271      1305      1419      1334       654
    ## York                   NA       355       566       603       399       364       344       448       809       730       635       801       951       869      1020       751
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SWAHILI/KISWAHILI"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        27        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        10        17        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        36        43        31        24        29        11        29        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        10        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        42        NA        20         5        11        14        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        25        NA        NA        NA        60        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        88        NA        17        15        NA        15        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        14        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "SWEDISH"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
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
    ## Queens                 NA        NA        43        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "UKRAINIAN"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA         8        NA         3        11         4         1        NA        NA        NA        NA        NA        NA        NA        NA
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
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        51        35        32        22         3        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        80        45        NA        NA        10        NA        NA        NA        NA        NA        NA         0        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        26        19        25        NA        15        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA        24        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA       594       341       412        94        74        77        NA        NA        15        36        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA         9        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## 
    ## 
    ## -------------------------------------------
    ## [1] "YORUBA"
    ## -------------------------------------------
    ##                 1965 Fall 1968 Fall 1974 Fall 1977 Fall 1980 Fall 1983 Fall 1986 Fall 1990 Fall 1995 Fall 1998 Fall 2002 Fall 2006 Fall 2009 Fall 2013 Fall 2016 Fall 2021 Fall
    ## Baruch                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BMCC                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## BCC                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Brooklyn               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CSI                    NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## City                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hostos C               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Hunter                 NA        NA        21        NA        15        14        41        10        NA        NA        NA        NA        NA        NA        NA        NA
    ## John Jay               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Kingsborough C         NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## LaGuardia C            NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Lehman                 NA        NA         6        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Medger Evers           NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## CityTech               NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queens                 NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## Queensborough C        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA
    ## York                   NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA        NA

- Plotting the modern language enrollment data (by control \[community
  colleges & senior colleges\])

``` r
# plotting data
CUNYCCIndex = c("BMCC", "Hostos C", "BCC", "Kingsborough C", "LaGuardia C", "Queensborough C")
CUNYSCIndex = c("Baruch","Brooklyn","CSI", "City", "Hunter", "John Jay", "Lehman", "Medger Evers", "CityTech", "Queens", "York")

tempData = melt(MLA.CUNY.e3[CUNYCCIndex,])
colnames(tempData) <- c("Institution","Year","Enrollment")
tempData[is.na(tempData$Enrollment),"Enrollment"] <- 0
p <- ggplot(tempData, aes(Year, Enrollment, group = Institution)) + 
  geom_point(aes(color=Institution), size=2.5) + 
#  scale_shape_manual(values = c(0,1)) +
#  scale_color_brewer(palette = "PuOr") +
  geom_line(aes(color=Institution)) + 
    geom_text(aes(label=paste(addComma(Enrollment),sep="")), 
        position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.75)

  # using ggplot theme and modifying axis labels
p + theme_hc() + scale_fill_identity() + # + scale_colour_hc() +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
    ggtitle(paste("Modern Language Enrollments at CUNY Commmunity Colleges bet. 1958 and 2021\n(Data from MLA's Language Enrollment Database, 1958–2021)")) +
    theme(plot.margin = unit(c(2,2,2,2), "cm")) +
    theme(axis.title.x = element_text(margin=margin(t=10)))
```

<img src="RProcedureMLAEnrollmentSurveyCUNY2023_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
#  theme(legend.key.size = unit(1, 'cm'), #change legend key size
#    legend.key.height = unit(1, 'cm'), #change legend key height
#    legend.key.width = unit(1, 'cm'), #change legend key width
#    legend.title = element_text(size=12), #change legend title font size
#    legend.text = element_text(size=8)) #change legend text font size
ggsave(paste("data/CUNYMLEnrollmentMLAEnrollmentSurvey2023PlotCCAllLanguages.pdf",sep=""), width = 12, height = 10)


tempData = melt(MLA.CUNY.e3[CUNYSCIndex,])
colnames(tempData) <- c("Institution","Year","Enrollment")
tempData[is.na(tempData$Enrollment),"Enrollment"] <- 0
p <- ggplot(tempData, aes(Year, Enrollment, group = Institution)) + 
  geom_point(aes(color=Institution), size=2.5) + 
#  scale_shape_manual(values = c(0,1)) +
#  scale_color_brewer(palette = "PuOr") +
  geom_line(aes(color=Institution)) + 
    geom_text(aes(label=paste(addComma(Enrollment),sep="")), 
        position=position_dodge2(width=0.1, preserve="single", padding=5), vjust=2, size=2.75)

  # using ggplot theme and modifying axis labels
p + theme_hc() + scale_fill_identity() + # + scale_colour_hc() +
  theme(axis.text.x = element_text(angle = 40, vjust = 0.8, hjust=1, size=8)) +
    ggtitle(paste("Modern Language Enrollments at CUNY Senior Colleges bet. 1958 and 2021\n(Data from MLA's Language Enrollment Database, 1958–2021)")) +
    theme(plot.margin = unit(c(2,2,2,2), "cm")) +
    theme(axis.title.x = element_text(margin=margin(t=10)))
```

<img src="RProcedureMLAEnrollmentSurveyCUNY2023_files/figure-gfm/unnamed-chunk-5-2.png" style="display: block; margin: auto;" />

``` r
#  theme(legend.key.size = unit(1, 'cm'), #change legend key size
#    legend.key.height = unit(1, 'cm'), #change legend key height
#    legend.key.width = unit(1, 'cm'), #change legend key width
#    legend.title = element_text(size=12), #change legend title font size
#    legend.text = element_text(size=8)) #change legend text font size
ggsave(paste("data/CUNYMLEnrollmentMLAEnrollmentSurvey2023PlotSCAllLanguages.pdf",sep=""), width = 12, height = 10)
```
