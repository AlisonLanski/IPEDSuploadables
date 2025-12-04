# How to produce other key-value uploads

## When to use this How-To

Follow the instructions if you want to …

- Do your own data recoding and aggregation for IPEDS submission
- Get help converting your data into a key-value pair format
- Get help combining parts with different numbers of columns

If you want the package to help you with aggregation and (some)
recoding, look at the “setup” vignettes for supported uploads

*Note: the `produce_other_report` function can be used to prepare any
key-value pair file for an automated submission process (IPEDS or
non-IPEDS)*

## General Process

- **Download/Open the IPEDS Import Spec** for a particular survey and
  institution-type on the [IPEDS Survey Materials instructions
  page](https://surveys.nces.ed.gov/ipeds/public/survey-materials/results).
  *Between reporting cycles, this page may be down: consult [the
  archive](https://nces.ed.gov/ipeds/use-the-data/annual-survey-forms-packages-archived)
  to see previous specs.*
- **Use the “Key Value Pair File” instructions** to prep your data. This
  will be the second part of the document; the first part is for
  fixed-width files.
- **Create one dataframe for each survey part** using the exact IPEDS
  FieldIDs as column names and using only the values described in the
  table of specs and the additional information in the file’s appendix
  (scroll down). Follow the same order of columns and parts.
- **Use the package function `produce_other_report`** with your files.

*If you need assistance understanding what goes into the upload file,
contact IPEDS for advice.*

## Data prep example: Admissions (old)

This provides a start-to-finish example of preparing an old version of
the admissions submission based on sample data. Always check your
results after you upload your txt file to the IPEDS submission portal.

### Start with institutional data for applicants

``` r
#load packages
library(dplyr)
library(magrittr)
library(IPEDSuploadables)
```

``` r
#create data
adm_dat <- data.frame(StudentId = seq(1:24),
                      FtPt = c(rep('FT', 23), 'PT'),
                      Sex = rep(c("M", "F"), 12),
                      GenderDetail = c(rep(c("M", "F"), 11), "U", "A"),
                      Admit = c(rep(1, 16), rep(0, 8)),
                      Enroll = c(rep(1, 12), rep(0, 12)),
                      SAT = c(rep(1, 8), rep(0, 16)),
                      SAT_V = c(500, 560, 600, 660, 700, 760, 800, 800, rep(NA, 16)),
                      SAT_M = c(400, 460, 500, 560, 600, 660, 700, 700, rep(NA, 16)),
                      ACT = c(rep(0, 8), rep(1, 16)),
                      ACT_CMP = c(rep(NA, 8), 32, 32, 31, 31, 30, 30, 29, 29, 28, 28, 27, 27, 26, 26, 25, 25)
                      )
```

| StudentId | FtPt | Sex | GenderDetail | Admit | Enroll | SAT | SAT_V | SAT_M | ACT | ACT_CMP |
|----------:|:-----|:----|:-------------|------:|-------:|----:|------:|------:|----:|--------:|
|         1 | FT   | M   | M            |     1 |      1 |   1 |   500 |   400 |   0 |      NA |
|         2 | FT   | F   | F            |     1 |      1 |   1 |   560 |   460 |   0 |      NA |
|         3 | FT   | M   | M            |     1 |      1 |   1 |   600 |   500 |   0 |      NA |
|         4 | FT   | F   | F            |     1 |      1 |   1 |   660 |   560 |   0 |      NA |
|         5 | FT   | M   | M            |     1 |      1 |   1 |   700 |   600 |   0 |      NA |
|         6 | FT   | F   | F            |     1 |      1 |   1 |   760 |   660 |   0 |      NA |
|         7 | FT   | M   | M            |     1 |      1 |   1 |   800 |   700 |   0 |      NA |
|         8 | FT   | F   | F            |     1 |      1 |   1 |   800 |   700 |   0 |      NA |
|         9 | FT   | M   | M            |     1 |      1 |   0 |    NA |    NA |   1 |      32 |
|        10 | FT   | F   | F            |     1 |      1 |   0 |    NA |    NA |   1 |      32 |
|        11 | FT   | M   | M            |     1 |      1 |   0 |    NA |    NA |   1 |      31 |
|        12 | FT   | F   | F            |     1 |      1 |   0 |    NA |    NA |   1 |      31 |
|        13 | FT   | M   | M            |     1 |      0 |   0 |    NA |    NA |   1 |      30 |
|        14 | FT   | F   | F            |     1 |      0 |   0 |    NA |    NA |   1 |      30 |
|        15 | FT   | M   | M            |     1 |      0 |   0 |    NA |    NA |   1 |      29 |
|        16 | FT   | F   | F            |     1 |      0 |   0 |    NA |    NA |   1 |      29 |
|        17 | FT   | M   | M            |     0 |      0 |   0 |    NA |    NA |   1 |      28 |
|        18 | FT   | F   | F            |     0 |      0 |   0 |    NA |    NA |   1 |      28 |
|        19 | FT   | M   | M            |     0 |      0 |   0 |    NA |    NA |   1 |      27 |
|        20 | FT   | F   | F            |     0 |      0 |   0 |    NA |    NA |   1 |      27 |
|        21 | FT   | M   | M            |     0 |      0 |   0 |    NA |    NA |   1 |      26 |
|        22 | FT   | F   | F            |     0 |      0 |   0 |    NA |    NA |   1 |      26 |
|        23 | FT   | M   | U            |     0 |      0 |   0 |    NA |    NA |   1 |      25 |
|        24 | PT   | F   | A            |     0 |      0 |   0 |    NA |    NA |   1 |      25 |

### Prepare Part A - General Questions (not from sample data)

``` r
#### PART A: General Admissions Criteria
partA <- data.frame(UNITID = 999999,
                    SURVSECT = 'ADM',
                    PART = 'A',
                    ADMCON1 = 2, #GPA
                    ADMCON2 = 1, #Rank
                    ADMCON3 = 1, #Record
                    ADMCON4 = 2, #HS grad
                    ADMCON5 = 1, #Recs
                    ADMCON6 = 3, #Portfolio
                    ADMCON7 = 5, #SAT/ACT  #1 or 5 = have to do part C
                    ADMCON8 = 2, #TOEFL
                    ADMCON9 = 3, #other test
                    ADMCON10 = 2, #work exp
                    ADMCON11 = 1, #personal statement
                    ADMCON12 = 3 #legacy
                    )
```

| UNITID | SURVSECT | PART | ADMCON1 | ADMCON2 | ADMCON3 | ADMCON4 | ADMCON5 | ADMCON6 | ADMCON7 | ADMCON8 | ADMCON9 | ADMCON10 | ADMCON11 | ADMCON12 |
|-------:|:---------|:-----|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|---------:|---------:|---------:|
| 999999 | ADM      | A    |       2 |       1 |       1 |       2 |       1 |       3 |       5 |       2 |       3 |        2 |        1 |        3 |

### Prepare Part B - Student Counts (from sample data)

``` r
##### PART B: Admission Counts; FirstTime UG only
partB <- data.frame(UNITID = 999999,
                    SURVSECT = 'ADM',
                    PART = 'B',
                    APPLCNM = nrow(adm_dat[adm_dat$GenderDetail == 'M', ]),
                    APPLCNW = nrow(adm_dat[adm_dat$GenderDetail == 'F', ]),
                    APPLCNT = nrow(adm_dat),
                    ADMSSNM = nrow(adm_dat[adm_dat$GenderDetail == 'M' & 
                                             adm_dat$Admit == 1,]),
                    ADMSSNW = nrow(adm_dat[adm_dat$GenderDetail == 'F' & 
                                             adm_dat$Admit == 1,]),
                    ADMSSNT = nrow(adm_dat[adm_dat$Admit == 1,]),
                    ENRLFTM = nrow(adm_dat[adm_dat$GenderDetail == 'M' & 
                                             adm_dat$Enroll == 1 & 
                                             adm_dat$FtPt == 'FT', ]),
                    ENRLFTW = nrow(adm_dat[adm_dat$GenderDetail == 'F' & 
                                             adm_dat$Enroll == 1 & 
                                             adm_dat$FtPt == 'FT', ]),
                    ENRLFTT = nrow(adm_dat[adm_dat$Enroll == 1 & 
                                             adm_dat$FtPt == 'FT', ]),
                    ENRLPTM = nrow(adm_dat[adm_dat$GenderDetail == 'M' & 
                                             adm_dat$Enroll == 1 & 
                                             adm_dat$FtPt == 'PT', ]),
                    ENRLPTW = nrow(adm_dat[adm_dat$GenderDetail == 'F' & 
                                             adm_dat$Enroll == 1 & 
                                             adm_dat$FtPt == 'PT', ]),
                    ENRLPTT = nrow(adm_dat[adm_dat$Enroll == 1 & 
                                             adm_dat$FtPt == 'PT', ]),
                    #can you report another gender? 1 = yes, 2 = no
                    ADMGU01 = 1,
                    #if you said 1, keep the code below as-is
                    #if you said 2, remove code, and assign -2 to all 4 columns
                    APPLCNAG = nrow(adm_dat[adm_dat$GenderDetail == 'A', ]),
                    ADMSSNAG = nrow(adm_dat[adm_dat$GenderDetail == 'A' & 
                                              adm_dat$Admit == 1, ]),
                    ENRLFTAG = nrow(adm_dat[adm_dat$GenderDetail == 'A' & 
                                              adm_dat$Enroll == 1 & 
                                              adm_dat$FtPt == 'FT', ]),
                    ENRLPTAG = nrow(adm_dat[adm_dat$GenderDetail == 'A' & 
                                              adm_dat$Enroll == 1 & 
                                              adm_dat$FtPt == 'PT', ])
                    )

#mask data if you ARE able to report "Another Gender", 
# but the count is below 5 in any category
#if you are NOT able to report "Another Gender", 
# this code will not change your data, even if you run it
if((partB$APPLCNAG < 5 | partB$ADMSSNAG < 5 | 
    partB$ENRLFTAG < 5 | partB$ENRLPTAG < 5) & partB$ADMGU01 == 1){
  partB$ADMGU01 <- 3
  partB$APPLCNAG <- -2
  partB$ADMSSNAG <- -2
  partB$ENRLFTAG <- -2
  partB$ENRLPTAG <- -2
}
```

| UNITID | SURVSECT | PART | APPLCNM | APPLCNW | APPLCNT | ADMSSNM | ADMSSNW | ADMSSNT | ENRLFTM | ENRLFTW | ENRLFTT | ENRLPTM | ENRLPTW | ENRLPTT | ADMGU01 | APPLCNAG | ADMSSNAG | ENRLFTAG | ENRLPTAG |
|-------:|:---------|:-----|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|---------:|---------:|---------:|---------:|
| 999999 | ADM      | B    |      11 |      11 |      24 |       8 |       8 |      16 |       6 |       6 |      12 |       0 |       0 |       0 |       3 |       -2 |       -2 |       -2 |       -2 |

### Part C: Test Scores (from sample data)

``` r
#### PART C: Test Scores

adm_enr <- adm_dat %>%
  filter(Enroll == 1)

#in this example we are not supplying ACT test percentiles by subject
partC <- data.frame(UNITID = 999999,
                    SURVSECT = 'ADM',
                    PART = 'C',
                    SATINUM = nrow(adm_enr[adm_enr$SAT == 1, ]),
                    SATIPCT = round(nrow(adm_enr[adm_enr$SAT == 1, ])*100/nrow(adm_enr), 0),
                    ACTNUM = nrow(adm_enr[adm_enr$ACT == 1,]),
                    ACTPCT = round(nrow(adm_enr[adm_enr$ACT == 1,])*100/nrow(adm_enr), 0),
                    SATVR25 = quantile(adm_enr$SAT_V[!is.na(adm_enr$SAT_V)], .25),
                    SATVR75 = quantile(adm_enr$SAT_V[!is.na(adm_enr$SAT_V)], .75),
                    SATMT25 = quantile(adm_enr$SAT_M[!is.na(adm_enr$SAT_M)], .25),
                    SATMT75 = quantile(adm_enr$SAT_M[!is.na(adm_enr$SAT_M)], .75),
                    ACTCM25 = quantile(adm_enr$ACT_CMP[!is.na(adm_enr$ACT_CMP)], .25),
                    ACTCM75 = quantile(adm_enr$ACT_CMP[!is.na(adm_enr$ACT_CMP)], .75),
                    ACTEN25 = -2,
                    ACTEN75 = -2,
                    ACTMT25 = -2,
                    ACTMT75 = -2,
                    SATVR50 = quantile(adm_enr$SAT_V[!is.na(adm_enr$SAT_V)], .5),
                    SATMT50 = quantile(adm_enr$SAT_M[!is.na(adm_enr$SAT_M)], .5),
                    ACTCM50 = quantile(adm_enr$ACT_CMP[!is.na(adm_enr$ACT_CMP)], .5),
                    ACTEN50 = -2,
                    ACTMT50 = -2)

#mask data for an exam if you have fewer than 5 students counted for it
if(partC$SATINUM < 5){
  partC <- partC %>%
    mutate(across(c("SATVR25", "SATVR75", "SATVR50",
                    "SATMT25", "SATMT75", "SATMT50"), 
                  function(x) -2))
}
if(partC$ACTNUM < 5){
  partC <- partC %>%
    mutate(across(c("ACTCM25", "ACTCM75", "ACTCM50", 
                    "ACTMT25", "ACTMT75", "ACTMT50", 
                    "ACTEN25", "ACTEN75", "ACTEN50"), 
                  function(x) -2))
}
```

| UNITID | SURVSECT | PART | SATINUM | SATIPCT | ACTNUM | ACTPCT | SATVR25 | SATVR75 | SATMT25 | SATMT75 | ACTCM25 | ACTCM75 | ACTEN25 | ACTEN75 | ACTMT25 | ACTMT75 | SATVR50 | SATMT50 | ACTCM50 | ACTEN50 | ACTMT50 |
|-------:|:---------|:-----|--------:|--------:|-------:|-------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|--------:|
| 999999 | ADM      | C    |       8 |      67 |      4 |     33 |     590 |     770 |     490 |     670 |      -2 |      -2 |      -2 |      -2 |      -2 |      -2 |     680 |     580 |      -2 |      -2 |      -2 |

### Use this package to convert those dataframes into a single uploadable txt file

*The file format is shown below, but the package will actually save this
as a txt file at the location of your choice.*

``` r
produce_other_report(partA, partB, partC, survey = "Admissions")
```

|                                                                                                                                                                                                                                                      |
|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| UNITID=999999,SURVSECT=ADM,PART=A,ADMCON1=2,ADMCON2=1,ADMCON3=1,ADMCON4=2,ADMCON5=1,ADMCON6=3,ADMCON7=5,ADMCON8=2,ADMCON9=3,ADMCON10=2,ADMCON11=1,ADMCON12=3                                                                                         |
| UNITID=999999,SURVSECT=ADM,PART=B,APPLCNM=11,APPLCNW=11,APPLCNT=24,ADMSSNM=8,ADMSSNW=8,ADMSSNT=16,ENRLFTM=6,ENRLFTW=6,ENRLFTT=12,ENRLPTM=0,ENRLPTW=0,ENRLPTT=0,ADMGU01=3,APPLCNAG=-2,ADMSSNAG=-2,ENRLFTAG=-2,ENRLPTAG=-2                             |
| UNITID=999999,SURVSECT=ADM,PART=C,SATINUM=8,SATIPCT=67,ACTNUM=4,ACTPCT=33,SATVR25=590,SATVR75=770,SATMT25=490,SATMT75=670,ACTCM25=-2,ACTCM75=-2,ACTEN25=-2,ACTEN75=-2,ACTMT25=-2,ACTMT75=-2,SATVR50=680,SATMT50=580,ACTCM50=-2,ACTEN50=-2,ACTMT50=-2 |

### Upload your final txt file to the IPEDS website

This step is no different than any other upload.
