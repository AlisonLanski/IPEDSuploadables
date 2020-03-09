=================================

ReadMe for Completions Uploadable

=================================

This script starts with institutional data and returns a text file that meets the IPEDS standards for import.
You will be able to select your file destination.

After production of the file, you can upload it to the IPEDS website.  Select the "key-value" import option.

In current form, this script can only handle one unitid at a time. If you are reporting data for multiple unitids, you will need to run the script separately for each unitid.

The script requires two dataframes of institutional data in the following structure
A) Student data: data should have one row for each combination of: unitid, student, awardlevel, and major
B) CIP data: data should have one row per for each combination of: unitid, cipcode, awardlevel, and distance ed 

See below for the required column names and datatypes for each dataframe 

Note: Additional columns (for internal data handling purposes) will not break the script
Note: Columns do not have to be in this order

A: Student dataframe -- "startingdf"

Unitid
  Numeric
  (relevant unitid; xxxxxx)

StudentId
  Text or numeric  
     (any unique identifier for your students)

RaceEthnicity
  Numeric
    1 - Nonresident alien
    2 - Hispanic/Latino
    3 - American Indian or Alaska Native
    4 - Asian
    5 - Black or African American
    6 - Native Hawaiian or Other Pacific Islander
    7 - White
    8 - Two or more races
    9 - Race and ethnicity unknown

Sex
  Numeric
    1 - Men
    2 - Women

DegreeLevel
  Numeric
    1 - Postsecondary award, certificate, or diploma of (less than one academic year)
            less than 900 clock hours
            less than 30 semester or trimester credit hours
            less than 45 quarter credit hours
    2 - Postsecondary award, certificate,or diploma of (at least one but less than two academic years)
            at least 900 but lessthan 1800 clock hours)
            at least 30 but less than 60 semester or trimester credit hours
            at least 45 but less than 90 quarter credit hours
    3 - *Associate's Degree
    4 - Postsecondary award, certificate,or diploma of (at least two but less than four academic years)
            1800 or more clock hours
            60 or more semester or trimester credit hours
            90 or more quarter credit hours
    5 - *Bachelor's Degree or equivalent
    6 - Postbaccalaureate Certificate
    7 - *Master's Degree
    8 - Post-Master's Certificate
    17 - *Doctor's degree - research/scholarship
    18 - *Doctor's degree - professional practice
    19 - *Doctor's degree - Other 
    * Use only 3, 5, 7, 17, 18 and 19 when reporting second majors.

MajorNumber
  Numeric
    1 - First Major
    2 - Second Major

MajorCip
  Text or Numeric
    xx.xxxx preferred,
    x.xxx (etc) accepted

DistanceEd
  Numeric
    1 = Distance Ed program
    2 = Not Distance Ed program

Age
  Numeric
    Demical or integer ages     


B: Cip dataframe -- "extra cips"
Note: this dataframe contains some demographic information set to IPEDS default values
  these fields are required so IPEDS can track awards that are possible to be earned but have no completers in the current year of reporting

Unitid
  Numeric
  (relevant unitid; multiple unitids per dataframe is acceptable)

DegreeLevel
  Numeric
    1 - Postsecondary award, certificate, or diploma of (less than one academic year)
            less than 900 clock hours
            less than 30 semester or trimester credit hours
            less than 45 quarter credit hours
    2 - Postsecondary award, certificate,or diploma of (at least one but less than two academic years)
            at least 900 but lessthan 1800 clock hours)
            at least 30 but less than 60 semester or trimester credit hours
            at least 45 but less than 90 quarter credit hours
    3 - *Associate's Degree
    4 - Postsecondary award, certificate,or diploma of (at least two but less than four academic years)
            1800 or more clock hours
            60 or more semester or trimester credit hours
            90 or more quarter credit hours
    5 - *Bachelor's Degree or equivalent
    6 - Postbaccalaureate Certificate
    7 - *Master's Degree
    8 - Post-Master's Certificate
    17 - *Doctor's degree - research/scholarship
    18 - *Doctor's degree - professional practice
    19 - *Doctor's degree - Other 
    * Use only 3, 5, 7, 17, 18 and 19 when reporting second majors.

MajorNumber
  Numeric
    1 - First Major
    2 - Second Major

MajorCip
  Text or Numeric
    xx.xxxx preferred,
    x.xxx (etc) accepted

RaceEthnicity
  Numeric
    set all values to 1

Sex
  Numeric
    set all values to 1

Count
  Numeric
    set all values to 0
DistanceEd
  Numeric
    1 = Distance Ed program
    2 = Not Distance Ed program

