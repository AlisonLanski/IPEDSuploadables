=================================

ReadMe for Human Resources Uploadable

=================================

This script starts with institutional data and returns a text file that meets the IPEDS standards for import.
You will be able to select your file destination.

After production of the file, you can upload it to the IPEDS website.  Select the "key-value" import option.

In current form, this script can only handle one unitid at a time. If you are reporting data for multiple unitids, you will need to run the script separately for each unitid.

The script requires one dataframes of institutional data in the following structure
One row for each combination of: unitid, employee/HR-reportable person

See below for the required column names and datatypes for each dataframe 

Note: Additional columns (for internal data handling purposes) will not break the script
Note: Columns do not have to be in this order

Employee dataframe -- "ipeds_df"

Unitid
  Numeric
  (relevant unitid; xxxxxx)

EmpId
  Text or numeric  
     (any unique identifier for each employees)

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

Gender
  Numeric
    1 - Men
    2 - Women

Rank
  Numeric
    1 - Professors
    2 - Associate professors
    3 - Assistant professors
    4 - Instructors
    5 - Lecturers
    6 - No academic rank (Faculty)
    7 - Not faculty

Tenure
  Numeric
    1 - Tenured 
    2 - On tenure track 
    3 - Multi-year contract or employment agreement. Not on Tenure Track or no tenure system 
    4 - Annual contract or employment agreement.  Not on Tenure Track or no tenure system
    5 - Less than annual contract or employment agreement.  Not on Tenure Track or no tenure system
    6 - Without faculty status
    7 - Indefinite duration (continuing or at-will) contract or employment agreement. Not on Tenure Track or no tenure system

IsMedical
  Numeric
    0 - Not medical school staff
    1 - Medical school staff

NewHire
  Numeric
    0 - Not a new hire
    1 - New Hire

FtPt
  Text
    F - Full Time
    P - Part Time

Salary
  Numeric
    xxxxxxx.xx (or just xxxxx): should be the value corresponding with Months (see below)

Months
  Numeric
    8 - Annual salary covers less than 9 months
    9 - Annual salary covers 9 months
    10 - Annual salary covers 10 months
    11 - Annual salary covers 11 months
    12 - Annual salary covers 12 months
    99 - Any other situation/employee which will not be reported and is not included above

OccCategory3
  Numeric
    1 - Primarily Instruction - exclusively credit
    2 - Primarily Instruction - exclusively not-for-credit
    3 - Primarily Instruction - Combined credit/not-for-credit
    4 - Instruction/research/public service
    5 - Research Staff
    6 - Public Service Staff
    7 - Archivists, Curators, and Museum Technicians (25-4010)
    8 - Librarians and Media Collections Specialists (25-4020)
    9 - Library Technicians (25-4030)
    10 - Student and Academic Affairs and Other Education Services Occupations (25-2000 + 25-3000 + 25-9000)
    11 - Management Occupations (11-0000)
    12 - Business and Financial Operations Occupations (13-0000)
    13 - Computer, Engineering, and Science Occupations (15-0000 + 17-0000 + 19-0000)
    14 - Community, Social Service, Legal, Arts, Design, Entertainment, Sports, and Media Occupations (21-0000 + 23-0000 + 27-0000) 
    15 - Healthcare Practitioners and Technical Occupations (29-0000)
    16 - Service Occupations (31-0000 + 33-0000 + 35-0000 + 37-0000 + 39-0000)
    17 - Sales and Related Occupations (41-0000)
    18 - Office and Administrative Support Occupations (43-0000)
    19 - Natural Resources, Construction, and Maintenance Occupations (45-0000 + 47-0000 + 49-0000)
    20 - Production, Transportation, and Material Moving Occupations (51-0000 + 53-0000)
    22 - Teaching Assistants, Postsecondary (25-9044)
    23 - Graduate Assistant Research
    24 - Graduate Assistant Other

*21 is used for an intermediate total and is not a value in the upload file
*the values for OccCategory3 are used in the script to generate all other occupation-related fields and to determine instructional staff (groups 1-4)


