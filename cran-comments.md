## Resubmission

This is a resubmission. In this version I have

* Added the IPEDS submission URL to the description field of the DESCRIPTION file.  

* Uncommented all code in produce_other_report examples and added a \donttest wrapper to one piece instead.  

* Unwrapped all other \dontrun wrappers in the examples so they will execute. (They run quickly). 

* Removed set.seed() from functions.  
  

## R CMD check results

There were no ERRORs or WARNINGs. 

There were two NOTEs from rhub testing:

* New submission

  This is a first submission
  
* Possibly misspelled words in DESCRIPTION:  
    IPEDS (2:58, 14:63)  
    Postsecondary (14:81)  
    
  These are common vocabulary words for the package audience and are adopted from language on the Department of Education website

  
## Downstream dependencies
There are currently no downstream dependencies for this package
