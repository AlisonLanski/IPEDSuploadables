## R CMD check results

There were no ERRORs or WARNINGs. 

There were several NOTEs from rhub testing:

* New submission

  This is a first submission
  
* Possibly misspelled words in DESCRIPTION:  
    IPEDS (2:58, 14:63)  
    Postsecondary (14:81)  
    
  These are common vocabulary words for the package audience and are adopted from language on the Department of Education website

* Found the following (possibly) invalid URLs:  
    URL: https://surveys.nces.ed.gov/ipeds/public/survey-materials/results  
    From: inst/doc/howto_use_autoformat.html  
    Status: Error  
    Message: libcurl error code 56:  
      OpenSSL SSL_read: Connection reset by peer, errno 104

  This website becomes unavailable when IPEDS is doing their yearly documentation update between reporting cycles. 
      It is the correct URL during the reporting season, when this package would be used.
  
## Downstream dependencies
There are currently no downstream dependencies for this package
