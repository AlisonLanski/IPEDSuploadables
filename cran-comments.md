## Update notes
This version of the package contains maintenance updates and compatibility updates.
Some examples have been changed to be tagged as "donttest" because they exceed 5 seconds of processing time during Linux checks and/or require user input to run correctly.
  
  
## R CMD check results

There were no ERRORs or WARNINGs. 

There were two NOTES from rhub testing:

* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
This note is a known Rhub bug that is generated on Windows Server 2022, R-devel, 64 bit. I am unable to clean up the temp directory on that server.

  
* checking HTML version of manual ... NOTE
Skipping checking HTML validation: no command 'tidy' found
  
This note is only generated on Fedora Linux (R-hub): I cannot change that Tidy is not on the path, or update Tidy on the external Fedora Linux server.
  
    
## Downstream dependencies
There are currently no downstream dependencies for this package
