## Update notes
Code changes to support new elements of existing reporting
  
  
## R CMD check results

There were no ERRORs WARNINGs or NOTEs from testing using Github Actions.
CMD checks conducted on
* MacOS (latest)
* Windows (latest)
* Ubuntu (development)
* Ubuntu (current release)
* Ubuntu (previous release)
* Windows (development) via win-builder.r-project.org  
* MacOS (development) via mac.r-project.org/macbuilder was down and not available for testing

## Offical CRAN Checks for Existing Package Version

There is one WARNING on the CRAN website for the existing package  
  
r-devel-linux-x86-_64-debian-gcc provides this warning:  
"Cannot process vignettes  
Packages suggested but not available for checking:  
  'knitr', 'rmarkdown', 'kableExtra'  
VignetteBuilder package required for checking but not installed: ‘knitr’"  
    
This seems to be an issue with the check environment's ability load these packages. 
knitr is included in the Description file and is listed in all vignette scripts as the engine
    
## Downstream dependencies
There are currently no downstream dependencies for this package
