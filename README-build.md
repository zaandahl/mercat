# MERCAT Build Instructions

## Version History
v0.3.4  -  SNP support and Octavia dataset
v0.3.3  -  Force sliders for m_interactive, resistance freq legend
v0.3.2  -  Unit tests for m_analysis, exportable m_res function
v0.3.1  -  Graph speed improvements, m_interactive bug fixes
v0.3.0  -  Packaged for submission to IGE on 31/05/19. 

## Notes for building
The packages devtools and usethis were both used heavily for this build.

Most guidance came from the book "R packages" by Hadley Wickham
http://r-pkgs.had.co.nz/intro.html

https://cran.r-project.org/web/packages/devtools/index.html
https://cran.r-project.org/web/packages/usethis/index.html

Namespaces for all external packages including core packages like stats 
and utils were qualified: e.g. dplyr::summarise() instead of summarise()


## Commands that were performed once for package setup 
> usethis::use_pipe()
Allow the use of the magritter pipe %>% without specifying
the package namespace explicitly

> usethis::use_vignette("mercat")
Sets up a basic vignette

Vignette writing and knitting was performed in RStudio. 

> devtools::usetestthat()
Sets up the tests/testthat directory and modifies the DESCRIPTION file

The monteserin data was saved as an RData object in a /data directory and a 
corresponding file named data.R was placed in the /R directory. The data.R
file contains help markup for the monteserin data which is lazy loaded by
the package.

## Commands that are part of a workflow
> devtools::load_all()
Load all R code in the R/ subdirectory, test and modify code, repeat

> devtools::document()
Build documentation, check and change documentation, repeat

### After document has finished copy the smef.Rd file to the Shiny directory
$ cp man/smef.Rd inst/shiny-examples/shinycat/www

> devtools::test()
Run all unit tests and reports results, update code to correct, repeat

## Shiny and JavaScript code for m_interactive
The code is located in the following directory
/inst/shiny-examples/shinycat/

Default data for m_interactive
/inst/shiny-examples/shinycat/data

Primary Shiny app file
/inst/shiny-examples/shinycat/app.R

Primary HTML file
/inst/shiny-examples/shinycat/www/html/index.html

JavaScript (docmgt.js, forest.js, mst.js)
/inst/shiny-examples/shinycat/www/js/

CSS (styling)
/inst/shiny-examples/shinycat/www/css/spol2.css

Example data
/inst/shiny-examples/shinycat/www/data-examples/

