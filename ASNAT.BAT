@echo off
set PATH=Windows\bin;%PATH%
Rscript -e "if (!require(shiny)) install.packages('shiny', repos = 'http://cran.us.r-project.org'); shiny::runApp('../ASNAT', launch.browser = TRUE)"
