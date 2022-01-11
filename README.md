# RefereeReport

This is an R Markdown report that scrapes http://www.oregonsoccercentral.com for historical information and generates a report for a named referee with details on their assignment history, including the leagues, levels, and other referees.

This must remain a private repository because it contains sensitive information.

# Running the Report

To try this from scratch, follow these instructions.  

1. Clone the GitHub repo bkayser/RefereeReport.Rmd.  If you are reading this, you may have already done this.  
1. Download and install the R environment from https://cloud.r-project.org.
1. Download and install the free RStudio Desktop from https://www.rstudio.com/products/rstudio/
1. Open RStudio on this project.
1. Make sure you have the following packages installed using the package installer in RStudio:
    * `tidyverse`
    * `rvest`
    * `knitr`
1. Open `RefereeReport.Rmd` and edit the values at the top for the referee name, and username/password for that account.  
1. Run the `RefereeReport.Rmd` file.  Use the Run menu dropdown option `Run all...`.
    * The first time you run it you may get errors for missing packages.  Use the package installer in R Studio to install any additional missing packages. 
    * Once you run the report successfully, the username and password will be cached locally so you can set it to NULL in the script.
    * Depending on the permissions and organizations the user belongs to, you may see different amounts of detail.  Frankly I don't know what you'll see.  I've only ever run this for two accounts.  It may not even work unless the account has assignor privileges in at least one org.
    * The report takes a long time to run the first time as it loads every game report for games you've done, and it throttles the server access with  a 1 second sleep between requests to avoid hammering the server.  Subsequent runs used cache data and take less than 15 seconds or so.
1. Use the `Preview` button to see the report, or one of the `Knit...` options to generate different versions.

# Generating iCal 