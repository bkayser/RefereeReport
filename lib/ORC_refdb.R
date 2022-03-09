library(tidyverse)
library(rvest)

# Return the spreadsheet DB of referees
referee_database.2022 <- function() {
    refs <- 
        # dir("RefereeList", "Referee_Licenses_*", full.names=T) |> 
        # sort() |>
        # first() |>
        read_csv("RefereeList/ActiveRefereeLicenses_3-1-2022.csv", show_col_types = FALSE) |>
        rename(ID=`USSF-Id`,
               Apt=`Apt/suite/unit`) |>
        mutate(DOB=lubridate::mdy(DOB)) |>
        prepare_db()
    return(refs)
}

# Return the spreadsheet DB of referees
referee_database.2021 <- function() {
    refs <-
        readxl::read_xlsx("RefereeList/Referee Data 2021-06-30.xlsx") |>
        rename(License=`Referee Type`,
               ID=`Referee ID`,
               DOB=Birthdate,
               Address=`Mailing Street`,
               `Zip Code`=`Mailing Zip`,
               City = `Mailing City`,
               Phone=`Home Phone`,
               Email=`Primary Email`) |>
        mutate(Apt="") |>
        prepare_db()
    
    return(refs)
}

referee_database <- function() {
    refs.2022 <- referee_database.2022() |> mutate(reg.22=T)
    refs.2021 <- referee_database.2021() |> mutate(reg.21=T)
    
    combined <- select(refs.2021, ID, reg.21) |>
        right_join(refs.2022, by='ID')
    
    return(
        bind_rows(combined, filter(refs.2021, !(ID %in% combined$ID))) |>
        mutate(reg.21=falseIfNA(reg.21),
               reg.22=falseIfNA(reg.22)) |>
        select(ID, reg.21, reg.22, everything())
    )
           
}

# Return the spreadsheet DB of referees
#
# Expected column names (with optional spaces and case):
# Address 
# Apt 
# City 
# Country 
# Curriculum 
# DOB 
# Email 
# First Name 
# Gender 
# ID 
# Last Name 
# License (Grassroots Referee, Regional Referee, etc)
# Mobile Phone 
# Phone 
# Secondary Email 
# State 
# Zip Code


prepare_db <- function(db) {
    refs <- db |>
        rename_with(~ str_to_title(.x)) |>
        rename_with(~ str_remove_all(.x, '[ /\\-]')) |> # Strip spaces from columns 
        rename(DOB=Dob,
               ID=Id) |>
        filter(!is.na(DOB)) |>
        mutate(Gender = ifelse(Gender == "Famle", "FEMALE", Gender), # Fix typo
               Referee = str_match(License, "(.*)(?<!Futsal) Referee")[,2],
               Futsal = str_match(License, "(.*) Futsal Referee")[,2],
               Assessor = str_match(License, "(.*) Assessor")[,2],
               Instructor = str_match(License, "(.*) Instructor")[,2],
               Mentor = License == "Referee Mentor" | NA,
               Gender=factor(ifelse(is.na(Gender), 'U', str_to_upper(Gender))),
               DOB=as.Date(DOB),
               Age=age(DOB),
               FirstName=str_to_title(FirstName),
               LastName=str_to_title(LastName),
               Zip=as.character(ZipCode),
               FullName=str_c(FirstName, ' ', LastName)) |>
        select(ID, FullName, LastName, FirstName, 
               Gender, DOB, Age,
               Referee, Instructor, Assessor, Mentor, Futsal, 
               Address, Apt, City, Zip,
               Phone, MobilePhone, Email, SecondaryEmail) |>
        group_by(ID, FullName, LastName, FirstName,
                 Gender, DOB, Age,
                 Phone, MobilePhone, Email, SecondaryEmail) |>
        summarize(Referee = firstVal(Referee),
                  Futsal = firstVal(Futsal),
                  Assessor = firstVal(Assessor),
                  Instructor = firstVal(Instructor),
                  Mentor = firstVal(Mentor),
                  Gender = firstVal(Gender),
                  Address = firstVal(Address),
                  Apt = firstVal(Apt),
                  City = firstVal(City),
                  Zip = firstVal(Zip)) |>
        ungroup() 
    refs <- refs[!duplicated(refs$ID),]  # Not sure why, but there are still some duplicates.
    return(refs)
}


## Referee Info
## Vectorized age function
age <- function(dob) {
    sapply(dob, function(d0) { return(length(seq(d0, Sys.Date(), by = 'year'))-1) })
}
falseIfNA <- \(v) {
    ifelse(is.na(v), F, v)
}
firstVal <- function(v) {
    first(v[!is.na(v)])
}

