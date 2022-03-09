library(tidyverse)
library(rvest)

source('./lib/ORC_refdb.R')
# Utility functions to suspport reports

# Make sure you call this the first time with the username and password arguments.  
# Return a list having the list of all games and the list of all referees.
create_session <- function(username=NULL, password=NULL) {
    
    account_cache <- cache('accounts')
    
    if (!is.null(username) & !is.null(password)) {
        account_cache$put(username, list(username=username, password=password))
    } else {
        cached_credentials <- account_cache$get(username)
        if (is.null(cached_credentials)) {
            stop('No cached credentials for ', username, '.')
        }
        username <- cached_credentials$username
        password <- cached_credentials$password
    }
    return(login(username, password))
}
# 87 of these are unmatched!  
# refs <- referee_database.2022()

# nrefs <- referee_list(session, 2820)

read_ncsc_refs <- function(session) {
    
    resp <- httr::POST("https://www.oregonsoccercentral.com/OSICORE/user/results.php", 
                       encode="form",
                       body=list(
                           `selOrgID`="2820",
                           `selOrgName`="North Clackamas SC",
                           `userInfoColumns[]`="2",
                           `userInfoColumns[]`="16",
                           `userInfoColumns[]`="32",
                           `userInfoColumns[]`="64",
                           `userInfoColumns[]`="256",
                           `userInfoColumns[]`="1024",
                           `userInfoColumns[]`="2048",
                           `userInfoColumns[]`="4096",
                           `userInfoColumns[]`="65536",
                           `userInfoColumns[]`="2097152",
                           `userInfoColumns[]`="4194304",
                           `userInfoColumns[]`="8388608",
                           `userOrgInfoColumns[]`="1",
                           `userOrgInfoColumns[]`="32",
                           `userOrgInfoColumns[]`="128",
                           `userOrgInfoColumns[]`="64",
                           `userOrgInfoColumns[]`="256",
                           `sortBy`="name",
                           `mode`="view",
                           `find`="Find Users",
                           `showResultsCount`="250"
                       ),
                       config=session$config,
                       handle=session$handle)
    
    
    users_table <- html_elements(httr::content(resp), css="#userSearchTable > tbody > tr")
    orgrefs <- 
        lapply(users_table, \(row) {
            cells <- html_elements(row, css="td")
            email <- html_text(cells[6])
            list(
                Name = html_text(cells[3]),
                ID = html_text(cells[14]),
                UID = html_text(cells[2]) |> as.integer(),
                Email=str_match(email, "^\\s*(\\S*)\\s")[,2],
                Age=html_text(cells[9]) |> as.integer(),
                Gender=html_text(cells[8]),
                LastLogin=html_text(cells[10]) |> lubridate::mdy(),
                InitialYear=html_text(cells[12]) |> as.integer(),
                CurrentYear=html_text(cells[13]) |> as.integer(),
                Rank.Center = html_text(cells[16]) |> as.integer(),
                Rank.AR =  html_text(cells[17]) |> as.integer(),
                Org.InitialYear=html_text(cells[18]) |> as.integer(),
                Org.CurrentYear=html_text(cells[19]) |> as.integer()
            ) |> suppressWarnings()
        }) |> 
        bind_rows() |>
        mutate(LastName = str_match(Name, "^(.*),")[,2],
               FirstName = str_match(Name, ",.(.*)")[,2],
               FullName = str_c(FirstName, " ", LastName))

    orgrefs.registered <- select(orgrefs, 
                                 ID, UID, LastLogin, InitialYear, CurrentYear, Rank.Center, Rank.AR, Org.InitialYear, Org.CurrentYear) |>
        inner_join(referee_database(),
                  by="ID")
    

    return (
        bind_rows(orgrefs.registered,
                  filter(orgrefs, !(UID %in% orgrefs.registered$UID))) |>
            mutate(reg.21=falseIfNA(reg.21),
                   reg.22=falseIfNA(reg.22)) |>
            select(ID, LastName, FirstName, Age, Gender, City, Zip, Referee, LastLogin, Registered.2021=reg.21, Registered.2022=reg.22, 
                   InitialYear, CurrentYear, Rank.Center, Rank.AR, Org.InitialYear, Org.CurrentYear, Email, SecondaryEmail)
    )
}