library(tidyverse)
library(rvest)

# Return the spreadsheet DB of referees
referee_database.2022 <- function() {
    return(
        readxl::read_xlsx("data/Active Referees - July 12.xlsx") |>
            select(ID=`USSF-Id`) |>
            unique() |>
            mutate(reg.22=T)
    )
}

# Return the spreadsheet DB of referees
referee_database.2021 <- function() {
    return(
        readxl::read_xlsx("data/Referee Data 2021-06-30.xlsx") |>
            select(ID=`Referee ID`) |>
            unique() |>
            mutate(reg.21=T)
    )
}

referee_database <- function() {
    return(
        full_join(referee_database.2022(),
                  referee_database.2021(),
                  by='ID') |>
            mutate(reg.21=falseIfNA(reg.21),
                   reg.22=falseIfNA(reg.22))
    )
}

falseIfNA <- \(v) {
    ifelse(is.na(v), F, v)
}

#### Oregon Soccer Central Data

# Scrape the site for referee details.  If one org is specified, it will include
# the information on the years active in the org as well as the current ranks.
# Results held in daily cache
osc_refs <- function(session, orgs=2820) {
    osc.cache <- cache('oscrefs')
    today <- as.character(Sys.Date())
    cache_key <- str_c('refs',today, "_",
                       cli::hash_md5(str_c(as.character(sort(orgs)), collapse=",")))
    orgrefs <- osc.cache$get(cache_key)
    if (is.null(orgrefs)) {
        orgrefs <- read_osc_refs(session, orgs)
        osc.cache$put(cache_key, orgrefs)
    }
    before <- nrow(orgrefs)
    orgrefs.registered <- 
        left_join(orgrefs, referee_database(), by="ID") |>
        mutate(Registered.2021=falseIfNA(reg.21),
               Registered.2022=falseIfNA(reg.22)) 
    # I used to filter out refs who joined the org but whose registration had not come through so
    # I'm not doing that anymore:
    #    filter(ID != '')
    # This would print out the names of those refs:
    unregistered <- nrow(orgrefs)-nrow(orgrefs.registered)
    if (unregistered > 0) {
        warning("There are ", unregistered, " refs who were removed because there is no registration for them: ",
                str_c(setdiff(orgrefs$FullName, orgrefs.registered$FullName), collapse = ', '))
        
    }
    return (
        orgrefs.registered |>
            select(ID, UID, LastName, FirstName, FullName, Age, Gender, City, LastLogin, Registered.2021, Registered.2022, 
                   InitialYear, CurrentYear, Grade, 
                   starts_with('Rank'),
                   starts_with('Org'),
                   Email, SecondaryEmail, Cell,
                   Notes.Self, Notes.Assignor)
    )
}


read_osc_refs <- function(session, orgs) {
    request_params <- list(
        `selOrgID`=str_c(orgs, collapse=","),
        `role`="32",
        `minLoginDate` = "1/1/2017",
        #        `currCertYearFrom`="2022",
        `userInfoColumns[]`="2",
        `userInfoColumns[]`="16",
        `userInfoColumns[]`="32",
        `userInfoColumns[]`="64",
        `userInfoColumns[]`="256",
        `userInfoColumns[]`="1024",
        `userInfoColumns[]`="2048",
        `userInfoColumns[]`="4096",
        `userInfoColumns[]`="8192",
        `userInfoColumns[]`="16384",
        `userInfoColumns[]`="65536",
        `userInfoColumns[]`="2097152",
        `userInfoColumns[]`="4194304",
        `userInfoColumns[]`="8388608",
        `sortBy`="name",
        `mode`="view",
        `find`="Find Users",
        `showResultsCount`="10000")
    
    if (length(orgs) == 1) {
        request_params <- append(request_params,
                                 list(
                                     `userOrgInfoColumns[]`="1",
                                     `userOrgInfoColumns[]`="32",
                                     `userOrgInfoColumns[]`="128",
                                     `userOrgInfoColumns[]`="64",
                                     `userOrgInfoColumns[]`="256"))
    }
    resp <- httr::POST("https://www.oregonsoccercentral.com/OSICORE/user/results.php", 
                       encode="form",
                       body=request_params,
                       config=session$config,
                       handle=session$handle)
    
    
    users_table <- html_elements(httr::content(resp), css="#userSearchTable > tbody > tr")
    
    orgrefs <- 
        lapply(users_table, \(row) {
            cells <- html_elements(row, css="td")
            if (length(orgs) == 1) {
                cells <- tail(cells, -1)
            }
            rec <- list(
                UID = html_text(cells[1]),
                Name = html_text(cells[2]),
                City = html_text(cells[3]),
                Email =  html_text(cells[5]),
                Cell = html_text(cells[6]),
                Age = html_text(cells[8]),
                Gender = html_text(cells[7]),
                LastLogin = html_text(cells[9]),
                Notes.Self = html_text(cells[10]),
                Notes.Assignor = html_text(cells[11]),
                Grade = html_text(cells[12]) ,
                InitialYear = html_text(cells[13]),
                CurrentYear = html_text(cells[14]),
                ID = html_text(cells[15]))
            if (length(orgs) == 1) {
                rec$Rank.Center <- html_text(cells[17])
                rec$Rank.AR <- html_text(cells[18])
                rec$Org.InitialYear <- html_text(cells[19])
                rec$Org.CurrentYear <- html_text(cells[20])
            }
            return(rec)
        }) |>
        bind_rows() |>
        mutate(LastName = str_match(Name, "^(.*),")[,2],
               FirstName = str_match(Name, ",.(.*)")[,2],
               FullName = str_c(FirstName, " ", LastName),
               InitialYear = ifelse(is.na(InitialYear), CurrentYear, InitialYear),
               SecondaryEmail = str_match(Email,"\\n.*\\n\\s*(\\S+)\\s*$")[,2],
               Email = str_match(Email, "^\\s*(\\S*)\\s")[,2],
               Cell = str_match(Cell, "Cell: (.*)$")[,2],
               LastLogin = lubridate::mdy(LastLogin),
               across(c(UID, Age, Grade, InitialYear, CurrentYear), parseint))
    
    if (length(orgs) == 1) {
        orgrefs <- mutate(orgrefs,
                          across(starts_with("Rank"), parseint),
                          across(starts_with("Org"), parseint))
    }
    return(orgrefs)
}