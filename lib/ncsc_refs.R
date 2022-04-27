library(tidyverse)
library(rvest)

source('./lib/ORC_refdb.R')
source('./lib/utils.R')

read_refs <- function(session, org=2820) {
    
    resp <- httr::POST("https://www.oregonsoccercentral.com/OSICORE/user/results.php", 
                       encode="form",
                       body=list(
                           `selOrgID`=org,
                           `selOrgName`="North Clackamas SC",
                           `role`="32",
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
            phone <- html_text(cells[7])
            list(
                UID = html_text(cells[2]) |> parseint(),
                Name = html_text(cells[3]),
                City = html_text(cells[4]),
                Email = str_match(email, "^\\s*(\\S*)\\s")[,2],
                SecondaryEmail = str_match(email,"\\n.*\\n\\s*(\\S+)\\s*$")[,2],
                Cell = str_match(phone, "Cell: (.*)$")[,2],
                Age = html_text(cells[9]) |> parseint(),
                Gender = html_text(cells[8]),
                LastLogin = html_text(cells[10]) |> lubridate::mdy(),
                Notes.Self = html_text(cells[11]),
                Notes.Assignor = html_text(cells[12]),
                Grade = html_text(cells[13]) |> parseint(),
                InitialYear = html_text(cells[14]) |> parseint(),
                CurrentYear = html_text(cells[15]) |> parseint(),
                ID = html_text(cells[16]),
                Rank.Center = html_text(cells[18]) |> parseint(),
                Rank.AR = html_text(cells[19]) |> parseint(),
                Org.InitialYear = html_text(cells[20]) |> parseint(),
                Org.CurrentYear = html_text(cells[21]) |> parseint())
        }) |> 
        bind_rows() |>
        mutate(LastName = str_match(Name, "^(.*),")[,2],
               FirstName = str_match(Name, ",.(.*)")[,2],
               FullName = str_c(FirstName, " ", LastName),
               InitialYear = ifelse(is.na(InitialYear), CurrentYear, InitialYear))

    orgrefs.registered <- 
        select(orgrefs, 
               ID, UID, 
               LastName, FirstName, FullName, City, Age, Gender,
               Email, SecondaryEmail, Cell,
               LastLogin, InitialYear, CurrentYear, Grade, Rank.Center, Rank.AR, Org.InitialYear, Org.CurrentYear,
               Notes.Self, Notes.Assignor) |>
        left_join(referee_database(),
                  by="ID")
    

    return (
        bind_rows(orgrefs.registered,
                  filter(orgrefs, !(UID %in% orgrefs.registered$UID))) |>
            mutate(Registered.2021=falseIfNA(reg.21),
                   Registered.2022=falseIfNA(reg.22)) |>
            select(ID, UID, LastName, FirstName, FullName, Age, Gender, City, LastLogin, Registered.2021, Registered.2022, 
                   InitialYear, CurrentYear, Grade, Rank.Center, Rank.AR, 
                   Org.InitialYear, Org.CurrentYear, Email, SecondaryEmail, Cell,
                   Notes.Self, Notes.Assignor)
    )
}
parse_ref_id <- function(TD) {
    group_match <- html_element(TD, 'a') |> 
        html_attr('onclick') |> 
        str_match("'(\\d+)'")
    return(parseint(group_match[,2]))
}

LevelMap <- list (
    `3rd Grade` = "7v7",
    `4th Grade` = "7v7",
    `5th Grade` = "9v9",
    `5th-6th Grade` = "9v9",
    `6th Grade` = "9v9",
    `7th-8th Grade` = "11v11",
    `8th Grade` = "11v11",
    `U09` = "7v7",
    `U09 7v7` =  "7v7",
    `U10` =  "7v7",
    `U10 7v7` = "7v7",
    `U11` = "9v9",
    `U11 9v9` = "9v9",
    `U11/12 9v9` = "9v9", 
    `U12` = "9v9",
    `U12 9v9` = "9v9",
    `U13` = "11v11",
    `U13/14` = "11v11",
    `U14` = "11v11"
)

lookupGame <- function(level) {
    return(sapply(level, \(v) { as.character(LevelMap[v])}))
}

read_games <- function(session, org=2820) {
    
    game_cache <- cache('games', org)
    today <- as.character(Sys.Date())
    games <- game_cache$get(org)
    if (!is.null(games)) return(games)
    pageNo <- 1
    pageSize <- 1000
    rows <- list()
    # Read results using paging
    repeat {
        resp <- 
            str_c("https://www.oregonsoccercentral.com/OSICORE/game/results.php?showResultsPage=", pageNo) |>
            httr::POST(encode="form",
                       body=list(
                           `selOrgID`= org,
                           `showCols[]`="2",
                           `dateFirst`="Thu 3/10/2001",
                           `sMin`="00",
                           `sAmPm`="am",
                           `eMin`="00",
                           `eAmPm`="am",
                           `showCols[]`="2048",
                           `showCols[]`="16384",
                           `showCols[]`="4096",
                           `divisionID`="1064",
                           `showCols[]`="131072",
                           `status`="Active",
                           `showCols[]`="524288",
                           `showCols[]`="1048576",
                           `showCols[]`="2097152",
                           `mentored`="0",
                           `sortBy`="gameid",
                           `mode`="View",
                           `showResultsCount`=pageSize,
                           `doGameSearch`="1"
                       ),
                       config=session$config,
                       handle=session$handle)
        page <- html_elements(httr::content(resp), css="#gameSearchTable > tbody > tr") 
        rows <- append(rows, page)
        if (length(page) < pageSize) break;
        pageNo <- pageNo + 1
    }
    
    games <- 
        lapply(rows, \(row) {
            cells <- html_children(row)
            list(
                Date = html_text(cells[3]) |> mdy(),
                Level = html_text(cells[4]),
                Season = html_text(cells[6]),
                Rank = html_text(cells[7]) |> parseint(),
                Center = parse_ref_id(cells[8]),
                AR1 = parse_ref_id(cells[9]),
                AR2 = parse_ref_id(cells[10])            
            )
        }) |> 
        bind_rows() |>
        mutate(Level = as.factor(Level),
               Season = as.factor(Season),
               Game = lookupGame(Level))
    game_cache$put(today, games)
    return(games)
}

# This will do a bulk update of the refs in the table using:
# * Org.InitialYear
# * Org.CurrentYear
# * Rank.Center
# * Rank.AR

updateRefDetails <- function(session, refs, org=2820) {

    if (nrow(refs) == 0) {
        return();
    }
    
    params <- list(updateRefInfo="Update Referee Information")
    
    values <- refs$Org.CurrentYear
    names(values) <- str_c("currYear[", refs$UID, "][", org, "]")
    params <- append(params, as.list(values))
    
    values <- refs$Org.InitialYear
    names(values) <- str_c("initYear[", refs$UID, "][", org, "]")
    params <- append(params, as.list(values))
    
    values <- ifelse(is.na(refs$Rank.Center), 0, refs$Rank.Center)
    names(values) <- str_c("ctrRank[", refs$UID, "][", org, "]")
    params <- append(params, as.list(values))
    
    values <- ifelse(is.na(refs$Rank.AR), 0, refs$Rank.AR)
    names(values) <- str_c("arRank[", refs$UID, "][", org, "]")
    params <- append(params, as.list(values))

    #params <- params[!is.na(params)]
    
    # Perform the search to set up the session.  If you don't do this, the bulk update doesn't work.
    resp <-  httr::POST("https://www.oregonsoccercentral.com/OSICORE/user/results.php",
                        encode="form",
                        body=list(
                            `selOrgID`="2820",
                            `userID`="111863",
                            `userOrgInfoColumns[]`="32",
                            `userOrgInfoColumns[]`="128",
                            `userOrgInfoColumns[]`="64",
                            `userOrgInfoColumns[]`="256",
                            `sortBy`="name",
                            `mode`="refdata",
                            `find`="Find Users",
                            `showResultsCount`="250"),
                        config=session$config,
                        handle=session$handle)
    
    # Execute the bulk update
    resp <-  httr::POST("https://www.oregonsoccercentral.com/OSICORE/user/results.php",
                        encode="form",
                        body=params,
                        config=session$config,
                        handle=session$handle)
    cat("STATUS: ", httr::status_code(resp), "\n")
}

generateEmailTo <- function(refs) {
    addresses <- select(refs, FullName, Email1=Email, Email2=SecondaryEmail) |>
        pivot_longer(cols=c('Email1', 'Email2'),
                     values_to='Email') |>
        filter(!is.na(Email)) |>
        select(-name)
    
    return(str_c('"',addresses$FullName,'"','<',addresses$Email,'>', collapse=', '))
}
