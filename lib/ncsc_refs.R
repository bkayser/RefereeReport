library(tidyverse)
library(rvest)

source('./lib/RefereeDB.r')
source('./lib/utils.R')

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
    games <- game_cache$get(today)
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

season_summary <- function(session, begin_date=mdy("1/1/2010"), end_date = Sys.Date()) {
    
    games <- read_games(session) |>
        pivot_longer(all_of(c('Center', 'AR1', 'AR2')),
                     names_to="Position",
                     values_to="UID") |>
        mutate(Year=year(Date), Years=year(Date)) |>
        filter(!is.na(UID) & Date >= begin_date & Date <= end_date) |>
        mutate(rownum = row_number()) |>
        pivot_wider(names_from=Years, values_from=Years, values_fn=length, names_prefix='GamesIn', values_fill=0) |>
        group_by(UID) |>
        summarize(
            Center.7v7 = sum(Game == '7v7'),
            Center.9v9 = sum(Game == '9v9' & Position == 'Center'),
            Center.11v11 = sum(Game == '11v11' & Position == 'Center'),
            Center = sum(Position == 'Center'),
            AR = sum(Position == "AR1" | Position == "AR2"),
            Fall = sum(Season == 'Fall'),
            Spring = sum(Season == 'Spring'),
            across(starts_with("GamesIn"), sum),
            Rank1 = sum(Rank == 1),
            Rank2 = sum(Rank == 2),
            Rank3 = sum(Rank == 3),
            RankOther = sum(Rank > 3),
            FirstYear = min(Year),
            LastYear = max(Year),
            Total=n(),
        )  |>
        right_join(x=osc_refs(session), by='UID') |>  # read_refs returns refs currently in the org
        filter(ID != "") |>
        mutate(across(!(1:23), \(v) ifelse(is.na(v), 0, v)))  # Write zeros into all the NAs introduced in the join.
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
