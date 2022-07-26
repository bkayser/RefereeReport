library(tidyverse)
library(rvest)
source('lib/utils.R')
source('lib/RefereeDB.R')
# Utility functions to suspport reports

# Make sure you call this the first time with the username and password arguments.  
# Return a list having the list of all games and the list of all referees.
#
# For the given ref, returns a list of:
#   Games recorded by the ref
#   Table of referees who have worked with this ref
#   Summary of the ref.
#
collect_ref_data <- function(referee_name, username=NULL, password=NULL) {
    
    session <- create_session(referee_name, username, password)

    orgs.all <- organizations(session)
   
    ref <- referee(session, orgs.all, referee_name)
    if (is_empty(ref$referee_name)) {
      stop("Could not find ref with name ", referee_name)
    }
    
    referee_id <- ref$referee_id
    referee_name <- ref$referee_name
    friendly_name <- ref$friendly_name
    
    #ordered_levels <- referees('level', orgs.all, session)
    # 0,1080,2800,1,1841,4,2680,2820,2120,4420,3300,14,4520,4780,3560,3700,4160,4440,1100,1180,3221,12,4600
    
    games <- all_game_ids(session, orgs.all)
    game_cache <- cache('games', username)
    all_game_details = tibble()
    for (game in games) {
        details <- game_cache$get(game)
        if (is.null(details)) {
            Sys.sleep(1)
            details <- game_details(session, game, friendly_name)   
            game_cache$put(game, details)
        }
        all_game_details <- bind_rows(all_game_details, details)
    }
    
    games <- all_game_details |> 
        mutate(
            time = strptime(str_c(date, time, collaps=" "), "%a, %b %e, %Y %l:%M %p"),
            upcoming = time >= Sys.time(),
            org = as.factor(org),
            field = as.factor(field),
            system = as.factor(system),
            city = as.factor(city),
            position = factor(position, c('assessor', 'center', 'ar1', 'ar2'), ordered = T) |> fct_drop(),
            gender = factor(gender, c('Girls', 'Boys', 'Women', 'Mixed', 'Coed', 'Men', ordered = T)) |> fct_drop(),
            level = factor(level, ordered_levels, ordered = T) |> fct_drop())
        
    return(list(games=games,
                referees=osc_refs(session, orgs.all),
                ref=ref$ref))
}


organizations <- function(session) {
    # Get the complete list of org ids
    orgs_page <- session_jump_to(session, "https://www.oregonsoccercentral.com/OSICORE/org/orgtree.php?chkbxs=true&affView=0")
    orglist <- html_elements(orgs_page, css = '#tree-0 li > input') 
    sapply(orglist, function(elem){
        html_attr(elem, 'id') |> str_remove("check-") |> as.integer()
    }) |> 
        sort() %>%
        return()
    
}

# Execute a search for games in which the session user was assigned.  
# Return a vector of game ids
all_game_ids <- function(session, orgs){
    page_size <- 250
    game_search <- session_jump_to(session, "https://www.oregonsoccercentral.com/OSICORE/game/assignment.php")
    search_form <- html_form(game_search)[[1]] |>
        html_form_set(selOrgID=str_c(sort(orgs), collapse=','),
                      dateFirst='1/1/2001',
                      showResultsCount=page_size,
                      refSearchOption = 'assigned') |>
      suppressWarnings()
    
    response <- session_submit(session, search_form)
    
    games <- html_elements(response, css='a[title^="click here"]') |>
        lapply(html_text) |>
        as.integer() 
    last_fetch <- length(games)
    
    canceled_games <- html_elements(response, css='tr.cancelledGame a[title^="click here"]') |>
        lapply(html_text) |>
        as.integer()
    
    games <- setdiff(games, canceled_games)
    page <- 2
    while (last_fetch >= page_size) {
        next_page_url <- str_glue('https://www.oregonsoccercentral.com/OSICORE/game/results.php?showResultsPage=', page)
        response <- session_jump_to(session, next_page_url)
        
        next_games <- html_elements(response, css='a[title^="click here"]') |>
            lapply(html_text) |>
            as.integer() 
        last_fetch <- length(next_games)
        
        canceled_games <- html_elements(response, css='tr.cancelledGame a[title^="click here"]') |>
            lapply(html_text) |>
            as.integer()
        
        next_games <- setdiff(next_games, canceled_games)
        
        page <- page + 1   
        if (last_fetch > 0) {
            games <- rlist::list.append(games, next_games)
        }
    }
    return(games)
}

game_details <- function(session, game, friendly_name) {
    game_page <- session_jump_to(session, 
                                 str_c("https://www.oregonsoccercentral.com/OSICORE/game/view.php?gameID=", game))
    
    rows <- html_elements(game_page, css='table.thin_border_cascade > tr')
    retry_count <- 0
    date <- html_element(rows[2], css='td:nth-child(2)') |> html_text()
    while (str_length(date) == 0) {
        retry_count <- retry_count + 1
        if (retry_count > 10) {
            warning("Can't get data for game ", game)
        }
        print(str_c("retry game ", game), quote=F)
        game_page <- session_jump_to(session, 
                                     str_c("https://www.oregonsoccercentral.com/OSICORE/game/view.php?gameID=", game))
        
        rows <- html_elements(game_page, css='table.thin_border_cascade > tr')
        date <- html_element(rows[2], css='td:nth-child(2)') |> html_text()
    }
    details <- list(
        gameID = game,
        date = date,
        time = html_element(rows[3], css='td:nth-child(2)') |> html_text(),
        org = html_element(rows[2], css='td:nth-child(4)') |> html_text(),
        field = html_element(rows[4], css='td:nth-child(2)') |> html_text(),
        system = html_element(rows[4], css='td:nth-child(4)') |> html_text(),
        city = html_element(rows[6], css='td:nth-child(2)') |> html_text(),
        home = html_element(rows[7], css='td:nth-child(2)') |> html_text(),
        away = html_element(rows[7], css='td:nth-child(4)') |> html_text()
    )
    
    level <- html_element(rows[5], css='td:nth-child(4)') |> 
        html_text() |>
        str_split(", ")
    details$gender = level[[1]][1]
    details$level = level[[1]][2]
    
    if (length(rows) > 7) {
        notes <- html_element(rows[8], css='td:nth-child(1)') |> html_text() |> str_trim() |> str_remove("^Notes: ?") |> str_trim()
        if (str_length(notes) > 0) {
            details$notes <- notes
        }
        for (row in rows[10:length(rows)]) {
            label <- html_element(row, css='td:nth-child(1)') |>
                html_text() |> 
                str_trim() |>
                tolower()
            value <- html_element(row, css='td:nth-child(2)') |>
                html_text() |> 
                str_trim()
            details[[label]] <- value
        }
        position <- names(details[details==friendly_name])
        if (!is_empty(position)) {
            details['position'] <- position[1]
        }
    } else {
        details['position'] <- NA
    }
    return(details)
}

referee <- function(session,
                    org_ids,
                    referee_name) {
    referee_id <- NULL
    refs <- osc_refs(session, org_ids) |> 
        mutate(Name = str_c(LastName, ", ", FirstName))
    ref <- filter(refs, Name == referee_name)
    if (is_empty(ref)) {
        stop("Cannot find id of ", referee_name)
    }
    referee_name <- ref$Name
    return(list(referee_id = ref$ID,
                referee_name = ref$Name,
                friendly_name = ref$FullName,
                ref = ref))
}

ordered_levels <- c('n/a',
            'Standby',
            'Standby 2',
            'Stnby/Mntr',
            'Various',
            'Various 1 hr',
            '2 hr block',
            '2.5 hr block',
            '3 hr block',
            '3.5 hr block',
            '4 hr block',
            'Dev',
            'Kinder',
            'Kinder-1st Grade',
            '1st Grade',
            '2nd Grade',
            '2nd-3rd Grade',
            '3rd Grade',
            '4th Grade',
            '4th-5th Grade',
            '5th Grade',
            '5th-6th Grade',
            '6th Grade',
            'Grade Sch 8v8',
            'U05',
            'U06',
            'U07',
            'U07-08',
            'U07-08 5v5',
            'U08',
            'U08 4v4',
            'U08-09',
            'U09',
            'U09 7v7',
            'U09-10',
            'U09-10 7v7',
            'U10',
            'U10 7v7',
            '6th-7th Grade',
            '7th Grade',
            '7th-8th Grade',
            '8th Grade',
            '8th-9th Grade',
            '9th-10th Grade',
            'Rec',
            'U10-11',
            'U10-11 9v9',
            'U11',
            'U11 9v9',
            'U11-12',
            'U11-12 9v9',
            'U12',
            'U12 9v9',
            'U12-13',
            'U12-14',
            'Junior High',
            'Middle Sch',
            'U13',
            'U13-14',
            'U13/14 & HS',
            'U14',
            'U14-15',
            'U15',
            'U15-16',
            'U15-17',
            'Freshman',
            'Fresh/Soph',
            'Jr/Sr',
            'High Sch',
            'HS Coed',
            'JV 2',
            'JV',
            'U16',
            'U16-17',
            'U16-18',
            'U16-19',
            'U17',
            'U17-18',
            'U17-19',
            'U18',
            'Varsity',
            'U18-19',
            'U19',
            'Masters',
            'Over 65',
            'Over 58',
            'Over 50',
            'Over 40',
            'Over 35 B',
            'Over 35 A',
            'Over 35',
            'Over 30',
            'U19-20',
            'U20',
            'Open',
            'College Club',
            'Unified',
            'Nwacc',
            'NW Conf',
            'Naia',
            'Ncaa',
            'Pro')
