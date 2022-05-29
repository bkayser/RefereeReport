# Make sure you call this the first time with the username and password arguments.  
# Return a list having the list of all games and the list of all referees.
create_session <- function(refname, username=NULL, password=NULL) {
    
    account_cache <- cache('accounts')
    
    if (!is.null(username) & !is.null(password)) {
        account_cache$put(refname, list(username=username, password=password))
    } else {
        cached_credentials <- account_cache$get(refname)
        if (is.null(cached_credentials)) {
            stop('No cached credentials for ', refname, '.  Call again with user/pass params.')
        }
        username <- cached_credentials$username
        password <- cached_credentials$password
    }
    
    # Start Session by logging in
    session <- session("https://www.oregonsoccercentral.com")
    login_page <- session_jump_to(session, "https://www.oregonsoccercentral.com/OSICORE/login/login.php?lm=lo")
    form <- html_form(login_page)[[1]] |>
        html_form_set(username=username, password=password) 
    response <- session_submit(session, form)
    if (response$response$status_code != 200) {
        stop("Failed to login: ", read_html(response))
    }
    return(session)
    
}

cache <- function(name='cache', namespace=NULL) {
    
    cachedir <- function() {
        d <- str_glue('cache/',name)
        if (!is.null(namespace)) {
            d <- str_glue(d, '/{namespace}')
        }
        return(d)
    }
    filename <- function(id) {
        return(str_glue(cachedir(), '/{id}.RDS'))
    }    
    if (!dir.exists(cachedir())) {
        dir.create(cachedir(), recursive = T)
    }
    get <- function(id) {
        f <- filename(id)
        if (file.exists(f)) {
            return(readRDS(f))
        } else {
            return(NULL)
        }
    }
    put <- function(id, data) {
        saveRDS(data, filename(id))
    }
    
    return(list(get = get, 
                put = put))
}

# Utility parsing helpers

parseint <- function(v) { 
    parse_integer(v, c("0", "NA", "N/A", ""))
}

