# Create a calendar file based on upcoming games for the given ref.
source('./lib/referee_data.R')

# make_vcs("Kayser, Felix")
# make_vcs("Kayser, Bill")
make_vcs <- function(name, username = NULL, password = NULL) {
    db <- collect_ref_data(name, username, password)
    upcoming <- filter(db$games, upcoming)
    filename <- str_c(db$ref$FirstName,'-games.vcs')
    sink(filename)
    cat("BEGIN:VCALENDAR\n")
    for (i in 1:nrow(upcoming)) {
        cat(sep="", "BEGIN:VEVENT\n")
        cat(sep="", "SUMMARY:", db$ref$FirstName, " @ ", as.character(upcoming$field[i]), "\n")
        cat(sep="", "LOCATION:", as.character(upcoming$field[i]), "\n")
        cat(sep="", "DTSTART:", strftime(upcoming$time[i], "%Y%m%dT%H%M%00"), "\n")
        cat("DURATION:PT1H0M0S\n")  
        
        crew <- t(select(upcoming[i,], center, ar1, ar2, assessor))[,1]
        crew <- crew[!is.na(crew) & crew != ""]
        
        cat(sep="", 
            "DESCRIPTION:Game #", upcoming$gameID[i]," for ", as.character(upcoming$org[i]), "\\n",
            "Position: ", str_to_upper(upcoming$position[i]), "\\n", 
            str_c(str_to_upper(names(crew)), crew, sep=": ", collapse="\\n"), "\\n",
            "Home: ", upcoming$home[i], "\\n",
            "Visitor: ", upcoming$away[i], "\n")
        cat("END:VEVENT\n")
    }
    cat("END:VCALENDAR\n")
    sink(NULL)
    cat("Completed: ", filename)
}