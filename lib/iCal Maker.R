library(tidyverse)

data <- bind_rows(read.csv("~/Downloads/Putnam Junior Varsity Boys Soccer Schedule.csv") %>% mutate(Team="JV"),
                  read.csv("~/Downloads/Putnam Junior Varsity 2 Boys Soccer Schedule.csv") %>% mutate(Team="JV2")) %>%
    filter(Status == "Scheduled") %>%
    mutate(Start = strptime(str_c(Date, " ", Time), "%m/%d/%y %I:%M %p"),
           Home = Home.Team == "Putnam")

filename <- "putnam_sched.vcs"
sink(filename)
cat("BEGIN:VCALENDAR\n")
for (i in 1:nrow(data)) {
    cat(sep="", "BEGIN:VEVENT\n")
    cat(sep="", "SUMMARY:", "Felix ", data$Team[i], ifelse(data$Home[i], " Home ", " Away "), "Game\n")
    cat(sep="", "LOCATION:", as.character(data$Location[i]), "\n")
    cat(sep="", "DTSTART:", strftime(data$Start[i], "%Y%m%dT%H%M%00"), "\n")
    cat("DURATION:PT1H30M0S\n")  
    cat(sep="", 
        "DESCRIPTION:", data$Type[i], ifelse(data$Home[i], " Home ", " Away "), "Game #", data$Contest.ID[i],
        " against ", ifelse(data$Home[i], data$Away.Team[i], data$Home.Team[i]),"\\n", 
        "Depart at ", data$Depart[i], "\\n",
        "Return at ", data$Return[i], "\n")
    cat("END:VEVENT\n")
}
cat("END:VCALENDAR\n")
sink(NULL)
