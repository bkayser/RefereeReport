library(tidyverse)
library(rvest)

# Return the spreadsheet DB of referees
referee_database.2022 <- function() {
    return(
        read_csv("data/Active Referees_March 29.csv", show_col_types = FALSE) |>
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
