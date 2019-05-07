#' @title Fars Read: Reading a Fatality Traffic dataset
#' @description This function return a tidy tible dataset of from the US National
#'              Highway Traffic Safety Administration's Fatality Analysis Reporting System in a zip format
#' @usage \code{"fars_read(filename)"}
#' @param filename A file with csv extension
#' @import readr
#' @import tibble
#' @details The function will return a message of "File not exists" if the file requested
#'          in the argument does not exist
#' @return Return a tidy tibble dataset

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}


#' @title File Naming: Rename the Fars dataset
#' @description This function returns a character vector containing a formatted
#' @param year numeric value of the year
#' @return "accident_2006.csv.bz2" a formatted character vector of a dataset

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' @title Reading the Fars Dataset
#' @description This function takes a list of years for FARS dataset and convert it to the  tidy dataset with
#'              additional column of the year.
#' @param years Integer or numerical index for the years
#' @details The function will return an error if the years inserted is not available within the list
#' @return The function return a list of FARS dataset with corrected names and time signal columns
#' @import dplyr


fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' @title Summarize the FARS Dataset
#' @description This function returns a tibble dataset of multi-years labeled FARS dataset
#' @param years a list with numerical values depicting the years of interest in FARS dataset
#' @return The function returns a wide tibble object with the result of total number of events in year and month
#' @import dplyr
#' @import tidyr


fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' @title Create of Map on the Number of incidents by state and year
#' @description The function return a map with data points of the number of incidents happened over
#'              over a particular year and time. It he specified character(s) are plotted, centered
#'              at the coordinates and the map is limited to the state boundaries
#' @param state.num a numerical value that represent the state selection in the function
#' @param year a numerical value that represent the year when the accidents happened
#' @details The function will return an Error saying that the State is not found if the state.num parameter is not found within the dataset
#'          The function will return an Error saying that there is no accidents at that particular years and state if the length of Data is 0
#' @return The function will return a map graphic with points to show incidents at particular state and year
#' @import dplyr
#' @import maps
#' @import graphics

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
