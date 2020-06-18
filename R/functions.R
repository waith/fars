#' Define 5 functions for export : need to add export before each function
#'
#' \code{fars_read(filename)} read csv file with name filename
#' \code{make_filename(year)} make csv file of the year
#' \code{fars_read_years(years)} make csv file of years
#' \code{fars_summarize_years(years)} summarise data of years.
#' \code{fars_map_state(state.num, year)} plot map of the state.num
#'
#' @param state.num Number representing a US state.
#' @param year  a single year
#' @param years a list of years
#' @param filename is the location of the data file
#'
#' @return NULL
#' @examples
#' \dontrun{
#' fars_map_state(18, 2015)
#' }
#'
#' @import maps
#' @import tidyr
#' @import readr
#' @import graphics
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        tibble::as_tibble(data)
}

#' Define 5 functions for export : need to add export before each function
#'
#' \code{fars_read(filename)} read csv file with name filename
#' \code{make_filename(year)} make csv file of the year
#' \code{fars_read_years(years)} make csv file of years
#' \code{fars_summarize_years(years)} summarise data of years.
#' \code{fars_map_state(state.num, year)} plot map of the state.num
#'
#' @param state.num Number representing a US state.
#' @param year  a single year
#' @param years a list of years
#' @param filename is the location of the data file
#'
#' @return NULL
#' @examples
#' \dontrun{
#' fars_map_state(18, 2015)
#' }
#'
#' @import maps
#' @import tidyr
#' @import readr
#' @import graphics
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Define 5 functions for export : need to add export before each function
#'
#' \code{fars_read(filename)} read csv file with name filename
#' \code{make_filename(year)} make csv file of the year
#' \code{fars_read_years(years)} make csv file of years
#' \code{fars_summarize_years(years)} summarise data of years.
#' \code{fars_map_state(state.num, year)} plot map of the state.num
#'
#' @param state.num Number representing a US state.
#' @param year  a single year
#' @param years a list of years
#' @param filename is the location of the data file
#'
#' @return NULL
#' @examples
#' \dontrun{
#' fars_map_state(18, 2015)
#' }
#'
#' @import maps
#' @import tidyr
#' @import readr
#' @import graphics
#' @export
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

#' Define 5 functions for export : need to add export before each function
#'
#' \code{fars_read(filename)} read csv file with name filename
#' \code{make_filename(year)} make csv file of the year
#' \code{fars_read_years(years)} make csv file of years
#' \code{fars_summarize_years(years)} summarise data of years.
#' \code{fars_map_state(state.num, year)} plot map of the state.num
#'
#' @param state.num Number representing a US state.
#' @param year  a single year
#' @param years a list of years
#' @param filename is the location of the data file
#'
#' @return NULL
#' @examples
#' \dontrun{
#' fars_map_state(18, 2015)
#' }
#'
#' @import maps
#' @import tidyr
#' @import readr
#' @import graphics
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Define 5 functions for export : need to add export before each function
#'
#' \code{fars_read(filename)} read csv file with name filename
#' \code{make_filename(year)} make csv file of the year
#' \code{fars_read_years(years)} make csv file of years
#' \code{fars_summarize_years(years)} summarise data of years.
#' \code{fars_map_state(state.num, year)} plot map of the state.num
#'
#' @param state.num Number representing a US state.
#' @param year  a single year
#' @param years a list of years
#' @param filename is the location of the data file
#'
#' @return NULL
#' @examples
#' \dontrun{
#' fars_map_state(18, 2015)
#' }
#'
#' @import maps
#' @import tidyr
#' @import readr
#' @import graphics
#' @export
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
