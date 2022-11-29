#' Reactive Dataset
#'
#' @param file R-shiny input file
#' @param choice 1 = Example data, 2 = From local file, 3 = From BRAPI
#' @param header TRUE or FALSE
#' @param sep Cell separation character
#' @param miss missing values as NA, Empty or Other
#' @param string when miss = Other, string character for missing values
#' @param sheet when the extension is xslx, define the name of the sheet
#' @param dataBMS if choice is 3, data.frame with information from BRAPI
#'
#' @return data.frame
#' @noRd
data_react <- function(file = NULL,
                       choice = 1,
                       header = TRUE,
                       sep = ",",
                       miss = "NA",
                       string = "",
                       sheet = NULL,
                       dataBMS = NULL) {
  inFile <- file
  Ext <- tools::file_ext(inFile$datapath)

  if (choice == 1) {
    dt <- Dar16C_hiP
  } else if (choice == 2) {
    if (is.null(inFile)) {
      dt <- data.frame()
    } else {
      if (Ext == "xlsx" | Ext == "xls") {
        if (miss == "Empty") {
          P <- "\" \""
        } else {
          P <- "NA"
          if (miss == "Other") {
            P <- string
          }
        }
        dt <- as.data.frame(
          readxl::read_excel(
            path = inFile$datapath,
            col_names = header,
            na = P,
            sheet = sheet
          )
        )
      } else {
        dt <- utils::read.csv(
          file = inFile$datapath,
          header = header,
          sep = sep
        )
        if (miss == "Other") {
          P <- string
          dt <- utils::read.csv(
            file = inFile$datapath,
            header = header,
            sep = sep,
            na.strings = P
          )
        }
      }
    }
  } else if (choice == 3) {
    dt <- dataBMS
  }
  return(dt)
}


#' Subset dataset
#'
#' @param data data.frame
#' @param subset whether subset or not the dataset
#' @param variable variable to subset
#' @param level level to subset
#'
#' @return data.frame
#' @noRd
data_subset <- function(data = NULL, subset = TRUE, variable = "", level = "") {
  if (subset == TRUE) {
    if (variable == "" | paste0(level, collapse = "_") == "") {
      data <- data
    }
    if (variable != "" & paste0(level, collapse = "_") != "") {
      req(variable %in% names(data))
      data <- data %>% dplyr::filter(.data[[variable]] %in% level)
    }
    return(data)
  } else {
    return(data)
  }
}
