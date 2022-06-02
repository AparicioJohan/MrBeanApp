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
dataReact <- function(file = NULL, 
                      choice = 1, 
                      header = TRUE, 
                      sep = ",",
                      miss = "NA",
                      string = "" ,
                      sheet = NULL,
                      dataBMS = NULL ){ 
  
  inFile <- file
  Ext <- tools::file_ext(inFile$datapath)
  
  if (choice == 1) {
    dt <- Dar16C_hiP
    } else if (choice == 2) {
      if (is.null(inFile)) { 
        dt <- data.frame() 
        } else { 
          if (Ext == "xlsx" | Ext == "xls") {
            if (miss == "Empty" ){ 
              P = "\" \""
              } else {
                P = "NA"
                if (miss == "Other") {
                  P = string
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
        dt <-  read.csv(
          file = inFile$datapath,
          header = header,
          sep = sep
          )
        if (miss == "Other") {
          P = string
          dt <- read.csv(
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


  

