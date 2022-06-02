
dts <- function(SUBSET = TRUE, k = "", p.sub = "", data = NULL){   
  if (SUBSET == TRUE) {
    if (k == "" | paste0(p.sub, collapse = "_") == "" ) {
      data <- data    
      } 
    if( k != "" & paste0(p.sub, collapse = "_") != "") {
      req( k %in% names(data))
      data <- data %>% dplyr::filter(.data[[k]] %in% p.sub)  
      }
    return(data)
  } else {
    return(data)}
}