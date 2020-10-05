
dts <- function(SUBSET, k, p.sub, data){   
  # SUBSET = input$subset ( TRUE / FALSE )  ; k =  input$varsubset (  variable for subset ) ; p.sub = input$levelessub
  if(SUBSET==TRUE) {
    if(k==""|paste0(p.sub,collapse = "_")=="" ){
      data <- data                                     # if(k==""|p.sub=="" )
    } 
    if(k!=""&paste0(p.sub,collapse = "_")!="") {
      req(k%in%names(data))
      data <- data %>% dplyr::filter(.data[[k]]%in%p.sub)  #  k!=""&p.sub!="" get(k)==p.sub
    }
    return(data)
  } else {
    return(data)}
}