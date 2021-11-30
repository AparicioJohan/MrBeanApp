cropbrapi <- function(brapi_crop){
  if(is.null(brapi_crop)|brapi_crop=="") return()
  cbase <- brapirv1::brapi_db()[[brapi_crop]]
  programs <- brapirv1::brapi_get_programs(cbase, pageSize = 9999999)
  list_programs <- programs$objective
  return(list(cbase = cbase, programsDT = programs , list_programs = list_programs)) 
}

trialbrapi <- function(brapi_program, cropbrapi){
  if(is.null(brapi_program)|brapi_program=="") return()
  if(is.null(cropbrapi)) return()
  programs <- cropbrapi$programsDT
  list_programs <- programs$objective
  programDbId_selected <- programs[programs$objective == brapi_program, "programDbId"]
  trials <- brapirv1::brapi_get_studies(cropbrapi$cbase, programDbId = programDbId_selected, pageSize = 9999999)
  list_trials <- trials$studyName
  return(list(cbase = cropbrapi$cbase, trialsDT = trials,list_trials = list_trials))
}

databrapi <- function(brapi_trials, trialbrapi){
  if(is.null(brapi_trials)|any(brapi_trials %in% "")) return()
  if(is.null(trialbrapi)) return()
  trials <- trialbrapi$trialsDT
  # trials
  list_trials <- trials$studyName
  studyDbIds_selected <- trials[trials$studyName %in% brapi_trials , "studyDbId"]
  searchResults <- brapirv1::brapi_post_search_observationtables(
    trialbrapi$cbase, 
    studyDbIds = studyDbIds_selected,
    pageSize = 9999999
  )
  datos <- brapirv1::brapi_get_search_observationtables_searchResultsDbId(
    trialbrapi$cbase,
    Accept = "text/csv",
    searchResults$searchResultsDbId,
    pageSize = 9999999
  )
  datos <- datos[-1,] %>% 
    data.frame(check.names = TRUE, stringsAsFactors = T) %>%
    type.convert()
  return(datos)
}
