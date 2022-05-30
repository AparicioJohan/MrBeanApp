

qbmsbrapi <- function(url = "https://bms.ciat.cgiar.org/ibpworkbench/controller/auth/login", 
                      engine = c("bms", "breadbase"),
                      path = ifelse(engine == "bms", "bmsapi", ""),
                      time_out = ifelse(engine == "bms", 120, 300),
                      no_auth = FALSE,
                      username = NULL,
                      password = NULL){
  if(is.null(url)|url=="") return()
  
  bmsbase <- QBMS::set_qbms_config(url = url, path = path, time_out = time_out, no_auth = no_auth, engine = engine )
  
  if(!no_auth){
    if(is.null(username)|username=="") return()
    if(is.null(password)|password=="") return()
    bmslogin <- QBMS::login_bms(username = username , password = password)
  } else {
    bmslogin <- NULL
  }
  crops <- QBMS::list_crops()
  return(list(bmsbase = bmsbase, bmslogin = bmslogin , crops = crops)) 
}

qbmsprograms <- function(crop = NULL){
  if(is.null(crop)) return()
  QBMS::set_crop(crop)
  programs <- QBMS::list_programs()
  return(programs)
}

qbmstrials <- function(program = NULL){
  if(is.null(program)) return()
  QBMS::set_program(program)
  trials <- QBMS::list_trials()
  return(trials)
}


qbmsstudies <- function(trial = NULL){
  if(is.null(trial)) return()
  QBMS::set_trial(trial)
  studies <- QBMS::list_studies()
  return(studies)
}


dataqbms <- function(studies = NULL, dt_studies = NULL){
  if(is.null(studies)) return()
  if(is.null(dt_studies)) return()
  
  trial_study <- function(study, dt_studies){
    trial <- dt_studies[dt_studies$studyName == study, "trial"] %>% as.character
    QBMS::set_trial(trial)
    QBMS::set_study(study)
    data <- QBMS::get_study_data() %>% 
      data.frame(check.names = TRUE, stringsAsFactors = T) %>%
      type.convert(as.is = FALSE)
    return(data)
  }
  
  mult_dt <- lapply(studies, trial_study, dt_studies = dt_studies)
  mult_dt <- dplyr::bind_rows(mult_dt)
  return(mult_dt)
}




