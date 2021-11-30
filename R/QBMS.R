

qbmsbrapi <- function(url = "https://bms.ciat.cgiar.org/ibpworkbench/controller/auth/login", username = NULL , password = NULL){
  if(is.null(url)|url=="") return()
  if(is.null(username)|username=="") return()
  if(is.null(password)|password=="") return()
  bmsbase <- QBMS::set_qbms_config(url)
  bmslogin <- QBMS::login_bms(username = username , password = password)
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
  studies <- studies[1,1] 
  return(studies)
}


dataqbms <- function(studies = NULL){
  if(is.null(studies)) return()
  QBMS::set_study(studies)
  data <- QBMS::get_study_data() %>% 
            data.frame(check.names = TRUE, stringsAsFactors = T) %>%
            type.convert()
  return(data)
}


mult_dataqbms <- function(trials){
  trials <- trials
  data_all <- list()
  for (variable in trials) {
    a <- qbmsstudies(trial = variable)
    data_all[[variable]] <- dataqbms(studies = a)
  }
  data_all <- dplyr::bind_rows(data_all)
  return(data_all)
}




