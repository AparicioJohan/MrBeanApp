conectBMS <- function( user="" , password= "", protocol = "http://" , db = "bms.ciat.cgiar.org" ){
  bmscon <- try(brapi::ba_connect(brapiDb = NULL, secure = FALSE, protocol = protocol,
                                  db = db , port = 48080, apipath = "bmsapi", multicrop = FALSE,
                                  crop = "", user = user, password = password, token = "", clientid = "rbrapi", bms = TRUE), silent = T  )
  bmscon <- try(login_bms(con = bmscon), silent = T)
  if(length(bmscon)==1) return()
  bmscon
}


cropBMS <- function(conection){
  bmscon <- conection  
  if(is.null(bmscon)) return()
  crops <- suppressWarnings(data.frame(ba_crops_new(con = bmscon))[,1])
  crops
}

programBMS <- function(conection, crop ){
  bmscon <- conection
  if(is.null(bmscon)) return()
  if(is.null(crop)) return()
  if(crop=="") return()
  bmscon$crop <-  as.character(crop)
  programs <- as.character(ba_programs_new(bmscon)$name)
  programs 
}

trialBMS <- function(conection, cropSele, programSele, rclass = "data.frame" ){
  bmscon <- conection
  if(is.null(bmscon)) return()
  if(is.null(programSele)) return()
  if(programSele=="") return()
  if(is.null(cropSele)) return()
  if(is.null(cropSele)|cropSele=="") return()
  bmscon$crop <-  as.character(cropSele)
  programs <- ba_programs_new(bmscon)
  pr.DbId <- as.character(programs[programs$name==programSele,1])
  trials <- try(ba_trials_new(con = bmscon, programDbId = pr.DbId , rclass = rclass) ,silent = T)
  if(length(trials)==1) return()
  trials <- trials$trialName 
  trials <- make.names( trials  , unique=TRUE)  
  trials
}

BMS <- function(conection, crop, programSele, trialSel){
  bmscon <- conection
  if(is.null(bmscon)) return()
  if(is.null(crop)) return()
  if(crop=="") return()
  bmscon$crop <-  as.character(crop)
  dt <- data.frame() 
  programs <- ba_programs_new(bmscon)
  pr.DbId <- as.character(programs[programs$name==programSele,1])
  trials <- try(ba_trials_new(con = bmscon, programDbId = pr.DbId , rclass = "data.frame") ,silent = T)
  if(length(trials)==1) return()
  trials$trialName2 <- make.names( trials$trialName  , unique=TRUE)  
  studyDbId <- as.character(trials[trials$trialName2 %in% trialSel,  "studyDbId"])  
  if(length(trialSel)>=2){
    # Function for Merge data frames
    MultBMS <- function(studyDbID){
      studyDbID <- as.character(studyDbID)
      studyTable21 <- ba_studies_table_new(con = bmscon , studyDbId = studyDbID, rclass = "data.frame") %>%  data.frame()
      names(studyTable21) <-  sub("\\..*", "", names(studyTable21))
      studyTable21 <- studyTable21  %>% dplyr::mutate_if(is.factor, as.character) 
      return(studyTable21)
    }
    list_manual = mapply(MultBMS, studyDbId, SIMPLIFY = F, USE.NAMES = T)
    dt <- dplyr::bind_rows(list_manual) %>% type.convert()
    dt
  } else {
    dt <-  ba_studies_table_new(con = bmscon , studyDbId = studyDbId, rclass = "data.frame") %>% data.frame()
    names(dt) <-  sub("\\..*", "", names(dt))
    dt
  }
  
}




# function for login 
# the change in this function is remove the port 48080

login_bms <- function (con) 
{
  stopifnot(brapi::is.ba_con(con))
  brapi::ba_can_internet()
  omc <- con$multicrop
  con$multicrop <- FALSE
  brp <- brapi:::get_brapi(con = con)
  callurl <- paste0(brp, "token")
  callurl <- gsub(":48080", "", callurl)  # johan
  con$multicrop <- omc
  dat <- list(username = con$user, password = con$password, 
              grant_type = "password", client_id = "")
  brapi:::ba_message(msg = jsonlite::toJSON(x = dat, pretty = TRUE))
  resp <- httr::POST(url = callurl, body = dat, encode = ifelse(con$bms == 
                                                                  TRUE, "json", "form"))
  if (resp$status_code == 401) {
    httr::stop_for_status(x = resp, task = "authenticate. Check your username and password!")
  }
  else {
    if (resp$status_code != 200) {
      httr::stop_for_status(x = resp)
    }
    else {
      xout <- httr::content(x = resp)
      token <- xout$access_token
      con$token <- token
      con$expires_in <- httr::content(x = resp)$expires_in
      brapi:::ba_message(jsonlite::toJSON(x = xout, pretty = TRUE))
      message("Authenticated!")
    }
  }
  return(con)
}


# The change is remove the port
get_brapi_new <- function (con = NULL) 
{
  if (is.null(con)) 
    return(NULL)
  if (!is.null(con$apipath)) {
    con$apipath <- paste0("/", con$apipath)
  }
  if (con$secure) {
    con$protocol <- "https://"
  }
  # port <- ifelse(con$port == 80, "", paste0(":", con$port))
  port <- ""
  version <- ifelse("version" %in% names(con), con$version, 
                    "v1")
  brapi_version <- paste0("/brapi/", version, "/")
  if (con$multicrop) {
    url <- paste0(con$protocol, con$db, port, con$apipath, 
                  "/", con$crop, brapi_version)
  } else {
    url <- paste0(con$protocol, con$db, port, con$apipath, 
                  brapi_version)
  }
  return(url)
}


ba_crops_new <- function (con = NULL, rclass = c("tibble", "data.frame", "list", 
                                                 "json", "vector")) 
{
  .Deprecated(new = "ba_commoncropnames")
  stopifnot(brapi::is.ba_con(obj = con))
  rclass <- match.arg(rclass)
  omc <- con$multicrop
  con$multicrop <- FALSE
  brapi:::ba_check(con = con, verbose = FALSE, brapi_calls = "crops")
  brp <- get_brapi_new(con = con)
  callurl <- paste0(brp, "crops")
  orclass <- rclass
  rclass <- brapi:::df2tibble(rclass = rclass)
  out <- try({
    res <- brapi:::brapiGET(url = callurl, con = con)
    res2 <- httr::content(x = res, as = "text", encoding = "UTF-8")
    out <- brapi:::dat2tbl(res = res2, rclass = rclass)
    if (any(class(out) %in% c("tbl_df", "data.frame"))) {
      names(out)[1] <- "crops"
    }
    if (orclass == "data.frame") 
      out <- as.data.frame(out)
    class(out) <- c(class(out), "ba_crops")
    out
  })
  con$multicrop <- omc
  return(out)
}


ba_programs_new <- function (con = NULL, programName = "", abbreviation = "", commonCropName = "", 
                             pageSize = 1000, page = 0, rclass = c("tibble", "data.frame", 
                                                                   "list", "json")) 
{
  brapi:::ba_check(con = con, verbose = FALSE, brapi_calls = "programs")
  brapi:::check_character(programName, abbreviation, commonCropName)
  rclass <- match.arg(rclass)
  brp <- get_brapi_new(con = con) %>% paste0("programs") # johan
  callurl <- brapi:::get_endpoint(brp, programName = programName, 
                                  abbreviation = abbreviation, commonCropName = commonCropName, 
                                  pageSize = pageSize, page = page)
  try({
    resp <- brapi:::brapiGET(url = callurl, con = con)
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    out <- brapi:::dat2tbl(res = cont, rclass = rclass)
    class(out) <- c(class(out), "ba_programs")
    brapi:::show_metadata(resp)
    return(out)
  })
}


ba_trials_new <- function (con = NULL, programDbId = "", locationDbId = "", active = NA, 
                           sortBy = "", sortOrder = "", pageSize = 1000, page = 0, 
                           rclass = c("tibble", "data.frame", "list", "json")) 
{
  brapi:::ba_check(con = con, verbose = FALSE)
  brapi:::check_character(programDbId, locationDbId, sortBy, sortOrder)
  stopifnot(is.logical(active))
  rclass <- match.arg(rclass)
  brp <- get_brapi_new(con) %>% paste0("trials")
  callurl <- brapi:::get_endpoint(brp, programDbId = programDbId, 
                                  locationDbId = locationDbId, active = active, sortBy = sortBy, 
                                  sortOrder = sortOrder, pageSize = pageSize, page = page)
  try({
    resp <- brapi:::brapiGET(url = callurl, con = con)
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    out <- NULL
    if (rclass %in% c("list", "json")) {
      out <- brapi:::dat2tbl(res = cont, rclass = rclass)
    }
    if (rclass %in% c("data.frame", "tibble")) {
      out <- brapi:::trl2tbl2(res = cont, rclass = rclass)
      # out <- try(brapi:::trl2tbl2(res = cont, rclass = rclass), silent = T)
      # if(class(out)=="try-error"){
      #   out <- brapi:::dat2tbl(res = cont, rclass = rclass)
      # } 
    }
    class(out) <- c(class(out), "ba_trials")
    brapi:::show_metadata(resp)
    return(out)
  })
}

ba_studies_table_new <- function (con = NULL, studyDbId = "", format = c("csv", "tsv", 
                                                                         "json"), rclass = c("tibble", "data.frame", "json")) 
{
  brapi:::ba_check(con = con, verbose = FALSE, brapi_calls = "studies/id/table")
  brapi:::check_req(studyDbId = studyDbId)
  brapi:::check_character(studyDbId)
  format <- match.arg(format)
  rclass <- match.arg(rclass)
  if (format == "csv" && rclass == "json") {
    stop("Please read the details section in the function documentation\nabout specifying the \"format\" and \"rclass\" arguments.")
  }
  if (format == "tsv" && rclass == "json") {
    stop("Please read the details section in the function documentation\nabout specifying the \"format\" and \"rclass\" arguments.")
  }
  brp <- get_brapi_new(con = con) %>% paste0("studies/", studyDbId, 
                                             "/table")
  format <- ifelse(format == "json", "", format)
  callurl <- brapi:::get_endpoint(pointbase = brp, format = format)
  try({
    resp <- brapi:::brapiGET(url = callurl, con = con)
    cont <- httr::content(x = resp, as = "text", encoding = "UTF-8")
    out <- NULL
    if (rclass == "json") {
      out <- dat2tbl(res = cont, rclass = rclass)
    }
    if (rclass %in% c("data.frame", "tibble")) {
      if (format == "") {
        resList <- jsonlite::fromJSON(txt = cont)$result
        out <- as.data.frame(x = resList$data, stringsAsFactors = FALSE)
        if ((length(resList$headerRow) + length(resList$observationVariableNames)) != 
            ncol(out)) {
          stop("Header row length does not coincide with column count. Contact database provider.")
        }
        colnames(out) <- c(resList$headerRow, resList$observationVariableNames)
      }
      if (format == "csv") {
        if (con$bms == TRUE) {
          out <- read.csv(textConnection(cont))
          colnames(out) <- gsub("\\.", "|", colnames(out))
        }
        else {
          url <- jsonlite::fromJSON(txt = cont)$metadata$datafiles[1]
          out <- readr::read_csv(file = url, progress = TRUE)
          out <- as.data.frame(x = out, stringsAsFactors = FALSE)
        }
      }
      if (format == "tsv") {
        if (con$bms == TRUE) {
          out <- read.delim(textConnection(cont))
          colnames(out) <- gsub("\\.", "|", colnames(out))
        }
        else {
          url <- jsonlite::fromJSON(txt = cont)$metadata$datafiles[1]
          out <- readr::read_tsv(file = url, progress = TRUE)
          out <- as.data.frame(x = out, stringsAsFactors = FALSE)
        }
      }
      if (rclass == "tibble") {
        out <- tibble::as.tibble(out)
      }
    }
  })
  class(out) <- c(class(out), "ba_studies_table")
  return(out)
}

