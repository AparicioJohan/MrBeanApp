## code to prepare `data` dataset goes here

#' @title  Dar16C_hiP is an dataset that
#' @usage data(Dar16C_hiP)
#' @format A dataframe 4,032 rows and 8 variables:
#' \describe{
#'   \item{dataset}{stock ticker symbol}
#'   \item{unique_identifier}{trade date}
#'   \item{plot}{stock ticker symbol}
#'   \item{line}{trade date}
#'   \item{rep}{stock price at the open of trading, in USD}
#'   \item{treatment}{stock price at the highest point during trading, in USD}
#'   \item{block}{stock price at the lowest point during trading, in USD}
#'   \item{row}{stock price at the close of trading, in USD}
#'   \item{col}{number of shares traded}
#'   \item{DF}{stock price at the close of trading adjusted for stock splits, in USD}
#'   \item{DPM}{stock ticker symbol}
#'   \item{PLHA}{trade date}
#'   \item{POM_c2}{stock ticker symbol}
#'   \item{SW100}{trade date}
#'   \item{TSW}{stock price at the open of trading, in USD}
#'   \item{YDHA}{stock price at the highest point during trading, in USD}
#'   \item{YDHPL}{stock price at the lowest point during trading, in USD}
#' }

Dar16C_hiP <- read.csv("data-raw/Dar16C_hiP.csv")

usethis::use_data(Dar16C_hiP, overwrite = TRUE)

load("data-raw/MODLIST.Rda")
#' @title  mod.list is an object that pass a dataset to the environment variables
mod.list <- mod.list

usethis::use_data(mod.list, overwrite = TRUE)
