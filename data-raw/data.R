## code to prepare `Dar16C_hiP` dataset goes here

#' @title  Dar16C_hiP is an dataset that
#' @name Dar16C_hiP
#' @format dataframe
#' @usage data(Dar16C_hiP)
Dar16C_hiP <- utils::read.csv("data-raw/Dar16C_hiP.csv")
usethis::use_data(Dar16C_hiP, overwrite = TRUE)

load("data-raw/MODLIST.Rda")
#' @title  mod.list is an object that pass a dataset to the environment variables
mod.list <- mod.list

usethis::use_data(mod.list, overwrite = TRUE)
