## code to prepare `data` dataset goes here



Dar16C_hiP <- read.csv("data-raw/Dar16C_hiP.csv")

usethis::use_data(Dar16C_hiP, overwrite = TRUE)

load("data-raw/MODLIST.Rda")
mod.list <- mod.list

usethis::use_data(mod.list, overwrite = TRUE)
