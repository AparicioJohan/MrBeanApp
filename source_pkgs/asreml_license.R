library(asreml)
old.readline <- readline
my.readline <- function(prompt="") {"EBBG-CJAE-xxxx-xxxx"}
assignInNamespace("readline", my.readline, ns="base")
asreml.license.activate()
assignInNamespace("readline", old.readline, ns="base")