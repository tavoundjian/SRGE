rm(list = ls())
##This should detect and install missing packages before loading them - hopefully!

#list.of.packages <- c("shiny","leaflet","shinyjs","DT","dplyr","tidyr", "colorspace", "reshape2", "plotrix", "ggplot2")

library(shiny)
library(leaflet)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(colorspace)
library(reshape2)
library(plotrix)
library(ggplot2)


#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)
#lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 


rev(heat_hcl(length(seq(.40,.95,.05))+1)) -> my_palette
assign('my_palette',my_palette, envir=.GlobalEnv)
brk.int <- NULL


# library(dplyr)
# library(tidyr)
# library(shiny)
# library(DT)
# library(colorspace)
# library(reshape2)
# library(plotrix)
# library(ggplot2)