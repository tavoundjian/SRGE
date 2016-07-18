rm(list = ls())

library(dplyr)
library(tidyr)
library(shiny)
library(DT)
library(colorspace)
library(reshape2)
library(plotrix)
library(ggplot2)

setwd("C:/Users/Tigran/Dropbox/UW PhD/RAships/Y1 DPH/CBA/Reactor Grids/data")

read.table("stacked") -> df
read.table("fulltab") -> df.full

display.save.df <- data.frame()
df.addl <- data.frame()