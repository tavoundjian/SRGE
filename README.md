# SRGE
SRGE is a Shiny web application that can be used to visualize the syphilis reactor grid and evaluate the impact of user-defined administrative closure algorithms in real time.

## Web Application
A web version of the application is available at https://tavoun.shinyapps.io/SRGE. Please email tavoun@uw.edu if you would like technical assistance to set up a web instance of SRGE.

## Local Installation

### Requirements
You must have [**R**](https://cran.r-project.org/mirrors.html) (3.3.0 or later) and [**R Studio**](https://www.rstudio.com/products/rstudio/download/) (0.99.896 or later) installed in order to run SRGE locally. 

### Install Required Packages
Once you've installed R and R Studio, run the following code in R Studio to install all required packages:

```
list.of.packages <- c("shiny", "leaflet", "shinyjs", "dplyr", "tidyr", "colorspace", "reshape2", "plotrix", "ggplot2", "devtools")
install.packages(list.of.packages)

devtools::install_github('rstudio/DT')
```

### Download Files from Github Repository
To run SRGE locally, download all of the files in this repository to the same local folder. 

### Run App
In R Studio, use the following command to run the application:

```
runApp("C:/Path/to/SRGE/Download/Folder/")
```
