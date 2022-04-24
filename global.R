library(shiny)
library(shinymaterial)
library(tidyverse)
library(stringr)
library(echarts4r)
library(dplyr)
library(lubridate)
load("data/nom_pays.RData")
load("data/covid_clean.RData")
load("data/vacs.RData")

vac_selection <- unique(df_vac$vaccin_type)[-5]


git_refs <- function(){
  shiny::tagList(br(), br(), br(),
    tags$a(target = "_blank",
           href = "https://linkedin.com/in/cyprien-cambus",
           HTML('<h3>Linkedin<i class="material-icons">open_in_new</i></h3>')),
    br(), br(), 
    tags$a(target = "_blank",
           href = "https://github.com/CyprienCambus",
           HTML('<h3>GitHub<i class="material-icons">open_in_new</i></h3>')),
    br(),
    br(),
    br(),
    tags$a(target = "_blank",
           href = "https://www.data.gouv.fr/fr/pages/donnees-coronavirus/",
           HTML('<h3>Data sources<i class="material-icons">open_in_new</i></h3>'))
  )
}
