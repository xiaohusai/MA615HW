#Maps in Shiny

#Team members: Guangyan Yu, Yaotang Luo, Shiyu Zhang, Jinfei Xue


#data preparation
library(tidyverse)
library(readxl)
library(magrittr)
library(maps)

# Donations Dataset
dono <- read_csv("11-5 MASSCONTRIBUTIONS-csv.csv")
donor_Q08aa <- dono %>% group_by(city, state, party) %>% 
  summarize(total = sum(amount), num = n()) %>% arrange(desc(total))
dmap <- spread(donor_Q08aa, party, party)

dmap_stR <- dmap %>% filter(R==R)
dmap_stD <- dmap %>% filter(D==D)
dmap_stI <- dmap %>% filter(I==I)

############################################################# Republican donor map

dmapR <- dmap_stR %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))

st_abrev <- dmapR$state

dmapR %<>% rename(st_abrev=state)

# States Dataset
states <- map_data("state")
states %<>% select(long,lat,group,order,region) %>% rename(state=region)
st_name <- unique(states$state)

st <- read_csv("states.csv")
st %<>% rename(state=st_name)

datastates <- left_join(states, st, by="state")
states <- left_join(datastates, dmapR, by="st_abrev")
states$Donors <- as.character(states$Donors)
states %<>% select(state,Donations,Donors) %>%
  unique()

#draw map using leaflet
library(leaflet)
library(sp)
library(rgdal)
library(dplyr)
library(WDI)

## Download data from Natural Earth
url <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_1_states_provinces.zip"

tmp <- tempdir()

file <- basename(url)

download.file(url, file)

unzip(file, exdir = tmp)

## Read the data into R

state_spatial <- readOGR(dsn=tmp,
                         layer = "ne_50m_admin_1_states_provinces", 
                         encoding = "UTF-8")

#get the states name in spatial data
a<-tolower(state_spatial@data[["gn_name"]]) #because the state name in "states" are in lower format
state_spatial@data[["gn_name"]]<-a

data<-sp::merge(state_spatial,states,by.x="gn_name",by.y="state",sort=FALSE,duplicateGeoms = TRUE,all.x=FALSE)

labels <- sprintf(
  "<strong>%s</strong><br/>%s Donors<br/>%g Donations",
  data$gn_name, data$Donors, data$Donations
) %>% lapply(htmltools::HTML)


m <- leaflet(data) %>%
  addTiles()%>%
  setView(-96, 37.8, 4)

#bins <- c(0, 10000, 20000, 50000, 100000, 200000, Inf)
pal <- colorQuantile("YlOrRd", domain = states$Donations)
#pal(states$Donations)
m_R <- m %>% addPolygons(
  fillColor = ~pal(data$Donations),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")
) %>%
  addLegend(pal = pal, values = ~Donations, opacity = 0.7, title = "Quantile in the Range of Donations for Republicans",
            position = "bottomright")

################################################################ Democrats donor map

dmapD <- dmap_stD %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))

st_abrev <- dmapD$state

dmapD %<>% rename(st_abrev=state)

# States Dataset
states <- map_data("state")
states %<>% select(long,lat,group,order,region) %>% rename(state=region)
st_name <- unique(states$state)

st <- read_csv("states.csv")
st %<>% rename(state=st_name)

datastates <- left_join(states, st, by="state")
states <- left_join(datastates, dmapD, by="st_abrev")
states$Donors <- as.character(states$Donors)
states %<>% select(state,Donations,Donors) %>%
  unique()

## Read the data into R

state_spatial <- readOGR(dsn=tmp,
                         layer = "ne_50m_admin_1_states_provinces", 
                         encoding = "UTF-8")

#get the states name in spatial data
a<-tolower(state_spatial@data[["gn_name"]]) #because the state name in "states" are in lower format
state_spatial@data[["gn_name"]]<-a

data<-sp::merge(state_spatial,states,by.x="gn_name",by.y="state",sort=FALSE,duplicateGeoms = TRUE,all.x=FALSE)

labels <- sprintf(
  "<strong>%s</strong><br/>%s Donors<br/>%g Donations",
  data$gn_name, data$Donors, data$Donations
) %>% lapply(htmltools::HTML)


m <- leaflet(data) %>%
  addTiles()%>%
  setView(-96, 37.8, 4)

#bins <- c(0, 10000, 20000, 50000, 100000, 200000, Inf)
pal <- colorQuantile("YlOrRd", domain = states$Donations)

m_D <- m %>% addPolygons(
  fillColor = ~pal(data$Donations),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlight = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")
) %>%
  addLegend(pal = pal, values = ~Donations, opacity = 0.7, title = "Quantile in the Range of Donations for Republicans",
            position = "bottomright")



########################################################################################################
library(shiny)
library(leaflet)

ui <- navbarPage("Quantile in the Range of Donations for Republicans and Democrats",
                 bootstrapPage(
     absolutePanel(top = 80, right = 15, style="z-index:500;",
                   selectInput("party", "Party", choices = c("Republicans", "Democrats")
                   )),
     leafletOutput("mymap", height = "700")
))
   
server <- function(input, output) {
  output$mymap = renderLeaflet({
       if(input$party == "Republicans"){
         m_R
         }
       else{
        m_D
       }
  })
     }
   
shinyApp(ui=ui, server=server)   