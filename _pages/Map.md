---
title: "Building an interactive crime map with shiny in RStudio, an overly verbose tutorial"
permalink: /blog/
author: "Alex Marsella"
output: html_document
date: '2022-07-27'
---


```{r setup, include=FALSE}
knitr::opts_chunk$set(eval = FALSE)
```



To begin, it's important to note that this tutorial assumes you are working with spatial data. In other words, somewhere in your data are columns for latitude and longitude OR a column named something like "geometry" (which I will show you how to convert to separate lat and long). 

To cover all our bases, here are some libraries you should load. Note that these library() commands *must* stay in your code for when you finally deploy the app to shinyapps.io, or else your app will fail to start. There is no need to include installation commands in your code, as shinyapps.io will do that automatically. 

```{r}
library(shiny)
library(shinydashboard)
library(leaflet)
library(rsconnect)
library(dplyr)
library(sf)
```

First, I am going to do some data processing. I have a character variable for date in my data, but I am going to need it as an actual date for later. The `desc` parts are going to provide labels for our crimes on our map later. 

```{r}
main <- main %>%
  mutate(date = as.Date(dispatch_date_time)) %>% 
  mutate(crime = as.factor(text_general_code)) %>%
  mutate(desc = paste('<b>',crime,'</b>')) %>% # Crime type in bold letters
  mutate(desc = paste(sep = '<br/>', desc,dispatch_date_time,location_block)) 
#location_block is a character variable in my data that describes the block the crime occurred on. You can really put whatever you want in `here.

#my data has a geometry column. This is not going to be very useful later on. What I actually need are lat and lon
main <- main %>% extract(geometry, c('lat', 'lon'), '\\((.*), (.*)\\)', convert = TRUE) 
#this will convert the geometry column into two separate columns. Note that this does not preserve the geometry column
#if you already have separate lat and lon columns, you don't need this
saveRDS(data, "main.rds") #saves to your working directory

```
I recommend `saveRDS()`. `saveRDS()` is nice because you can keep the format of your file, whether it's a `SpatialpolygonsDataFrame`, a `data.frame`, an `sf`, etc. and it offers excellent compression. Base R is powerful!

The chunk above should *not* stay in your final product, it will just slow it down. Generally speaking, you want to avoid this kind of data processing in your actual app deployment. Ideally, you'd manage your data in a separate script (you always want to keep a record of data manipulation) and save it to your files. You *must* read it in in your app's script though, as below.

For actual deployment, you'll want something like this in your code:
```{r}
main <- readRDS("main.rds") 
tract <- readRDS("tract.rds") 
prm2 <- readRDS("prm2")
# in my case, I have a spatialpolygonsdataframe for tracts in my city, and another one for the Promise Zone. You do not need this, as leaflet will give you a map automatically. These will just provide an overlay on top of the leaflet map so people can see the tract borders.
```
Keep in mind that this entire app is self-contained. While you can load whatever you want into your global environment and simply run the app line by line, any deployment to shinyapps.io will require everything to be included. You'll be uploading your data at the same time, but keep in mind that shinyapps.io does not have access to the global environment on your PC's R; it has to do everything from scratch, like load your data. 

Now it's time to actually build our shiny app, which we can do all in one script. It is important to note that much of this code is self-referential. Think of it like doing backward induction. We will give the user options for input in our dashboard, which will then be read by our app in the server input at the bottom (which I will explain more later), which then returns some output.
```{r}
#Shiny will know what to do with this, so we can keep it simple. 
header <- dashboardHeader(title = p("Crime in Philadelphia"), titleWidth = 400) #here I am just giving it a title and specifying its width.

#This is the part that will allow our user to interact with our app. 
dashboard <- column(width =4,
#I recommend a column format instead of a box format. We need to be consistent, regardless. If we do box instead of column, things won't look as good.
                    box(width = NULL , title =tagList(shiny::icon("cog",class = 'fa-3x'), "Crime, time, date.") , #Defining the main title for the dashboard, and putting a cog icon on it that's 3x the normal size
                        solidHeader = T, collapsible = T, status = 'primary',
                        selectizeInput('crimeType','Crime Type', choices = unique(main$text_general_code), width = 320, #Allows user to pick the crime from a list. 
#The first argument is how it will be read by our app behind the scenes, the second argument is what the user sees. 
                                       selected = c('Robbery', 'Theft of Vehicle',"Theft from Vehicle",
                                                    "Breaking & Entering","Theft","Assault with Deadly Weapon",
                                                    "Sexual Assault","Homicide","Arson"),multiple = T),
                        dateRangeInput('dates', label = "Date Range",width = 380,
                                       start = '2010-01-01', end = '2010-01-02', #dates displayed on start
                                       min = "2010-01-01", max = "2019-12-31" #allowable range
                        ),
                        sliderInput('hour_of_day','Hour of the day', min = 0, max = 23,width = 320, 
# My dataset has the hour in it as well. This tells it to display a slider for the hour of the day.
                                   value = c(0,23), step = 1),

                        submitButton(text = "Submit",icon =icon('filter')) # Allows user to confirm changes.
                    )
)

```


Now we need to make a space for our map. 
```{r}
map <- column(width =8,box(width = NULL, solidHeader = TRUE,leafletOutput('crimeMap',height = 500))) 
#This leafletOutput will tell shiny to put something ('crimeMap') into this space we made. 


#This just tells shiny that the body of our app is made up of the dashboard and the map.
body <- dashboardBody(fluidRow(dashboard, map))

#This defines the entire user interface. It has a header, which we defined, and a body, which we defined. 
ui <- dashboardPage(header,sidebar = dashboardSidebar(disable = T), body)
#I disable the sidebar because I have nothing to put in it right now. 

```


Now, we define the interaction between the user and the app. 
NOTE: If you have more than even a few hundred observations for your map, you MUST use `clusterOptions = markerClusterOptions()` below. That way, the map doesn't try to render all the markers all the time, just the ones you're zoomed in on. 
```{r}
server <- function(input, output) {
  # Note that this is the input part. This is interacting with the dashboard we built above. 
  filteredData <- reactive({
    main %>%
      filter(text_general_code %in% input$crimeType) %>% 
      #User choice of `crimeType` as `text_general_code` from the dashboard gets read in here. 
      filter(incident_date >= input$dates[1] & incident_date <= input$dates[2]) %>%
      #These lines filter out depending on the date range and hour range.
      filter(hour_ >= input$hour_of_day[1] & hour_ <= input$hour_of_day[2])
  })
  # Now, the filteredData created above in this reactive environment as defined by the user's choice, is rendered by leaflet. 
  output$crimeMap <- renderLeaflet({
    leaflet(filteredData()) %>%
      addTiles() %>% #this tells leaflet to create a map, by not filling it in, we get the default
      addMarkers(~lat, ~lon, popup = ~desc,clusterOptions = markerClusterOptions()) %>% 
#this adds markers, as defined of course by the filtered data. When you click a marker, you get the `desc' defined earlier in our code. 
      addPolygons(data=prm2, weight = 2, fillColor = "red")  %>% 
# I add this because my research is on the Philadelphia Promise Zone, so I want it to be visible in this map.
      addPolygons(data=tract, weight = 2, fillColor = "green", popup=tract$NAME10) 
#I want to add polygons for tracts, since my paper's analysis is at the tract level. 
  })
}

# A quick way to run the shiny app locally is the following line of code. Leave this in no matter what
shinyApp(ui = ui, server = server)
```

If this all works locally and you can get a little window with your interactive map to pop up in R upon running that last line, you're ready to work on deployment to shinyapps.io

If you plan to deploy this to shinyapps.io, you'll need the following line. If you make an account, they will give you a token and a secret code.
```{r}
rsconnect::setAccountInfo(name='', token='', secret='')
```

This tutorial helps explain deployment to shinyapps.io: https://shiny.rstudio.com/articles/shinyapps.html
However, I included below some experience fixing problems I had in deploying my own app, which you can see at https://alexmarsella.shinyapps.io/test2

### Issues I initially had with deployment to shinyapps.io 
Deploying this locally was easy, but it took lots of troubleshooting for me to get this online.
For one, keep in mind that every library used must be loaded with library().

Again, recall that shinyapps.io does not have access to your working directory, nor does it have access to your global environment. The in-app publishing button provided by R allows you to upload your data as you publish the app, this essentially creates an online working directory for you. By keeping those readRDS commands in your script, you are allowing it to read from the working directory into your global environment. Many an error are caused (and many stackexchange questions to boot!) are caused by creators missing one of those two things. 


In shinyapps.io, you have a logs pane. If it fails, look at the log. Generally, anything that opens a webpage and says "exit status ___" is a failure to launch that can be read about in your logs.

There is another type of issue, which I ran into *after* finally troubleshooting my way through the logs, which is that I would get to a webpage that would slow down my firefox and the app itself would never quite load. This is because my `selectizeInput` on line 60 did not have a `unique` command on its choices. In other words, my dashboard wanted to read each of the 632,510 observations I have in my data, instead of the unique crime types, of which there are only 11. 

The `SpatialPolygonsDataFrames` that I used did not have the same coordinate reference system (CRS), which caused some issues in the final display of my map. To fix this, I used `st_as_sf(promise)` and then used `st_transform(prm1, st_crs(tract))` to make them compatible. The first command turned the `SpatialPolygonsDataFrame` to an `sf`, then transformed its coordinate system to the one used by `tract`. 
