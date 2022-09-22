#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(rvest)
library(shiny)
library(tidyr)
require(dplyr)
library(shinythemes)
library(reactable)
library(fmsb)
library(tibble)
library(XML)

source("build_functions.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("fmPal.css"),
  navbarPage(
    "FM Pal v1.0",
    collapsible = TRUE,
    theme = shinytheme("darkly"),
    tabPanel("Get Started",
             fluidPage(tabPanel(
               "Get Started",
               sidebarLayout(
                 sidebarPanel(
                   fileInput(
                     "squad_file",
                     "Upload your Squad HTML File",
                     multiple = TRUE,
                     accept = ".html"
                   ),
                   tableOutput("files"),
                   htmlOutput("teamName"),
                   downloadButton("downloadSampleRoster", "Sample Roster File"),
                   #downloadButton("downloadData", "Sample Roster File"),
                   
                   width = 3
                 ),
                 mainPanel(
                   textOutput("instructions"),
                   hr(style = "border-top: 2px solid #FFFFFF;"),
                   htmlOutput("description")
                 )
               )
             ))),
    tabPanel("Roster",
             fluidPage(
               tabsetPanel(
                 tabPanel(
                   "Full Roster View",
                   tags$br(),
                   sidebarLayout(
                     sidebarPanel(
                       radioButtons(
                         "select",
                         h3("Player Visibility"),
                         choices = list(
                           "All" = "All",
                           "Current Squad" = "Squad",
                           "Loaned" = "Loaned"
                         ),
                         selected = "All"
                       ),
                       selectInput(
                         "position",
                         h3("Select Position"),
                         choices = list(
                           "All" = "All",
                           "Goalkeepers" = "GK",
                           "Defenders" = "CD",
                           "Fullbacks/Wingbacks" = "FB",
                           "Defensive Midfielders" = "DM",
                           "Midfielders" = "M",
                           "Wingers" = "W",
                           "Attacking Midfielders" = "AM",
                           "Strikers" = "ST"
                         ),
                         selected = "All"
                       ),
                       width = 3
                     ),
                     mainPanel(reactableOutput("currentSquad"))
                   )
                 ),
                 tabPanel(
                   "Position Ratings",
                   tags$br(),
                   sidebarLayout(
                     sidebarPanel(
                       radioButtons(
                         "selectPosRating",
                         h3("Player Visibility"),
                         choices = list(
                           "All" = "All",
                           "Current Squad" = "Squad",
                           "Loaned" = "Loaned"
                         ),
                         selected = "All"
                       ),
                       selectInput(
                         "positionPosRating",
                         h3("Select Position"),
                         choices = list(
                           "All" = "All",
                           "Goalkeepers" = "GK",
                           "Defenders" = "CD",
                           "Fullbacks/Wingbacks" = "FB",
                           "Defensive Midfielders" = "DM",
                           "Midfielders" = "M",
                           "Wingers" = "W",
                           "Attacking Midfielders" = "AM",
                           "Strikers" = "ST"
                         ),
                         selected = "All"
                       ),
                       width = 3
                     ),
                     mainPanel(reactableOutput("PositionRatings"))
                   )
                 ),
                 tabPanel(
                   "Role Ratings",
                   tags$br(),
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(
                         "positionRoleRating",
                         h3("Select Position"),
                         choices = list(
                           "Goalkeepers" = "GK",
                           "Defenders" = "CD",
                           "Fullbacks/Wingbacks" = "FB",
                           "Defensive Midfielders" = "DM",
                           "Midfielders" = "M",
                           "Wingers" = "W",
                           "Attacking Midfielders" = "AM",
                           "Strikers" = "ST"
                         ),
                         selected = "GK"
                       ),
                       width = 3
                     ),
                     mainPanel(reactableOutput("RoleRatings"))
                   )
                 )
                 #,
                 # tabPanel(
                 #   "Player Comparison Tool",
                 #   tags$br(),
                 #   sidebarLayout(
                 #     sidebarPanel(
                 #       uiOutput(
                 #         outputId = "selectPlayerOne"
                 #       )
                 #     ),
                 #     mainPanel(
                 #       plotOutput('radarPlot')
                 #     )
                 #   )
                 # )
               )
             )),
    tabPanel("Staff",),
    tabPanel("Scouting",)
  ),
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  players = ""
  
  BuYlRd <-
    function(x)
      rgb(colorRamp(c("#F8696B", "#FFAE06", "#65BE7B"))(x), maxColorValue = 255)
  
  options(
    reactable.theme = reactableTheme(
      color = "#FFFFFF",
      backgroundColor = "#262626",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "#2b2b2b",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"),
      cellStyle = list(
        display = "flex",
        flexDirection = "column",
        justifyContent = "center"
      )
    )
  )
  
  make_matrix <- function(df, rownames = NULL) {
    my_matrix <-
      matrix(as.numeric(as.matrix(df)), ncol = ncol(as.matrix(df)))
    if (!is.null(rownames))
      rownames(my_matrix) = rownames
    if (!is.null(colnames(df)))
      colnames(my_matrix) = colnames(df)
    return(my_matrix)
  }
  
  output$instructions <- renderText({
    paste("Hey there!")
  })
  
  output$description <- renderUI({
    HTML(
      paste(
        '<div class="adjust-main-height">Welcome to FM Pal, an app that uses data analysis and formulas from various sources that will help you optimize your FM team! To get started, please follow these instructions:',
        '</div><div class="adjust-line-height">',
        '<b>1.</b> Download this custom FM22 view from the following <a href="https://www.dropbox.com/s/vdxycqj4b9zydch/FM%20Pal%20View.fmf?dl=0">link</a>',
        "<b>2.</b> Boot up whichever save you wish to view with FM Pal and go to your Squad tab",
        "<b>3.</b> From there go to the Views tab beside the PLAYERS title near the top of the window, look for a Custom tab and Import the view you just downloaded.",
        "<b>4.</b> Click on the checkmark next to the top row and hit CTRL + A, ensure that all players have been selected and proceed to hit CTRL + P to export the file into a Web Page format",
        "<b>5.</b> Now you can upload the HTML file and use FM Pal as it helps analyze and organize your team for you :)",
        "",
        "NOTE: If you don't have your own roster file and just want to check the project out, you can download the sample roster file on the sidebar!",
        '</div><br>',
        sep = "<br>"
      )
    )
  })
  
  output$teamName <- renderUI({
    req(input$squad_file)
    team_df <- tableCheck(input$squad_file)
    team <<- names(which.max(table(team_df$Club)))
    HTML(paste('<div class="center">', team, " is your club</div>"))
  })
  
  output$downloadSampleRoster <- downloadHandler(
    filename <- function() {
      paste("CurrentSquad.html", sep="")
    },
    
    content <- function(file) {
      file.copy("CurrentSquad.html", file)
    }#,
    #contentType = "text/html"
  )
  
  output$files <- renderTable(input$squad_file[, c(1:3)])
  
  output$currentSquad <- renderReactable({
    req(input$squad_file)
    team_df <- tableCheck(input$squad_file)
    if (input$select != "All")
    {
      if (input$select == "Squad")
      {
        team_df <- team_df[team_df$Club == team,]
      }
      else if (input$select == "Loaned") {
        team_df <- team_df[team_df$Club != team,]
      }
    }
    if (input$position != "All") {
      team_df <-
        switch(
          input$position,
          "GK" = team_df <-
            filter(team_df, GKBool == TRUE),
          "CD" = team_df <-
            filter(team_df, CDBool == TRUE),
          "FB" = team_df <-
            filter(team_df, FBBool == TRUE),
          "DM" = team_df <-
            filter(team_df, DMBool == TRUE),
          "M" = team_df <-
            filter(team_df, MBool == TRUE),
          "AM" = team_df <-
            filter(team_df, AMBool == TRUE),
          "W" = team_df <-
            filter(team_df, AMBool == TRUE),
          "ST" = team_df <- filter(team_df, STBool == TRUE)
        )
    }
    
    reactable(
      team_df[, c(3:6, 8, 14:16, 67:69, 85)],
      bordered = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center", ),
    )
  })
  
  output$PositionRatings <- renderReactable({
    req(input$squad_file)
    team_df <- tableCheck(input$squad_file)
    if (input$selectPosRating != "All")
    {
      if (input$selectPosRating == "Squad")
      {
        team_df <- filter(team_df, Club == team)
      }
      else if (input$selectPosRating == "Loaned") {
        team_df <- filter(team_df, Club != team)
      }
    }
    if (input$positionPosRating != "All") {
      team_df <-
        switch(
          input$positionPosRating,
          "GK" = team_df <-
            filter(team_df, GKBool == TRUE),
          "CD" = team_df <-
            filter(team_df, CDBool == TRUE),
          "FB" = team_df <-
            filter(team_df, FBBool == TRUE),
          "DM" = team_df <-
            filter(team_df, DMBool == TRUE),
          "M" = team_df <-
            filter(team_df, MBool == TRUE),
          "AM" = team_df <-
            filter(team_df, AMBool == TRUE),
          "W" = team_df <-
            filter(team_df, AMBool == TRUE),
          "ST" = team_df <- filter(team_df, STBool == TRUE)
        )
    }
    reactable(
      team_df[, c(3:4, 8, 107, 99:106)],
      bordered = TRUE,
      filterable = TRUE,
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%",
      defaultColDef = colDef(align = "center", ),
    )
  })
  
  output$RoleRatings <- renderReactable({
    req(input$squad_file)
    team_df <- tableCheck(input$squad_file)
    role_df <- roleCheck(team_df, input$positionRoleRating)
    role_df <- role_df[, c(3, 107:ncol(role_df))]
    my_matrix <- make_matrix(select(role_df,-Name),
                             pull(role_df, Name))
    my_matrix <- my_matrix[,-1]
    reactable(
      my_matrix,
      defaultColDef = colDef(
        style = function(value)
        {
          if (!is.numeric(value))
            return()
          normalized <-
            (value - min(my_matrix)) / (max(my_matrix) - min(my_matrix))
          color <- BuYlRd(normalized)
          list(background = color)
        },
        format = colFormat(digits = 1),
        minWidth = 50,
        align = "center"
      ),
      bordered = TRUE,
      showPageSizeOptions = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      width = "112.9%"
    )
  })
  
  output$selectPlayerOne <- renderUI({
    req(input$squad_file)
    team_df <- tableCheck(input$squad_file)
    selectInput(
      inputId = "selectPlayer1",
      label = "Select player",
      choices =
        team_df$Name
    )
  })
  
  output$radarPlot <- renderPlot({
    req(input$squad_file)
    team_df <- tableCheck(input$squad_file)
    team_df <- team_df[, c(3, 20:66)]
    team_df <-
      filter(team_df, Name %in% input$selectPlayer1)[, 1:48]
    team_df <- rbind(rep(20, 47) , rep(0, 47) , team_df)
    team_df <- add_column(team_df, Num = 1:3, .before = "Acc")
    print(team_df)
    my_matrix <- make_matrix(select(team_df,-Num),
                             pull(team_df, Num))
    radarchart(as.data.frame(my_matrix))
  })
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
