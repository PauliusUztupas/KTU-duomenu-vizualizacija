library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Odontologinės praktikos veikla - 862300",
    titleWidth = 450,
    tags$li(class = "dropdown",
            tags$style(HTML(".main-header .logo {background-color: lightgreen;} .skin-green .main-header .navbar {background-color: lightgreen;}")))
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Vidutinė alga", tabName = "line_graph"),
      menuItem("Algos ir darbuotojų histogramos", tabName = "histograms"),
      menuItem("Kiti duomenys", tabName = "other_data")
    ),
    selectizeInput(inputId = "imones_pavadinimas", label = "Pavadinimas", choices = NULL, selected = NULL, multiple = TRUE),
    textOutput("no_selection_text")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "line_graph",
              plotOutput("plot", height = "600px")),
      tabItem(tabName = "histograms",
              plotOutput("histogram_avgWage", height = "300px"),
              plotOutput("histogram_numInsured", height = "300px")),
      tabItem(tabName = "other_data",
              tableOutput("table"))
    )
  )
)

server <- function(input, output, session) {
  data <- read_csv("https://github.com/PauliusUztupas/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  
  sorted_data <- data %>%
    filter(ecoActCode == 862300)
  
  updateSelectizeInput(session, "imones_pavadinimas",
                       choices = sorted_data$name,
                       server = TRUE)
  
  output$no_selection_text <- renderText({
    if (is.null(input$imones_pavadinimas)) {
      "Pasirinkite kompaniją"
    } else {
      ""
    }
  })
  
  output$plot <- renderPlot(
    sorted_data %>%
      filter(name %in% input$imones_pavadinimas) %>%
      ggplot(aes(x = month, y = avgWage, color = name)) +
      theme_minimal() +
      geom_line(size = 1) +
      labs(x = "Mėnesis", y = "Vidutinė alga") +
      scale_y_continuous(labels = dollar) +
      scale_color_discrete(name = "Kompanija")
  )
  
  output$histogram_avgWage <- renderPlot({
    avgWage_data <- sorted_data %>%
      group_by(name) %>%
      summarize(avgWage = mean(avgWage))
    
    ggplot(avgWage_data, aes(x=avgWage)) +
      geom_histogram(binwidth=100) +
      geom_vline(data=avgWage_data %>% filter(name %in% input$imones_pavadinimas), aes(xintercept=avgWage, color=name), size=2) +
      labs(x="Vidutinė alga", y="Kiekis") +
      scale_color_discrete(name = "Kompanija")
  })
  
  output$histogram_numInsured <- renderPlot({
    numInsured_data <- sorted_data %>%
      group_by(name) %>%
      summarize(numInsured = mean(numInsured))
    
    ggplot(numInsured_data, aes(x=numInsured)) +
      geom_histogram(binwidth=2) +
      geom_vline(data=numInsured_data %>% filter(name %in% input$imones_pavadinimas), aes(xintercept=numInsured, color=name), size=2) +
      labs(x="Apraustų darbuotojų kiekis", y="Kiekis") +
      scale_color_discrete(name = "Kompanija")
  })
  
  output$table <- renderTable({
    sorted_data %>%
      filter(name %in% input$imones_pavadinimas)
  })
}

shinyApp(ui = ui, server = server)