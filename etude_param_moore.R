library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Modélisation : Somme d'exponentielles"),
  
  sidebarLayout(
    sidebarPanel(
      # Paramètres pour la première partie : a(1 - exp(-bt))
      h4("Premier composant"),
      sliderInput("a", "Amplitude a :", min = 0, max = 1000, value = 50, step = 0.5),
      sliderInput("b", "Taux b :", min = 0, max = 20, value = 0.5, step = 0.01),
      
      hr(),
      
      # Paramètres pour la deuxième partie : c(1 - exp(-dt))
      h4("Second composant"),
      sliderInput("c", "Amplitude c :", min = 0, max = 1000, value = 30, step = 0.5),
      sliderInput("d", "Taux d :", min = 0, max = 20, value = 0.2, step = 0.01),
      
      hr(),
      
      sliderInput("t_max", "Horizon temporel (t) :", min = 1, max = 100, value = 20)
    ),
    
    mainPanel(
      plotOutput("curvePlot"),
      wellPanel(
        uiOutput("formula")
      )
    )
  )
)

server <- function(input, output) {
  
  # Calcul des données
  data_reactive <- reactive({
    t <- seq(0, input$t_max, length.out = 500)
    # La fonction : a(1-exp(-bt)) + c(1-exp(-dt))
    y <- input$a * (1 - exp(-input$b * t)) + input$c * (1 - exp(input$d * t))
    
    data.frame(t = t, y = y)
  })
  
  # Rendu du graphique
  output$curvePlot <- renderPlot({
    df <- data_reactive()
    
    ggplot(df, aes(x = t, y = y)) +
      geom_line(color = "#2c3e50", size = 1.2) +
      scale_y_continuous(limits = c(0, input$a + input$c + 10)) +
      labs(title = "Évolution de la fonction en temps réel",
           x = "Temps (t)",
           y = "Valeur f(t)") +
      theme_minimal() +
      theme(title = element_text(size = 14))
  })
  
  # Affichage de la formule dynamique
  output$formula <- renderUI({
    withMathJax(
      helpText(paste0("$$f(t) = ", input$a, "(1 - e^{-", input$b, "t}) + ", 
                      input$c, "(1 - e^{-", input$d, "t})$$"))
    )
  })
}

shinyApp(ui, server)
