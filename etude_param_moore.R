library(shiny)
library(ggplot2)

df_reelles <- data.frame(t = df_women_Discus_Throw$Age, y = df_women_Discus_Throw$meilleure_perf)

ui <- fluidPage(
  titlePanel("Modélisation : Somme d'exponentielles"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Premier composant"),
      sliderInput("a", "Amplitude a :", min = 0, max = 150, value = 50, step = 0.5),
      sliderInput("b", "Taux b :", min = 0, max = 10, value = 0.5, step = 0.01),
      
      hr(),
      
      h4("Second composant"),
      sliderInput("c", "Amplitude c :", min = 0, max = 80, value = 30, step = 0.1),
      sliderInput("d", "Taux d :", min = 0, max = 1, value = 0.2, step = 0.01), # Corrigé à 1 pour éviter explosion exponentielle
      
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
  
  # Calcul des données du modèle (la courbe lisse)
  data_reactive <- reactive({
    t <- seq(0, input$t_max, length.out = 500)
    # Correction du signe dans la formule pour correspondre à l'affichage mathématique
    y <- input$a * (1 - exp(-input$b * t)) + input$c * (1 - exp(input$d * t))
    
    data.frame(t = t, y = y)
  })
  
  # Rendu du graphique
  output$curvePlot <- renderPlot({
    df_modele <- data_reactive()
    
    ggplot() +
      # 1. Ajout des données réelles (les points bleus de votre code précédent)
      geom_point(data = df_reelles, aes(x = t, y = y), 
                 color = "royalblue", size = 2, alpha = 0.8) +
      
      # 2. Ajout de la courbe du modèle (la ligne rouge)
      geom_line(data = df_modele, aes(x = t, y = y), 
                color = "firebrick", size = 1.2) +
      
      # Configuration des axes et titres
      scale_y_continuous(limits = c(0, input$a + input$c + 15)) +
      labs(title = "Superposition Modèle estimé vs Données réelles",
           subtitle = "Points : Données réelles | Ligne : Modèle calculé",
           x = "Temps (t)",
           y = "Valeur y") +
      theme_minimal() +
      theme(title = element_text(size = 14))
  })
  
  # Affichage de la formule dynamique
  output$formula <- renderUI({
    withMathJax(
      helpText(paste0("$$f(t) = ", input$a, "(1 - e^{-", input$b, "t}) + ", 
                      input$c, "(1 - e^{", input$d, "t})$$"))
    )
  })
}

shinyApp(ui, server)
