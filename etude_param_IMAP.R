library(shiny)
library(ggplot2)
library(lubridate)

ui <- fluidPage(
  titlePanel("Modélisation : IMAP"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Filtres"),
      selectInput("groupe", "Groupe de disciplines :",
                  choices = c("Tous", unique(df_final$Group_Discipline))),
      selectInput("discipline", "Discipline :", choices = NULL),
      selectInput("sexe", "Sexe :",
                  choices = c("Tous", "men", "women")),
      
      hr(),
      h4("Paramètres IMAP"),
      sliderInput("alpha0", "α₀ (taux initial de croissance) :", min = 0, max = 1000, value = 50, step = 1),
      sliderInput("alphar", "αᵣ (vitesse de saturation) :", min = 0, max = 100, value = 20, step = 0.1),
      sliderInput("beta0", "β₀ (taux initial de déclin) :", min = 0, max = 10, value = 1, step = 0.01),
      sliderInput("betar", "βᵣ (vitesse de déclin) :", min = 0, max = 20, value = 2, step = 0.01),
      sliderInput("Ninf", "N∞ (population cellulaire asymptotique) :", min = 0, max = 100, value = 10, step = 0.1),
      sliderInput("td", "tᵈ (âge de décès estimé) :", min = 50, max = 200, value = 110, step = 1),
      
      hr(),
      sliderInput("t_max", "Horizon temporel (t) :", min = 1, max = 100, value = 50)
    ),
    
    mainPanel(
      plotOutput("curvePlot"),
      wellPanel(uiOutput("formula"))
    )
  )
)

server <- function(input, output, session) {
  
  # Mise à jour de la liste des disciplines selon le groupe sélectionné
  observe({
    if (input$groupe == "Tous") {
      dispos <- unique(df_final$discipline)
    } else {
      dispos <- unique(df_final$discipline[df_final$Group_Discipline == input$groupe])
    }
    updateSelectInput(session, "discipline", choices = dispos)
  })
  
  # Données réelles filtrées
  df_reelles <- reactive({
    df <- df_final
    
    if (input$groupe != "Tous")
      df <- df[df$Group_Discipline == input$groupe, ]
    
    if (!is.null(input$discipline) && input$discipline != "")
      df <- df[df$discipline == input$discipline, ]
    
    if (input$sexe != "Tous")
      df <- df[df$Sex == input$sexe, ]
    
    df$Age <- as.numeric(df$season) - as.numeric(year(parse_date_time(df$DOB, orders = c("dmy", "ymd", "mdy"))))
    
    # Meilleure performance par âge
    df$mark_abs <- abs(df$mark_numeric)
    aggregate(mark_abs ~ Age, data = df, FUN = max)
  })
  
  # Courbe IMAP
  data_reactive <- reactive({
    t <- seq(0, input$t_max, length.out = 500)
    y <- input$beta0 * input$Ninf *
      exp((input$alpha0 / input$alphar) * (1 - exp(-input$alphar * t))) *
      (1 - exp(input$betar * (t - input$td)))
    y <- pmax(y, 0)
    data.frame(t = t, y = y)
  })
  
  output$curvePlot <- renderPlot({
    df_mod <- data_reactive()
    df_obs <- df_reelles()
    
    ggplot() +
      geom_point(data = df_obs, aes(x = Age, y = mark_abs),
                 color = "royalblue", size = 2, alpha = 0.8) +
      geom_line(data = df_mod, aes(x = t, y = y),
                color = "firebrick", size = 1.2) +
      labs(title = "Superposition Modèle IMAP vs Données réelles",
           subtitle = "Points : Données réelles | Ligne : Modèle IMAP",
           x = "Âge (t)", y = "Performance") +
      theme_minimal() +
      theme(title = element_text(size = 14))
  })
  
  output$formula <- renderUI({
    withMathJax(
      helpText(paste0(
        "$$P(t) = ", input$beta0, " \\cdot ", input$Ninf,
        " \\cdot e^{\\frac{", input$alpha0, "}{", input$alphar,
        "}(1 - e^{-", input$alphar, "t})} \\cdot (1 - e^{", input$betar,
        "(t - ", input$td, ")})$$"
      ))
    )
  })
}

shinyApp(ui, server)
