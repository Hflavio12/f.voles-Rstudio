library(shiny)

# Shiny app finale per la visualizzazione delle varie distanze ------------
# Definisco la funzione ui
ui <- fluidPage(
  # Imposto il titolo della pagina
  titlePanel("Grafico dei dati del dataset f.voles in base al metodo di distanza"),
  # Imposto il layout della pagina con una sidebar e un main panel
  sidebarLayout(
    # Imposto la sidebar con un selettore per scegliere il metodo di distanza
    sidebarPanel(
      selectInput("metodo", "Scegli il metodo di distanza:", choices = c("Mahalanobis", "Euclide", "Manhattan", "Minkowski con p = 3", "Minkowski con p = 100"))
    ),
    # Imposto il main panel con un grafico che mostra i dati con il metodo di distanza scelto
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Definisco la funzione server
server <- function(input, output) {
  # Imposto il grafico come output della funzione server
  output$plot <- renderPlot({
    # Carico i dati del dataset f.voles
    data(f.voles)
    # Carico la libreria StatMatch
    library(StatMatch)
    # Creo una variabile per i colori dei punti
    colors <- colorRampPalette(c("blue", "red"))(100)
    # Creo una variabile per il titolo del grafico
    titolo <- paste("Grafico della matrice derivata dal metodo di", input$metodo)
    # Creo una variabile per la matrice di distanza in base al metodo scelto dall'utente
    if (input$metodo == "Mahalanobis") {
      d <- mahalanobis.dist (data.x = californicus, data.y = ochrogaster)
      matrice <- as.matrix(d)
    } else if (input$metodo == "Euclide") {
      e <- rdist (californicus, ochrogaster)
      matrice <- as.matrix(e)
    } else if (input$metodo == "Manhattan") {
      m <- dist(rbind(californicus, ochrogaster), method = "manhattan")
      matrice <- as.matrix(m)
    } else if (input$metodo == "Minkowski con p = 3") {
      o <- dist(rbind(californicus, ochrogaster), method = "Minkowski", p = 3)
      matrice <- as.matrix(o)
    } else if (input$metodo == "Minkowski con p = 100") {
      r <- dist(rbind(californicus, ochrogaster), method = "Minkowski", p = 100)
      matrice <- as.matrix(r)
    }
    # Creo il grafico con la matrice di distanza scelta dall'utente
    plot(matrice, pch = 19, col = colors[cut(matrice, breaks = 100)], main = titolo, xlab = "Indice di californicus", ylab = "Indice di ochrogaster")
  })
}

# Creo la shiny app con le due funzioni ui e server
shinyApp(ui = ui, server = server)
