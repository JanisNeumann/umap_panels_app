#### Dependencies ####

library(shiny)
library(umap)
library(ggplot2)
library(dplyr)



#### UI ####
ui <- fluidPage(

  # Application title
  titlePanel("UMAP Plot"),
  sidebarLayout(
    sidebarPanel(
      # Define inputs as two .csv files
      fileInput("umap_data", "Choose CSV file containing numeric data",
        accept = c(".csv")
      ),
      fileInput("metadata", "Choose CSV file containing metadata",
        accept = c(".csv")
      ),
      selectInput("color_var", "Select color variable",
        choices = NULL, selected = NULL
      )
    ),

    # Define UMAP plot as output
    mainPanel(
      plotOutput("umap_plot", height = 1000, width = 1100)
    )
  )
)



#### Server ####
server <- function(input, output, session) {
  # Reactive expression that reads the files
  # TODO: Checkbox for header in UI
  umap_data <- reactive({
    req(input$umap_data)
    read.csv(input$umap_data$datapath, header = TRUE, sep = ",") %>%
      select(where(is.numeric))
  })

  metadata <- reactive({
    req(input$metadata)
    read.csv(input$metadata$datapath, header = TRUE, sep = ",", stringsAsFactors = TRUE)
  })

  observe({
    req(input$metadata)
    updateSelectInput(session, "color_var",
      choices = names(metadata()),
      selected = names(metadata())[1]
    )
  })

  # Reactive expression for UMAP & plot
  output$umap_plot <- renderPlot({
    req(umap_data())

    set.seed(16)
    umap_object <- umap(umap_data())

    umap_plot_df <- data.frame(
      U1 = umap_object$layout[, 1],
      U2 = umap_object$layout[, 2]
    )

    umap_plot <- ggplot(umap_plot_df, aes(x = U1, y = U2, color = metadata()[, input$color_var])) +
      geom_point(size = 2) +
      labs(x = "", y = "") +
      theme_bw() +
      theme(
        axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        strip.text.x = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)
      ) +
      theme(legend.position = "right")

    umap_plot
  })
}



#### Run the app ####

shinyApp(ui, server)
