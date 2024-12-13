library(readxl)
library(ggplot2)
library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h2 {
        text-align: center;
      }
    "))
  ),
  titlePanel("Cumulative Paid Claims ($)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Attach Claims Data here", accept = c(".xlsx")),
      sliderInput("tailFactor", "Tail Factor", 
                  min = 1, 
                  max = 2, 
                  value = 1.1, 
                  step = 0.1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cumulative Paid Claims ($)", tableOutput("claims")),
        tabPanel("Plot", plotOutput("plot"))
      )
    )
  )
)

server <- function(input, output, session){
  #reads uploaded file
  imported_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath, range = "Sheet1!A1:C7")
  })
  
  claims_paid <- reactive({
    filepath_1 <- imported_data()
    num_loss_year <- length(unique(filepath_1$'Loss Year'))
    num_development_year <- max(filepath_1$'Development Year')
    loss_year <- sort(unique(filepath_1$'Loss Year'))
    development_year <- c(1:num_development_year)
    
    #store cumulative claims
    cum_claims_1 <- array(0, dim = c(length(loss_year), length(development_year)),
                          dimnames = list(loss_year, development_year))
    
    #fill matrix with data
    for(a in 1:(nrow(filepath_1))){
      loss_yr <- as.character(filepath_1[a, "Loss Year"])
      develop_yr <- as.character(filepath_1[a, "Development Year"])
      cum_claims_1[loss_yr, develop_yr] <- as.double(filepath_1[a, "Amount of Claims Paid"])
    }
    
    #cumulative sum calculation
    cumulative <- apply(cum_claims_1, 1, cumsum)
    cum_claims_2 <- t(cumulative)
    
    #development factor calculation
    b <- num_development_year
    development_factor <- numeric(b)
    for(k in 1:(b-1)){
      development_1 <- sum(cum_claims_2[1:(b-k), k+1])
      development_2 <- sum(cum_claims_2[1:(b-k), k])
      development_factor[k] <- development_1/development_2
    }
    development_factor[b] <- input$tailFactor
    
    #final cumulative matrix
    cum_claims_final <- cbind(cum_claims_2, "4" = rep(0, num_loss_year))
    
    #projecting future claims using development factors
    for(c in 1:b){
      cum_claims_final[(b-c+1):b, c+1] <- round(cum_claims_final[(b-c+1):b, c] * development_factor[c])
    }
    cum_claims_final
  })
  
  line_graph <- reactive({
    cum_claims_final <- claims_paid()
    filepath_2 <- imported_data()
    num_loss_year <- length(unique(filepath_2$'Loss Year'))
    num_development_year <- max(filepath_2$'Development Year')
    loss_year <- sort(unique(filepath_2$'Loss Year'))
    development_year <- c(1:num_development_year)
    
    #data prep
    LossYear <- rep(rownames(cum_claims_final), ncol(cum_claims_final))
    DevYear <- rep(1:(ncol(cum_claims_final)), each = nrow(cum_claims_final))
    PaidAmount <- round(as.vector(cum_claims_final))
    
    df_1 <- data.frame(LossYear, DevYear, PaidAmount)
    ggplot(df_1, aes(x = DevYear, y = PaidAmount, 
                     group = LossYear, color = LossYear)) +
      geom_line(size = 1) +
      geom_point(size = 2.5) +
      labs(title = "Cumulative Paid Claims ($)", 
           x = "Development Year",
           y = "Paid Claims ($)") +
      geom_text(aes(label = paste("$", PaidAmount)), size = 4.5, color = "black",
                vjust = -0.8) +
      scale_color_manual(values = c("pink", "purple", "blue")) + 
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 18, hjust = 0.7, color = "black"),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.position = "left") 
  })
  
  
  output$claims <- renderTable({
    claims_paid()
  }, rownames = TRUE, align = "c", digits = 0)
  
  output$plot <- renderPlot({
    line_graph()
  }, width = 900, height = 450)
}

shinyApp(ui, server)
