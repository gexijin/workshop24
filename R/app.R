library(shiny)
library(ggplot2)
library(dplyr)
library(broom)

# Load the data
data <- read.csv("heartatk4R.csv")

# Remove rows with missing values
data <- na.omit(data)

# Define unique diagnosis codes, died statuses, and outcome statuses
unique_diagnosis <- unique(data$diagnosis)
unique_died <- unique(data$died)
unique_outcome <- unique(data$outcome)

# Define the UI
ui <- fluidPage(
  titlePanel("Heart Attack Patients Analysis"),

  sidebarLayout(
    sidebarPanel(
      radioButtons("gender",
                   "Select Gender:",
                   choices = c("Female" = "F", "Male" = "M"),
                   selected = "M"
      ),
      selectInput("diagnosis",
                  "Select Diagnosis Code:",
                  choices = unique_diagnosis,
                  selected = unique_diagnosis[1]
      ),
      selectInput("died",
                  "Select Died Status:",
                  choices = unique_died,
                  selected = unique_died[1]
      ),
      selectInput("outcome",
                  "Select Outcome Status:",
                  choices = unique_outcome,
                  selected = unique_outcome[1]
      )
    ),

    mainPanel(
      plotOutput("charges_plot"),
      plotOutput("length_plot"),
      tableOutput("summary_table")
    )
  )
)

# Define the server
server <- function(input, output) {

  filtered_data <- reactive({
    data[data$sex == input$gender &
           data$diagnosis == input$diagnosis &
           data$died == input$died &
           data$outcome == input$outcome, ]
  })

  output$charges_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = charges, fill = sex)) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Density Plot of Charges for", ifelse(input$gender == "F", "Female", "Male"),
                         "with Diagnosis Code", input$diagnosis,
                         "and Died Status", input$died,
                         "and Outcome Status", input$outcome),
           x = "Charges",
           y = "Density")
  })

  output$length_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = length, fill = sex)) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Density Plot of Length of Stay for", ifelse(input$gender == "F", "Female", "Male"),
                         "with Diagnosis Code", input$diagnosis,
                         "and Died Status", input$died,
                         "and Outcome Status", input$outcome),
           x = "Length of Stay",
           y = "Density")
  })

  output$summary_table <- renderTable({
    summary_data <- filtered_data() %>%
      group_by(sex) %>%
      summarize(
        Mean_Charges = mean(charges),
        Median_Charges = median(charges),
        Lower_CI = tidy(confint(lm(charges ~ 1)))$conf.low,
        Upper_CI = tidy(confint(lm(charges ~ 1)))$conf.high
      )

    summary_data
  })

}

# Create the Shiny app
shinyApp(ui = ui, server = server)
