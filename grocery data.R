install.packages("tidyverse")
library(lattice)
df<-read.csv("grc.csv",header = T) 
df
duplicated(df)
sum(duplicated(df))
df_without=unique(df)
df_without
sum(is.na(df_without))
attach(df_without)
boxplot(df_without[,2:4])
boxplot(df_without$count)
boxplot(df_without$age)
outlier=boxplot(df_without$count)$out
dff=df_without[-which(df_without$count %in% outlier),]
boxplot(dff$count)

glimpse(dff)
View(dff) 
names(dff)

install.packages("dplyr")
library(dplyr)
clusters <- as.double(readline("Input the number of clusters : "))
Groups_of_data<-dff %>% group_by(customer,age) %>% summarize(totalSpending_sum=sum(total),.groups='drop')
print(Groups_of_data)
customerInfo<-Groups_of_data[,c("customer","age","totalSpending_sum")]
Kmeans_clustering_groups<-kmeans(customerInfo[,c("age","totalSpending_sum")] , centers = clusters )
Kmeans_clustering_groups
customerInfo$computed_cluster<-Kmeans_clustering_groups$cluster
print(customerInfo)
View(customerInfo)

install.packages("arules")
library(arules)
transactions <- as(dff, "transactions")
min_support <- 0.1
min_confidence <- 0.5
rules <- apriori(transactions,parameter = list(support = min_support, confidence = min_confidence))
inspect(rules)
summary(rules)

library(shiny)
library(ggplot2)
library(shiny)
library(cluster)
library(shinydashboard)
ui <- fluidPage(
  titlePanel("Data Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset_path", "Choose CSV File"),
      actionButton("clean", "Clean"),
      actionButton("visualize", "Visualizations"),
      numericInput("clusters", "Number of Clusters (2-4):", min = 2, max = 4, value = 2),
      actionButton("cluster", "Clustering"),
      actionButton("mine_rules", "Association Rules"),
      numericInput("min_support", "Enter minimum support (between 0.001 and 1):", min = .001, max = 1, value = .001),
      numericInput("min_confidence", "Enter minimum confidence (between 0.001 and 1):", min = .001, max = 1, value = .001)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Cleaned Data", tableOutput("cleaned_data")),
        tabPanel("Visualizations", 
                 plotOutput("paymentType_plot"),
                 plotOutput("age_spending_plot"),
                 plotOutput("city_spending_plot"),
                 plotOutput("total_spending_boxplot")
        ),
        tabPanel("Clustering", 
                 tabPanel("Summary", verbatimTextOutput("cluster_summary_text")),
                 tabPanel("Cluster Summary", tableOutput("cluster_summary_table")),
                 plotOutput("cluster_summary_plot")
        ),
        tabPanel("Association Rules", verbatimTextOutput("association_rules"))
      )
    )
  )
)

server <- function(input, output) {
  data <- reactiveVal(NULL)  # Reactive conductor to store the data
  
  observeEvent(input$dataset_path, {
    req(input$dataset_path)
    data(read.csv(input$dataset_path$datapath))
  })
  
  output$cleaned_data <- renderTable({
    req(input$clean)
    data_cleaned <- na.omit(data())  # Remove rows with missing values
    data_cleaned <- distinct(data_cleaned)  # Remove duplicate rows
    data_cleaned <- data_cleaned[-which(data_cleaned$count %in% outlier),]
    data_cleaned
  })
  
  observeEvent(input$visualize, {
    req(data())
    comparison <- table(data()$paymentType)
    percent <- paste0(round(100 * comparison / sum(comparison)), "%")
    plot_data <- data.frame(
      paymentType = names(comparison),
      frequency = as.numeric(comparison)
    )
    output$paymentType_plot <- renderPlot({
      barplot(sort(table(paymentType),decreasing = T))
      
    })
    
    summarized_data <- data() %>%
      group_by(age) %>%
      summarize(total = sum(total))
    
    output$age_spending_plot <- renderPlot({
      dff %>%
        select(age , total) %>%
        group_by(age) %>%
        reframe(total_spending = sum(total))%>%
        ggplot(aes(x = reorder(age,total_spending), y = total_spending))+
        geom_col()+
        labs(title = "Compare each age and sum of total spending", x= "age", y="Total Spending" )
      
    })
    
    summarized <- data() %>%
      group_by(city) %>%
      summarize(total = sum(total)) %>%
      arrange(desc(total))
    
    output$city_spending_plot <- renderPlot({
      dff %>%
        select(city , total) %>%
        group_by(city) %>%
        summarise(total_spending = sum(total))%>%
        ggplot(aes(x= reorder(city , -total_spending ) , y = total_spending))+
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Total Spending by City", x = "City", y = "Total Spending")
      
    })
    
    output$total_spending_boxplot <- renderPlot({
      hist(dff$total, 
           breaks = 20, 
           col = "yellow", 
           border = "black",
           xlab = "Total Spending",
           ylab = "Frequency",
           main = "Distribution of Total Spending"
      )
    })
  })
  library(dplyr)
  library(magrittr)
  
  cluster_summary <- reactive({
    req(input$cluster)
    dff <- data()  # Retrieve the cleaned data
    
    # Group data by customer
    Groups_of_data <- dff %>% group_by(customer, age) %>% summarize(totalSpending_sum = sum(total), .groups = 'drop')
    
    # Cluster the data
    kmeans_model <- kmeans(Groups_of_data[, c("age", "totalSpending_sum")], centers = input$cluster)
    
    # Assign clusters to each customer
    Groups_of_data$computed_cluster <- kmeans_model$cluster
    
    return(Groups_of_data)
  })
  
  output$cluster_summary_table <- renderTable({
    req(input$cluster)
    cluster_summary()
  })
  
  output$association_rules <- renderPrint({
    req(input$mine_rules)
    req(data())
    
    library(arules)
    
    # Extract items as a list for each transaction
    transactions <- strsplit(as.character(data()$items), ",")
    
    # Mine association rules
    rules <- apriori(transactions, parameter = list(support = input$min_support, confidence = input$min_confidence),minlen=2)
    
    # Print the rules
    inspect(rules)
  })
}
shinyApp(ui = ui, server=server)