library(shiny)
library(shinythemes)
library(queueing)
library(ggplot2)
library(plotly)
library(tidyr)

# Helper functions
generate_plot <- function(data, title, xlim, ylim) {
  p <- ggplot(data = data, aes(x = Counter, y = Ws)) +
    geom_line(color = "blue") +
    geom_point(color = "red") +
    labs(
      title = title,
      x = "Number of Counters",
      y = "Waiting Time (with service time)"
    ) +
    xlim(xlim) +
    ylim(ylim) +
    theme_minimal() +
    scale_x_continuous(limits = c(min(data$Counter), max(data$Counter))) +  # Adjust x-axis limits
    scale_y_continuous(limits = c(0, max(data$Ws) + 5))
  ggplotly(p)
}

calculate_Wq <- function(min_counter, lambda, mu) {
  if (lambda / (min_counter * mu) >= 1) {
    return(Inf)
  }
  i_mmcc <- NewInput.MMCC(lambda, mu, min_counter, 0)
  mmcc <- QueueingModel(i_mmcc)
  Wq <- Wq(mmcc)
  return(Wq)
}

find_mincounter <- function(lambda, mu, Wq_target) {
  min_counter <- 1
  while (calculate_Wq(min_counter, lambda, mu) > Wq_target) {
    min_counter <- min_counter + 1
  }
  return(min_counter)
}

run_mmcmodel <- function(min_counter, lambda, mu) {
  counters <- min_counter:(min_counter + 15)
  results <- data.frame(
    Counter = integer(),
    L = numeric(),
    Lq = numeric(),
    W = numeric(),
    Wq = numeric(),
    P0 = numeric(),
    Ws = numeric()
  )
  for (counter in counters) {
    input_mmc <- NewInput.MMC(lambda = lambda, mu = mu, c = counter)
    queue_mmc <- QueueingModel(input_mmc)
    L <- L(queue_mmc)
    Lq <- Lq(queue_mmc)
    W <- W(queue_mmc)
    Wq <- Wq(queue_mmc)
    P0 <- Pn(queue_mmc, 0)
    results <- rbind(results, data.frame(
      Counter = counter,
      L = L,
      Lq = Lq,
      W = W,
      Wq = Wq,
      P0 = P0,
      Ws = L / lambda * 60
    ))
  }
  return(results)
}
find_optimal_counters <- function(usqueue, nonusqueue) {
  # Initialize variables
  optimal_us_counters <- NA
  average_us_wait_time <- NA
  j <- NA
  
  # For loop to find the optimal number of US counters
  for (i in 1:(length(usqueue$Ws)-1)) {
    expected_new_us_waiting_time <- usqueue$Ws[i] * 0.2496
    if (expected_new_us_waiting_time <= usqueue$Ws[i+1]) {
      optimal_us_counters <- usqueue$Counter[i]
      average_us_wait_time <- usqueue$Ws[i]
      j <- i
      break
    } else {
      expected_new_us_waiting_time <- usqueue$Ws[i+1]
    }
  }
  
  # For loop to find the optimal number of non-US counters
  optimal_nonus_counters <- NA
  average_nonus_wait_time <- NA
  
  for (i in 1:(length(nonusqueue$Ws)-1)) {
    expected_new_nonus_waiting_time <- nonusqueue$Ws[i] * 0.2897
    if (expected_new_nonus_waiting_time <= nonusqueue$Ws[i+1]) {
      optimal_nonus_counters <- nonusqueue$Counter[i]
      average_nonus_wait_time <- nonusqueue$Ws[i]
      break
    } else {
      expected_new_nonus_waiting_time <- nonusqueue$Ws[i+1]
    }
  }
  
  # Adjust the number of US counters if needed
  if (!is.na(j)) {
    for (i in j:(length(usqueue$Ws)-1)) {
      if (average_us_wait_time >= average_nonus_wait_time) {
        optimal_us_counters <- usqueue$Counter[i+1]
        average_us_wait_time <- usqueue$Ws[i+1]
      } else {
        break
      }
    }
  }
  
  # Return the results as a list
  return(list(
    optimal_us_counters = optimal_us_counters,
    average_us_wait_time = average_us_wait_time,
    optimal_nonus_counters = optimal_nonus_counters,
    average_nonus_wait_time = average_nonus_wait_time
  ))
}



# Define UI

ui <- fluidPage(
  theme = shinytheme("united"),
  navbarPage(
    tags$div(
      tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;"),
      "Queue Optima"
    ),
    tabPanel("Home",
             titlePanel("Welcome to Queue Optima, where we conquer queues with ease!"),
             sidebarPanel(
               tags$h3("Input:"),
               numericInput("total_passengers", "Number of passengers arriving:", value = 0, min = 0, max = 7799),
               actionButton("go", "Start optimizer", icon("refresh"), style = "color: #fff; background-color: #f76336; border-color: #f76336"),
               p("Starting the optimizer would run these values through a M/M/k priority queue model."),
               tags$script(HTML("
             document.getElementById('total_passengers').addEventListener('keydown', function (e) {
             if (!((e.key >= '0' && e.key <= '9') || e.key === 'Backspace' || e.key === 'Delete' || e.key === 'ArrowLeft' || e.key === 'ArrowRight')) {
             e.preventDefault();
                }
              });
             ")) #prevents non number inputs
             ),
             mainPanel(
               h1("Best Results"),
               textOutput("info_text"),  # Added textOutput for dynamic info
               tabsetPanel(type = "tab",
                           tabPanel("Summary",
                                    h4("Number of Counters to open for U.S passengers:"),
                                    h6("This is how many counters you should open for U.S passengers! 😄✨ "),
                                    verbatimTextOutput("txtout_us_counters"),
                                    h4("Average waiting time for U.S passengers:"),
                                    h6("For the above number of counters, this is the total time US passengers spent waiting in queue and being served! 😊🎉 "),
                                    verbatimTextOutput("txtout_us_waiting"),
                                    h4("Number of Counters to open for non-U.S passengers:"),
                                    h6("This is how many counters you should open for non-U.S passengers! 😇✨ "),
                                    verbatimTextOutput("txtout_nonus_counters"),
                                    h4("Average waiting time for non-U.S passengers:"),
                                    h6("For the above number of counters, this is the total time spent waiting in queue and being served, but for non-US passengers! 🥰🚀"),
                                    verbatimTextOutput("txtout_nonus_waiting"),
                                    
                           ),
                           tabPanel("Unified Queue", plotlyOutput("unifiedqueue")),
                           tabPanel("Double Queue", plotlyOutput("usqueue"), plotlyOutput("nonusqueue")),
                           tabPanel("Priority Queue", plotlyOutput("priority"))
               )
             )
    ),
    tabPanel("About",
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>About us</h1> </center><br>"),
                      div(align = "center",
                          div(tags$img(src = "cabin.jpg",
                                       width = "500px", 
                                       height = "500px"))),
                      shiny::HTML("<br><center> <h4>We would like to introduce you to The Cabin Crew, owner of Queue Optima!
                                                Here is a little information about The Cabin Crew.</h4></center><br>")
               ),
               column(3)
             ),
             fluidRow(
               
               style = "height:50px;"),
             
             fluidRow(
               column(3),
               
               # Our Mission
               column(6,
                      div(class="panel panel-default", 
                          div(class="panel-body",  width = "600px",
                              align = "center",
                              div(
                                tags$img(src = "mission.jpg",
                                         width = "80px", 
                                         height = "80px")
                              ),
                              div(
                                tags$h3("OUR MISSION")
                              ),
                              div(
                                "Our mission is to create a seamless and satisfying experience for travelers by empowering immigration authorities with the tools they need to optimize operations and improve overall efficiency."
                              )
                          )
                      )
               ),column(3)),
             # Our Vision
             fluidRow(
               column(3),
               column(6,
                      div(class="panel panel-default",
                          div(class="panel-body", width = "600px", 
                              align = "center",
                              div(
                                tags$img(src = "vision.webp",
                                         width = "70px", 
                                         height = "50px")
                              ),
                              div(
                                tags$h3("OUR VISION")
                              ),
                              div(
                                "We envision a future where airports operate at peak efficiency. Resources will be wisely allocated to ensure minimal delays for travelers, resulting in smoother and more enjoyable journeys for everyone."
                              )
                          )
                      )
               ),column(3)),
             # Our Values
             fluidRow(
               column(3),
               column(6,
                      div(class="panel panel-default",
                          div(class="panel-body",  width = "600px", 
                              align = "center",
                              div(
                                tags$img(src = "values.jpg",
                                         width = "80px", 
                                         height = "80px")),
                              div(
                                tags$h3("OUR VALUES")
                              ),
                              div(
                                "EXCELLENCE. SAFETY. CUSTOMER-FOCUS. CARE. INTEGRITY. TEAMWORK."
                              ),
                              div("These are the core values that form the foundation of The Cabin Crew.") 
                              
                          )
                      )
               ),column(3))
    ),
    tabPanel("Help",
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><br><center> <h1>How it works</h1> </center><br>"),
                      shiny::HTML("<br><center><h3 style='color: #f76336;'>1) At the 'Home' page, input the number of passengers arriving at immigration in the side panel. Once done, click the 'Start optimizer' button.</h3></center><br>"),
                      div(align = "center", div(tags$img(src = "step1.png", width = "800px" , height = "300px"))),
                      shiny::HTML("<br><center><h5>After clicking the button, the software would estimate the number of U.S and non-U.S passengers, and run these values through a M/M/k priority queue model.</h4></center><br>"),
               )),
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><center><h3 style='color: #f76336;'>2) To get your best results, click on the 'Summary' tab in the main panel.</h3></center><br>"),
                      div(align = "center", div(tags$img(src = "step2.png", width = "800px" , height = "400px"))),
                      shiny::HTML("<br><center><h5>This would display the number of counters to be opened for U.S and non-U.S passengers respectively, alongside the respective average waiting times (including service time).</h4></center><br>"),
               )),
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><center><h3 style='color: #f76336;'>3) To get a graphical overview of a Unified Queue policy, click on the 'Unified Queue' tab in the main panel.</h3></center><br>"),
                      div(align = "center", div(tags$img(src = "step3.png", width = "800px" , height = "500px"))),
                      shiny::HTML("<br><center><h5> The y-axis of the plot represents the total average waiting time of each passenger (including service time). The x-axis of the plot represents the total number of counters opened.</h4></center><br>"),
               )),
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><center><h3 style='color: #f76336;'>4) To get a graphical overview of a Double Queue policy, click on the 'Double Queue' tab in the main panel.</h3></center><br>"),
                      div(align = "center", div(tags$img(src = "step4.png", width = "800px" , height = "600px"))),
                      shiny::HTML("<br><center><h5>The first graph (top) displayed represents the queue for U.S passengers, while the second graph (bottom) displayed represents the queue for non-U.S passengers.</h4></center><br>"),
                      shiny::HTML("<br><center><h5>For each graph, the y-axis of the plot represents the total average waiting time for each passenger type (including service time). The x-axis of the plot represents the total number of counters opened for each passenger type.</h5></center><br>"),
                      
               )),
             
             fluidRow(
               column(3),
               column(6,
                      shiny::HTML("<br><center><h3 style='color: #f76336;'>5) To get a graphical overview of a Priority Queue policy, click on the 'Priority Queue' tab in the main panel.</h3></center><br>"),
                      div(align = "center", div(tags$img(src = "step5.png", width = "800px" , height = "600px"))),
                      shiny::HTML("<br><center><h5>The graph displayed represents the unified queue that allows for queue-cutting by U.S passengers.</h4></center><br>"),
                      shiny::HTML("<br><center><h5>The y-axis of the plot represents the total average waiting time for each passenger type (including service time). The x-axis of the plot represents the total number of counters opened.</h5></center><br>"),
                      shiny::HTML("<br><center><h3 style = 'color: #f76336;'> Enjoy the process and let's conquer queues together! Happy Optimizing! 😉</h3></center><br>"),
               )))
    
    
    
    
    
    
    
  )) #fluid page
# Define server function
server <- function(input, output) {
  observeEvent(input$go, {
    if (input$total_passengers > 7799) {
      showModal(modalDialog(
        title = "Input Error",
        "The number of passengers cannot exceed 7799. Please enter a valid number.🙇",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    } else if(input$total_passengers < 13){
      showModal(modalDialog(
        title = "Input Error",
        "Demand Rate too small. Please enter a larger arrival rate.🙇",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }
    
    else{
      output$info_text <- renderText({
        paste(
          "You have keyed in a total of", total_passengers, "passengers! 👭",
          "From historical data, we assume that there are", us_passengers, "US citizens 🍔and",
          non_us_passengers, "non-US citizens 🌍 in the queue."
        )
      })
    }
    
    mu <- 60
    Wq_target <- 1  # Define a target wait time (Wq) in minutes
    total_passengers <- input$total_passengers
    us_passengers <- round(23082 / 591613 * total_passengers)
    non_us_passengers <- total_passengers - us_passengers
    
    # Unified queue model
    min_counter_unified <- find_mincounter(total_passengers, mu, Wq_target)
    unifiedqueue <- run_mmcmodel(min_counter_unified, total_passengers, mu)
    
    # For US queue
    min_counter_us <- find_mincounter(us_passengers, mu, Wq_target)
    usqueue <- run_mmcmodel(min_counter_us, us_passengers, mu)
    
    # For non-US queue
    min_counter_nonus <- find_mincounter(non_us_passengers, mu, Wq_target)
    nonusqueue <- run_mmcmodel(min_counter_nonus, non_us_passengers, mu)
    
    doublequeueresult <- find_optimal_counters(usqueue, nonusqueue)
    optimal_us_counters <- as.numeric(doublequeueresult[[1]])
    average_us_wait_time <- as.numeric(doublequeueresult[[2]])
    optimal_nonus_counters <- as.numeric(doublequeueresult[[3]])
    average_nonus_wait_time <- as.numeric(doublequeueresult[[4]])
    
    # Unified with priority classes########################################################################################
    lambda1 <- us_passengers / 60  # arrival rate for priority class 1
    lambda2 <- non_us_passengers / 60  # arrival rate for priority class 2
    mupriority <- mu / 60  # service rate
    
    # Erlang C formula for congestion coefficient
    erlang_c <- function(c, rho) {
      rho_c <- rho * c
      
      numerator <- (rho_c ^ c / factorial(c)) * (1 / (1 - rho))
      denominator <- sum(sapply(0:(c-1), function(k) (rho_c ^ k) / factorial(k))) + numerator
      return(numerator / denominator)
    }
    
    # Initialize vectors to store results
    results <- data.frame(c = numeric(), T1 = numeric(), T2 = numeric())
    
    # Loop for c from min_counter_unified to min_counter_unified+15
    for (c in min_counter_unified:(min_counter_unified + 15)) {
      total_lambda <- lambda1 + lambda2
      rho <- total_lambda / (c * mupriority)
      C_c_rho <- erlang_c(c, rho)
      
      W1 <- (C_c_rho / (c * (1 - (lambda1 / (c * mupriority))))) * (1 / mupriority)
      W2 <- (C_c_rho / c) * ((1 - (lambda1 / c)) / (1 - (total_lambda / c))) * (1 / mupriority)
      T1 <- W1 + (1 / mupriority)
      T2 <- W2 + (1 / mupriority)
      
      # Store results
      results <- rbind(results, data.frame(c = c, T1 = T1, T2 = T2))
    }
    
    # Gather data for plotting
    results_long <- gather(results, key = "Variable", value = "Value", -c)
    results_long$Variable <- factor(results_long$Variable, levels = c("T1", "T2"), labels = c("US Citizen", "Non-US Citizen"))
    
    output$priority <- renderPlotly({
      prioritygraph <- ggplot(results_long, aes(x = c, y = Value, color = Variable)) +
        geom_line() +
        geom_point() +
        labs(title = "Waiting Time (mins) vs Number of Counters",
             x = "Number of Counters",
             y = "Waiting Time (with service time)",
             color = "Citizenship Status") +  # Set the legend title here
        scale_color_manual(values = c("US Citizen" = "blue", "Non-US Citizen" = "red")) +
        theme_minimal()
      
      ggplotly(prioritygraph)
    })
    ############################################################################################################################
    
    # Find common x and y limits
    all_data <- rbind(unifiedqueue, usqueue, nonusqueue)
    xlim <- range(all_data$Counter)
    ylim <- range(all_data$Ws)
    
    output$unifiedqueue <- renderPlotly({
      generate_plot(unifiedqueue, "Waiting Time (mins) vs Number of Counters", xlim, ylim)
    })
    
    output$usqueue <- renderPlotly({
      generate_plot(usqueue, "Waiting Time (mins) vs Number of Counters for US Passengers", xlim, ylim)
    })
    
    output$nonusqueue <- renderPlotly({
      generate_plot(nonusqueue, "Waiting Time (mins) vs Number of Counters for Non-US Passengers", xlim, ylim)
    })
    
    # output$priority <- renderPlotly({
    #   prioritygraph <- ggplot(results_long, aes(x = c, y = Value, color = Variable)) +
    #           geom_line() +
    #           geom_point() +
    #           labs(title = "Performance Metrics vs. Number of Servers (c)",
    #               x = "Number of Servers (c)",
    #               y = "Value") +
    #           scale_color_manual(values = c("T1" = "blue", "T2" = "red"),
    #                             labels = c("T1" = "US Citizen", "T2" = "Non-US Citizen")) +
    #           theme_minimal() +
    #           theme(legend.title = element_blank())
    #           ggplotly(prioritygraph)
    # })
    
    # Extract and display the appropriate points
    output$txtout_us_waiting <- renderText({
      paste(round(average_us_wait_time, 2), "mins")
    })
    
    output$txtout_us_counters <- renderText({
      optimal_us_counters
    })
    
    output$txtout_nonus_waiting <- renderText({
      paste(round(average_nonus_wait_time, 2), "mins")
    })
    
    output$txtout_nonus_counters <- renderText({
      optimal_nonus_counters
    })
    
    # Display dynamic info text
    # output$info_text <- renderText({
    #   paste(
    #     "You have keyed in a total of", input$total_passengers, "passengers! 👭",
    #     "From historical data, we assume that there are", us_passengers, "US citizens 🍔and",
    #     non_us_passengers, "non-US citizens 🌍 in the queue."
    #   )
    # })
  })
}
# Run the app
shinyApp(ui = ui, server = server)