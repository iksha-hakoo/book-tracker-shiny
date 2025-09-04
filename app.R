# Libraries
library(shiny)
library(readr)
library(here)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyjs)

books <- read_csv(here::here("Data", "summer_reading.csv")) %>%
  mutate(date_read = as.Date(date_read, format = "%b %d, %Y"))

ui <- fluidPage(
  useShinyjs(),
  
  # add confetti library
  tags$head(HTML('<script src="https://cdn.jsdelivr.net/npm/canvas-confetti@1.5.1/dist/confetti.browser.min.js"></script>')),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #A7DFFF; /* soft sky blue */
        font-family: 'Georgia', serif;
      }
      h1 {
        font-size: 48px;
        text-align: center;
        color: #2C3E50; /* deep ocean blue */
        margin-bottom: 20px;
      }
      #summary {
        font-size: 20px;
        font-weight: bold;
        color: #FF7F66; /* coral accent */
        text-align: center;
        margin-top: 15px;
        margin-bottom: 25px;
      }
      #topbooks {
        font-size: 22px;
        font-weight: bold;
        color: #2C3E50;
        text-align: center;
        margin-bottom: 30px;
      }
      #goal_message {
        font-size: 18px;
        font-weight: bold;
        text-align: center;
        margin-top: 10px;
      }
      .panel-box {
        background-color: #F6E7CB; /* sand beige */
        border-radius: 12px;
        padding: 15px;
        margin-bottom: 20px;
      }
      .data-box {
        background-color: #F6E7CB; /* sand beige outside */
        border-radius: 12px;
        padding: 15px;
        margin-bottom: 20px;
      }
      table.dataTable tbody tr:nth-child(even) {
        background-color:rgb(255, 249, 231); 
      }
      table.dataTable tbody tr:nth-child(odd) {
        background-color:rgb(255, 241, 248);
      }
      .btn-coral {
        background-color: #FF7F66; 
        color: white;
        font-weight: bold;
        border: none;
      }
      .btn-coral:hover {
        background-color: #e76b55;
      }
      .progress {
        height: 25px;
        background-color: #f3f3f3;
        border-radius: 12px;
        overflow: hidden;
        flex-grow: 1;
      }
      .progress-bar {
        height: 100%;
        text-align: center;
        font-weight: bold;
        color: white;
        line-height: 25px;
      }
      #reading-highlights {
      font-size: 20px;  
      line-height: 1.6;
      color: #2C3E50;    
    }

    #reading-highlights h3 {
      font-size: 24px;   
      font-weight: bold;
      margin-bottom: 14px;
    }
    "))
  ),
  
  # Title
  tags$h1("Summer Reading Tracker 📖🌿✨"),
  
  # Top 5-star books
  div(id="topbooks", textOutput("top_books")),
  
  # Progress tracker
  div(class="panel-box",
      h3("📊 Reading Goal Tracker"),
      numericInput("goal", "Set your reading goal:", value = 15, min = 1),
      uiOutput("progress_ui"),
      div(id="goal_message", textOutput("goal_message")),
      div(id="confetti")
  ),
  
  # Bar chart
  div(class="panel-box",
      plotOutput("books_per_month", height="300px")
  ),
  
  # Reading Highlights Panel
div(class="panel-box",
    h3("🌟 Reading Highlights"),
    textOutput("total_pages"),
    textOutput("avg_length"),
    textOutput("top_genre"),
    textOutput("percent_recommended")
),
  
  # Filters
# Filters
div(class="panel-box",
    h3("Filter Your Books"),
    dateRangeInput("date_range", "Filter by Date Read",
                   start = min(books$date_read, na.rm = TRUE),
                   end = max(books$date_read, na.rm = TRUE)),
    selectInput("genre", "Choose Genre",
                choices = c("All", unique(books$genre))),
    selectInput("source", "Where I Bought It",
                choices = c("All", unique(books$source))),
    sliderInput("rating_filter", "Minimum Rating",
                min = 1, max = 5, value = 1, step = 0.5)
),

  # Summary
  div(id="summary", textOutput("summary")),
  
  # Searchable Table
  div(class="data-box",
      h3("🔍 Your Library"),
      DTOutput("table")
  ),
  
  # Add Book Section
  div(class="panel-box",
      h3("➕ Add a New Book"),
      textInput("new_title", "Book Title"),
      textInput("new_author", "Author"),
      textInput("new_genre", "Genre"),
      numericInput("new_pages", "Pages", value = 300, min = 1),
      numericInput("new_rating", "Rating (1–5)", value = 4, min = 1, max = 5, step = 0.5),
      dateInput("new_date", "Date Read"),
      textInput("new_format", "Format (Paperback, Hardcover, etc.)"),
      textInput("new_source", "Source"),
      numericInput("new_price", "Price ($)", value = 10, min = 0),
      actionButton("add_book", "Add Book", class="btn-coral")
  )
)

server <- function(input, output, session) {
  books_data <- reactiveVal(books)
  
filtered <- reactive({
  data <- books_data()
  if (!is.null(input$date_range)) {
    data <- data[data$date_read >= input$date_range[1] &
                   data$date_read <= input$date_range[2], ]
  }
  if (input$genre != "All") {
    data <- data[data$genre == input$genre, ]
  }
  if (input$source != "All") {
    data <- data[data$source == input$source, ]
  }
  # ⭐ filter by rating
  data <- data[data$rating >= input$rating_filter, ]
  data
})
  
  # summary
  output$summary <- renderText({
    data <- filtered()
    paste0("You’ve read ", nrow(data), " books, spent $", sum(data$price, na.rm = TRUE),
           ", with an average rating of ", round(mean(data$rating, na.rm = TRUE), 2), " ⭐ this summer")
  })
  
  # top books
  output$top_books <- renderText({
    data <- filtered()
    top <- data[data$rating == 5, "title"]
    if (nrow(top) == 0) return("🌟 No 5-star books yet!")
    paste0("🎉 5-Star Favorites: ", paste(top$title, collapse = " ✨, "), " 🎉")
  })
  
  # progress tracker
  output$progress_ui <- renderUI({
    data <- filtered()
    total_books <- nrow(data)
    goal <- input$goal
    progress <- round((total_books / goal) * 100, 1)
    
    if (progress >= 100) {
      runjs("
        confetti({
          particleCount: 150,
          spread: 90,
          origin: { y: 0.6 }
        });
      ")
    }
    
    tagList(
      div(style="display:flex; align-items:center; gap:10px;",
          div(class="progress",
              div(class="progress-bar",
                  role="progressbar",
                  style=paste0("width:", progress, "%; background-color:#FF7F66;"),
                  paste0(total_books, "/", goal, " books (", progress, "%)")
              )
          ),
          span("🌊🐚", style="font-size: 22px;")
      )
    )
  })
  
  output$goal_message <- renderText({
    data <- filtered()
    total_books <- nrow(data)
    goal <- input$goal
    
    if (total_books >= goal) {
      return("🎉 Goal reached! Amazing job! 🎉")
    } else {
      return(paste0("📖 You’ve read ", total_books, " of ", goal, " books — keep going! 🌱"))
    }
  })
  
  # bar chart 📖 emojis
  output$books_per_month <- renderPlot({
    monthly_expanded <- filtered() %>%
      mutate(month = format(date_read, "%B"),
             month = factor(month, 
                            levels = month.name, 
                            ordered = TRUE)) %>% 
      count(month) %>%
      rowwise() %>%
      do(data.frame(month = .$month, y = 1:.$n))   
    
    ggplot(monthly_expanded, aes(x = month, y = y)) +
      geom_text(label = "📖", size = 5, vjust = 0) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      labs(title = "Books Read Per Month",
           x = "Month", y = "Number of Books") +
      theme_minimal(base_family = "Georgia") +
      theme(
        text = element_text(color = "#2C3E50", size = 14),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
      )
  })

  # Total Pages
output$total_pages <- renderText({
  data <- filtered()
  paste0("🌱 Total pages read: ", sum(data$pages, na.rm = TRUE))
})

# Average Length
output$avg_length <- renderText({
  data <- filtered()
  paste0("🌊 Average book length: ", round(mean(data$pages, na.rm = TRUE)), " pages")
})

# Top Genre
output$top_genre <- renderText({
  data <- filtered()
  top <- data %>%
    group_by(genre) %>%
    tally(sort = TRUE)
  if (nrow(top) == 0) return("🌞 Most read genre: None yet")
  paste0("🌞 Most read genre: ", top$genre[1])
})

# % Recommended
output$percent_recommended <- renderText({
  data <- filtered()
  if (nrow(data) == 0) return("💌 Recommended to friends: None yet")
  percent <- round(mean(data$recommend_to_friend == TRUE, na.rm = TRUE) * 100)
  paste0("💌 Recommended to friends: ", percent, "%")
})

  # interactive table
output$table <- renderDT({
  data <- filtered()
  data$reread <- ifelse(data$reread, "Yes", "No")
  data$recommend_to_friend <- ifelse(data$recommend_to_friend, "Yes", "No")
  datatable(
    data,
    options = list(
      pageLength = 5,
      scrollX = TRUE   # enables horizontal scroll
    ),
    class = 'cell-border stripe hover'
  )
})
  
  # add book
  observeEvent(input$add_book, {
    new_entry <- data.frame(
      title = input$new_title,
      author = input$new_author,
      genre = input$new_genre,
      pages = input$new_pages,
      rating = input$new_rating,
      date_read = input$new_date,
      format = input$new_format,
      source = input$new_source,
      price = input$new_price,
      reread = FALSE,
      recommend_to_friend = FALSE,
      stringsAsFactors = FALSE
    )
    books_data(rbind(books_data(), new_entry))
  })
}

shinyApp(ui, server)
