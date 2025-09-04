# book-tracker-shiny
An interactive R Shiny app to track and visualize reading progress with custom filters and themes.
# 📚 Book Tracker Shiny App

An interactive R Shiny app for tracking and visualizing reading progress.  
Built with **R, Shiny, ggplot2, dplyr, and shinythemes**.

## ✨ Features
- Filter books by rating, genre, and status
- Visualize reading progress with interactive charts
- Custom themes for a polished look

## 📂 Files
- `app.R` – Main Shiny app
- `books.csv` – Sample dataset of books

## 🛠️ How to Run
Clone the repo and run in R:

```R
# install required packages
install.packages(c("shiny", "ggplot2", "dplyr", "shinythemes"))

# run the app
shiny::runApp("app.R")
