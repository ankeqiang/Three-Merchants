library(shiny)
library(timevis)




# Assuming your data is stored in 'ZBS_ChroLine2'
# Convert the 'Year' column to a character format
ZBS_ChroLine2$Year <- as.character(ZBS_ChroLine2$Year)

# Create a new data frame in the required format
data <- data.frame(
  id = 1:nrow(ZBS_ChroLine2),
  content = ZBS_ChroLine2$InstiPos,
  start = as.POSIXct(paste0(ZBS_ChroLine2$Year, "-01-01")), # Start date (1st January of the year)
  end = as.POSIXct(paste0(as.numeric(ZBS_ChroLine2$Year) + 1, "-01-01")), # End date (1st January of the next year)
  stringsAsFactors = FALSE
)

# Remove NA values from the 'Position1' column of the 'data' data frame
data <- data[complete.cases(data$content), ]

ui <- fluidPage(
  timevisOutput("timeline")
)

server <- function(input, output, session) {
  output$timeline <- renderTimevis({
    timevis(data)
  })
}

shinyApp(ui = ui, server = server)
