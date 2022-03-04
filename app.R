library(shiny)

source("wordlist.R")

ui <- fluidPage(
  textInput("guess", ""),
  actionButton("go", "Go"),
  verbatimTextOutput("result", placeholder = TRUE)
)

set.seed(as.integer(Sys.Date()))
target <- "gives"
  #sample(words_common, 1)

server <- function(input, output) {
  
  output$result <- renderPrint({
    if (!(input$guess %in% words_all)) {
      req(FALSE, cancelOutput = TRUE)
    }
    
    result <- check_words(target, input$guess)
    format_result(result)
    
  }) |> 
    bindEvent(input$go)
}

format_result <- function(r) {
  out_str <- ""
  for (i in seq_along(r$letters)) {
    if (r$result[i] == "correct") {
      out_str <- paste0(out_str, "[", r$letters[i], "]")
    } else if (r$result[i] == "in-word") {
      out_str <- paste0(out_str, "(", r$letters[i], ")")
    } else {
      out_str <- paste0(out_str, " ", r$letters[i], " ")
    }
  }
  out_str
}

compare_words <- function(target_str, guess_str) {
  if (nchar(target_str) != nchar(guess_str)) {
    stop("target and guess must be the same length.")
  }
  
  target <- strsplit(target_str, "")[[1]]
  guess <- strsplit(guess_str, "")[[1]]
  remaining <- character(0)
  result <- rep("not-in-word", 5)
  
  for (i in seq_along(guess)) {
    if (guess[i] == target[i]) {
      result[i] <- "correct"
    } else {
      # The `remaining` vector contains the letters in target
      # we didn't get right
      remaining <- c(remaining, target[i])
    }
  }
  
  for (i in seq_along(guess)) {
    # For a letter to have the status 'in-word' it needs
    # - To be in the vector of target letters we didn't get right
    # - NOT to be in the target in the right position
    if (guess[i] != target[i] && guess[i] %in% remaining) {
      result[i] <- "in-word"
      # Removing the 'in-word' letter from `remaining`
      remaining <- remaining[-match(guess[i], remaining)]
    }
  }
  
  result
}


# A new function that returns a data structure with more information
check_words <- function(target_str, guess_str) {
  compare_result <- compare_words(target_str, guess_str)
  correct <- FALSE
  if (all(compare_result == "correct")) {
    correct <- TRUE
  }
  
  list(
    guess = guess_str,
    letters = strsplit(guess_str, "")[[1]],
    result = compare_result,
    correct = correct
  )
}

shinyApp(ui, server)