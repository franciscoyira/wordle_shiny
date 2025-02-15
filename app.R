library(shiny)

source("wordlist.R")

ui <- fluidPage(
  tags$head(tags$style(
    HTML("
      .container-fluid {
        max-width: 500px;
      }
    
      #result {
        display: grid;
        grid-template-columns: repeat(5, 50px);
        gap: 5px;
        padding: 15px;
        justify-content: center;
      }
      
      #result > .guess-letter {
        width: 50px;
        height:50px;
        font-size: 20px;
        display: grid;
        place-content: center;
      }
      
      .guess-letter {
      padding: 4px;
        border-radius: 5px;
        color: white;
        font-weight: bold;
      }
      
      .in-word {
        background-color: #c8b458;
      }
      
      .not-in-word {
        background-color: #787c7e;
      }
      
      .correct {
        background-color: #6aa964;
      }
      
      .input-wrapper {
        display: flex;
        align-items: flex-end;
        justify-content: center;
        padding-bottom: 10px;
      }
      
      .input-wrapper > div {
        margin-bottom: 0;
      }
      
      .input-wrapper > button {
        margin-left: 10px;
      }
    ")
  )),
  uiOutput("result"),
  p(span("x", class="guess-letter correct"), "means the letter is correct"),
  p(span("x", class="guess-letter in-word"), "means the letter is in the word but in a different place"),
  div(
    textInput("guess", ""),
    actionButton("go", "Go"),
    class = "input-wrapper"
  ),
  verbatimTextOutput("keyboard", placeholder = TRUE)
)

set.seed(as.integer(Sys.Date()))
target <- "gives"
  #sample(words_common, 1)

server <- function(input, output) {
  
  all_gueses <- reactiveVal(character()) 
  
  output$result <- renderUI({
    if (!(input$guess %in% words_all)) {
      req(FALSE, cancelOutput = TRUE)
    }
    
    all_gueses_new <- c(all_gueses(), input$guess) 
    all_gueses(all_gueses_new)
    
    # Not efficient, but fine for now
    out_str <- lapply(all_gueses(), function(guess) {
      result <- check_words(target, guess)
      format_result(result)
    })

    out_str
    
  }) |> 
    bindEvent(input$go)
  
  output$keyboard <- renderText({
    keys <- paste(
      " q  w  e  r  t  y  u  i  o  p ",
      "  a  s  d  f  g  h  j  k  l ",
      "   z  x  c  v  b  n  m ",
      sep = "\n"
    )
    
    used_letters <- paste(all_gueses(), collapse = "")
    used_letters <- strsplit(used_letters, "")[[1]]
    used_letters <- unique(used_letters)
    
    for (letter in used_letters) {
      keys <- sub(letter, " ", keys)
    }
    
    keys
  })
}

format_result <- function(r) {
  out_divs <- tagList()
  for (i in seq_along(r$letters)) {
    out_divs[[i]] <-
      div(r$letters[i], class = paste("guess-letter", r$result[i]))
  }
  out_divs
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