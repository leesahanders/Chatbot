# Loads and runs the chat model for chatbot Leafey in the console  
# Lisa Anders from code on https://www.r-bloggers.com/2021/01/eliza-chatbot-in-r-build-yourself-a-shrink/

source("chatbot_leafey.R")

# To run this in the console run this: 
input <- ""
cat("Leafey: Hello, I am Leafey!\n")
while (TRUE) {
  input <- readline("You: ")
  if (input == "quit") break
  cat("Leafey:", chatbot(input))
}

