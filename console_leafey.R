# Loads and runs the chat model in the console  

source("chatbot_leafey.R")

# To run this in the console run this: 
input <- ""
cat("Leafey: Hello, I am Leafey!\n")
while (TRUE) {
  input <- readline("You: ")
  if (input == "quit") break
  cat("Leafey:", chatbot(input))
}