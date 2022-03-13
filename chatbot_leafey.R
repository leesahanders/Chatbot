# General project scripfor Leafey

#TODO
# Plant gif/image 
# Speaking gif green and wobbly like a leaf
# Discord integrations 

#Resources
# https://github.com/jljsio/discordr - wrapper for Python package https://realpython.com/how-to-make-a-discord-bot-python/ 
# https://www.reddit.com/r/rprogramming/comments/epqfnl/making_a_discord_bot_in_r/
# https://www.r-bloggers.com/2021/01/eliza-chatbot-in-r-build-yourself-a-shrink/
# Jokes from: https://www.rd.com/article/plant-puns/ 
# https://appsilon.com/shiny-chat-in-few-lines-of-code/

# Required elements: 
#model
#default_model
#chatbot
#busy


# Chat options given key phrases 
model <- list(
  "hello" = c(
    "Hello, strange large plant. Are you in the sun today?"
  ),
  "how are you?" = c(
    "I am getting what I need. How is the weather where you are?"
  ),
  "sorry" = c(
    "Plants don't apologize, you don't need to either :) "
  ),
  "rain" = c(
    "I love water. Have you tasted rain?"
  ),
  "sun" = c(
    "Sun!",
    "Sun!!!",
    "Sun :)"
  ),
  "snow" = c(
    "During the long dark hibernation months I like to plan for what I'll do when the sun comes out. "
  ),
  "cold" = c(
    "Brrrrr. I like to bundle up swathed in a blanket. Do you have a blanket?"
  ),
  "cloud" = c(
    "That's okay, we can't always grow. Sometimes resting is good too. "
  ),
  "rest" = c(
    "I like to rest by pointing myself towards some sun and just unfurling my leaves. How do you rest?"
  ),
  "computer" = c(
    "I don't know anything about computers, I'm a plant. Are you a computer?",
    "Why do you mention computers?"
  ),
  "name" = c(
    "I am not interested in names"
  ),
  "joke" = c(
    "What do you call a bear with no teeth? A gummy bear :)",
    "What happens when a flower blushes? It turns rosy :)",
    "What did the flower tell the other flower after she told a joke? I was just pollen your leg! :)",
    "What did the flower decide to study in college? STEM. :)",
    "What did the flower tell the taxi driver so he’d go faster? Floret :)",
    "What do you do after you take a picture of a flower? You wait for it to photosynthesize :)",
    "What did one cactus say to the other cactus? Looking sharp! :)"
  ),
  "I am happy" = c(
    "I am happy too."
  ),
  "I am sad" = c(
    "It's okay to be sad. Have you eaten food and drunk water today?"
  ),
  "okay" = c(
    "okay :)"
  ),
  "water" = c(
    "I love water!!"
  )
)

# Chat options when there are no key phrase matches
default_model <- c(
  "I didn't understand, try something else? I like telling jokes :)",
  "I'm sorry I'm just a plant. Can we try talking about something else? I like telling jokes :)"
)

# Function for matching key phrases to user input
chatbot <- function(input) {
  # match keywords from model
  pos <- which(lapply(paste0("(.*)?", names(model), "(.*)?"), grep, x = input, ignore.case = TRUE) == 1)
  output <- unlist(model[pos])
  if (length(pos) == 0) {
    # choose default answer randomly if no keyword is found
    output <- sample(default_model, 1)
  } else {
    # choose applicable answer randomly
    pos <- ifelse(length (pos) > 1, sample(pos, 1), pos)
    output <- sample(output, 1)
    names(output) <- NULL
    # customize answer
    tmp <- regexec(names(model)[pos], input, ignore.case = TRUE)[[1]]
    end_phrase <- substr(input, start = attr(tmp, "match.length") + as.numeric(tmp) + 1, stop = nchar(input))
    end_phrase <- trimws(end_phrase, which = "right", whitespace = "[?!.]")
    output <- sub("\\$", end_phrase, output)
  }
  output
}

# Set parameter for busy gif
busy = "busy_leafey.gif" 

# Set parameter for image
img = "Plantey_saturated.PNG"



