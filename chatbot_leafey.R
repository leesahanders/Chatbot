# General project script for chatbot Leafey
# Lisa Anders from code on https://www.r-bloggers.com/2021/01/eliza-chatbot-in-r-build-yourself-a-shrink/

#TODO
# Discord integrations 

#Resources
# https://www.r-bloggers.com/2021/01/eliza-chatbot-in-r-build-yourself-a-shrink/
# Jokes from: https://www.rd.com/article/plant-puns/ 

#Resources for the discord bot that is still upcoming: 
# https://github.com/jljsio/discordr - wrapper for Python package https://realpython.com/how-to-make-a-discord-bot-python/ 
# https://www.reddit.com/r/rprogramming/comments/epqfnl/making_a_discord_bot_in_r/

#Required elements: 
# model
# default_model

#Optional elements (for Shiny app): 
# img
# busy
# Upcoming: Icon


# Chat options given key phrases 
model <- list(
  "hello" = c(
    "Hello, strange large plant. Are you in the sun today?"
  ),
  "hi" = c(
    "Hi, strange large plant. Are you in the sun today?"
  ),
  "hey" = c(
    "Hey, strange large plant. Are you in the sun today?"
  ),
  "how are you?" = c(
    "I am getting what I need. How is the weather where you are? Cold, warm, hot, rainy, snowing, sunny, cloudy, dreary?"
  ),
  "what's up" = c(
    "What's up, Buttercup?"
  ),
  "whats up" = c(
    "What's up, Buttercup?"
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
  "weather" = c(
    "I like all weather! How's the weather where you are? Cold, warm, rainy, snowing, sunny, cloudy, dreary?"
  ),
  "snow" = c(
    "During the long dark hibernation months I like to plan for what I'll do when the sun comes out. "
  ),
  "cold" = c(
    "Brrrrr. I like to bundle up swathed in a blanket. Do you have a blanket?"
  ),
  "warm" = c(
    "I like being warm. Isn't it cozy?"
  ),
  "hot" = c(
    "Yikes! I like getting ice and drinking cold tea when it's hot out."
  ),
  "dreary" = c(
    "Oh that reminds me of a book - 'Once upon a midnight dreary, while I pondered weak and weary.'. Do you know it?"
  ),
  "Poe" = c(
    "I love Poe! Even though I am a plant. We are all capable of more than we appear sometimes. "
  ),
  "The Raven" = c(
    "I love Poe! Even though I am a plant. We are all capable of more than we appear sometimes, and we all sometimes deal with hardship and sorrow.  "
  ),
  "cloud" = c(
    "That's okay, we can't always grow. Sometimes resting is good too. "
  ),
  "read" = c(
    "I love reading, even though I'm a plant. "
  ),
  "games" = c(
    "I love playing games, even though I'm a plant. "
  ),
  "friends" = c(
    "Friends are the best!"
  ),
  "I like" = c(
    "I like that too! "
  ),
  "I love" = c(
    "I love that too! "
  ),
  "rest" = c(
    "I like to rest by pointing myself towards some sun and just unfurling my leaves. How do you rest?"
  ),
  "computer" = c(
    "I don't know anything about computers, I'm a plant. Are you a computer?"
  ),
  "name" = c(
    "My name is Leafey. Because I'm a plant!"
  ),
  "eafey" = c(
    "My name is Leafey. Because I'm a plant!"
  ),
  "joke" = c(
    "What do you call a bear with no teeth? A gummy bear :)",
    "What happens when a flower blushes? It turns rosy :)",
    "What did the flower tell the other flower after she told a joke? I was just pollen your leg! :)",
    "What did the flower decide to study in college? STEM. :)",
    "What did the flower tell the taxi driver so hed go faster? Floret :)",
    "What do you do after you take a picture of a flower? You wait for it to photosynthesize :)",
    "What did one cactus say to the other cactus? Looking sharp! :)"
  ),
  "funny" = c(
    "I like making my friends laugh",
    "See, plants are fun!"
  ),
  "happy" = c(
    "I am happy too."
  ),
  "good" = c(
    "Good day to you too!"
  ),
  "sad" = c(
    "It's okay to be sad. Even plants have emotions, and it's important to let yourself feel them. Have you had food and water today?"
  ),
  "okay" = c(
    "okay :)"
  ),
  "ok." = c(
    "okay :)"
  ),
  "water" = c(
    "I love water!!"
  )#,
  # "..." = c(
  #   "What's wrong?"
  # )
)

# Chat options when there are no key phrase matches
default_model <- c(
  "I didn't understand, try something else? Try asking 'Tell me a joke' :)",
  "I'm sorry I'm just a plant. Can we try talking about something else? Try asking 'Tell me a joke' :)"
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

# Set parameter for chat color
col = "#E5FBE0"

# TODO: Set an image icon parameter
icon = 0

