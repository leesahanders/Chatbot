# Plant Therapy

Hey there! This is still a work in progress. Come back later :) 

## Resources
 - https://github.com/jljsio/discordr - wrapper for Python package https://realpython.com/how-to-make-a-discord-bot-python/ 
 - https://www.reddit.com/r/rprogramming/comments/epqfnl/making_a_discord_bot_in_r/
 - https://www.r-bloggers.com/2021/01/eliza-chatbot-in-r-build-yourself-a-shrink/

Credit where credit is due - jokes are from: 
 - https://www.rd.com/article/plant-puns/ 

## How this works 

### Chat model

Shout out to the incredible resource at https://www.r-bloggers.com/2021/01/eliza-chatbot-in-r-build-yourself-a-shrink/ where these code parts are from: 
1. List of answers based on key phrases
2. List of default answers if no key phrases are found
3. Pattern matching function

### Hosting 

The chat bot can now be kicked off inside whichever system wanted - whether that is in a shiny app, discord integration, or just in console using a while loop. 

#### Console 

Shout out to the incredible resource at https://www.r-bloggers.com/2021/01/eliza-chatbot-in-r-build-yourself-a-shrink/ where this code is from for kicking off your chatbot to interact with in the console until you hit exit:  

``` r
cat("Leafey: Hello, I am Leafey!\n")
while (TRUE) {
  input <- readline("You: ")
  if (input == "quit") break
  cat("Leafey:", Leafey(input))
}
```

#### Shiny

Upcoming!

#### Discord 

Upcoming!

