# Chatbots

Hey there! This is still a work in progress. Come back later :) 

## Resources
 - https://github.com/jljsio/discordr - wrapper for Python package https://realpython.com/how-to-make-a-discord-bot-python/ 
 - https://www.reddit.com/r/rprogramming/comments/epqfnl/making_a_discord_bot_in_r/
 - https://www.r-bloggers.com/2021/01/eliza-chatbot-in-r-build-yourself-a-shrink/

Credit where credit is due - jokes are from: 
 - https://www.rd.com/article/plant-puns/ 

## Meet the bots 

| Chatbot | Name | Description | 
| ------------- | ------------- | ------------- |
| <img src="/files/Plantey_saturated.PNG" width="100" height="100"> | Leafey | Leafey is here to provide some plant therapy. Leafey is very simple with just key phrase look ups based on user inputs.  | 

<!---
![Leafey Image Too Big](/files/Plantey_saturated.PNG)
![Leafey Image Too Big](/files/Plantey_saturated.PNG =250x250)
<img src="/files/Plantey_saturated.PNG" width="100" height="100">
-->

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

