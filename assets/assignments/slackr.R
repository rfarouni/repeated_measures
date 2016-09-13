library(slackr)

slackrSetup(api_token = "xoxp-71372473621-71376508737-72789843841-2f81915155",
            username = "rfarouni",
            channel = "#incomingwebhook", 
            incoming_webhook_url = "https://hooks.slack.com/services/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")

slackr('hello')

# send images
library(ggplot2)
qplot(mpg, wt, data=mtcars)
dev.slackr("#incomingwebhook")

barplot(VADeaths)
dev.slackr("@rfarouni")

ggslackr(qplot(mpg, wt, data=mtcars))