library(shinyWidgets)

Q_100 <- read.csv("BigFiveQuestions.csv")
Q_100
#Q_100[,2]
#Q_100[,3:7]

choices <- c("starke Ablehnung" = 1,
             "Ablehnung" = 2,
             "neutral" = 3,
             "Zustimmung" = 4,
             "starke Zustimmung" = 5)


buttons <- list()

for(i in 1:nrow(Q_100)) {
    
     buttons[[i*2-1]] <- radioGroupButtons(inputId = paste0("q",Q_100[i, 1]), 
                                 label = Q_100[i,2],
                                 choices = choices,
                                 individual = TRUE,
                                 selected = character())
     buttons[[i*2]] <- br()
    
}
print(buttons)



