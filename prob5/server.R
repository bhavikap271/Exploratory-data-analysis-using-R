# Please set the working directory below where the json file of weekly tweets have been saved.

#setwd("C:\\Users\\Bhavika\\Courses\\CSE587\\Project1\\Problem5")
library(shiny)
library(streamR)
library(ROAuth)
library(wordcloud)

load("my_oauth.Rdata")

count_republican = 0
count_democrat = 0
count_others = 0

count_trump = 0
count_hillary = 0
count_sanders = 0
count_ted = 0
count_carson = 0
count_cruz = 0
count_kasich = 0

trump_text <- vector()
hillary_text <- vector()
sanders_text <- vector()
romney_text <- vector()
ted_text <- vector()
carson_text <- vector()
kasich_text <- vector()

shinyServer(function(input, output, session){
  
    output$currentTime <- renderText({invalidateLater(500, session)
    paste("Current time is: ",Sys.time())})
    
    week_result <- jsonlite::fromJSON(paste(readLines("election.json"), collapse=""))
    week_trump = 0
    week_hillary = 0
    week_sanders = 0
    week_kasich = 0
    week_cruz = 0
    week_ted = 0
    week_carson = 0
    
    week_republican = 0
    week_democratic = 0
    
    for (i in 1:nrow(week_result)){
      if (week_result[i,]$created >= Sys.Date()-7 ){
        if (grepl("Trump", week_result[i,]$text, ignore.case = TRUE) | grepl("Donald", week_result[i,]$text, ignore.case = TRUE)){
          week_trump = week_trump + 1
          
        }
        if (grepl("Hillary", week_result[i,]$text, ignore.case = TRUE) | grepl("Clinton", week_result[i,]$text, ignore.case = TRUE)  ){
          week_hillary = week_hillary + 1
          
        }
        if (grepl("sanders", week_result[i,]$text, ignore.case = TRUE) | grepl("bernie", week_result[i,]$text, ignore.case = TRUE) ){
          week_sanders = week_sanders + 1
          
        }
        if (grepl("Ted", week_result[i,]$text, ignore.case = TRUE) | grepl("Cruz", week_result[i,]$text, ignore.case = TRUE) | grepl("Democrat", week_result[i,]$text, ignore.case = TRUE)) { 
          week_ted = week_ted + 1
          
        }
        if (grepl("Ben", week_result[i,]$text, ignore.case = TRUE) | grepl("Carson", week_result[i,]$text, ignore.case = TRUE) | grepl("Democrat", week_result[i,]$text, ignore.case = TRUE)) { 
          week_carson = week_carson + 1
          
        }
        
        if(grepl("Ted", week_result[i,]$text, ignore.case = TRUE) | grepl("Cruz", week_result[i,]$text, ignore.case = TRUE)){
          week_cruz = week_cruz + 1
            
        }
        
        if(grepl("Kasich", week_result[i,]$text, ignore.case = TRUE) | grepl("John", week_result[i,]$text, ignore.case = TRUE)){
          week_kasich = week_kasich + 1
        }
        
        
        
      }
    }
    
    week_republican = week_trump + week_carson + week_cruz + week_kasich
    week_democrat = week_hillary + week_sanders
    
    
    output$previousweek <- renderPlot({
      week_results =data.frame(tweets = c("TRUMP","HILLARY","SANDERS","CRUZ","CARSON","KASICH"), numbers = c(week_trump,week_hillary,week_sanders,week_cruz,week_carson,week_kasich))
      barplot(week_results$numbers, names = week_results$tweets, xlab = "Presidential Candidates", ylab = "Counts", col = c("Green","Red","Blue","Yellow","Orange","Purple"))
    })
    
    output$partyWeekPlot <- renderPlot({
      results = data.frame(tweets = c("REPUBLICAN","DEMOCRATIC"), numbers = c(week_republican,week_democrat))
      barplot(results$numbers, names = results$tweets, xlab = "Parties", ylab = "Count", col = c("Red","Blue","Green"))
    })
    
  
  observe({
  
    invalidateLater(30000 , session)
    
    dat_all = filterStream(file.name = "", track=c("election","Election"),language='en',tweets = 30, oauth = my_oauth)
    dat_all = parseTweets(dat_all)
    
      for(i in 1:nrow(dat_all)){
            
          if (grepl("Trump", dat_all[i,]$text, ignore.case = TRUE) | grepl("Donald", dat_all[i,]$text, ignore.case = TRUE)){
               count_trump <<- count_trump + 1
               trump_text <<- c(trump_text, as.character(dat_all[i,]$text))
              
          }
            
            if(grepl("Ben", dat_all[i,]$text, ignore.case = TRUE) | grepl("Carson", dat_all[i,]$text, ignore.case = TRUE)){
                count_carson <<- count_carson + 1 
                carson_text <<- c(carson_text, as.character(dat_all[i,]$text))       
            }
            
            if(grepl("Ted", dat_all[i,]$text, ignore.case = TRUE) | grepl("Cruz", dat_all[i,]$text, ignore.case = TRUE)){
                count_cruz <<- count_cruz + 1
                ted_text <<- c(ted_text, as.character(dat_all[i,]$text))   
            }
            
            if(grepl("Kasich", dat_all[i,]$text, ignore.case = TRUE) | grepl("John", dat_all[i,]$text, ignore.case = TRUE)){
               count_kasich <<- count_kasich + 1
               kasich_text <<- c(kasich_text, as.character(dat_all[i,]$text))
            }
          
  
          if (grepl("Hillary", dat_all[i,]$text, ignore.case = TRUE) | grepl("Clinton", dat_all[i,]$text, ignore.case = TRUE)){
            count_hillary <<- count_hillary + 1
            hillary_text <<- c(hillary_text, as.character(dat_all[i,]$text))
          }
            
            
          if (grepl("sanders", dat_all[i,]$text, ignore.case = TRUE) | grepl("bernie", dat_all[i,]$text, ignore.case = TRUE)){
            count_sanders <<- count_sanders + 1
            sanders_text <<- c(sanders_text, as.character(dat_all[i,]$text))        
          }  
        
      }
    
      count_republican <<- count_trump + count_carson + count_cruz + count_kasich
      count_democrat <<- count_hillary + count_sanders
    
    #}
    
    output$candidatePlot <-renderPlot({
      
      results = data.frame(tweets = c("TRUMP","HILLARY","SANDERS","CRUZ","CARSON","KASICH"), numbers = c(count_trump,count_hillary,count_sanders,count_cruz,count_carson,count_kasich))
      barplot(results$numbers, names = results$tweets, xlab = "Presidential Candidates", ylab = "Count", col = c("Green","Red","Blue","Yellow","Orange","Purple"))
      
      if (length(trump_text) > 0){
        output$trump_wordcloud <- renderPlot({ wordcloud(paste(trump_text, collapse=" "), min.freq = 50, random.color=TRUE, max.words=30 ,colors=brewer.pal(8, "Dark2"))  })
      }
      
      if (length(hillary_text) > 0) {
        output$hillary_wordcloud <- renderPlot({ wordcloud(paste(hillary_text, collapse=" "), random.color=TRUE,  min.freq = 50, max.words=30 ,colors=brewer.pal(8,"Set3"))  })
      }
      
      if (length(sanders_text) > 0){
        output$sanders_wordcloud <- renderPlot({ wordcloud(paste(sanders_text, collapse=" "), min.freq = 50, random.color=TRUE , max.words=30 ,colors=brewer.pal(8, "Dark2"))  })
      }
      if (length(ted_text) > 0){
        output$ted_wordcloud <- renderPlot({ wordcloud(paste(ted_text, collapse=" "), min.freq = 50, random.color=TRUE , max.words=30 ,colors=brewer.pal(8, "Dark2"))  })
      }
      if (length(carson_text) > 0){
        output$carson_wordcloud <- renderPlot({ wordcloud(paste(carson_text, collapse=" "), min.freq = 50, random.color=TRUE , max.words=30 ,colors=brewer.pal(8, "Dark2"))  })
      }
      if (length(kasich_text) > 0){
        output$kasich_wordcloud <- renderPlot({ wordcloud(paste(kasich_text, collapse=" "), min.freq = 50, random.color=TRUE , max.words=30 ,colors=brewer.pal(8, "Dark2"))  })
      }
      
      
      })
    
    output$partyPlot <- renderPlot({
      results = data.frame(tweets = c("REPUBLICAN","DEMOCRATIC"), numbers = c(count_republican,count_democrat))
      barplot(results$numbers, names = results$tweets, xlab = "Parties", ylab = "Count", col = c("Red","Blue","Green"))
    })
  
  })

})
  
  
  
  
  
  
  
  
  
  