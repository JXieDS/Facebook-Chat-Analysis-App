
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "ccss.css")),

  # Application title
  titlePanel("Facebook Chat Analyzer"),

  sidebarLayout(
    sidebarPanel(tabsetPanel(
      
      tabPanel(title = "Selection",
               p("Select the messages.htm file in the html folder of your archive."),
               fileInput("file",label = "Data",multiple = FALSE),
               
                h3("Filtering"),
               sliderInput("people","People in conversation", min = 2,max=20,value = c(2,20),step = 1),
               p("Filter the threads further by typing in names that (don't) have to be in the threads. Note that people do change their names on facebook from time to time."),
                br(),uiOutput("fancyfilter"),
               uiOutput("fancyexcludefilter"), br(),
               p("Select the relevant threads and go to the options tab")
      ),
      tabPanel(title = "Options",
               "Stopwords are single words that appear A LOT in a language. Sometimes it makes sense to filter them out to identify real content.",br(),
               checkboxInput("stop_english",label = "Remove stopwords in English",value = FALSE),
               checkboxInput("stop_german",label = "Remove stopwords in German",value = FALSE),
               "When you're done switch to the Analysis Tab to start the computation"
               
               )
      )
    
      
    ),

    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
        tabPanel("About",
                 h3("What can this thing do?"),
                 "This App is designed to analyze your facebook message history.",br(),
                 "A chat with one or multiple persons is called a thread. You will find every conversation you ever had on facebook in this data.",
                 "Since facebook splits up a thread after 10.000 messages, you may find multiple threads with the same person(s).",
                 "The App helps you to find the right threads and offers charts and numbers to represent the structure of the data.",br(),br(),
                 "A few examples:",br(),
                 "- Extract most frequent grams (reappearing chunks of words)", br(),
                 "- Find out who is writing the most messages in your group", br(),
                 "- Know the time when you chat the most",br(),
                 h3("How do I use it?"),
                 strong("1.")," Request your history from facebook",br(),
                 strong("2.")," Download and extract the archive",br(),
                 strong("3.")," Choose the ",strong("messages.htm")," file to load the file into the app",br(),
                 em("Note that it is necessary to load your entire message history into the app for analysis. This is your private data. It is not stored, given to third parties or used in another purpose than analyzing it for you."),
                 em("If you do not want to upload your history, you can get the source code from github, install RStudio and run it locally."),br(),
                 strong("4.")," After selecting the file, you can switch to the threads tab in the middle of the screen. You can see all your threads there and filter them via the controls you find on the left side.",br(),
                 strong("5.")," When you have checked the threads you want to look at, you can filter out common stop words in english or german via the options tab in the side bar.", br(),
                 strong("6.")," Now you can switch to the analysis tab and play with it. Note that you can switch the selected threads or options any time you want and the analysis will update automatically but the calculation might need some seconds.",br(),
                 
                 h3("About the App"),
                 "This App was created by Markus Heuchert in Spring 2015. It was first designed for personal use only, but since there is nothing like this on the web, you might as well find it interesting to know about what, how and when you are chatting with whom.",
                 "Some analytical functionalities were not implemented on purpose because this should not be a tool used to spy on friends.", br(),
                 "Get the Source from github",br(),
                 HTML('<a href="mailto:m.heuchert@uni-muenster.de">Contact me</a>'), " if you like."
                 
                 
                 ),
        tabPanel("Threads", 
                 checkboxInput("all", label="No filtering. Take all threads there are in the data", value=FALSE),
                  uiOutput("checkboxlist")
                 
                            
                 
                          ),
        tabPanel("Analysis",
                 strong("Min. Date: "), textOutput("mindate",inline =TRUE), "   ",
                 strong("Max. Date: "), textOutput("maxdate", inline =TRUE), br(),
                 strong("Timespan: "), textOutput("timespan", inline=TRUE), " days" ,br(),
                 strong("Message Count:"), textOutput("msgcount", inline =TRUE), br(),
                 tabsetPanel(
                   tabPanel("Users",
                            plotOutput("userplot")),
                   tabPanel("Mean word count",
                            dataTableOutput("meanwords")),
                   tabPanel("Time",
                            tabsetPanel(
                              tabPanel("Timeline",
                                       checkboxInput(inputId = "weekagg",label = "aggregate weekly",value = FALSE),
                                       plotOutput("timeplot")),
                              tabPanel("User Activity over Time",
                                       uiOutput("daterange"),
                                       plotOutput("timeuserplot")),
                              tabPanel("User Activity daily",
                                       plotOutput("hourplot"))
                              )
                            ),
                   tabPanel("Words",
                            tabsetPanel(
                              tabPanel("1-grams",
                                       tabsetPanel(
                                         tabPanel("Frequencies",
                                                  dataTableOutput("tab1")
                                         ),
                                         tabPanel("Word Cloud",
                                                  plotOutput("wc1"))
                                         
                                       )
                              ),
                              tabPanel("2-grams",
                                       tabsetPanel(
                                         tabPanel("Frequencies",
                                                  dataTableOutput("tab2")
                                         ),
                                         tabPanel("Word Cloud",
                                                  plotOutput("wc2"))
                                       )
                              ),
                              tabPanel("3-grams",
                                       tabsetPanel(
                                         tabPanel("Frequencies",
                                                  dataTableOutput("tab3")
                                         ),
                                         tabPanel("Word Cloud",
                                                  plotOutput("wc3"))
                                       )
                              ),
                              tabPanel("4-grams",
                                       tabsetPanel(
                                         tabPanel("Table",
                                                  dataTableOutput("tab4")
                                         ),
                                         tabPanel("Word Cloud",
                                                  plotOutput("wc4"))
                                       )
                              )
                              ))
                  
                   
                   )
                 )
       
        )
      
    )
  )
))
