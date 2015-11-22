dashboardPage(
  dashboardHeader(#dropdownMenuOutput("taskMenu"),
    title = strong("Restopicer")),
  dashboardSidebar(
    strong(textOutput("missionid",inline = T)),
    sidebarMenuOutput("menu"),
    #dateRangeInput("daterange", "Date Range:",
    #               start  = "2001-01-01",end="2010-01-01",
    #               min    = "2001-01-01",max="2012-01-01",
    #               format = "yyyy",startview="decade"),
    plotOutput("wordcloud",height = "200px"),
    sidebarSearchForm(textId="searchkeyword", buttonId="addkeyword", label = "Search Your Keyword ..."),
    actionButton("recommend", "Go Recommendation!",icon("thumbs-up"),width="100%"),
    actionButton("finished", "Mission Clear!",icon("times-circle"),width="100%")
  ),
  dashboardBody(
    tabItems(
      # questionaire
      tabItem(tabName = "questionaire",h2("Initiate Your Preference"),
              fluidRow(
                fluidRow(column(4,h4("BUSINESS")),
                         column(width = 8,sliderInput("rating1","Your Rating",min=1,max=5,value=0,step = 1))),
                fluidRow(column(4,h4("COMPUTER SCIENCE, ARTIFICIAL INTELLIGENCE")),
                         column(width = 8,sliderInput("rating2","Your Rating",min=1,max=5,value=0,step = 1))),
                fluidRow(column(4,h4("COMPUTER SCIENCE, INFORMATION SYSTEMS")),
                         column(width = 8,sliderInput("rating3","Your Rating",min=1,max=5,value=0,step = 1))),
                fluidRow(column(4,h4("COMPUTER SCIENCE, SOFTWARE ENGINEERING")),
                         column(width = 8,sliderInput("rating4","Your Rating",min=1,max=5,value=0,step = 1))),
                fluidRow(column(4,h4("INFORMATION SCIENCE & LIBRARY SCIENCE")),
                         column(width = 8,sliderInput("rating5","Your Rating",min=1,max=5,value=0,step = 1))),
                fluidRow(column(4,h4("MANAGEMENT")),
                         column(width = 8,sliderInput("rating6","Your Rating",min=1,max=5,value=0,step = 1))),
                fluidRow(column(4,h4("OPERATIONS RESEARCH & MANAGEMENT SCIENCE")),
                         column(width = 8,sliderInput("rating7","Your Rating",min=1,max=5,value=0,step = 1))),
                fluidRow(
                  actionButton("newmission","submit")))
              
              
      ) ,       
      tabItem(tabName = "login",
              textInput("loginmissionid", "Input Your Mission ID:", "1",width="300px"),
              actionButton("loginmission", "Mission Continues!",icon("bolt"),width="300px")
      ),
      # papers
      tabItem(tabName = "papers",
              fluidRow(
                tabBox(
                  title = textOutput("title1"),
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1",width = 12, 
                  tabPanel("Detail", textOutput("abstract1"),tableOutput("detail1"),
                           radioButtons("rate1", "Your Rating:",c("Not Interest" = "1","Hardly Interest" = "2","Somewhat Interest" = "3","Very Interest" = "4","Extremely Interest" = "5"),inline = T))
                )
              ),
              fluidRow(
                tabBox(
                  title = textOutput("title2"),
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset2",width = 12, 
                  tabPanel("Detail", textOutput("abstract2"),tableOutput("detail2"),
                           radioButtons("rate2", "Your Rating:",c("Not Interest" = "1","Hardly Interest" = "2","Somewhat Interest" = "3","Very Interest" = "4","Extremely Interest" = "5"),inline = T))
                )
              ),
              fluidRow(
                tabBox(
                  title = textOutput("title3"),
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset3",width = 12, 
                  tabPanel("Detail", textOutput("abstract3"),tableOutput("detail3"),
                           radioButtons("rate3", "Your Rating:",c("Not Interest" = "1","Hardly Interest" = "2","Somewhat Interest" = "3","Very Interest" = "4","Extremely Interest" = "5"),inline = T))
                )
              ),
              fluidRow(
                tabBox(
                  title = textOutput("title4"),
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset4",width = 12, 
                  tabPanel("Detail", textOutput("abstract4"),tableOutput("detail4"),
                           radioButtons("rate4", "Your Rating:",c("Not Interest" = "1","Hardly Interest" = "2","Somewhat Interest" = "3","Very Interest" = "4","Extremely Interest" = "5"),inline = T))
                )
              ),
              fluidRow(
                tabBox(
                  title = textOutput("title5"),
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset5",width = 12, 
                  tabPanel("Detail", textOutput("abstract5"),tableOutput("detail5"),
                           radioButtons("rate5", "Your Rating:",c("Not Interest" = "1","Hardly Interest" = "2","Somewhat Interest" = "3","Very Interest" = "4","Extremely Interest" = "5"),inline = T))
                )
              )
      ),
      # dashboard
      tabItem(tabName = "dashboard",
              h2("dashboard tab content")
      )
    )
  )
)
