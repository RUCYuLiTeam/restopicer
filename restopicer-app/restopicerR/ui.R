dashboardPage(
  dashboardHeader(dropdownMenuOutput("taskMenu"),title = strong("Restopicer")),
  dashboardSidebar(
    sidebarMenuOutput("menu"),
    #dateRangeInput("daterange", "Date Range:",
    #               start  = "2001-01-01",end="2010-01-01",
    #               min    = "2001-01-01",max="2012-01-01",
    #               format = "yyyy",startview="decade"),
    plotOutput("wordcloud",height = "200px"),
    sidebarSearchForm(textId="searchTerm", buttonId="recommend", label = "Search Your Keyword ..."),
    actionButton("recommend", "Go Recommendation!",icon("thumbs-up"),width="100%")
  ),
  dashboardBody(
    tabItems(
      # questionaire
      tabItem(tabName = "questionaire",
              h2("questionaire")
      ),
      # papers
      tabItem(tabName = "papers",
              h2("papers tab content")
      ),
      # dashboard
      tabItem(tabName = "dashboard",
              h2("dashboard tab content")
      )
    )
  )
)