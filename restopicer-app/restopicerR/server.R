function(input, output, session) {
  # taskMenu dropdownMenuOutput for showing taskprogress
  output$taskMenu <- renderMenu({
    dropdownMenu(taskItem("Task Progress", value = taskprogress, color = "aqua", href = NULL),type = "tasks",badgeStatus = NULL)
  })
  # sidebarMenuOutput
  output$menu <- renderMenu({
    goRecommend()
    if(taskprogress>0){
      sidebarMenu(
        menuItem("Scientific Papers", tabName = "papers", icon = icon("files-o")),
        menuItem("Preference Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        #menuItem("Parameter Configuration", tabName = "config", icon = icon("cogs"))
      )
    }else{
      sidebarMenu(
        menuItem("Questionaire", icon = icon("file-text-o"), tabName = "questionaire", badgeLabel = "Initial", badgeColor = "green")
        #menuItem("About Restopicer", tabName = "about", icon = icon("at"))
      )
    }
  })
  # submit questionaire, start recommend, next recommend
  goRecommend<- function(){
    input$recommend
    taskprogress <<- taskprogress + 1
  }
  
}