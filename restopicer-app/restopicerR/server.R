function(input, output, session) {
  mission_info <- data.frame()
  recommend_paper <- data.frame()
  # taskMenu dropdownMenuOutput for showing taskprogress
  #output$taskMenu <- renderMenu({
  #  input$loginmission
  #  dropdownMenu(taskItem("Task Progress", value = taskprogress, color = "aqua", href = NULL),type = "tasks",badgeStatus = NULL)
  #})
  # text for missionid
  output$missionid <- renderText({
    input$loginmission
    input$recommend
    if(nrow(mission_info)==1){
      paste("Your Mission Id: ",mission_info$mission_id," (Round ",mission_info$round,")",sep = "")
    }else{
      "Sorry, No Mission Found!"
    }
  })
  # sidebarMenuOutput
  output$menu <- renderMenu({
    input$loginmission
    if(nrow(mission_info)==1){
      sidebarMenu(
        menuItem("Scientific Papers", tabName = "papers", icon = icon("files-o")),
        menuItem("Preference Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        #menuItem("Parameter Configuration", tabName = "config", icon = icon("cogs"))
      )
    }else{
      sidebarMenu(
        menuItem("New Mission", icon = icon("file-text-o"), tabName = "questionaire", badgeLabel = "Here!", badgeColor = "green"),
        menuItem("Mission Continues", tabName = "login", icon = icon("bolt"))
      )
    }
  })
  
  # show recommended papers
  output$title1 <- renderText({
    input$recommend
    recommend_paper$article_title[1]
  })
  output$title2 <- renderText({
    input$recommend
    recommend_paper$article_title[2]
  })
  output$title3 <- renderText({
    input$recommend
    recommend_paper$article_title[3]
  })
  output$title4 <- renderText({
    input$recommend
    recommend_paper$article_title[4]
  })
  output$title5 <- renderText({
    input$recommend
    recommend_paper$article_title[5]
  })
  output$abstract1 <- renderText({
    input$recommend
    recommend_paper$abstract[1]
  })
  output$abstract2 <- renderText({
    input$recommend
    recommend_paper$abstract[2]
  })
  output$abstract3 <- renderText({
    input$recommend
    recommend_paper$abstract[3]
  })
  output$abstract4 <- renderText({
    input$recommend
    recommend_paper$abstract[4]
  })
  output$abstract5 <- renderText({
    input$recommend
    recommend_paper$abstract[4]
  })
  output$detail1 <- renderTable({
    input$recommend
    rownames(recommend_paper) <- NULL
    recommend_paper[1,c(4,5,6,7)]
  })
  output$detail2 <- renderTable({
    input$recommend
    rownames(recommend_paper) <- NULL
    recommend_paper[2,c(4,5,6,7)]
  })
  output$detail3 <- renderTable({
    input$recommend
    rownames(recommend_paper) <- NULL
    recommend_paper[3,c(4,5,6,7)]
  })
  output$detail4 <- renderTable({
    input$recommend
    rownames(recommend_paper) <- NULL
    recommend_paper[4,c(4,5,6,7)]
  })
  output$detail5 <- renderTable({
    input$recommend
    rownames(recommend_paper) <- NULL
    recommend_paper[5,c(4,5,6,7)]
  })
  # login mission, mission continues
  observe({
    input$loginmission
    mission_info <<- getMissionInfo(isolate(input$loginmissionid))
    if(nrow(mission_info)==1){
      recommend_paper <<- goRecommendation(mission_info$mission_id,mission_info$round) 
    }
  })
  # add searching keywords
  observe({
    input$addkeyword
    if(nrow(mission_info)==1){
      addPreferenceKeyword(mission_info$mission_id,isolate(input$searchkeyword))
    }
  })
  # go recommend
  observe({
    input$recommend
    if(nrow(mission_info)==1&&nrow(recommend_paper)==5){
      rating(mission_info$mission_id,recommend_paper$item_ut[1],input$rate1)
      rating(mission_info$mission_id,recommend_paper$item_ut[2],input$rate2)
      rating(mission_info$mission_id,recommend_paper$item_ut[3],input$rate3)
      rating(mission_info$mission_id,recommend_paper$item_ut[4],input$rate4)
      rating(mission_info$mission_id,recommend_paper$item_ut[5],input$rate5)
      recommend_paper <<- goRecommendation(mission_info$mission_id,mission_info$round)
      mission_info <<- getMissionInfo(mission_info$mission_id)
    }
  })
}