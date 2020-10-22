library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyalert)
library(shinydashboardPlus)
library(tidyverse)

source("mod_inputs.R")
source("mod_save_load.R")
source("head_foot.R")
source("pw_module.R")

jscode <- 'window.onbeforeunload = function() { return "Please use the button on the webpage"; };'


# Set up questionnaire interface ----
ui <- 
  dashboardPagePlus(
    
    title = titlePanel("Trusted 10",
               tags$head(tags$link(rel = "icon", type = "image/png", href = "icon.png"),
                         tags$head(tags$script(jscode)), # to provide an error message if they try to press the back button
                         tags$title("Trusted 10"))
    ),
    
    skin = "red",
    sidebar_fullCollapse = TRUE,
    
    dashboardHeader(title = "Personal Network"),
    
    dashboardSidebar(
      useShinyjs(),
      sidebarMenu(id = "sidebarmenu", inactiveLink,
                  menuItem("Your Info",tabName = "section-1", icon = icon("user-circle") ),
                  menuItem( "Initialize Network",tabName = "section-2",icon = icon("people-carry") ),
                  menuItem("Description",tabName = "section-3",icon = icon("comments")),
                  # menuItem( "Results", tabName = "section-4", icon = icon("external-link-alt")),
                  menuItem( "Discussion", tabName = "section-4", icon = icon("user-friends"))
      )
    ),
    
    
    
    dashboardBody(useShinyjs(), useShinyalert(),
                  tabItems(
                    # id = "tabs",
                    tabItem(tabName = "section-1",
                            h2("Welcome!"),
                            h4("Thank you for your participation in this exercise."), #'<br/>',
                            h5("Please fill out every section completely and wait for your instructor before moving on to the next section"),
                            h5("After recieving and entering the code from your instructor the next section will become available."),
                            # actionButton("submit", "Submit"),
                            # tableOutput('SummaryTable'),
                            # plotOutput('RadarSummary'),
                            fluidRow(
                              # fluidPage(
                              #   box(
                              #     h5("Click button if you would like to change your selection."),
                              #     actionButton("show", "Censent Form"),
                              #     textOutput("ConsentTxt")
                              #     )
                              #   ),
                              
                              box(
                                title = "Your Demographics", status="primary", solidHeader = TRUE, collapsible = TRUE,
                                select_School(id="self_school", wording="your"),
                                select_Class(id="self_class", wording="your"),
                                select_year(id="self_year", wording="your"),
                                select_gender(id="self_gender", wording="your"),
                                select_Orientation(id="self_orientation", wording="your"),
                                select_age(id="self_age", wording="your"),
                                select_SES(id = "self_SES", wording="your"),
                                select_ED(id = "self_ED", wording="your"),
                                select_Race(id = "self_Race", wording="your"),
                                select_marital(id = "self_marital", wording="your"),
                                select_able(id = "self_able", wording="your")
                                
                              ),
                              
                              fluidPage(
                                Password_UI(id = "PWD_sec1")
                              )
                            )
                            
                            
                    ),
                    
                    tabItem(tabName = "section-2",
                            h2("Select the size of your advice network"),
                            h4("You are faced with a challenge, it is the biggest challenge you have ever faced. in order to solve it you reach out to those whom you trust the most, your inner circle. Who are those people?"),
                            h4("You must list at least 7, but if possible try to list up to 10. List them in order of preference, you may toggle the slider at the top of the page to add or eliminate the number of people listed."),
                            box(
                              title = "Select the amount of people you regularly seek advice from:",
                              sliderInput("amt", "", min = 7, max=10, value = 7),
                              br(),
                              text_influ(personNum = 1),
                              text_influ(personNum = 2),
                              text_influ(personNum = 3),
                              text_influ(personNum = 4),
                              text_influ(personNum = 5),
                              text_influ(personNum = 6),
                              text_influ(personNum = 7),
                              text_influ(personNum = 8),
                              text_influ(personNum = 9),
                              text_influ(personNum = 10)
                            ),
                            fluidPage(
                              Password_UI(id = "PWD_sec2")
                            )
                    ),
                    
                    tabItem(tabName = "section-3",
                            h2("Please fill out the following demographic questions about each person you listed. "),
                            br(),
                            fullBox(linkNM = "influ1"),
                            fullBox(linkNM = "influ2"),
                            fullBox(linkNM = "influ3"),
                            fullBox(linkNM = "influ4"),
                            fullBox(linkNM = "influ5"),
                            fullBox(linkNM = "influ6"),
                            fullBox(linkNM = "influ7"),
                            fullBox(linkNM = "influ8"),
                            fullBox(linkNM = "influ9"),
                            fullBox(linkNM = "influ10"),
                            fluidPage(
                              Password_UI(id = "PWD_sec3")
                            )
                            ),
                    
                    # tabItem(tabName = "section-4",
                    #         h2("Show the homophily of your network"),
                    #         h4("How similar is your network?"),
                    #         
                    #         fluidRow(
                    #           plotOutput("BarSummary"),
                    #           # plotOutput('RadarSummary'),
                    #           tableOutput('SummaryTable')
                    #         ),
                    #         fluidPage(
                    #           Password_UI(id = "PWD_sec4")
                    #         )
                    # ),
                    
                    tabItem(tabName = "section-4",
                            h4("Thank you for your participation in this exercise."), #'<br/>',
                            h5("We would like to now gather some feedback from you so that we can improve upon this exercise in future classes"),
                            selectizeInput(inputId = "circle-bad", 
                                          label= HTML("<strong>Feedback Question 1:</strong> <br>", paste0(" A trusted circle is a bad thing.")),
                                          width = "1000px",
                                          choices=list("True", "False"),
                                          options = Placeholder_list
                            ),
                            textAreaInput(inputId = "define-bias", 
                                          label= HTML("<strong>Feedback Question 2:</strong> <br>", paste0(" how would you define implicit bias?")),
                                          width = "1000px"
                            ),
                            selectizeInput(inputId = "produce-bias", 
                                          label= HTML("<strong>Feedback Question 3:</strong> <br>", paste0(" Implicit bias can be produced based on your trusted circle because....")),
                                          width = "1000px",
                                          choices=list("those poeple do not have your best intrests at heart", 
                                                       "everyone has implicit biases",
                                                       "we all have blind spots, and so we are limited by the views experiences and perceptions of our circles",
                                                       "none of the above"),
                                          options = Placeholder_list
                            ),
                            textAreaInput(inputId = "diversify", 
                                          label= HTML("<strong>Feedback Question 4:</strong> <br>", paste0(" The best way to diversify your trusted circle is...")),
                                          width = "1000px"
                            ),
                            
                            
                            
                            textAreaInput(inputId = "like-activity", 
                                          label= HTML("<strong>Feedback Question 5:</strong> <br>", paste0(" What did you like about this activity?")),
                                          width = "1000px"
                            ),
                            textAreaInput(inputId = "learn-activity", 
                                          label= HTML("<strong>Feedback Question 6:</strong> <br>", paste0(" What did you learn from this activity?")),
                                          width = "1000px"
                            ),
                            textAreaInput(inputId = "change-activity", 
                                          label= HTML("<strong>Feedback Question 7:</strong> <br>", paste0(" What would you change about this activity?")),
                                          width = "1000px"
                            ),
                            selectizeInput(inputId = "overall-activity", 
                                           label= HTML("<strong>Question 8:</strong> <br>", paste0(" Overall I liked this activity:")),
                                           choices=list("Strongly agree", "Agree", "Somewhat agree", "Somewhat disagree", "Disagree", "Strongly disagree"),
                                           options = Placeholder_list
                            )
                    )
                    
                  )),
    footer = dashboardFooter(left_text = leftText,
                             right_text = rightText)
  )
    

# Reactive functions ----
server = function(input, output, session) {
  
  # Return the UI for a modal dialog with data selection input. If 'failed' is
  # TRUE, then display a message that the previous value was invalid.
  dataModal <- function(failed = FALSE) {
    modalDialog(
      selectInput("consent", label= "Do you consent to the use of your data being used for research purposes",
                    choices= c("Yes, you may use my data" = "yes",
                               "No, do not use my data" = "no")
      ),

      footer = tagList(
        modalButton("Submit")
      )
    )
  }
  
  output$ConsentTxt <- renderPrint({
    cat("Thank you for making a selection. You have chosen: ", input$consent)
  })
  
  # Show modal when button is clicked.
  # observeEvent(input$show, {
    # showModal(dataModal())
  # })

  observeEvent(input$show, {
    showModal(dataModal())
  })

  # Create a unique file name, only one per session so that it will overwrite with user changes 
  session.id <- reactive({ 
      sprintf(
      "%s_%s.rds",
      as.integer(Sys.Date()),
      as.character(floor(runif(1)*1e20)) 
    )
    })

  # When whenever the user changes the tab they are on, save the data
  observeEvent(input$sidebarmenu, {
    saveData(input, session.id())
  })
  
  summaryData <- reactive({
    # code to build the output.
    req(input$sidebarmenu)

    test <-
      readRDS(here::here("responses",session.id())) %>% 
      # readr::read_csv(here::here("responses",session.id())) %>%  #"18326_74194495053961871360.rds"
      mutate_all(as.character) %>%
      pivot_longer(col=everything(), "fields", "values") %>%
      separate("fields", into = c("prefix", "type")) %>%
      mutate(InfluID = case_when(str_detect(prefix,"influ")  ~ gsub("influ", "", prefix), #stringr::str_sub(prefix,start=-1),
                                 TRUE ~ ""),
             InfluID = as.numeric(InfluID))

    influAMT <-
      test %>%
      filter(prefix == "amt") %>%
      mutate(value = as.numeric(value)) %>%
      pull(value)

    test %>%
      filter(InfluID <= influAMT,
             !is.na(InfluID)) %>%
      left_join(.,
                test %>% filter(prefix == "self"),
                by = c("type")) %>%
      mutate(match = ifelse(value.x == value.y, 1, 0)) %>%
      ungroup()
    
      # group_by(type) %>%
      # summarise(count = n(),
      #           Matches = sum(match)) %>%
      # ungroup() %>%
      # mutate(PerMatch = Matches/count)

  })

  output$SummaryTable <- renderTable({
    SummTbl <- summaryData()
    SummTbl %>% 
      group_by(type) %>%
      summarise(count = n(),
                Matches = sum(match)) %>%
      ungroup() %>%
      mutate(PerMatch = Matches/count)
  })
  
  # output$RadarSummary <- renderPlot({
  #   RadTbl <- summaryData()
  #   
  #   library(ggradar)
  #   
  #   RadTbl %>% 
  #     group_by(type) %>%
  #     summarise(count = n(),
  #               Matches = sum(match)) %>%
  #     ungroup() %>%
  #     mutate(PerMatch = Matches/count) %>% 
  #     select(-Matches) %>%
  #     pivot_wider(names_from = "type", values_from= "PerMatch") %>% 
  #     ggradar()
  #     
  #    
  # })

  output$BarSummary <- renderPlot({
    BarTbl <- summaryData()
    
    BarTbl %>% 
      group_by(type, value.x, match) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(match = factor(match,levels = c(0,1),
                            labels = c("Doesn't Match", "Matches"))) %>% 
      ggplot(aes(value.x, n, fill=match))+
      geom_bar(stat="identity", color='black')+ 
      facet_grid(.~type, scales="free_x")+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 30, hjust =1),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position="bottom")+ #, hjust = 1
      labs(x="", y="Count in category",
           title= "Your Network's Characteristics")+
      viridis::scale_fill_viridis(discrete=TRUE) +
      NULL
    
  })
  
  
  #Disable menuitem when the app loads
  sections <- list("section-2", "section-3", "section-4", "section-5")
  
  addCssClass(selector = "a[data-value='section-2']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='section-3']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='section-4']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='section-5']", class = "inactiveLink")
  
  
  # Set passwords for each screen
  entPW <- callModule(Password, id = "PWD_sec1")
  callModule(mod_server, id = "PWD_sec1", password = 100, 
             section = "a[data-value='section-2']", enteredPW=entPW, moveTo="section-2", parent=session)
  
  observeEvent(input$btn, {
    req(input$self_gender, input$self_age)

  })
  
  # callModule(mod_server, id = "PWD_sec1", password = 100, 
  #            section = "a[data-value='section-2']", enteredPW=entPW, moveTo="section-2", parent=session)
  
  entPW2 <- callModule(Password, id = "PWD_sec2")
  callModule(mod_server, id = "PWD_sec2", password = 200, 
             section = "a[data-value='section-3']", enteredPW=entPW2, moveTo="section-3", parent=session)
  
  entPW3 <- callModule(Password, id = "PWD_sec3")
  callModule(mod_server, id = "PWD_sec3", password = 300, 
             section = "a[data-value='section-4']", enteredPW=entPW3, moveTo="section-4", parent=session)
  
  entPW4 <- callModule(Password, id = "PWD_sec4")
  callModule(mod_server, id = "PWD_sec4", password = 400, 
             section = "a[data-value='section-5']", enteredPW=entPW4, moveTo="section-5", parent=session)
  
  output$influ1_nm <- renderText({
    paste0("Demographics for: ",input$influ1)
  })
 
  output$influ2_nm <- renderText({
    paste0("Demographics for: ",input$influ2)
  })
  
  output$influ3_nm <- renderText({
    paste0("Demographics for: ",input$influ3)
  })
  
  output$influ4_nm <- renderText({
    paste0("Demographics for: ",input$influ4)
  })
  
  output$influ5_nm <- renderText({
    paste0("Demographics for: ",input$influ5)
  })
  
  output$influ6_nm <- renderText({
    paste0("Demographics for: ",input$influ6)
  })
  
  output$influ7_nm <- renderText({
    paste0("Demographics for: ",input$influ7)
  })
  
  output$influ8_nm <- renderText({
    paste0("Demographics for: ",input$influ8)
  })

  output$influ9_nm <- renderText({
    paste0("Demographics for: ",input$influ9)
  })
  
  output$influ10_nm <- renderText({
    paste0("Demographics for: ",input$influ10)
  })
  
  ## observe the slide change and show/hide the different inputs
  
  observeEvent(input$amt, {
    shinyjs::hide(id = "influ1")
    shinyjs::hide(id = "outer-influ1")
    shinyjs::hide(id = "influ2")
    shinyjs::hide(id = "outer-influ2")
    shinyjs::hide(id = "influ3")
    shinyjs::hide(id = "outer-influ3")
    shinyjs::hide(id = "influ4")
    shinyjs::hide(id = "outer-influ4")
    shinyjs::hide(id = "influ5")
    shinyjs::hide(id = "outer-influ5")
    shinyjs::hide(id = "influ6")
    shinyjs::hide(id = "outer-influ6")
    shinyjs::hide(id = "influ7")
    shinyjs::hide(id = "outer-influ7")
    shinyjs::hide(id = "influ8")
    shinyjs::hide(id = "outer-influ8")
    shinyjs::hide(id = "influ9")
    shinyjs::hide(id = "outer-influ9")
    shinyjs::hide(id = "influ10")
    shinyjs::hide(id = "outer-influ10")
      
    if(input$amt  == 1){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1") }
    if(input$amt  == 2){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") }
    if(input$amt  == 3){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") 
      shinyjs::show(id = "influ3") 
      shinyjs::show(id = "outer-influ3") }
    if(input$amt  == 4){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") 
      shinyjs::show(id = "influ3") 
      shinyjs::show(id = "outer-influ3")    
      shinyjs::show(id = "influ4")
      shinyjs::show(id = "outer-influ4") }
    if(input$amt  == 5){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") 
      shinyjs::show(id = "influ3") 
      shinyjs::show(id = "outer-influ3")    
      shinyjs::show(id = "influ4")
      shinyjs::show(id = "outer-influ4")
      shinyjs::show(id = "influ5")
      shinyjs::show(id = "outer-influ5")}
    if(input$amt  == 6){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") 
      shinyjs::show(id = "influ3") 
      shinyjs::show(id = "outer-influ3")    
      shinyjs::show(id = "influ4")
      shinyjs::show(id = "outer-influ4")
      shinyjs::show(id = "influ5")
      shinyjs::show(id = "outer-influ5")    
      shinyjs::show(id = "influ6")
      shinyjs::show(id = "outer-influ6")}
    if(input$amt  == 7){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") 
      shinyjs::show(id = "influ3") 
      shinyjs::show(id = "outer-influ3")    
      shinyjs::show(id = "influ4")
      shinyjs::show(id = "outer-influ4")
      shinyjs::show(id = "influ5")
      shinyjs::show(id = "outer-influ5")    
      shinyjs::show(id = "influ6")
      shinyjs::show(id = "outer-influ6")
      shinyjs::show(id = "influ7")
      shinyjs::show(id = "outer-influ7")}
    if(input$amt  == 8){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") 
      shinyjs::show(id = "influ3") 
      shinyjs::show(id = "outer-influ3")    
      shinyjs::show(id = "influ4")
      shinyjs::show(id = "outer-influ4")
      shinyjs::show(id = "influ5")
      shinyjs::show(id = "outer-influ5")    
      shinyjs::show(id = "influ6")
      shinyjs::show(id = "outer-influ6")
      shinyjs::show(id = "influ7")
      shinyjs::show(id = "outer-influ7")
      shinyjs::show(id = "influ8")
      shinyjs::show(id = "outer-influ8")}
    if(input$amt  == 9){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") 
      shinyjs::show(id = "influ3") 
      shinyjs::show(id = "outer-influ3")    
      shinyjs::show(id = "influ4")
      shinyjs::show(id = "outer-influ4")
      shinyjs::show(id = "influ5")
      shinyjs::show(id = "outer-influ5")    
      shinyjs::show(id = "influ6")
      shinyjs::show(id = "outer-influ6")
      shinyjs::show(id = "influ7")
      shinyjs::show(id = "outer-influ7")
      shinyjs::show(id = "influ8")
      shinyjs::show(id = "outer-influ8")
      shinyjs::show(id = "influ9")
      shinyjs::show(id = "outer-influ9")}
    if(input$amt  == 10){
      shinyjs::show(id = "influ1")
      shinyjs::show(id = "outer-influ1")
      shinyjs::show(id = "influ2")
      shinyjs::show(id = "outer-influ2") 
      shinyjs::show(id = "influ3") 
      shinyjs::show(id = "outer-influ3")    
      shinyjs::show(id = "influ4")
      shinyjs::show(id = "outer-influ4")
      shinyjs::show(id = "influ5")
      shinyjs::show(id = "outer-influ5")    
      shinyjs::show(id = "influ6")
      shinyjs::show(id = "outer-influ6")
      shinyjs::show(id = "influ7")
      shinyjs::show(id = "outer-influ7")
      shinyjs::show(id = "influ8")
      shinyjs::show(id = "outer-influ8")
      shinyjs::show(id = "influ9")
      shinyjs::show(id = "outer-influ9")
      shinyjs::show(id = "influ10")
      shinyjs::show(id = "outer-influ10")}
    
  })


  
  output$table <- renderDataTable(iris)
   
}

shinyApp(ui, server)