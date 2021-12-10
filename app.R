source("helpers.R")

shinyApp(
  ui = dashboardPage(
    title = "Basic Dashboard",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "Estudios clínicos",
        color = "purple",
        href = "https://clinicaltrials.gov/",
        image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTPvutgbzc002Fgd4nlfGEpk9mdf5K3vf2aow&usqp=CAU"
      ),
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE
      # leftUi = tagList(
      #     dropdownMenu(
      #         badgeStatus = "info",
      #         type = "notifications",
      #         notificationItem(
      #             inputId = "triggerAction2",
      #             text = "Error!",
      #             status = "danger"
      #         )
      #     ),
      #     dropdownMenu(
      #         badgeStatus = "info",
      #         type = "tasks",
      #         taskItem(
      #             inputId = "triggerAction3",
      #             text = "My progress",
      #             color = "orange",
      #             value = 10
      #         )
      #     )
      # ),
      # rightUi = dropdownMenu(
      #     badgeStatus = "danger",
      #     type = "messages",
      #     messageItem(
      #         inputId = "triggerAction1",
      #         message = "message 1",
      #         from = "Divad Nojnarg",
      #         image = "https://adminlte.io/themes/v3/dist/img/user3-128x128.jpg",
      #         time = "today",
      #         color = "lime"
      #     )
      # )
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      status = "purple",
      elevation = 3,
      sidebarUserPanel(
        image = "",
        name = "Salud - Cáncer de mama"
      ),
      sidebarMenu(
        sidebarHeader("Menú"),
        menuItem(
          "Datos",
          tabName = "dAT",
          icon = icon("database")
        ),
        menuItem(
          "Visualización",
          tabName = "item2",
          icon = icon("chart-bar")
        ),
        menuItem(
          "Predicción",
          tabName = "item3",
          icon = icon("chart-line")
        ),
        menuItem(
          "Mapa",
          tabName = "item4",
          icon = icon("map")
        )
      )
    ),
    footer = dashboardFooter(
      left = a(
        href = "https://www.linkedin.com/in/mvalenzuelasan",
        target = "_blank", "@mvalenzuelasan"
      ),
      right = "2022"
    ),

## BODY ================================================================================================================
    body = dashboardBody(
    tabItems(
      
## CONTEXT =============================================================================================================

    tabItem(
      tabName = "data",
      fluidRow(
        column(width = 6,
               dashboard::dashboardCard(
                 width = 12,
                 inputId = "data_card",
                 title = "About Clinical",
                 status = "info",
                 solidHeader = FALSE,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 closable = FALSE,
                 includeMarkdown("mds/dataset.md")
               )
           )
      )
    
  ),

## DATA=================================================================================================================
    
    tabItem(
      tabName = "data",
      fluidRow(
        dashboard::dashboardCard(
          width = 12,
          inputId = "datainfo_card",
          title = "Data Information",
          status = "info",
          solidHeader = FALSE,
          collapsible = TRUE,
          collapsed = TRUE,
          closable = FALSE,
          includeMarkdown("mds/dataset.md")
        ),
        
        dashboard::dashboardCard(
          width = 12,
          inputId = "data_card",
          title = "Clinical studies",
          status = "info",
          solidHeader = FALSE,
          collapsible = FALSE,
          collapsed = FALSE,
          closable = FALSE,
          DT::dataTableOutput("data_clean5")
                )
              )
           )
        )
     )
),

 server = function(input, output) {

## DATA=================================================================================================================
  
   output$data_clean5 <- DT::renderDataTable({
     data("Clinical_Studies")
     
     DT::datatable(
       Cinical_Studies,
       class = 'cell-border stripe',
       rownames = FALSE, 
       filter = 'top',
       options = list(
         scrollX = TRUE,
         pageLength = 25  
       )
     )
   })
 }

)