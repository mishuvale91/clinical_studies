source("helpers.R")

# GRÁFICOS

data_clean1<-data.table(read_excel('Base/data_clean1.xlsx'))

# VARIABLES CATEGÓRICAS - TIPO FACTOR

data_clean1$Status<-as.factor(data_clean1$Status)
data_clean1$Gender<-as.factor(data_clean1$Gender)
data_clean1$Funded.Bys<-as.factor(data_clean1$Funded.Bys)
data_clean1$Study.Type<-as.factor(data_clean1$Study.Type)
data_clean1$Study.Results<-as.factor(data_clean1$Study.Results)
data_clean1$Phases<-as.factor(data_clean1$Phases)


Categorical.Variables = c("Status","Gender","Funded.Bys","Study.Type","Study.Results","Phases")

Numeric.Variables = c("Enrollment","Age")

# ================================================= PARA MAPA===========================================================================================

data<-data.table(read_excel('Base/data_clean.xlsx'))

data$Enrollment<-as.numeric(data$Enrollment)
data[is.na(data)]<-0
data$Status<-toupper(data$Status)
data$Study.Type<-toupper(data$`Study Type`)
data$Study.Results<-toupper(data$`Study Results`)
data$X<-data$Enrollment
fact_Study<-data[!duplicated(Study.Type),Study.Type]
factu_Res<-data[!duplicated(Study.Results),Study.Results]
fact_Status<-data[!duplicated(Status),Status]

# ======================================================================================

res<-data.table(read_excel('Base/relaciones.xlsx'))
cio<-res[order(-Enrollment),-c(3,5,6,7,8,9)]
df.long <- pivot_longer(cio, cols=4:13, names_to = "Name_City", values_to = "Country")

duto<-data.table(na.omit(df.long))
dtop<-duto[,by=.(NCT.Number),.(Frec=.N)] 

diooo<-dtop[ order(-Frec) ,]
dutiopw<-diooo[1:20,]
dato_final<-duto[NCT.Number %in% dutiopw$NCT.Number,-c(4)]
dato_final$X=dato_final$Enrollment


shinyApp(
  ui = dashboardPage(
    title = "Clinical Studies",
    fullscreen = TRUE,
    header = dashboardHeader(
      title = dashboardBrand(
        title = "Estudios clínicos",
        color = "maroon",
        # href = "https://clinicaltrials.gov/",
        image = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTPvutgbzc002Fgd4nlfGEpk9mdf5K3vf2aow&usqp=CAU"
      ),
      skin = "light",
      status = "secondary",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE
    ),

## SIDEBAR============================================================================================================
    
  sidebar = dashboardSidebar(
      skin = "light",
      status = "maroon",
      elevation = 3,
      sidebarUserPanel(
        image = "",
        name = ""
      ),
      sidebarMenu(
        sidebarHeader(h3("Menú")),
        menuItem(
          "Salud - Cáncer de mama",
          tabName = "Intro",
          icon = icon("user-nurse")
        ),
        menuItem(
          "Mapa - Inscritos",
          tabName = "map",
          icon = icon("map")
        ),
        menuItem(
          "Mapa - Relaciones",
          tabName = "map2",
          icon = icon("map")
        ),
        
        menuItem(
          "Visualización",
          tabName = "viz",
          icon = icon("chart-bar")
        ),
        menuItem(
          "Predicción",
          tabName = "pred",
          icon = icon("chart-line")
        ),
        menuItem(
          "Datos",
          tabName = "dAT",
          icon = icon("database")
        ),
        menuItem(
          "Sobre mi",
          tabName = "about",
          icon = icon("id-card")
        ),
        menuItem(
          "Guía de usuario",
          tabName = "contest",
          icon = icon("thumbs-up")
        )
      )
    ),

## FOOTER ==================================================================================================== 

    footer = dashboardFooter(
      left = a(
        href = "https://github.com/mishuvale91/clinical_studies",
        target = "_blank", "@mvalenzuelasan"
      ),
      right = "2022"
    ),

## BODY ================================================================================================================
    body = dashboardBody(
    tabItems(
      
## CONTEXT =============================================================================================================

    tabItem(
        tabName = "Intro",
          fluidRow(
            box(
                 width = 12,
                 inputId = "data_card",
                 title = "Sobre Estudios Clínicos",
                 status = "secondary",
                 solidHeader = TRUE,
                 collapsible = FALSE,
                 collapsed = FALSE,
                 closable = FALSE,
                 includeMarkdown("mds/intro.md")
               )
           )
      ),


tabItem(
      tabName = "about",
         fluidRow(
            box(
              width = 12,
              inputId = "about",
              title = "Sobre mi",
              status = "secondary",
              solidHeader = TRUE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              includeMarkdown("mds/about.md")
            )
        )
),

tabItem(
  tabName = "contest",
  fluidRow(
    box(
      width = 12,
      inputId = "contest",
      title = "Guía de usuario",
      status = "secondary",
      solidHeader = TRUE,
      collapsible = FALSE,
      collapsed = FALSE,
      closable = FALSE,
      includeMarkdown("mds/manual.md")
    )
  )

),

## DATA================================================================================================================

    tabItem(
      tabName = "dAT",
      fluidRow(
        
## DESCRIPCION DATA
        
        box(
          width = 12,
          inputId = "datainfo_card",
          title = "Descripción conjunto de datos",
          status = "secondary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          closable = FALSE,
          includeMarkdown("mds/dataset.md")
        ),
        
        # box(
              # width = 12,
              # inputId = "data_card",
              # title = "Estudios clínicos",
              # status = "lightblue",
              # solidHeader = FALSE,
              # collapsible = FALSE,
              # collapsed = FALSE,
              # closable = FALSE,
        column(4,
           selectInput("status",
                       "Status:",
                       c("all",
                         unique(as.character(indice_muestreado$Status))))
               ),
        column(4,
               selectInput("country",
                           "Country:",
                           c("all",
                             unique(as.character(indice_muestreado$Country))))
        ),
        column(4,
               selectInput("study_type",
                           "Study Type:",
                           c("all",
                             unique(as.character(indice_muestreado$Study_Type))))
                 )
              ),
        DT::dataTableOutput("table")
          ),

## VISUALIZACIÓN ======================================================================================================

  tabItem(
    tabName = "viz",
    fluidRow(

## DESCRIPCION PLOTS

     box(
       width = 12,
       inputId = "description_plots",
       title = "Descripción de Gráficos",
       status = "secondary",
       solidHeader = TRUE,
       collapsible = TRUE,
       collapsed = FALSE,
       closable = FALSE,
       includeMarkdown("mds/viz.md")
      ),

## VISUALIZACIÓN
     box(
       width = 12,
       inputId = "v_cuant",
       title = "Gráficos estadísticos",
       status = "secondary",
       solidHeader = TRUE,
       collapsible = FALSE,
       collapsed = FALSE,
       closable = FALSE,
       fluidRow(column(2,
                       selectInput("categorical_variable", label = h6("Seleccionar variable categórica:"), choices = Categorical.Variables)
                       
                       ,selectInput("numeric_variable", label = h6("Seleccionar variable numérica:"), choices = Numeric.Variables)
                       
       ),column(5,plotlyOutput('plot5')),column(5,plotlyOutput('plot6'))))
                 )
              ),

## PREDICTION========================================================================================================

    tabItem(
       tabName = "pred",
       
## DESCRIPCION PREDICTION

         box(
           width = 12,
           inputId = "description_prediction",
           title = "Descripción de la Predicción",
           status = "secondary",
           solidHeader = TRUE,
           collapsible = TRUE,
           collapsed = FALSE,
           closable = FALSE,
           includeMarkdown("mds/prediction.md")
            ),
## PLOT PREDICTION

             fluidRow(
              box(
              width = 12,height = '500%',
              inputId = "das",
              title = "Análisis de Correspondencia Simple - AC Model",
              status = "secondary",
              solidHeader = TRUE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              plotOutput('reg')
            ),
            box(
                   width = 12,height = '500%',
                   inputId = "solu",
                   title = "Resultado",
                   status = "secondary",
                   collapsible = FALSE,
                   collapsed = FALSE,
                   closable = FALSE,
                   textOutput('int'),solidHeader = T,align="justify"
            )
          )
        ),

## MAPA
        tabItem(
          tabName = "map",
          fluidRow(
            box(
              div(fluidRow(column(4,awesomeCheckboxGroup('sel1','Estado',status = "danger",choices=data[!duplicated(Status),Status],selected=data[!duplicated(Status),Status][1])),
                           column(4,awesomeCheckboxGroup('sel2','Tipo de Estudio',status = "danger",choices=data[!duplicated(Study.Type),Study.Type],selected=data[!duplicated(Study.Type),Study.Type][1])),
                           column(4,awesomeCheckboxGroup('sel3','Resultado de Estudio',status = "danger",choices=data[!duplicated(Study.Results),Study.Results],selected=data[!duplicated(Study.Results),Study.Results][1])))),
              
               width = 12,height = '500%',
               inputId = "data_map",
               title = "Mapa del Mundo",
               status = "danger",
               solidHeader = FALSE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               highcharter::highchartOutput('mpa1', height = 500)
                          )  
                        )
                     ),
        tabItem(
          tabName = "map2",
          fluidRow(box(div(align='left',fluidRow(column(10,selectInput('sel5','Título del estudio clínico',choices=dato_final[!duplicated(NCT.Number),Title],selected = dato_final[!duplicated(NCT.Number),Title][1])))),
               width = 12,height = '500%',
               inputId = "data_map2",
               title = "Mapa del Mundo",
               status = "danger",
               solidHeader = FALSE,
               collapsible = FALSE,
               collapsed = FALSE,
               closable = FALSE,
               highcharter::highchartOutput('mpa2',height = 600)
                              )
                           )  
                      )

                  )
            )
),

## SERVER ===========================================================================================================

 server = function(input, output) {

## DATA=================================================================================================================
   
   output$table <- DT::renderDataTable(DT::datatable({
       # class = 'display',
       # rownames = FALSE,
       # filter = 'top',
       # options = list(
       # scrollX = TRUE,
       # pageLength = 10
       
     data1 <- indice_muestreado
     
       if (input$status!="all"){
         data1 <-data1[data1$Status == input$status,]
       }
       if (input$country!="all"){
           data1 <-data1[data1$Country == input$country,]
       }
       if (input$study_type!="all"){
         data1 <-data1[data1$Study_Type == input$study_type,]
       }
       data1
      }))
   
# ## GRAFICOS
# 
   output$plot5<- renderPlotly({
     plot_ly(data_clean1,
             x = ~data_clean1[[input$numeric_variable]],
             color = ~data_clean1[[input$categorical_variable]],
             colors = "YlOrRd",
             type = "box") %>%
       layout(title = "",
              xaxis = list(title = "" ,
                           zeroline = FALSE))
   })
   
   output$plot6<- renderPlotly({
     data_clean1 %>%
       count(var = data_clean1[[input$categorical_variable]], name = "count") %>%
       plot_ly( x = ~var, y = ~ count, type = "bar", marker = list(color = "rgba(255, 0, 0, 0.6)",
                                                                   line = list(color = "rgb(20, 20, 20)", width = 2)), hoverinfo = "x+y") %>%
       add_text(text = ~paste0( " (",   scales::percent(count/sum(count)),")"),
                textposition = "bottom",
                textfont = list(size = 12, color = "white"),
                showlegend = FALSE) %>%
       layout(xaxis = list(title = ""), yaxis = list(title = ""))
     
   })
   
## PREDICTION -------------------------------------------------------------
   
## 
   output$reg <- renderPlot({
     
     pred<-data.table(read_xlsx("Base/prediction.xlsx"))
     
     pred$anio<-as.numeric(pred$anio)
     pred$Age<-as.numeric(pred$Age)
     pred$Enrollment<-rescale(as.numeric(pred$Enrollment))
     pred<-pred[!Gender=='Sin Registro',]
     pred$Gender<-as.factor(pred$Gender)
     
     pred<-na.omit(pred)
     pred<-pred[,-c(1,6)]
     dato_pred<-pred
     
     lal<-pred[,-c(2)]
     dado<-lal[,by=.(anio,Gender),.(Total=sum(Enrollment))]
     piv<-data.frame(dado %>%  pivot_wider(names_from = Gender,values_from = Total))
     piv$anio<-as.character(piv$anio)
     piv<-na.omit(piv)
     piv$Total<-rescale((piv$Total))
     #piv[is.na(piv)]<-0
     rownames(piv)<-piv[,1]
     piv<-piv[,-c(1)]
     Newtab1=ca(piv)
     
     gr=data.frame(Newtab1$colnames,Newtab1$colcoord[,1],Newtab1$colcoord[,2],Newtab1$colmass)
     colnames(gr)=c("Mes","Dim1","Dim2","Size")
    
     fviz_ca_biplot(Newtab1, repel = TRUE, labelsize = 5, pointsize = 2,col.row='black') +
       scale_y_continuous(limits = c(-0.1, 0.1), breaks = seq(-1, 1, by = 0.1))+
       scale_x_continuous(limits = c(-1, 1), breaks = seq(-1, 1, by = 0.1))+
       theme_classic()
     
   })
   
   output$int <- renderText({
     print('
            
            Se puede observar que en el presente estudio solo tomando dos variables se obtiene el 99.9% de variabilidad de los datos, la variable de respuesta es Enrollment, 
            en la gráfica se puede observar el comportamiento de Enrollment en los años 2016, 2003, 2008, 2019 tiene mayor similitud con el género "Female" lo que significa que en dichos años 
            existe una gran cantidad de individuos con género femenino que se enlistaron, asimismo se puede observar que en el 2018 hubo mayor cantidad de individuos enlistados del género "all" (tanto masculino como femenino), 
            y finalmente se evidencia que la categoría "male" de la variable género no es explicada en ningun año por lo cual no representa una variabilidad y significancia en la data.')
   })

# PREPARACIÓN DATA PARA EL MAPA
   
   dato_def<-reactive({
     
     sel1=ifelse(length(input$sel1)!=0,input$sel1,fact_Status)
     
     sel2=ifelse(length(input$sel2)!=0,input$sel2,fact_Study)
     sel3=ifelse(length(input$sel3)!=0,input$sel3,factu_Res)
     
     datos2n<-data[Status %in% sel1 & Study.Type %in%  sel2 & Study.Results %in% sel3,]
     
     datos2n$Country[datos2n$Country == "United States"] <- "United States of America"
     return(datos2n)
   })
   
   dato_graf2<-reactive({
    
     datuiiiiiia<-dato_final[Title==input$sel5,]
     datuiiiiiia$Country[datuiiiiiia$Country == "United States"] <- "United States of America"
     
     return(datuiiiiiia)
   })
   
# MAPA - NUMERO DE INSCRITOS EN LOS ESTUDIOS CLINICOS
   
   output$mpa1 <- highcharter::renderHighchart({
     datos=dato_def()
     mapdata<-data(worldgeojson, package = "highcharter")
     highchart(type = "map") %>%
       hc_add_series_map(worldgeojson,datos,value="X",joinBy =c('name','Country'), showInLegend = F,
                         name='', dataLabels = list(enabled = TRUE, format = "{point.name}")) %>% 
       hc_title(text = "<b>Número de inscritos en los estudios clínicos de Cáncer de Mama</b> <br></br>",
                margin = 20, align = "center",
                style = list(color = "#585f6e", useHTML = TRUE)) %>% 
       hc_subtitle(text = " ",
                   align = "center",
                   style = list(color = "#0C5C9E", fontWeight = "bold")) %>% 
       #hc_tooltip(followPointer =  FALSE,valueSuffix = "") %>%
       hc_legend(align = "center", x = 0, y = -30) %>%
       hc_mapNavigation(enabled = TRUE) %>%
       hc_add_theme(hc_theme_ffx()) %>% hc_colorAxis(minColor = "#ffe0d9", maxColor = "#fc1258",max=3000) %>% 
       highcharter::hc_chart(borderColor = "#768094",borderRadius = 10,borderWidth = 2) %>% 
       highcharter::hc_tooltip(crosshairs = F, shared = F, headerFormat = "",useHTML=T,
                               pointFormat='<b><i style="font-family: Lobster, cursive; font-size: 15px;">{point.name}</b></i><br><b>Enrollment: </b>{point.value}')
     
   })

# MAPA DE RELACIONES POR PAÍSES
   
   output$mpa2 <- highcharter::renderHighchart({
     datos2<-dato_graf2()
     mapdata<-data(worldgeojson, package = "highcharter")
     highchart(type = "map") %>%
       hc_add_series_map(worldgeojson,datos2,value="X",joinBy =c('name','Country'), showInLegend = F,
                         name='', dataLabels = list(enabled = TRUE, format = "{point.name}")) %>% 
       hc_title(text = "<b>Mapa de relaciones por estudio clínico</b> <br></br>",
                margin = 20, align = "center",
                style = list(color = "#585f6e", useHTML = TRUE)) %>% 
       hc_subtitle(text = " ",
                   align = "center",
                   style = list(color = "#0C5C9E", fontWeight = "bold")) %>% 
       #hc_tooltip(followPointer =  FALSE,valueSuffix = "") %>%
       hc_legend(align = "center", x = 0, y = -30) %>%
       hc_mapNavigation(enabled = TRUE) %>%
       hc_add_theme(hc_theme_ffx()) %>% hc_colorAxis(minColor = "#ffe0d9", maxColor = "#fc1258",max=1000) %>% 
       highcharter::hc_chart(borderColor = "#768094",borderRadius = 10,borderWidth = 2) %>% 
       highcharter::hc_tooltip(crosshairs = F, shared = F, headerFormat = "",useHTML=T,
                               pointFormat='<b><i style="font-family: Lobster, cursive; font-size: 15px;">{point.name}</b></i><br><b>Enrollment: </b>{point.value}')
     
   })
   
 }
)
