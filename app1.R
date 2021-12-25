source("helpers.R")

shiny::shinyApp(
  ui = bs4DashPage(
    old_school = TRUE,
    sidebar_min = TRUE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = TRUE,
    controlbar_overlay = TRUE,
    title = "pum!",
    
    ## NAVBAR ----------------------------------------------------------------------
    
    navbar = bs4DashNavbar(
      skin = "dark",
      status = "gray-light",
      border = TRUE,
      sidebarIcon = "bars",
      controlbarIcon = "th",
      fixed = FALSE
    ),
    
    ## SIDEBAR ----------------------------------------------------------------------
    
    sidebar = bs4DashSidebar(
      skin = "dark",
      status = "warning",
      title = "pum!",
      brandColor = "warning",
      url = "https://github.com/pcastellanoescuder/PUM",
      src = "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ab/Florence_Nightingale_%28H_Hering_NPG_x82368%29.jpg/1024px-Florence_Nightingale_%28H_Hering_NPG_x82368%29.jpg",
      elevation = 3,
      opacity = 0.8,
      bs4SidebarMenu(
        bs4SidebarMenuItem(
          "Florence Nightingale",
          tabName = "florence",
          icon = "user-nurse"
        ),
        bs4SidebarMenuItem(
          "The Data",
          tabName = "data",
          icon = "database"
        ),
        bs4SidebarMenuItem(
          "Visualization",
          tabName = "viz",
          icon = "chart-bar"
        ),
        bs4SidebarMenuItem(
          "Inference",
          tabName = "inference",
          icon = "not-equal"
        ),
        bs4SidebarMenuItem(
          "Prediction",
          tabName = "prediction",
          icon = "chart-line"
        ),
        bs4SidebarMenuItem(
          "About Us",
          tabName = "about",
          icon = "id-card"
        ),
        bs4SidebarMenuItem(
          "R-Ladies Contest",
          tabName = "contest",
          icon = "thumbs-up"
        )
      )
    ),
    
    ## CONTROLBAR ----------------------------------------------------------------------
    
    # controlbar = bs4DashControlbar(),
    
    ## FOOTER ----------------------------------------------------------------------
    
    footer = bs4DashFooter(
      copyrights = a(
        href = "https://twitter.com/martabofillr",
        target = "_blank", "@martabofillr"
      ),
      a(
        href = "https://twitter.com/polcastellano_",
        target = "_blank", "@polcastellano_"
      ),
      a(
        href = "https://twitter.com/G_Villacampa",
        target = "_blank", "@G_Villacampa"
      ),
      right_text = "2020, MIT License"
    ),
    
    ## BODY ----------------------------------------------------------------------
    
    body = bs4DashBody(
      bs4TabItems(
        
        ## CONTEXT ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "florence",
          fluidRow(
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "florence_card",
                     title = "About Florence",
                     status = "info",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/florence.md")
                   ),
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "crimean_card",
                     title = "Crimean War",
                     status = "danger",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/crimean_war.md")
                   )
            ),
            
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "contributions_card",
                     title = "Contributions",
                     status = "success",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/contributions.md")
                   )
            )
          )
        ),
        
        ## DATA ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "data",
          fluidRow(
            bs4Dash::bs4Card(
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
            bs4Dash::bs4Card(
              width = 12,
              inputId = "data_card",
              title = "Florence Nightingale Data",
              status = "info",
              solidHeader = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              DT::dataTableOutput("florence_data")
            )
          )
        ),
        
        ## VIZ ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "viz",
          
          fluidRow(
            
            ## PLOT DESCRIPTION ----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "description_plots_card",
              title = "Description of Plots",
              status = "light",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              includeMarkdown("mds/viz.md")
            ),
            
            ## FLORENCE PLOTS -----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "florence_plots_card",
              title = "Florence Nightingale Plots",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              plotOutput("florence_plots")
            ),
            
            ## CUMULATIVE PLOTS -----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "cum_plots_card",
              title = "Cumulative Causes of Death",
              status = "danger",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              prettySwitch("val", "Number of deaths", fill = TRUE, status = "danger"),
              plotOutput("cum_plots")
            ),
            
            ## CORRELATION PLOTS -----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "cor_plots_card",
              title = "Correlations",
              status = "warning",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              prettySwitch("val2", "Number of deaths", fill = TRUE, status = "warning"),
              plotOutput("cor_plot")
            ),
            
            ## BARPLOTS -----------------------------------------------------
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "bar_plots_card",
              title = "Barplots",
              status = "success",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              prettySwitch("val3", "Number of deaths", fill = TRUE, status = "success"),
              plotOutput("bar_plot")
            ) # here
          )
        ),
        
        ## INFERENCE ------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "inference",
          fluidRow(
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "description_inf_card",
              title = "Analysis Information",
              status = "light",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              includeMarkdown("mds/inference.md")
            ),
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "inf1_card",
              title = "Risk Difference",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              selectizeInput("pre1", "Month in the period pre-healthy measures:", 
                             choices = c("Apr 1854", "May 1854", "Jun 1854", "Jul 1854", "Aug 1854", "Sep 1854", "Oct 1854", "Nov 1854", "Dec 1854",
                                         "Jan 1855", "Feb 1855"),
                             selected = "Apr 1854"),
              selectizeInput("post1", "Month in the period post-healthy measures:", 
                             choices = c("Mar 1855", "Apr 1855", "May 1855", "Jun 1855", "Jul 1855", "Aug 1855", 
                                         "Sep 1855", "Oct 1855", "Nov 1855", "Dec 1855", "Jan 1856", "Feb 1856", "Mar 1856"),
                             selected = "Mar 1856"),
              numericInput("alpha1", "alpha", value = 0.025),
              selectizeInput("feat_inf1", "Feature to test", choices = c("Disease", "Wounds", "Other"), selected = "Disease"),
              DT::dataTableOutput("prop")
            ),
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "inf2_card",
              title = "Risk Ratio",
              status = "danger",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              selectizeInput("pre2", "Month in the period pre-healthy measures:", 
                             choices = c("Apr 1854", "May 1854", "Jun 1854", "Jul 1854", "Aug 1854", "Sep 1854", "Oct 1854", "Nov 1854", "Dec 1854",
                                         "Jan 1855", "Feb 1855"),
                             selected = "Apr 1854"),
              selectizeInput("post2", "Month in the period post-healthy measures:", 
                             choices = c("Mar 1855", "Apr 1855", "May 1855", "Jun 1855", "Jul 1855", "Aug 1855", 
                                         "Sep 1855", "Oct 1855", "Nov 1855", "Dec 1855", "Jan 1856", "Feb 1856", "Mar 1856"),
                             selected = "Mar 1856"),
              numericInput("alpha2", "alpha", value = 0.025),
              selectizeInput("feat_inf2", "Feature to test", choices = c("Disease", "Wounds", "Other"), selected = "Disease"),
              DT::dataTableOutput("risk")
            )
          )
        ),
        
        ## PREDICTION ------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "prediction",
          fluidRow(
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "description_pred_card",
              title = "Prediction Information",
              status = "light",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              includeMarkdown("mds/prediction.md")
            ),
            
            bs4Dash::bs4Card(
              width = 12,
              inputId = "pred_card",
              title = "Linear Model",
              status = "info",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              selectizeInput("end", "Month to Predict", 
                             choices = c("May 1854", "Jun 1854", "Jul 1854", "Aug 1854", "Sep 1854", "Oct 1854", "Nov 1854", "Dec 1854",
                                         "Jan 1855", "Feb 1855", "Mar 1855", "Apr 1855", "May 1855", "Jun 1855", "Jul 1855", "Aug 1855", 
                                         "Sep 1855", "Oct 1855", "Nov 1855", "Dec 1855", "Jan 1856", "Feb 1856", "Mar 1856"),
                             selected = "Mar 1856"),
              numericInput("start", "How many previous months do you want to use for prediction?", value = 10),
              selectizeInput("to_pred", "Feature to predict", choices = c("Disease.rate", "Wounds.rate", "Other.rate"), selected = "Disease.rate"),
              prettySwitch("showci", "Show CI", fill = TRUE, status = "info"),
              plotlyOutput("pred_plot")
            ),
            bs4Dash::bs4Card(
              width = 12,
              inputId = "pred2_card",
              title = "Exponential Smoothing Forecast",
              status = "danger",
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = TRUE,
              closable = FALSE,
              selectizeInput("end2", "Month to Predict", 
                             choices = c("May 1854", "Jun 1854", "Jul 1854", "Aug 1854", "Sep 1854", "Oct 1854", "Nov 1854", "Dec 1854",
                                         "Jan 1855", "Feb 1855", "Mar 1855", "Apr 1855", "May 1855", "Jun 1855", "Jul 1855", "Aug 1855", 
                                         "Sep 1855", "Oct 1855", "Nov 1855", "Dec 1855", "Jan 1856", "Feb 1856", "Mar 1856"),
                             selected = "Mar 1856"),
              numericInput("start2", "How many previous months do you want to use for prediction?", value = 10),
              selectizeInput("to_pred2", "Feature to predict", choices = c("Disease.rate", "Wounds.rate", "Other.rate"), selected = "Disease.rate"),
              prettySwitch("showci2", "Show CI", fill = TRUE, status = "danger"),
              plotlyOutput("pred_plot2")
            )
          )
        ),
        
        ## ABOUT ----------------------------------------------------------------------
        
        bs4TabItem(
          tabName = "about",
          fluidRow(
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "marta_card",
                     title = "Marta Bofill Roig",
                     status = "info",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/marta.md")
                   ),
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "pol_card",
                     title = "Pol Castellano Escuder",
                     status = "danger",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/pol.md")
                   )
            ),
            
            column(width = 6,
                   bs4Dash::bs4Card(
                     width = 12,
                     inputId = "guille_card",
                     title = "Guillermo Villacampa Javierre",
                     status = "success",
                     solidHeader = FALSE,
                     collapsible = TRUE,
                     collapsed = TRUE,
                     closable = FALSE,
                     includeMarkdown("mds/guille.md")
                   )
            )
          ),
          fluidRow(
            includeMarkdown("mds/pum_picture.md")
          )
        ),
        
        ## CONTEST ---------------------------------------------------------
        
        bs4TabItem(
          tabName = "contest",
          fluidRow(
            bs4Dash::bs4Card(
              width = 12,
              inputId = "contest_card",
              title = "R-Ladies!",
              status = "info",
              solidHeader = FALSE,
              collapsible = FALSE,
              collapsed = FALSE,
              closable = FALSE,
              includeMarkdown("mds/contest.md")
            )
          )
        )
        
      ) # bs4TabItems
    ) # bs4DashBody
  ),
  
  server = function(input, output, session) {
    
    ## DATA ----------------------------------------------------------------------
    
    output$florence_data <- DT::renderDataTable({
      
      # data <- readxl::read_xlsx("data/datos_florence.xlsx") %>%
      #   janitor::row_to_names(row_number = 1) %>%
      #   janitor::clean_names() %>%
      #   rename_at(vars(zymotic_diseases:all_other_causes), ~ paste0(., "_deaths")) %>%
      #   rename_at(vars(ends_with("_2")), ~ paste0(., "_MR1000")) %>%
      #   rename_at(vars(ends_with("_MR1000")), ~ stringr::str_remove(., "_2")) %>%
      #   mutate_at(vars(average_size_of_army:all_other_causes_MR1000), as.numeric) %>%
      #   mutate(month = stringr::str_replace(month, "_", " ")) %>%
      #   separate(month, into = c("month", "year"), sep = " ")
      
      data("Nightingale")
      
      DT::datatable(
        Nightingale,
        class = 'cell-border stripe',
        rownames = FALSE, 
        filter = 'top',
        options = list(
          scrollX = TRUE,
          pageLength = 25
        )
      )
      
      # save(data, file = "data/proc_data.RData")
      
    })
    
    ## VIZ - FLORENCE PLOTS ----------------------------------------------------------------------
    
    output$florence_plots <- renderPlot({
      
      source("florence_plots.R")
      p
    })
    
    ## VIZ - CUMULATIVE PLOTS ----------------------------------------------------------------------
    
    output$cum_plots <- renderPlot({
      
      if (input$val){
        aux <- Nightingale %>%
          select(Date, Month, Year, Wounds, Disease, Other)
        ylab_name <- "Cumulative number of deaths"
        
      } else {
        aux <- Nightingale %>%
          select(Date, Month, Year, Wounds.rate, Disease.rate, Other.rate)
        ylab_name <- "Cumulative mortality rate per 1000"  
      }
      
      aux <- aux %>%
        pivot_longer(cols = -c(Date, Month, Year)) %>%
        mutate(date = paste0(Month, " ", Year))
      
      ggplot(aux, aes(x = Date, y = value, fill = name)) +
        geom_area() +
        ylab(ylab_name) +
        xlab("") +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis(discrete = TRUE)
      
    })
    
    ## VIZ - CORRELATION PLOTS ----------------------------------------------------------------------
    
    output$cor_plot <- renderPlot({
      
      if (input$val2){
        aux <- Nightingale %>%
          select(Army, Wounds, Disease, Other)
        
      } else {
        aux <- Nightingale %>%
          select(Army, Wounds.rate, Disease.rate, Other.rate)
      }
      
      ggpairs(aux) +
        theme_bw()
      
    })
    
    ## VIZ - BARPLOTS ----------------------------------------------------------------------
    
    output$bar_plot <- renderPlot({
      
      if (input$val3){
        aux <- Nightingale %>%
          mutate(date = paste0(Month, " ", Year)) %>%
          select(Date, date, Wounds, Disease, Other)
        ylab_name <- "Number of deaths"
        
      } else {
        aux <- Nightingale %>%
          mutate(date = paste0(Month, " ", Year)) %>%
          select(Date, date, Wounds.rate, Disease.rate, Other.rate) 
        ylab_name <- "Mortality rate per 1000"  
      }
      
      aux <- aux %>%
        pivot_longer(cols = -c(Date, date)) %>%
        arrange(sort(date))
      
      ggplot(aux, aes(reorder(date, Date), value, fill = name)) +
        geom_col() +
        theme_bw() +
        xlab("") +
        ylab(ylab_name) +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_viridis(discrete = TRUE)
      
    })
    
    ## INFERENCE --------------------------------------------------------------
    
    ## RISK DIFFERENCE -------------------------------------------------------------
    
    output$prop <- DT::renderDataTable({
      
      source("tests.R")
      
      alpha <- input$alpha1
      time1 <- input$pre1
      time2 <- input$post1
      to_anal <- input$feat_inf1
      
      data_inf <- Nightingale %>%
        mutate(date = paste0(Month, " ", Year),
               pDisease = Disease/Army,
               pWounds = Wounds/Army,
               pOther = Other/Army) %>%
        filter(date == time1 | date == time2) %>%
        select_at(vars(contains(to_anal) | contains("Army"))) %>%
        select_at(vars(starts_with("p") | contains("Army"))) %>%
        select(Army, everything())
      
      phat_group0 <- data_inf[1,2]
      phat_group1 <- data_inf[2,2]
      samplesize0 <- data_inf[1,1]
      samplesize1 <- data_inf[2,1]
      
      res <- diffg_p(phat_group1 = phat_group1, phat_group0 = phat_group0, 
                     samplesize0 = samplesize0, samplesize1 = samplesize1, alpha = alpha)
      
      res_table <- data.frame(Prob_pre = round(res$prob0, 3), Prob_post = round(res$prob1, 3), 
                              RiskDifference = round(res$difference, 3), test = round(res$test, 3), Reject_H0 = res$reject)
      
      DT::datatable(
        res_table,
        class = 'cell-border stripe',
        rownames = FALSE)
    })
    
    ## RISK RATIO -------------------------------------------------------------
    
    output$risk <- DT::renderDataTable({
      
      source("tests.R")
      
      alpha <- input$alpha2
      time1 <- input$pre2
      time2 <- input$post2
      to_anal <- input$feat_inf2
      
      data_inf <- Nightingale %>%
        mutate(date = paste0(Month, " ", Year),
               pDisease = Disease/Army,
               pWounds = Wounds/Army,
               pOther = Other/Army) %>%
        filter(date == time1 | date == time2) %>%
        select_at(vars(contains(to_anal) | contains("Army"))) %>%
        select_at(vars(starts_with("p") | contains("Army"))) %>%
        select(Army, everything())
      
      phat_group0 <- data_inf[1,2]
      phat_group1 <- data_inf[2,2]
      samplesize0 <- data_inf[1,1]
      samplesize1 <- data_inf[2,1]
      
      res <- diffg_rr(phat_group1 = phat_group1, phat_group0 = phat_group0, 
                      samplesize0 = samplesize0, samplesize1 = samplesize1, alpha = alpha)
      
      res_table <- data.frame(Prob_pre = round(res$prob0, 3), Prob_post = round(res$prob1, 3), 
                              RiskRatio = round(res$riskratio, 3), test = round(res$test, 3), Reject_H0 = res$reject)
      
      DT::datatable(
        res_table,
        class = 'cell-border stripe',
        rownames = FALSE)
    })
    
    ## PREDICTION -------------------------------------------------------------
    
    ## LINEAR MODEL -----------------------------------------------------------
    
    output$pred_plot <- renderPlotly({
      
      data_pred <- Nightingale %>%
        mutate(date = paste0(Month, " ", Year)) %>%
        rownames_to_column("period")
      
      enter1 <- input$end
      enter2 <- input$start
      to_pred <- input$to_pred
      
      time_pred <- which(data_pred$date == enter1)
      start <- which(data_pred$date == enter1) - enter2
      
      x <- as.numeric(data_pred$period[start:(time_pred-1)])
      y <- data_pred[start:(time_pred-1),]
      y <- y %>%
        select_at(vars(matches(to_pred))) %>%
        pull()
      
      mod <- lm(y ~ x)
      new <- data.frame(x = time_pred)
      pred_linear <- predict(mod, new, interval = "prediction") %>%
        as.data.frame() %>%
        mutate(date = enter1,
               fit = ifelse(fit < 0, 0, fit),
               lwr = ifelse(lwr < 0, 0, lwr))
      
      data_pred <- data_pred[start:(time_pred) ,]
      
      data_pred <- data_pred %>%
        rename_at(vars(matches(to_pred)), ~ "my_var")
      
      p <- ggplot(data_pred) +
        geom_point(aes(x = reorder(date, Date), y = my_var), size = 3) +
        geom_point(data = pred_linear, aes(date, fit), size = 3, color = "red") +
        {if(input$showci)geom_errorbar(data = pred_linear, aes(x = date, y = fit, ymin = upr, ymax = lwr), width = 0.1, color = "red")} +
        xlab("") +
        ylab("Mortality Rate") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
      
    })
    
    ## EXPONENTIAL SMOOTHING -------------------------------------------------
    
    output$pred_plot2 <- renderPlotly({
      
      data_pred <- Nightingale %>%
        mutate(date = paste0(Month, " ", Year)) %>%
        rownames_to_column("period")
      
      enter1 <- input$end2
      enter2 <- input$start2
      to_pred <- input$to_pred2
      
      time_pred <- which(data_pred$date == enter1)
      start <- which(data_pred$date == enter1) - enter2
      
      x <- as.numeric(data_pred$period[start:(time_pred-1)])
      y <- data_pred[start:(time_pred-1),]
      y <- y %>%
        select_at(vars(matches(to_pred))) %>%
        pull()
      
      holt_res <- holt(y, h = 1)
      
      data_pred <- data_pred[start:(time_pred) ,]
      
      data_pred <- data_pred %>%
        rename_at(vars(matches(to_pred)), ~ "my_var")
      
      holt_res2 <- tibble(pred = holt_res$mean[1]) %>%
        mutate(date = enter1,
               upr = holt_res$upper[2],
               lwr = holt_res$lower[2],
               pred = ifelse(pred < 0, 0, pred),
               lwr = ifelse(lwr < 0, 0, lwr))
      
      p <- ggplot(data_pred) +
        geom_point(aes(x = reorder(date, Date), y = my_var), size = 3) +
        geom_point(data = holt_res2, aes(date, pred), size = 3, color = "red") +
        {if(input$showci2)geom_errorbar(data = holt_res2, aes(x = date, y = pred, ymin = upr, ymax = lwr), width = 0.1, color = "red")} +
        xlab("") +
        ylab("Mortality Rate") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggplotly(p)
      
    })
    
  }
  
)