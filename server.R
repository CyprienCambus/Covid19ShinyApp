



server <- function(input, output, session) {
  
  
  # output$housing_plot_error <- renderUI({
  #   if(input$from_year >= input$to_year){
  #     tags$span(
  #       style = "color:red; font-weight:bold",
  #       paste0('"From Year" (', input$from_year, ') must be before "To Year" (', input$to_year, ')')
  #     )
  #   } else {
  #     NULL
  #   }
  # })
  
  


  
  data_reac <- reactive({
    out <- dataf %>% filter(nom %in% input$country_select, date >=format(as.Date(mdy(input$from), origin = "1970-01-01"), "%Y-%m-%d") & date <= format(as.Date(mdy(input$to), origin = "1970-01-01"), "%Y-%m-%d")) 
    return(out)
  })
  
  
  
  output$nb_cases <- renderEcharts4r({
    
    

    
    if (input$ma_switch == FALSE){
      ts_base <-  data_reac() %>%
        e_charts(x = date) %>% 
        e_datazoom(
          type = "slider", 
          toolbox = FALSE,
          bottom = -5
        ) %>% 
        e_tooltip() %>% 
        e_x_axis(date, axisPointer = list(show = TRUE))
      
      if (input$par_hab_switch == FALSE){
        ts_base %>% e_line(nb_cas)
      }else{
        ts_base %>% e_line(nb_cas_100) %>% e_title("Per 100k population")
      }

      
      
    }else{
      
      if (input$par_hab_switch == FALSE){
        
        ts_base <- data_reac() %>% mutate(ma_nb_cas = stats::filter(nb_cas, rep(1,input$ma_parameters)/input$ma_parameters)) %>%
          e_charts(x = date) %>% 
          e_datazoom(
            type = "slider", 
            toolbox = FALSE,
            bottom = -5
          ) %>% 
          e_tooltip() %>% 
          e_x_axis(date, axisPointer = list(show = TRUE))
        
        
        
        ts_base %>% e_line(ma_nb_cas)
        
      }else{
        
        ts_base <- data_reac() %>% mutate(ma_nb_cas_100 = stats::filter(nb_cas_100, rep(1,input$ma_parameters)/input$ma_parameters)) %>%
          e_charts(x = date) %>% 
          e_datazoom(
            type = "slider", 
            toolbox = FALSE,
            bottom = -5
          ) %>% 
          e_tooltip() %>% 
          e_x_axis(date, axisPointer = list(show = TRUE))
        
        
        
        ts_base %>% e_line(ma_nb_cas_100) %>% e_title("Per 100k population")
        
      }
      
      
      
      
    }
  })
    
    
  output$nb_morts <- renderEcharts4r({
    
    
    
    
    if (input$ma_switch == FALSE){
      ts_base <-  data_reac() %>%
        e_charts(x = date) %>% 
        e_datazoom(
          type = "slider", 
          toolbox = FALSE,
          bottom = -5
        ) %>% 
        e_tooltip() %>% 
        e_x_axis(date, axisPointer = list(show = TRUE))
      
      if (input$par_hab_switch == FALSE){
        ts_base %>% e_line(nb_deces)
      }else{
        ts_base %>% e_line(nb_deces_100) %>% e_title("Per 1M population")
      }
      
      
      
    }else{
      
      if (input$par_hab_switch == FALSE){
        
        ts_base <- data_reac() %>% mutate(ma_nb_deces = stats::filter(nb_deces, rep(1,input$ma_parameters)/input$ma_parameters)) %>%
          e_charts(x = date) %>% 
          e_datazoom(
            type = "slider", 
            toolbox = FALSE,
            bottom = -5
          ) %>% 
          e_tooltip() %>% 
          e_x_axis(date, axisPointer = list(show = TRUE))
        
        
        
        ts_base %>% e_line(ma_nb_deces)
        
      }else{
        
        ts_base <- data_reac() %>% mutate(ma_nb_deces_100 = stats::filter(nb_deces_100, rep(1,input$ma_parameters)/input$ma_parameters)) %>%
          e_charts(x = date) %>% 
          e_datazoom(
            type = "slider", 
            toolbox = FALSE,
            bottom = -5
          ) %>% 
          e_tooltip() %>% 
          e_x_axis(date, axisPointer = list(show = TRUE))
        
        
        
        ts_base %>% e_line(ma_nb_deces_100) %>% e_title("Per 1M population")
        
      }
      
      
      
      
    }
  })
  
  data_percent <- reactive({
    
    tmp <- dataf %>% filter(nom != "europe")
    base <- sum(tmp$nb_cas, na.rm = TRUE)
    tmp1 <- data_reac() %>% filter(nom != "europe")
    valeur <- sum(tmp1$nb_cas, na.rm = TRUE)
    
    df_out <- data.frame(val = c(round(valeur/base, 2), round(valeur/base, 2)/2, round(valeur/base, 2)/4))
    
    return(df_out)
  })
  
  
  output$wave <- renderEcharts4r({
    
    data_percent() %>% 
      e_charts() %>% 
      e_liquid(val)
    
  })
  
  
  data_pie <- reactive({
    
    dataf %>% filter(nom %in% input$country_select & nom != "europe") %>% select(nom, nb_cas) %>%
      group_by(nom) %>% summarise(total_cas = sum(nb_cas, na.rm = TRUE))
    
  })
  
  
  output$pie <- renderEcharts4r({
    
    
    data_pie() %>%
      head() %>% 
      e_charts(nom) %>% 
      e_pie(total_cas, roseType = "radius") %>%  e_labels(show = TRUE,
                                                          formatter = "{c} \n {d}%",
                                                          position = "outside") %>%
      e_legend(right = 0, 
               orient = "vertical")
    
    
    
  })
  
  
  data_gauge <- reactive({
    tmp <- dataf %>% filter(nom %in% input$country_select & nom != "europe")
    tmp1 <- pays_habitant %>% filter(pays %in% input$country_select & pays != "europe")
    return(round(sum(tmp$nb_deces, na.rm = TRUE)/sum(tmp1$habitants)*10000))
  })
  
  
  output$gauge <- renderEcharts4r({
    
    e_charts() %>% 
      e_gauge(data_gauge(), "Per 10k pop")
    
    
  })
  
  
  
  
  
  data_bar <- reactive({
    
    tmp <- df_vac %>% filter(jour == format(as.Date(mdy(input$date_vac), origin = "1970-01-01"), "%Y-%m-%d")) %>% select(vaccin_type, n_cum_dose1, n_cum_dose2, n_cum_dose3)
    
    b <-as.data.frame(t(tmp))  
    
    colnames(b) <- b[1,]
    b <- b[-1,]
    
    d <- cbind(DOSE = rownames(b), b)
    
    d$DOSE <- c("First dose", "Second dose", "Third dose")
    
    
    d$Pfizer <- as.numeric(d$Pfizer)
    d$Moderna <- as.numeric(d$Moderna)
    d$AstraZeneka <- as.numeric(d$AstraZeneka)
    d$Janssen <- as.numeric(d$Janssen)
    d$All <- as.numeric(d$All)
    d <- d[order(d$Pfizer),]
    
    
    return(d)
  })

  
  output$bar_chart_plot <- renderEcharts4r({
  
    
    if (input$all_vac == TRUE){
      
      data_bar() %>% 
        e_charts(DOSE) %>%
        e_bar(All, stack = "grp") %>%
        e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
    }else{
      
      if(input$vac_select == "Pfizer" && length(input$vac_select) == 1){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Pfizer, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (input$vac_select == "Moderna" && length(input$vac_select) == 1){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Moderna, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (input$vac_select == "Janssen" && length(input$vac_select) == 1){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Janssen, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (input$vac_select == "AstraZeneka" && length(input$vac_select) == 1){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(AstraZeneka, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Pfizer", "Moderna")) == 2 && length(input$vac_select) == 2){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Pfizer, stack = "grp") %>%
          e_bar(Moderna, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
        
      }else if (sum(input$vac_select %in% c("Pfizer", "Janssen")) == 2 && length(input$vac_select) == 2){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Pfizer, stack = "grp") %>%
          e_bar(Janssen, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Pfizer", "AstraZeneka")) == 2 && length(input$vac_select) == 2){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Pfizer, stack = "grp") %>%
          e_bar(AstraZeneka, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Pfizer", "Moderna", "Janssen")) == 3 && length(input$vac_select) == 3){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Pfizer, stack = "grp") %>%
          e_bar(Janssen, stack = "grp") %>%
          e_bar(Moderna, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Pfizer", "Moderna", "AstraZeneka")) == 3 && length(input$vac_select) == 3){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Pfizer, stack = "grp") %>%
          e_bar(AstraZeneka, stack = "grp") %>%
          e_bar(Moderna, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Pfizer", "Janssen", "AstraZeneka")) == 3 && length(input$vac_select) == 3){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Pfizer, stack = "grp") %>%
          e_bar(AstraZeneka, stack = "grp") %>%
          e_bar(Janssen, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Pfizer", "Moderna", "Janssen", "AstraZeneka")) == 4 && length(input$vac_select) == 4){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Pfizer, stack = "grp") %>%
          e_bar(Janssen, stack = "grp") %>%
          e_bar(Moderna, stack = "grp") %>%
          e_bar(AstraZeneka, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Moderna", "Janssen")) == 2 && length(input$vac_select) == 2){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Moderna, stack = "grp") %>%
          e_bar(Janssen, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Moderna", "AstraZeneka")) == 2 && length(input$vac_select) == 2){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Moderna, stack = "grp") %>%
          e_bar(AstraZeneka, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("Moderna", "Janssen", "AstraZeneka")) == 3 && length(input$vac_select) == 3){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(Moderna, stack = "grp") %>%
          e_bar(Janssen, stack = "grp") %>%
          e_bar(AstraZeneka, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }else if (sum(input$vac_select %in% c("AstraZeneka", "Janssen")) == 2 && length(input$vac_select) == 2){
        data_bar() %>% 
          e_charts(DOSE) %>%
          e_bar(AstraZeneka, stack = "grp") %>%
          e_bar(Janssen, stack = "grp") %>%
          e_flip_coords() %>% e_labels(show = FALSE) %>% e_title(paste("At", input$date_vac, sep = " ")) %>% e_y_axis(axisLabel = list(rotate = 45))
      }
  
      
    }

    
  })
  
  
  data_pie_2 <- reactive({
    
    if (input$all_vac == FALSE){
      
      tmp <- df_vac %>% filter(jour == format(as.Date(mdy(input$date_vac), origin = "1970-01-01"), "%Y-%m-%d")) %>% filter(vaccin != 0) %>% filter(vaccin_type %in% input$vac_select)
      
    }else{
      
      tmp <- df_vac %>% filter(jour == format(as.Date(mdy(input$date_vac), origin = "1970-01-01"), "%Y-%m-%d")) %>% filter(vaccin != 0)
      
    }
    return(tmp)
    
    
  })
  

  output$pie_vac <- renderEcharts4r({
    
    if (input$dose_choose == "Third dose"){
      data_pie_2() %>% 
        e_charts(vaccin_type) %>%
        e_pie(n_cum_dose3, radius = c("50%", "70%")) %>%  e_labels(show = TRUE,formatter = "{c} \n {d}%",position = "outside") %>%
        e_legend(right = 0, 
                 orient = "vertical") %>% e_title("Third dose repartition")
      
    }else if(input$dose_choose == "Second dose"){
      
      data_pie_2() %>% 
        e_charts(vaccin_type) %>%
        e_pie(n_cum_dose2, radius = c("50%", "70%")) %>%  e_labels(show = TRUE,formatter = "{c} \n {d}%",position = "outside") %>%
        e_legend(right = 0, 
                 orient = "vertical") %>% e_title("Second dose repartition")
      
    }else{
      
      data_pie_2() %>% 
        e_charts(vaccin_type) %>%
        e_pie(n_cum_dose1, radius = c("50%", "70%")) %>%  e_labels(show = TRUE,formatter = "{c} \n {d}%",position = "outside") %>%
        e_legend(right = 0, 
                 orient = "vertical") %>% e_title("First dose repartition")
      
    }
    
    
  })
  
  
  data_compare <- reactive({
    
    tmp_france <- dataf %>% filter(nom == "france") %>% mutate(jour = date)
    tmp_vac <- df_vac %>% filter(vaccin == 0)
    
    out <- tmp_france %>% inner_join(., tmp_vac, by = 'jour') %>% mutate(vacs = -n_dose1/10) %>% filter(jour <= format(as.Date(mdy(input$date_vac), origin = "1970-01-01"), "%Y-%m-%d"))
    
    return(out)
    
  })

  
  output$graph_compare <- renderEcharts4r({
    
    data_compare() %>% e_charts(jour) %>%
      e_area(nb_cas, name = "Number of cases") %>%
      e_bar(vacs, name = "Number of vaccinated x 10^(-1)", x_index = 1) %>% # second y axis 
      e_mark_line("Sick basterd", data = list(type = "average"))
    
    
  })

  

  
  
  
}