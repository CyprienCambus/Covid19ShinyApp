

material_page(
  
  title = span("Covid Analysis", style = "color: black; font-size: 32px; font-family: fantasy"),
  list(
    tags$head(
      HTML('<link rel="icon" href="masque-medical.png" 
                type="image/png" />'))),
  nav_bar_color = "cyan darken-1",
  # nav_bar_fixed = TRUE,
  # include_fonts = TRUE,
  # Place side-nav in the beginning of the UI
  material_side_nav(
    fixed = TRUE, 
    image_source = "img/virus.jpg",
    # Place side-nav tabs within side-nav
    br(), br(), br(),br(), br(),
    material_side_nav_tabs(
      side_nav_tabs = c(
        "Dashboard" = "dash",
        "Vaccination in France" = "vaccine",
        "References" = "contact"
      ),
      icons = c("dashboard", "local_hospital", "message")
    )
  ),
  # Define side-nav tab content
  material_side_nav_tab_content(
    side_nav_tab_id = "dash",
    tags$br(),
    material_row(
      material_column(
        width = 10,
        offset = 1,
        material_row(
          material_column(
            width = 6,
            material_dropdown(
              input_id = "country_select",
              multiple = TRUE,
              label = "Country",
              selected = c('france', "espagne"),
              choices = pays_habitant$pays,
              color = "blue"
            )
          ),
          material_column(
            width = 3,
            material_date_picker("from", "From", value = "Feb 20, 2020", color = "blue")
          ),
          material_column(
            width = 3,
            material_date_picker("to", "To", value = "Nov 30, 2021", color = "blue")
          )
          ),
        material_row(
          material_column(width = 4, 
                          material_switch("par_hab_switch", on_label = "Per 100k population", initial_value = FALSE, color = "blue")
                          ),
          material_column(width = 4, 
                          material_switch("ma_switch", on_label = "Moving average", initial_value = FALSE, color = "blue")
                          ),
          material_column(width = 4, 
                          conditionalPanel(condition = "input.ma_switch == 1", 
                                           material_slider("ma_parameters", "Number of days for moving average", min_value = 2, max_value =10,step_size = 1, initial_value = 2, color = "blue"))
                                           )
                          
        )
        ),
        material_row(
          material_column(
            width = 6,
            material_card(
              title = "Daily cases",
              echarts4rOutput("nb_cases")
            )
          ),
          material_column(
            width = 6,
            material_card(
              title = "Daily deaths",
              echarts4rOutput("nb_morts")
              
            )
          )
        ),
      material_row(
        material_column(width = 3,
                        material_card(
                          title = "Cumulative proportion of cases",
                          echarts4rOutput("wave")
                          
                        )),
        material_column(width = 5,
                        material_card(
                          title = "Distribution of cases",
                          echarts4rOutput("pie")
                          
                        )),
        material_column(width = 4,
                        material_card(
                          title = "Number of death among selected countries",
                          echarts4rOutput("gauge")
                          
                        ))
      )
      )
    )
  ,
  material_side_nav_tab_content(
    side_nav_tab_id = "vaccine",
    tags$br(),
    material_row(
      material_column(
        width = 10,
        offset = 1,
        material_row(
          material_column(
            width = 1, offset = 1,
            material_checkbox(
              input_id = "all_vac",
              label = "All vaccine",
              initial_value = FALSE,
              color = "#ef5350"
            )),
          material_column(width = 3, offset = 1, 
                          conditionalPanel(condition = "input.all_vac == 0", 
                                           material_dropdown(
                                             input_id = "vac_select",
                                             multiple = TRUE,
                                             label = "Vaccine",
                                             selected = vac_selection[c(1,2)],
                                             choices = vac_selection,
                                             color = "blue"
                                           )
                                           )
                          )
            
          ,
          material_column(
            width = 3, offset = 1, 
            material_date_picker("date_vac", "Date", value = "Nov 20, 2021", color = "blue")
          ), 
          material_column(width =  2, conditionalPanel(condition = "input.all_vac == 0", 
                                              material_dropdown(
                                                input_id = "dose_choose",
                                                multiple = FALSE,
                                                label = "Dose",
                                                selected = "First dose",
                                                choices = c("First dose", "Second dose", "Third dose"),
                                                color = "blue"
                                              )
          ))),
        material_row(
          material_column(
            width = 6,
            material_card(
              title = "Number of people vaccinated",
              echarts4rOutput("bar_chart_plot")
            )
          ),
          material_column(
            width = 6,
            material_card(
              title = "Vaccine repartition",
              echarts4rOutput("pie_vac")
              
            )
          )
        ),
        material_row(
          material_column(
            width = 12,
            material_card(
              title = "Cases/Vaccination",
              echarts4rOutput("graph_compare")
            )
          )
        )
        )
      )
  ),
  material_side_nav_tab_content(
    side_nav_tab_id = "contact",
    material_row(
      material_column(
        width = 4,
        offset = 1,
        br(),
        git_refs()
      )
    )
  )
 
)