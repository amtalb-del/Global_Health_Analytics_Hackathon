#install.packages("shinydashboard")
#install.packages("plotly")
#install.packages("geodata")

options(shiny.autoreload = TRUE)
source('read_data.R')

## ui.R ##
library(shiny)
library(shinydashboard)
library(plotly)
library(geojsonio)
library(broom)
library(viridis)
library(scales)

## app.R ##
ui <- dashboardPage(
  dashboardHeader(title = "TIPAC"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Program Costs", tabName = "costs", icon = icon("coins")),
      menuItem("Medicine", tabName = "medicine", icon = icon("prescription-bottle-medical")),
      menuItem("Disease Burden", tabName = "disease", icon = icon("virus-covid")),
      menuItem("Activity Cost/Funding", tabName = "funding", icon = icon("hands-holding-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Costs tab content
      tabItem(tabName = "costs",
              fluidRow(
                box(
                  plotlyOutput("transport_cost"),
                  selectInput("transport_type", "Type:", c("Bar", "Pie")),
                  width = 4
                ),
                box(
                  plotlyOutput('personnel_cost'),
                  selectInput("personnel_type", "Type:", c("Bar", "Pie")),
                  width = 4
                ),
                box(
                  plotlyOutput('perdiem_cost'),
                  selectInput("perdiem_type", "Type:", c("Bar", "Pie")),
                  width = 4
                ),
                box(
                  plotlyOutput('total_cost'),
                  selectInput("total_type", "Type:", c("Bar", "Pie")),
                  width = 12
                ),
                box(
                  plotlyOutput('projected_cost'),
                  width=12
                )
              )
      ),
      
      # Medicine tab content
      tabItem(tabName = "medicine",
              fluidRow(
                box(
                  box(
                    align="center",
                    h5("Ivermectin per person"),
                    h2(textOutput('ivm')),
                    width=12
                  ),
                  box(
                    align="center",
                    h5("Albendazole per person"),
                    h2(textOutput('alb')),
                    width=12
                  ),
                  box(
                    align="center",
                    h5("Diethylcarbamizine per person"),
                    h2(textOutput('dec')),
                    width=12
                  ),
                  box(
                    align="center",
                    h5("Praziquantel per person"),
                    h2(textOutput('pzq')),
                    width=12
                  ),
                  width=6
                ),
                box(
                  plotlyOutput("medicine_pie"),
                  width=6
                ),
              )
      ),
      
      # Disease tab content
      tabItem(tabName = "disease",
              tabsetPanel(
                tabPanel(
                  title="Targeted Population",
                  column(width=12,
                         fluidRow(
                            plotOutput("target_population_map"),
                            selectInput("condition", "Condition:", colnames(target_population_df)[3:length(colnames(target_population_df))])
                         )
                  )
                ),
                tabPanel(
                  title="Disease Burden",
                  column(width=12,
                         fluidRow(
                           plotOutput("disease_burden_map"),
                           column(6, selectInput("burden", "Condition:", c("LF","Oncho","SCH","STH","Trachoma"))),
                           column(6, sliderInput("burden_year", "Number of observations:", min(disease_burden_df$year), max(disease_burden_df$year), min(disease_burden_df$year), sep="", ticks = FALSE))
                         )
                  )
                )
              )
      ),
      
      # Funding tab content
      tabItem(tabName = "funding",
              tabsetPanel(
                tabPanel(
                  title="Activities",
                  column(width=12,
                         plotlyOutput("financing_bar"),
                         fluidRow(
                             column(6, radioButtons("cost_funding", "Show: ", choices = c("Costs", "Funding", "Gap"), inline=TRUE)),
                             column(6, radioButtons("aggregation", "Aggregate By: ", choices = c("Activity", "Sub-activity"), inline = TRUE))
                         )
                  )
                ),
                tabPanel(
                  title="Donors",
                  column(width=12,
                         plotlyOutput("donor_bar"),
                         fluidRow(
                           column(12, radioButtons("donor_total", "Show: ", choices = c("Total donated", "By Activity (as % of total donated)"), inline=TRUE)),
                         )
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$transport_cost <- renderPlotly({
    work_days <- other_df$`Number of work days per year`
    if (input$transport_type == 'Pie') {
      hired_vehicles_df %>% 
        mutate(`Driver cost per day\r\n(if applicable)` = case_when(`Driver cost per day\r\n(if applicable)` == 'n/a' ~ 0)) %>%
        mutate(total = `Cost \r\nper day in GNF` + `Driver cost per day\r\n(if applicable)`) %>%
        mutate(total = total * other_df$`Number of work days per year`) %>%
        plot_ly(labels = ~`Vehicle type`, values = ~total, type = 'pie') %>% 
        layout(title = 'Transportation Cost per year',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               showlegend = FALSE)
    } else {
      hired_vehicles_df %>% 
        mutate(`Driver cost per day\r\n(if applicable)` = case_when(`Driver cost per day\r\n(if applicable)` == 'n/a' ~ 0)) %>%
        mutate(total = `Cost \r\nper day in GNF` + `Driver cost per day\r\n(if applicable)`) %>%
        mutate(total = total * other_df$`Number of work days per year`) %>%
        plot_ly(y = ~total, x = ~`Vehicle type`, color=~`Vehicle type`, type="bar") %>%
        layout(title = 'Transportation Cost per year',
               showlegend = F)
    }
  })
  output$personnel_cost <- renderPlotly({
    df <- personnel_costs_df %>%
      mutate(ft_equivalent = if_else(`Full or part-time \r\nNTD staff` == 'Full', `If full-time, how many \r\npeople under this title?`, `If full-time, how many \r\npeople under this title?`/2)) %>%
      mutate(salary = ft_equivalent * `If tracking salary,\r\nAnnual Salary`)
    if (input$personnel_type == "Bar") {
      df %>%
        plot_ly(y = ~salary, x = ~`Personnel title`, color=~`Personnel title`, type="bar") %>%
        layout(title = 'Personnel Costs',
               showlegend = F)
    } else {
      df %>% 
        plot_ly(labels = ~`Personnel title`, values = ~salary, type = 'pie') %>% 
        layout(title = 'Personnel Costs',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               showlegend = FALSE)
    }
  })
  output$perdiem_cost <- renderPlotly({
    if (input$perdiem_type == 'Pie') {
      per_diems_df %>% 
        mutate(`Per diem rate in GNF` = `Per diem rate in GNF` * other_df$`Number of work days per year`) %>%
        plot_ly(labels = ~`Per diem Title`, values = ~`Per diem rate in GNF`, type = 'pie') %>% 
        layout(title = 'Per Diem Cost',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               showlegend = FALSE)
    } else {
      per_diems_df %>% 
        mutate(`Per diem rate in GNF` = `Per diem rate in GNF` * other_df$`Number of work days per year`) %>%
        plot_ly(y = ~`Per diem rate in GNF`, x = ~`Per diem Title`, color=~`Per diem Title`, type="bar") %>%
        layout(title = 'Per Diem Cost',
               showlegend = F)
    }
  })
  output$total_cost <- renderPlotly({
    total_costs <- tribble(
      ~type, ~cost,
      "Transport", hired_vehicles_df %>% 
        mutate(`Driver cost per day\r\n(if applicable)` = case_when(`Driver cost per day\r\n(if applicable)` == 'n/a' ~ 0)) %>%
        mutate(total = `Cost \r\nper day in GNF` + `Driver cost per day\r\n(if applicable)`) %>%
        mutate(total = ifelse(is.na(total), 0, total)) %>%
        mutate(total = total * other_df$`Number of work days per year`) %>%
        select(total) %>%
        sum(),
      "Personnel", personnel_costs_df %>%
        mutate(ft_equivalent = if_else(`Full or part-time \r\nNTD staff` == 'Full', `If full-time, how many \r\npeople under this title?`, `If full-time, how many \r\npeople under this title?`/2)) %>%
        mutate(salary = ft_equivalent * `If tracking salary,\r\nAnnual Salary`) %>%
        select(salary) %>% 
        sum(),
      "Per diem", per_diems_df %>%
        mutate(`Per diem rate in GNF` = `Per diem rate in GNF` * other_df$`Number of work days per year`) %>%
        select(`Per diem rate in GNF`) %>%
        sum()
    )
    if (input$total_type == 'Pie') {
      total_costs %>% 
        plot_ly(labels = ~type, values = ~cost, type = 'pie') %>% 
        layout(title = 'Total Costs',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               showlegend = FALSE)
    } else {
      total_costs %>% 
        plot_ly(x = ~cost, y = ~type, color=~type, type="bar", orientation="h") %>% 
        layout(title = 'Total Costs',
               showlegend = F)
    }
  })
  output$target_population_map <- renderPlot({
    spdf <- geojson_read("data/gadm41_GIN_2.json",  what="sp")
    spdf_fortified <- tidy(spdf)
    spdf_fortified$id <- spdf_fortified$id %>% as.factor()
    
    mapping <- read_csv("data/guinea_region_mapping.csv", col_types="ccff")
    join_df <- target_population_df %>% left_join(. , mapping, by=(c("District"="subregion")))
    spdf_fortified <- spdf_fortified %>% left_join(. , join_df, by=c("id"="subregion_id"))
    spdf_fortified <- spdf_fortified %>% left_join(. , year_pop_df, by=c("Region"="Regions", "District"="Districts"))
    
    # fill NA with 0
    spdf_fortified[[toString(input$condition)]][ is.na(spdf_fortified[[toString(input$condition)]]) ] = 0
    
    ggplot() + 
      geom_polygon(data = spdf_fortified, aes(fill = (.data[[toString(input$condition)]] / .data[["total"]]), x = long, y = lat, group = group), size=0, alpha=0.9) + 
      theme_void() + 
      scale_fill_viridis(breaks = pretty_breaks(), 
                         name=paste("Percent (%)"), 
                         guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1),
                         option = "C",
                         labels=comma) +
      labs(
        title = "Guinea Target Population",
        subtitle = paste(input$condition,"targeted per district")
      ) +
      theme(
        text = element_text(color = "#22211d"),
        plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.text = element_text(size= 11, hjust=0.01, color = "#4e4d47", margin = margin(b = 0.1, unit = "cm")),
        legend.position = c(0.02, 0.09)
      ) +
      coord_map()
  })
  output$financing_bar <- renderPlotly({
    sum_var <- case_when(input$cost_funding == "Costs" ~ "Cost.of.Subactivity.in.GNF",
                       input$cost_funding == "Funding" ~ "Amount.of.Finance.Recevied",
                       input$cost_funding == "Gap" ~ "Gap")
    group_var <- case_when(input$aggregation == "Activity" ~ "Name.of.Activity",
                     input$aggregation == "Sub-activity" ~ "Name.of.Subactivity")
    
    financing_activities_df %>%
      group_by(!!sym(group_var)) %>% 
      summarize(sum_var=sum(!!sym(sum_var))) %>%
      mutate_at(group_var, str_trunc, width=30, side="right") %>%
      plot_ly(x = ~sum_var, y = reformulate(termlabels=group_var), color = reformulate(termlabels=group_var), type="bar", orientation="h") %>% 
      layout(title = input$cost_funding,
             subtitle = paste("Aggregated by", input$aggregation),
             showlegend = F,
             yaxis = list(categoryorder = "total ascending"))
  })
  output$donor_bar <- renderPlotly({
    if (input$donor_total == "Total donated") {
      p <- financing_activities_df %>%
        select(1, 6:15) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        pivot_longer(!`Name of Activity`, names_to = "Donor", values_to = "Amount") %>%
        group_by(Donor) %>%
        summarize(total_donated = sum(Amount)) %>%
        plot_ly(x = ~total_donated, y = ~Donor, type="bar", orientation="h") 
    } else if (input$donor_total == "By Activity (as % of total donated)") {
      p <- financing_activities_df %>%
        select(1, 6:15) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        pivot_longer(!`Name of Activity`, names_to = "Donor", values_to = "Amount") %>%
        group_by(Donor, `Name of Activity`) %>%
        summarize(activity_sum = sum(Amount)) %>%
        group_by(`Donor`) %>%
        mutate(pct=activity_sum/sum(activity_sum)*100) %>%
        plot_ly(x = ~pct, y = ~Donor, color=~`Name of Activity`, type="bar", orientation="h") %>%
        layout(barmode="stack")
    } else {
      p <- financing_activities_df %>%
        select(1, 6:15) %>%
        mutate_all(~replace(., is.na(.), 0)) %>%
        pivot_longer(!`Name of Activity`, names_to = "Donor", values_to = "Amount") %>%
        group_by(Donor) %>%
        summarize(total_donated = sum(Amount)) %>%
        plot_ly(labels = ~Donor, values = ~total_donated, type = 'pie')
    }
    p %>% 
      layout(title = input$donor_total)
  })
  output$disease_burden_map <- renderPlot({
    spdf <- geojson_read("data/gadm41_GIN_2.json",  what="sp")
    spdf_fortified <- tidy(spdf)
    spdf_fortified$id <- spdf_fortified$id %>% as.factor()
    
    mapping <- read_csv("data/guinea_region_mapping.csv", col_types="ccff")
    join_df <- disease_burden_df %>% left_join(. , mapping, by=(c("Districts"="subregion")))
    spdf_fortified <- spdf_fortified %>% left_join(. , join_df, by=c("id"="subregion_id"))
    
    spdf_fortified %>%
      filter(year == input$burden_year) %>%
      filter(field == paste0(input$burden, ".Disease.Burden.Code")) %>%
      ggplot() + 
        geom_polygon(aes(fill = total, x = long, y = lat, group = group), size=0, alpha=0.9) + 
        coord_map() +
        theme_void() + 
        labs(
          title = "Guinea Disease Burden",
          subtitle = input$burden
        ) + 
        theme(
          text = element_text(color = "#22211d"),
          plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
          legend.background = element_rect(fill = "#f5f5f2", color = NA),
          legend.text = element_text(size= 11, hjust=0.01, color = "#4e4d47", margin = margin(b = 0.1, unit = "cm")),
          legend.position = c(0.02, 0.09)
        ) +
        scale_fill_viridis_d(name=paste(input$burden, "Disease Burden Code"), 
                         guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1),
                         option = "C", na.translate=F)
  })
  output$medicine_pie <- renderPlotly({
    medicine_df <- drug_needs_df %>% left_join(. , target_population_df, by=c("region"="Region", "district"="District"))
    ivm_pp <- 
      mean(medicine_df$`IVM - Ivermectin` / medicine_df$`Oncho Round 1`,na.rm=T) +
      mean(medicine_df$`IVM - Ivermectin` / medicine_df$`LF Lymphedema Management`, na.rm=T)
    alb_pp <- 
      mean(medicine_df$`ALB - Albendazole (with IVM or DEC)` / medicine_df$`LF Lymphedema Management`,na.rm=T) +
      mean(medicine_df$`ALB - Albendazole (alone or with PZQ)` / medicine_df$`STH High risk adult`, na.rm=T)
    dec_pp <- 
      mean(medicine_df$`DEC - Diethylcarbamizine` / medicine_df$`LF Lymphedema Management`, na.rm=T)
    pzq_pp <- 
      mean(medicine_df$`PZQ - Praziquantel` / medicine_df$`SCH School Age Children`, na.rm=T)
    
    drugs_df <- tribble(
      ~drug, ~pp,
      "ALB - Albendazole", alb_pp,
      "IVM - Ivermectin", ivm_pp,
      "DEC - Diethylcarbamizine", dec_pp,
      "PZQ - Praziquantel", pzq_pp
    )
    
    drugs_df %>% 
      plot_ly(labels = ~drug, values = ~pp, type = 'pie') %>% 
      layout(title = 'Drug needs per person in target population',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = FALSE)
  })
  output$ivm <- renderText({
    medicine_df <- drug_needs_df %>% left_join(. , target_population_df, by=c("region"="Region", "district"="District"))
    mean(medicine_df$`IVM - Ivermectin` / medicine_df$`Oncho Round 1`,na.rm=T) + mean(medicine_df$`IVM - Ivermectin` / medicine_df$`LF Lymphedema Management`, na.rm=T)
  })
  output$alb <- renderText({
    medicine_df <- drug_needs_df %>% left_join(. , target_population_df, by=c("region"="Region", "district"="District"))
    mean(medicine_df$`ALB - Albendazole (with IVM or DEC)` / medicine_df$`LF Lymphedema Management`,na.rm=T) +
    mean(medicine_df$`ALB - Albendazole (alone or with PZQ)` / medicine_df$`STH High risk adult`, na.rm=T)
  })
  output$dec <- renderText({
    medicine_df <- drug_needs_df %>% left_join(. , target_population_df, by=c("region"="Region", "district"="District"))
    mean(medicine_df$`DEC - Diethylcarbamizine` / medicine_df$`LF Lymphedema Management`, na.rm=T)
  })
  output$pzq <- renderText({
    medicine_df <- drug_needs_df %>% left_join(. , target_population_df, by=c("region"="Region", "district"="District"))
    mean(medicine_df$`PZQ - Praziquantel` / medicine_df$`SCH School Age Children`, na.rm=T)
  })
  output$projected_cost <- renderPlotly({
    transport <- c(hired_vehicles_df %>% 
      mutate(`Driver cost per day\r\n(if applicable)` = case_when(`Driver cost per day\r\n(if applicable)` == 'n/a' ~ 0)) %>%
      mutate(total = `Cost \r\nper day in GNF` + `Driver cost per day\r\n(if applicable)`) %>%
      mutate(total = ifelse(is.na(total), 0, total)) %>%
      mutate(total = total * other_df$`Number of work days per year`) %>%
      select(total) %>%
      sum())
    personnel <- c(personnel_costs_df %>%
        mutate(ft_equivalent = if_else(`Full or part-time \r\nNTD staff` == 'Full', `If full-time, how many \r\npeople under this title?`, `If full-time, how many \r\npeople under this title?`/2)) %>%
        mutate(salary = ft_equivalent * `If tracking salary,\r\nAnnual Salary`) %>%
        select(salary) %>% 
        sum())
    per_diem <- c(per_diems_df %>%
        mutate(`Per diem rate in GNF` = `Per diem rate in GNF` * other_df$`Number of work days per year`) %>%
        select(`Per diem rate in GNF`) %>%
        sum())
    
    for (i in c(2:5)) {
      transport <- append(transport, (transport[i-1] * 0.028) + transport[i-1])
      personnel <- append(personnel, (personnel[i-1] * 0.028) + personnel[i-1])
      per_diem <- append(per_diem, (per_diem[i-1] * 0.028) + per_diem[i-1])
    }
    x <- c(2022:2026)
    
    data <- data.frame(x, transport, personnel, per_diem)
    
    fig <- plot_ly(data, x = ~x, y = ~transport, name = 'Transport', type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~personnel, name = 'Personnel', type = 'scatter', mode = 'lines') 
    fig <- fig %>% add_trace(y = ~per_diem, name = 'Per Diem', type = 'scatter', mode = 'lines')
    
    fig
  })
}

shinyApp(ui, server)