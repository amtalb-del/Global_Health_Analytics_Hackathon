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
      menuItem("Costs", tabName = "costs", icon = icon("coins")),
      menuItem("Population", tabName = "population", icon = icon("people-group")),
      menuItem("Disease Burden", tabName = "disease", icon = icon("prescription-bottle-medical")),
      menuItem("Funding", tabName = "funding", icon = icon("hands-holding-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Funding tab content
      tabItem(tabName = "costs",
              fluidRow(
                box(plotlyOutput("subactivity_cost_pie")),
              )
      ),
      
      # Population tab content
      tabItem(tabName = "population",
              h2("Widgets tab content")
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
      
      # Costs tab content
      tabItem(tabName = "funding",
              tabsetPanel(
                tabPanel(
                  title="Activities",
                  column(width=12,
                         plotOutput("financing_bar"),
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
  output$subactivity_cost_pie <- renderPlotly({
    subactivity_df %>% 
      group_by(`Name of Activity`) %>% 
      summarize(mean=mean(`Cost of Subactivity in GNF`)) %>%
      plot_ly(labels = ~`Name of Activity`, values = ~mean, type = 'pie') %>% 
      layout(title = 'Cumulative Cost by Activity',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = FALSE)
  })
  output$target_population_map <- renderPlot({
    spdf <- geojson_read("data/gadm41_GIN_2.json",  what="sp")
    spdf_fortified <- tidy(spdf)
    spdf_fortified$id <- spdf_fortified$id %>% as.factor()
    
    mapping <- read_csv("data/guinea_region_mapping.csv", col_types="ccff")
    join_df <- target_population_df %>% left_join(. , mapping, by=(c("District"="subregion")))
    spdf_fortified <- spdf_fortified %>% left_join(. , join_df, by=c("id"="subregion_id"))
    
    # fill NA with 0
    spdf_fortified[[toString(input$condition)]][ is.na(spdf_fortified[[toString(input$condition)]]) ] = 0.0001
    
    ggplot() + 
      geom_polygon(data = spdf_fortified, aes(fill = .data[[toString(input$condition)]], x = long, y = lat, group = group), size=0, alpha=0.9) + 
      theme_void() + 
      scale_fill_viridis(breaks = pretty_breaks(), 
                         name=paste("Number of", input$condition," targeted per district"), 
                         guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1),
                         option = "C",
                         labels=comma) +
      labs(
        title = "Guinea Target Population",
        subtitle = paste("Number of", input$condition," targeted per district")
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
  output$financing_bar <- renderPlot({
    sum_var <- case_when(input$cost_funding == "Costs" ~ "Cost of Subactivity in GNF",
                       input$cost_funding == "Funding" ~ "Amount of Finance Recevied",
                       input$cost_funding == "Gap" ~ "Gap")
    group_var <- case_when(input$aggregation == "Activity" ~ "Name of Activity",
                     input$aggregation == "Sub-activity" ~ "Name of Subactivity")
    
    financing_activities_df %>%
      group_by(!!sym(group_var)) %>% 
      summarize(sum_var=sum(!!sym(sum_var))) %>%
      ggplot(aes(x=!!sym(group_var), y=sum_var)) +
        geom_segment( aes(x=!!sym(group_var), 
                          xend=!!sym(group_var), 
                          y=1, 
                          yend=sum_var), color="grey") +
        geom_point( color="orange", size=4) +
        theme_light() +
        xlab("") +
        ylab("") +
        coord_flip() +
        scale_y_continuous(label=comma) +
        scale_x_discrete(label = function(x) stringr::str_trunc(x, 30)) +
        labs(
          title = input$cost_funding,
          subtitle = paste("Aggregated by", input$aggregation)
        ) +
        theme(
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank(),
          text = element_text(color = "#22211d",size=14),
          plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
          plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        )
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
}

shinyApp(ui, server)