library(shiny)
library(tidyverse)

theme_set(theme_bw(base_size = 15))

load("../../data/clean/shiny-data.RData")
label.choices <- unique(shinydat$credential_title)

ui <- fluidPage(
  selectInput("cred_input",
              "Credential: ",
              label.choices),

  plotOutput("data", height = "900px"),
  tableOutput("table")
)

server <- function(input, output){

  output$data <- renderPlot({
    cur <- shinydat %>% filter(credential_title == input$cred_input, n.maj.qtr >= 10)

    ggplot(data = cur, aes(x = o, y = ckey, label = sprintf("%0.2f", round(per.course.in.qtr, digits = 2)))) +
      geom_tile(aes(fill = 100*cur$per.course.in.qtr)) +
      geom_text(fontface = 'bold') +
      scale_y_discrete(limits = rev(unique(sort(cur$ckey)))) +
      scale_x_continuous(breaks = cur$o, labels = cur$term) +
      scale_fill_viridis_c(direction = -1, option = "C", begin = .5) +
      ylab("Course") +
      xlab("Quarter") +
      labs(caption = paste("n students in major:", max(cur$n.maj)),
           fill = "% in quarter",
           title = paste(unique(cur$program_title)),
           subtitle = paste(unique(cur$credential_title))) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_line(size = .5, color = 'gray80'))
  })

  output$table <- renderTable(
    shinydat %>%
      filter(credential_title == input$cred_input) %>%
      group_by(ckey) %>%
      summarize(n = max(n.maj.class)) %>%
      filter(n >= 10) %>%
      arrange(desc(n))
  )

}

shinyApp(ui, server)
