# 1 layout -----------------------------------------------------------------------



ui <- fluidPage(titlePanel("Ejemplo panel"),
                sidebarLayout(
                  sidebarPanel("Panel lateral"),
                  mainPanel("Panel principal")
                ))

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

