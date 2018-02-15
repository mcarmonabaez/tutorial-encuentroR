# 1 layout -----------------------------------------------------------------------



ui <- fluidPage(titlePanel("Ejemplo panel"),
sidebarLayout(
sidebarPanel("Panel lateral"),
mainPanel("Panel principal")
))

server <- function(input, output) {}

shinyApp(ui = ui, server = server)



# 2 layout -----------------------------------------------------------------------

ui <- fluidPage(titlePanel("Ejemplo panel"),
sidebarLayout(
position = "right",
sidebarPanel("Panel lateral"),
mainPanel("Panel principal")
))

server <- function(input, output) {}

shinyApp(ui = ui, server = server)



# 3 imagen-----------------------------------------------------------------------


ui <- fluidPage(titlePanel("Ejemplo imagen"),
sidebarLayout(
sidebarPanel("Panel lateral"),
mainPanel("Panel principal",
br(),
img(src =  "corgi.jpg"))
))

server <- function(input, output) {}

shinyApp(ui = ui, server = server)



# 4 widgets ---------------------------------------------------------------

ui <- fluidPage(h2("Algunos widgets..."),
br(),
sliderInput(inputId = "num",
label = "Número de simulaciones",
min = 1000, max = 10000,
value = 5000),
br(),
dateInput("date",
h5("Ingresa fecha"),
value = "2018-02-15"),
br(),
actionButton("action", "Ejecuta acción"),
br(),
br(),
checkboxGroupInput("checkGroup",
"Seleccionar opción",
choices = list("Opción 1" = 1,
"Opción 2" = 2,
"Opción 3" = 3),
selected = 1))


server <- function(input, output) {}

shinyApp(ui = ui, server = server)



# 5 interaccion --------------------------------------------------------------


ui <- fluidPage(sliderInput(inputId = "num",
label = "Número de simulaciones",
min = 1000, max = 10000,
value = 5000),
plotOutput("hist")
)

server <- function(input, output) {
    output$hist <- renderPlot({ # función de tipo render
        hist(rnorm(input$num)) # el objeto generado depende del input
    })
}

shinyApp(ui = ui, server = server)

### runApp(display.mode = "showcase")






# 6 mapas -----------------------------------------------------------------

# Esta parte del código es fija a lo largo de la app
# --------------------------------------------------
# cargar paquetes
library(shiny)
library(mxmaps)
# datos  a nivel entidad federativa
data("df_mxstate")
# --------------------------------------------------

ui <- fluidPage(
selectInput(inputId = "var", label = "Tipo de población",
choices = list("Total" = "pop",
"Masculina" = "pop_male",
"Femenina" = "pop_female",
"Afromexicana" = "afromexican",
"Indígena" = "indigenous"),
selected = "Total"),
plotOutput("mapa")
)

server <- function(input, output) {
    
    output$mapa <- renderPlot({
        df_mxstate$value <- df_mxstate[, input$var]
        mxstate_choropleth(df_mxstate,
        title = "Tamaño de la población por estado")
    })
    
}

shinyApp(ui = ui, server = server)



# file input --------------------------------------------------------------

library(shiny)
library(readxl)
library(DT)
library(lubridate)


ui <- fluidPage(
sidebarLayout(
sidebarPanel(
fileInput("file1", "Elige archivo xlsx",
accept = c(
".xlsx")
),
tags$hr()
),
mainPanel(
plotOutput("barras_dias"),
DT::dataTableOutput("tabla")

)
)
)

server <- function(input, output) {
    
    datos <- reactive({
        # input$file1 es NULL al principio. Después de seleccionar
        # y subir un arhivo, se convierte en un data frame con las
        # columnas 'name', 'size', 'type', and 'datapath'.
        # En 'datapath' está guardado el nombre del archivo.
        
        inFile <- input$file1
        
        if (is.null(inFile))
        return(NULL)
        
        read_excel(inFile$datapath, sheet = 1)
        
    })
    
    output$tabla <- DT::renderDataTable({
        DT::datatable(datos(),
        options = list(lengthMenu = c(5, 30, 50),
        pageLength = 5))
        
    })
    
    output$barras_dias <- renderPlot({
        
        if (is.null(datos()))
        return(NULL)
        
        datos() %>%
        mutate(dia = wday(fecha_orden, label = TRUE)) %>%
        group_by(dia) %>%
        summarise(n = n_distinct(id_orden)) %>%
        ggplot(aes(x = dia, y = n, fill = dia)) +
        geom_bar(stat = "identity") +
        theme(legend.position = "none") +
        theme_minimal() +
        labs(title = "Número de órdenes por día de la semana",
        subtitle = paste("Sucursal: ",
        first(datos()$sucursal)))
        
    })
    
    
}

shinyApp(ui = ui, server = server,
options = list(width = 800, height = 500))
