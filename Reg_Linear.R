library(shiny)

dados = read.csv("slr12.csv",sep=";")
modelo = lm(CusInic ~ FrqAnual, data=dados)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Previsão de custo inicial para montar uma franquia"),
    
    fluidRow(
        column(4,
               h2("Dados"),
               tableOutput("Dados")
        ),
        column(8,
               plotOutput("Grafico")
        )
    ),
    fluidRow(
        column(6,
               h3("Valor anual da Franquia"),
               numericInput("NovoValor","Insira um valor",1000,min=1,max=9999999),
               actionButton("Processar","Processar")
        ),
        column(6,
            h1(textOutput("Resultados"))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$Grafico = renderPlot({
        plot(CusInic ~ FrqAnual, data=dados)
        abline(modelo)
    })
    output$Dados = renderTable({ head(dados, 10) })
    observeEvent(input$Processar, {
        valr = input$NovoValor
        prev = predict(modelo, data.frame(FrqAnual = eval(parse(text=valr))))
        prev = paste0("Previsão de Custo Inicial: ", round(prev,2), " reais")
        output$Resultados = renderText({prev})
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
