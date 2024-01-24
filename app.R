library(shiny)
library(shinydashboard)
library(dplyr)
library(rhandsontable)
library(lubridate)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(collapsed = TRUE,
                   sidebarMenu(
                     menuItem("Gastos", tabName = "consumo_anual")
                   )
  ),
  dashboardBody(
    
    tabItem(tabName = "mete_gastos", 
            
            fluidPage(
              
              titlePanel("Gastos no decorrer do ano"),
              sidebarLayout(
                sidebarPanel(width = 2,
                             textInput("card", "Cartão utilizado"),
                             verbatimTextOutput("value_card"),
                             
                             textInput("instauratio", "Estabelecimento"),
                             verbatimTextOutput("value_instauratio"),
                             
                             textInput("mercatura", "Aquisição"),
                             verbatimTextOutput("value_mercatura"),
                             
                             dateInput("acquirunt", "Data da compra",
                                            value  = Sys.Date(),
                                            format = "dd/mm/yyyy",
                                            language = "pt-BR"),
                             
                             numericInput("valorem", "Valor Gasto", value = NA),
                             
                             radioButtons("pensionem", "Foi Pacerlada?", inline = TRUE, 
                               list("Não" = "nao","Sim" = "s1")
                             ),
                             conditionalPanel("input.pensionem == 's1'",
                                              numericInput("pensiones", "Parcelas", 2, 2)
                             ),
                             ##
                             radioButtons("devolution", "Houve Casback?", inline = TRUE, 
                                          list("Não" = "nao","Sim" = "s2")
                             ),
                             conditionalPanel("input.devolution == 's2'",
                                              numericInput("recipis", "Porcentagem",1)
                             ),
                             ##
                             radioButtons("turum", "Pagante", inline = TRUE, 
                                          list("Eu" = "Seu Nome","Outros" = "s3")
                             ),
                             conditionalPanel("input.turum == 's3'",
                                              textInput("debitor", "Pessoa", "Nome do Devedor"),
                                              verbatimTextOutput("value_turum"),
                             ),
                             ##
                             actionButton("add", "Novo Gasto"),
                             br(),br(),
                             actionButton("save", "Salvar"),
                             radioButtons("fileType", "File type", c("ASCII"))
                ),
                
                mainPanel(
                  rHandsontableOutput("hot_CONSUMOS")
                  
                )))
    )
  )
  
)
####

outdir <- "C:\\Users\\kahel\\dplyrplot\\consumo"

if(file.exists("C:\\Users\\kahel\\dplyrplot\\consumo\\DESPESAS_2024.txt")){ #se tiver o .txt com os dados, ele importa o arquivo
  
  DESPESAS <- dget("C:\\Users\\kahel\\dplyrplot\\consumo\\DESPESAS_2024.txt") #vai remover a linha na qual a coluna de gastos estiver vazia
  
  # Adicionando a linha para remover as entradas com a quinta coluna vazia
  
  DESPESAS <- DESPESAS[DESPESAS$Custo != " ", ]

} else{

DESPESAS <- data.frame(`Cartão` = c(" "),
                     Data = c(" "),
                     Estabelecimento = c(" "),
                     Mercadoria = c(" "),
                     Custo = c(" "),
                     Parcelamento = c(" "),
                     Cashback = c(" "),
                     Pagador = c(" "))
}

server <- function(input, output) {

  values <- reactiveValues()
values$consumo <- DESPESAS

###  

## Handsontable
observe({
  if (!is.null(input$hot_CONSUMOS)) {
    values[["previous"]] <- isolate(values[["DESPESAS"]])
    DESPESAS = hot_to_r(input$hot_CONSUMOS)
  } else {
    if (is.null(values[["DESPESAS"]]))
      DESPESAS <- DESPESAS
    else
      DESPESAS <- values[["DESPESAS"]]
  }
  values[["DESPESAS"]] <- DESPESAS
})

###

observeEvent(input$add, {

values$consumo <- rbind(values$consumo, c(input$card,
                                          format(input$acquirunt[1], "%d/%m/%Y"),
                                          input$instauratio,
                                          input$mercatura,
                                          input$valorem,
                                          ifelse(input$pensionem == "s1", input$pensiones, "Não"),
                                          ifelse(input$devolution == "s2", input$recipis, "Não"),
                                          ifelse(input$turum == "s3", input$debitor, "Seu Nome"))) 

})
  
output$hot_CONSUMOS <- renderRHandsontable({
  rhandsontable(values$consumo, stretchH = "all", width = 1080) %>% 
  hot_cols(colWidths = c(70, 73, 100, 260, 60, 70, 50, 85), valign='htCenter') %>% 
  hot_context_menu(allowRowEdit = T, allowColEdit = FALSE) %>% 
  hot_col("Custo", format = "$0,000.00")
})  
 
## Save 
observeEvent(input$save, {
  fileType <- isolate(input$fileType)
  finalDESPESAS <- isolate(values[["DESPESAS"]])
  if(fileType == "ASCII"){
    dput(finalDESPESAS, file=file.path(outdir, sprintf("%s.txt", "DESPESAS_2024")))
  }
  else{
    saveRDS(finalDESPESAS, file=file.path(outdir, sprintf("%s.rds", "DESPESAS_2024")))
  }
})

}

# Run the application 
shinyApp(ui = ui, server = server)
