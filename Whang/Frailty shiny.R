library(shiny)
library(frailtyHL)
ui<-navbarPage("FrailtyHL",
               tabPanel("Data Import",
                        sidebarLayout(sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                                    tags$hr(),
                                                    h5(helpText("Select the read.table parameters below")),
                                                    checkboxInput(inputId = 'header', label = 'Header', value = FALSE),
                                                    checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                                    radioButtons(inputId = 'sep', label = 'Separator', 
                                                                 choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        mainPanel(uiOutput("tb1"))
                        ) ),
               tabPanel("Model_dev",
                        sidebarLayout(sidebarPanel(
                         
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select"),
                          uiOutput("rest_var_select2")
                          ),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show"))))
)
server<-function(input,output) { data <- reactive({
  file1 <- input$file
  if(is.null(file1)){return()} 
  read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
  
})  
output$table <- renderTable({
  if(is.null(data())){return ()}
  data()
})
output$tb1 <- renderUI({
  tableOutput("table")
})

output$var1_select<-renderUI({
  selectInput("ind_var_select","Select Response Var", choices =as.list(names(data())),multiple = FALSE)
})

output$rest_var_select<-renderUI({
  checkboxGroupInput("other_var_select","Select fixed covariate",choices =as.list(names(data())))
})

output$rest_var_select2<-renderUI({
  checkboxGroupInput("other_var_select2","Select random covariate",choices =as.list(names(data())))
})
output$other_val_show<-renderPrint({
  input$ind_var_select
  input$other_var_select
  f<-data()
  
  form <- sprintf("%s~%s+(1|%s)",input$ind_var_select,paste0(input$other_var_select,collapse="+"),input$other_var_select2)
  print(form)
  reg<- frailtyHL(as.formula(form),data=f)
  print(summary(reg))

  
  
})

}
shinyApp(ui=ui,server=server)