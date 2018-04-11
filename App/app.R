library(shiny)
ui<-navbarPage("Albatross Regression",
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
                          uiOutput("model_select"),
                          uiOutput("var1_select"),
                          uiOutput("rest_var_select")),
                          mainPanel( helpText("Your Selected variables"),
                                     verbatimTextOutput("other_val_show")))),
               tabPanel("Model_plot",
                        mainPanel( helpText("Your model plots"),
                                     plotOutput("reg_plot"),
                                   plotOutput("reg_plot2")))
               
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
#output$model_select<-renderUI({
#  selectInput("modelselect","Select Algo",choices = c("Logistic_reg"="logreg","SVM"="svm"))
#})
output$var1_select<-renderUI({
  selectInput("ind_var_select","Select Dependent Var", choices =as.list(names(data())),multiple = FALSE)
})
#output$rest_var_select<-renderUI({
#  checkboxGroupInput("other_var_select","Select other Var",choices =as.list(names(data())))
#})
output$rest_var_select<-renderUI({
  textAreaInput("other_var_select","Write the R.H.S. of your model here",value=NULL)
})
output$other_val_show<-renderPrint({
  input$other_var_select
  input$ind_var_select
  f<-data()
  #form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  form<-paste(paste(input$ind_var_select,"~"),paste(input$other_var_select,cpllapse=""))
  print(form)
  
  logreg <-lm(as.formula(form),data=f)
  
  print(summary(logreg))
  
})
output$reg_plot <- renderPlot({
  input$other_var_select
  input$ind_var_select
  f<-data()
  #form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  form<-paste(paste(input$ind_var_select,"~"),paste(input$other_var_select,cpllapse=""))
  lreg <-lm(as.formula(form),data=f)
  #options(mfrow=c(2,1))
  plot(resid(lreg)~predict(lreg))
  #plot(qqnorm(resid(lreg)))
})
output$reg_plot2 <- renderPlot({
  input$other_var_select
  input$ind_var_select
  f<-data()
  #form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  form<-paste(paste(input$ind_var_select,"~"),paste(input$other_var_select,cpllapse=""))
  lreg <-lm(as.formula(form),data=f)
  #options(mfrow=c(2,1))
  #plot(resid(lreg)~predict(lreg))
  plot(qqnorm(resid(lreg)))
  #qqline(qqnorm(resid(lreg)))
})
}

shinyApp(ui=ui,server=server)