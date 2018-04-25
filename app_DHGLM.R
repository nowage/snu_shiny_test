library(shiny)
library(dhglm)
library(caret)
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
                          uiOutput("var1_select"),
                          uiOutput("Family_Main"),
                          uiOutput("Link_Main"),
                          uiOutput("MeanModelLP"),
                          uiOutput("RandFamily"),
                          uiOutput("DispModelLP"),
                          tags$hr(),
                          actionButton("Run", "Run") ),
                          mainPanel( helpText("Your fitted model"),
                                     verbatimTextOutput("other_val_show")))),
               tabPanel("Model_plot",
                        mainPanel( helpText("Your model plots"),
                                     plotOutput("reg_plot"),
                                   plotOutput("reg_plot2")))
               
)
server<-function(input,output,session) { data <- reactive({
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
output$Family_Main<-renderUI({
  selectInput("Family_Main","Select a Family for the response",choices = c("gaussian"="gaussian","binomial"="binomial","poisson"="poisson","gamma"="gamma"))
})
output$Link_Main<-renderUI({
  selectInput("Link_Main","Select a link for the response dist.",choices = c("identity"="identity","log"="log","logit"="logit","probit"="probit","cloglog"="cloglog","inverse"="inverse"))
})

output$var1_select<-renderUI({
  selectInput("ind_var_select","Select Dependent Var", choices =as.list(names(data())),multiple = FALSE)
})
#output$rest_var_select<-renderUI({
#  checkboxGroupInput("other_var_select","Select other Var",choices =as.list(names(data())))
#})
output$MeanModelLP<-renderUI({
  textAreaInput("other_var_select","Write the R.H.S. of mean Model:",value=NULL)
})
output$DispModelLP<-renderUI({
  textAreaInput("DispLP","Dispersion Model",value="phi~1")
})

observe({
  input$other_var_select
  rhsMM<-isolate(input$other_var_select)
  if(is.null(rhsMM)) rhsMM<-"1"
  trms<-strsplit(rhsMM,"+")
  trms<-trms[[1]]
  RandTerm<-trms=="|"
  nRand<<-sum(as.numeric(RandTerm))
  if(nRand>=1){
    for(i in 1:nRand){
  output$RandFamily<-renderUI({
    dynamic_selection_list <- lapply(1:nRand, function(i) {
      #radioButtons(inputId = paste0("mVariable",i), label = paste0("mVariable",i), choices = c("A","B","C"))
      selectInput(paste0("rand_family",i),paste("Family for Random effects",i),choices=c("gaussian","gamma","beta"))
      })
    do.call(tagList, dynamic_selection_list)
    #selectInput("rand_family","Family for Random effects",choices=c("gaussian","gamma"))
  })
    }
  }
})

output$other_val_show<-renderPrint({
  #input$other_var_select
  #input$ind_var_select
  input$Run
  f<-data()
  #form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse="+"))
  form<-paste(paste(isolate(input$ind_var_select),"~"),paste0(isolate(input$other_var_select),collapse=""))
  #print(paste("Mean model:",form))
  form2<-paste(paste(isolate(input$DispLP),cpllapse=""),collapse = "")
  print(paste("Dispersion Model:",form2))
  print(paste("Mean model:",form))
  RandDistM<-NULL
  if(nRand>0){
    for(i in 1:nRand){
    RandDistM<-c(RandDistM,input[[paste0("rand_family",i)]]) 
    }
  }
  MM<-DHGLMMODELING(Model="mean",Link=isolate(input$Link_Main),LinPred=as.formula(form),RandDist = RandDistM)
  DM<-DHGLMMODELING(Model="dispersion",Link = "log",LinPred = form2) 
  fittedModel <<-dhglmfit(RespDist=isolate(input$Family_Main),DataMain=f,MeanModel = MM,DispersionModel = DM)
  
  #print(summary(fittedModel))
  
})
output$reg_plot <- renderPlot({
  input$Run
  plotdhglm(fittedModel)
  
})
}
shinyApp(ui=ui,server=server)