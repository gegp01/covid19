server <- function(input, output, session) {
    
    dataframe<-reactive({
        if (is.null(input$datafile)) return(NULL)  
        data<-read.csv(input$datafile$datapath)
    })
    
    
    observe({
                if (is.null(input$datafile)) return(NULL)
        var.opts<-names(dataframe())
        updateSelectInput(session, "z", choices = var.opts, selected=NULL)
        updateSelectInput(session, "n", choices = var.opts, selected=NULL)
        
    })
    
    w<-reactive({
        if (is.null(input$datafile)) return(NULL)
        X<-dataframe()
        w<-f.bins(X, z=input$z, n=input$n)
    })
    
    output$w<-reactive({
        if (is.null(input$datafile)) return(NULL)
        w()
    })
    
    output$bins<-renderTable({
        if (is.null(input$datafile)) return(NULL)
        w()
    })
    
    output$plot<-renderPlot({
        if (is.null(input$datafile)) return(NULL)
        boxplot(w()[,2]~w()[,3], col="lightblue", xlab="decil", ylab=input$z)
    })
    
    output$downloadData <- downloadHandler(
        filename="output_bins.csv"
        , content = function(file) {
            data<-w()
            write.csv(data, file=file, row.names=F)
    })
}
