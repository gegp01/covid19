server <- function(input, output) {
    who<-read.csv("https://raw.githubusercontent.com/eebrown/data2019nCoV/master/data-raw/WHO_SR.csv")
    usa_covid19<-who[grep("UnitedStatesofAmerica", names(who))]
    italia_covid19<-who[grep("Italy", names(who))]
    spain_covid19<-who[grep("Spain", names(who))]
    mex_covid19<-who[grep("Mex", names(who))]
    dia<-c(1:length(who$Date))

    output$log_plot <- renderPlot({
        
        par(bg="grey30", col.lab="white", col.main="white", col.axis="white", lwd=1.5, cex=1.5)
        plot(spline(log(mex_covid19[,1]+1)~dia), type="l", col="red"
             , axes=F
             , xlab="días desde el 20 de enero de 2020"
             , ylab="ln casos"
             , ylim=c(0,15))
        
        lines(spline(log(mex_covid19[,2]+1)~dia), type="l", col="red", lty="dotdash")            
        lines(spline(log(usa_covid19[,1]+1)~dia), type="l", col="lightblue")
        lines(spline(log(usa_covid19[,2]+1)~dia), type="l", col="lightblue", lty="dotdash")
        lines(spline(log(italia_covid19[,1]+1)~dia), type="l", col="green")
        lines(spline(log(italia_covid19[,2]+1)~dia), type="l", col="green", lty="dotdash")
        lines(spline(log(spain_covid19[,1]+1)~dia), type="l", col="gold")
        lines(spline(log(spain_covid19[,2]+1)~dia), type="l", col="gold", lty="dotdash")
        
        axis(1, col="white")
        axis(2, col="white")
        grid(col="ivory3")
        title(paste("Reporte de la OMS:", tail(who$Date, 1)))
        
        legend(1, 14, legend=c("MX", "US", "IT", "ES", "confirmados", "muertos"),
               col=c("red", "lightblue", "green", "gold", "black", "black")
               , lty=c(rep(1, 4), 1, 2), cex=0.8, bg="beige")
        
    })
}
