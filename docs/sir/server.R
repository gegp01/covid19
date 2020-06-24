server <- function(input, output) {

    output$modelo <- renderPlot({
#        función del modelo seird
        seird <- function(time, state, parameters) {
            with(as.list(c(state, parameters)), {

                dS <- -beta * S * I - delta * S * E
                dE <-  beta * S * I - c * E + delta * S * E
                dI <-  c*E - gamma * I - letalidad * I
                dR <- gamma * I - letalidad * I
                dD <- letalidad * I

                return(list(c(dS, dE, dI, dR, dD)))
            })
        }


        # proporciones iniciales de cada compartimento
        init<- c(S = 1, E = 0.1, I =0.0, R = 0.0, D = 0.0)

        # parametros
        parameters <- c(beta = input$beta, gamma = input$gamma, delta = input$delta, c = input$c, letalidad = input$letalidad)

        # Tiempo de la modelación (365 días)
        times      <- seq(0, 365, by = 1)

        # Resolver el sistema de ecuaciones con ode (General Solver for Ordinary Differential Equations)
        out <-as.data.frame(ode(y = init, times = times, func = seird, parms = parameters))

        par(bg="transparent", col="darkslategrey", col.axis="darkslategrey", col.lab="darkslategrey", mai=c(1, 1, 0.5, 1.3), cex=1.3, lwd=2)
        
        plot(spline(out$time, out$S), type="l", col="green"
             , axes=F
             , frame.plot=F
             , xlab="días desde el primer caso comunitario"
             , ylab="proporción de la población suceptible"
        )
        
        
        lines(spline(out$time, out$E), col="orange")
        lines(spline(out$time, out$I), col="red")
        lines(spline(out$time, out$R), col="lightblue")
        lines(spline(out$time, out$D), col="royalblue")
        axis(1, col="darkslategrey")
        axis(2, col="darkslategrey")
        axis(4, at=c(0.1, 0.2, 0.5,1)
             #     , labels=c("10 millones", "20 millones", "60 millones", "120 millones")
             , labels=c("10 000", "20 000", "50 000", "100 000")
             , col="darkslategrey", cex=0.7, las=2)
        
        legend("top", c("suceptibles", "asintomáticos-infecciosos", "enfermos-infecciosos", "recuperados", "decesos")
               , cex=0.9, col=c("green", "orange", "red", "lightblue", "royalblue")
               , lty=1, bg="transparent", box.lwd=0)
        mtext("personas", side=4, line=1, las=2, at=1.1)
    })
 
}
