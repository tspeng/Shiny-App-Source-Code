library(UsingR)
data(cement)
shinyServer(
    function(input,output){
        output$newfig <- renderPlot({
            if (!is.null(input$vn)) {
                #Fit y~x1 and plot the confidence interval
                if (length(input$vn)==1 & input$vn[1]=="x1"){
                    model <- lm(y ~ x1, data=cement)
                    pred <- predict(model,newdata =data.frame(x1=cement$x1),interval="confidence",level=input$level)
                    plot(cement$y,pred[,1],xlim=c(min(cement$y),max(cement$y)),ylim=c(min(cement$y),max(cement$y)),xlab="True y",ylab="Predicted y")
                    abline(0,1)
                    if (input$CIplot==1) {
                        polygon(c(sort(cement$y,decreasing = TRUE), sort(cement$y)), c(sort(pred[ ,3],decreasing = TRUE), sort(pred[ ,2])), col = 'lightblue1')
                        lines(sort(cement$y),sort(pred[,3]),lty="dashed",col="green")
                        lines(sort(cement$y),sort(pred[,2]),lty="dashed",col="green")
                    }
  
                }
                #Fit y~x2 and plot the confidence interval
                else if (length(input$vn)==1 & input$vn[1]=="x2") {
                    model <- lm(y ~ x2, data=cement)
                    pred <- predict(model,newdata =data.frame(x2=cement$x2),interval="confidence",level=input$level)
                    plot(cement$y,pred[,1],xlim=c(min(cement$y),max(cement$y)),ylim=c(min(cement$y),max(cement$y)),xlab="True y",ylab="Predicted y")
                    abline(0,1)
                    if (input$CIplot==1) {
                        polygon(c(sort(cement$y,decreasing = TRUE), sort(cement$y)), c(sort(pred[ ,3],decreasing = TRUE), sort(pred[ ,2])), col = 'lightblue1')
                        lines(sort(cement$y),sort(pred[,3]),lty="dashed",col="green")
                        lines(sort(cement$y),sort(pred[,2]),lty="dashed",col="green")
                    }
                    
                }
                else {
                    #Fit y~x1+x2 and plot the confidence interval
                    model <- lm(y ~ x1 +  x2, data=cement)
                    pred <- predict(model,newdata =data.frame(x1=cement$x1,x2=cement$x2),interval="confidence",level=input$level)
                    plot(cement$y,pred[,1],xlim=c(min(cement$y),max(cement$y)),ylim=c(min(cement$y),max(cement$y)),xlab="True y",ylab="Predicted y")
                    abline(0,1)
                    if (input$CIplot==1) {
                        polygon(c(sort(cement$y,decreasing = TRUE), sort(cement$y)), c(sort(pred[ ,3],decreasing = TRUE), sort(pred[ ,2])), col = 'lightblue1')
                        lines(sort(cement$y),sort(pred[,3]),lty="dashed",col="green")
                        lines(sort(cement$y),sort(pred[,2]),lty="dashed",col="green")
                    }
                }
                
            }
        })

    }
)