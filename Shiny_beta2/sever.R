library(shiny)

shinyServer(function(input,output){
  
  output$distPlot<-renderPlot({
    if (input$mean > input$mode)
      stop("mean must be <= mode if the mode exists")
    # mode = alpha / (alpha + beta - 2) iff alpha + beta > 2
    # mean = alpha / (alpha + beta)
    # mode * (alpha + beta - 2) = alpha = mean * (alpha + beta)
    # mode * (alpha + beta) - mean * (alpha + beta) = 2 * mode
    # (mode - mean) * (alpha + beta) = 2 * mode
    # (alpha + beta) = concentration = 2 * mode / (mode - mean)
    concentration <- 2 * input$mode / (input$mode - input$mean)
    if (is.infinite(concentration)) {
      alpha <- beta <- 1
    }
    else {
      alpha <-     input$mean  * concentration
      beta <- (1 - input$mean) * concentration
    }
    theta <- ppoints(1000)
    cols <- rainbow(11)
    
    par(mfrow = c(2,2), mar = c(5,4,1,0) + .1, cex = 2, las = 1, lwd = 2, bg = "ivory2", 
        cex.main = 0.8, cex.axis = 0.8)
    plot(theta, dbeta(theta, alpha, beta), type = "l", col = "black", main = "Initial Beliefs",
         xlab = "Prior p", ylab = "PDF", axes = FALSE, lty = "dotted")
    axis(1)
    axis(2)
    if(input$update) {
      size <- 10
      plot(theta, dbinom(0, size, theta), type = "l", col = cols[1], ylim = 0:1, lty = "dashed",
           xlab = "Given p", ylab = "", axes = FALSE, main = "Pr(a|p)")
      axis(1)
      axis(2)
      for(i in 1:size) lines(theta, dbinom(i, size, theta), col = cols[i + 1], lty = "dashed")
      mle <- input$pins / size
      stopifnot(is.numeric(mle), length(mle) == 1)
      se <- sqrt( mle * (1 - mle) / size )
      stopifnot(is.numeric(se), length(se) == 1)
      peak <- dbinom(input$pins, size, mle)
      stopifnot(is.numeric(peak), length(peak) == 1)
      segments(x0 = mle, y0 = 0, x1 = mle, y1 = peak, col = cols[input$pins + 1], lty = "dashed")
      legend("top", legend = 0:size, text.col = cols, ncol = 6, box.lwd = 0, cex = 0.6, 
             title = "For a =", title.col = "black")
      d <- beta(alpha, beta)
      plot(0:1, rep(choose(size, 0) * beta(0 + alpha, size - 0 + beta) / d, 2), ylim = c(0,.2),
           type = "l", ylab = "", axes = FALSE, main = "Data-Dependent Constants", lty = "dashed", 
           xlab = "Probability of Data", col = cols[1])
      for(i in 1:size) {
        abline(h = choose(size, i) * beta(i + alpha, size - i + beta) / d, 
               col = cols[i + 1], lty = "dashed")
      }
      i <- input$pins
      abline(h = choose(size, i) * beta(i + alpha, size - i + beta) / d, 
             col = cols[i + 1], lty = "dashed")
      axis(1)
      axis(2)
      y <- dbeta(theta, alpha + input$pins, beta + 10 - input$pins)
      plot(theta, y, main = "Comparison",
           type = "l", col = cols[input$pins + 1], xlab = "Posterior p", 
           ylab = "PDF", axes = FALSE)
      lines(theta, dbeta(theta, alpha, beta), col = cols[input$pins + 1], lty = "dotted")
      axis(1)
      axis(2)
      if (mle == 0) abline(v = 0, col = cols[1], lty = "dashed")
      else if (mle == 1) abline(v = 1, col = cols[11], lty = "dashed")
      else {
        x <- qnorm(ppoints(length(theta)), mean = mle, sd = se)
        lines(x, dnorm(x, mean = mle, sd = se), lty = "dashed", col = cols[input$pins + 1])
        segments(x0 = mle, y0 = 0, x1 = mle, y1 = dnorm(mle, mean = mle, sd = se), 
                 col = cols[input$pins + 1], lty = "dashed")
      }
    }
    output$caption <- renderText(
      paste("In the Comparison graph, the prior distribution is the dotted line and is", 
            "the same as in the Initial Beliefs graph. The posterior distribution is the",
            "solid line. The frequentist point estimate is the vertical dashed line.",
            "The dashed curve is the asymptotic sampling distribution")
    )
  },
  width=800,
  height=600
  
  )
  
  
})
Status API Training Shop Blog About Pricing
