# Information and decision-making in educational resource allocation 
suppressMessages(stopifnot(require(shiny)))
suppressMessages(stopifnot(require(shinythemes)))
suppressMessages(stopifnot(require(sn)))
suppressMessages(stopifnot(require(nleqslv)))

ui <- navbarPage(
  
  theme = shinytheme("cerulean"),
  titlePanel(h4("Information and Decision-making in Educational Resource Allocation")),
  
  tabPanel(title = "Introduction",
           
           mainPanel(
             h2("Welcome!"),
             
             p("Thank you for agreeing to participate in this study on information and decision making in educational resource allocation. 
               The purpose of this study is to explore how decision-makers in education respond to effectiveness and cost information obtained from program evaluations.
               You will be asked to specify your beliefs on what would happen in terms of effectiveness and cost if two hypothetical reading programs were replicated at a hypothetical setting.
               There are no right or wrong answers, just your subjective beliefs. Please note that the data collection will be anonymous. 
               "),
             
             p("As approved by the Teachers College IRB, this research is conducted by Yilin Pan, doctoral student in Economics and Education, Teachers College Columbia University, under the advisory of Prof. Henry Levin. "),
             
             p("We recognize that your time is valuable, and we will treat it accordingly. The survey is designed to take about half an hour. If you choose not to participant in the study anymore, there will be no repercussions and you are free to do so at any time."),
             
             p("Before proceeding, we will ask a few questions on your background."), 
             
             column(3, 
                    numericInput("age", 
                                 label = h4("Age"), 
                                 value = 25)),
             
             column(3, 
                    checkboxGroupInput("gender", 
                                       label = h4("Gender"), 
                                       choices = list("Male" = 1, 
                                                      "Female" = 2, "Other" = 3),
                                       selected = 0)),
             column(3, 
                    checkboxGroupInput("edu", 
                                       label = h4("Educational level"), 
                                       choices = list("Bachelor" = 1, 
                                                      "Master" = 2, "PhD/EdD" = 3, "Other" = 4),
                                       selected = 0))
             ),
           
           fluidRow(
             
           ),
           
           fluidRow(
             column(3, 
                    numericInput("exp", 
                                 label = h4("Years of experience related to education"), 
                                 value = 10)),
             
             column(3, 
                    textInput("role", label = h4("Role in education"), 
                              value = "Specify your position")) 
           )
           ), 
  
  tabPanel(title = "Reading Now",
           
           verticalLayout(
             
             mainPanel(
               h2("Settings"),
               
               p("Suppose you are a principal who needs to make a decision on which after-school reading program to acquire for 100 struggling readers in Grade 1-3.
                 You are interested in improving students' test scores in reading comprehension, while you are also concerned about the cost of the program. 
                 Among all the reading programs, you are hesitating between two alternatives, Reading Now and Literacy Ladders, which target the same objective and population but differ in the methodologies they use. 
                 In order to make a well-informed decision, you asked your assistant to collect some research evidence on the effectiveness and cost of these two programs."),
               
               hr(),
               
               h3("Reading Now"), 
               
               p("Reading Now provides one-to-one tutoring sessions to struggling readers twice a week for 16 weeks, each lasting half an hour. 
                 Students are tutored by community volunteers who may not have working experience in education but have received 3 hours of training related to the program.
                 During the tutor sessions, the tutor would read aloud with the student and ask the student comprehension questions to check his/her understanding."),
               
               
               p("The program is collaborated between the school and Reading Now Organization. 
                 Each school pays an annual license fee of $20,000 to Reading Now Organization, 
                 and the organization provides a site-coordinator to oversee the program implementation at the school. 
                 The school principal and the literacy coach have regular meetings with the site-coordinator. 
                 The school is also responsible for providing a dedicated classroom for the program."), 
               
               hr(), 
               
               p(strong("Based on your previous experience and the program description, do you have a subjective belief on the effectiveness of Reading Now if it is replicated?")), 
               
               checkboxGroupInput("prior_effect_RN", 
                                  
                                  label = h5("Prior Belief on the effect of Reading Now"), 
                                  
                                  choices = list("No, I don't have a belief on the effect." = 1, 
                                                 "Yes, I have a belief on the effect." = 2),
                                  selected = 0), 
               
               p(strong("If you do have a belief, please specify your best guess (i.e., mode of your belief), your guess for the almost worst-case scenario (i.e., 2.5th percentile of your belief), 
                        and your guess for the almost best-case scenario (i.e., 97.5th percentile of your belief) of the effectiveness of the program on the sliderbars below ")),
               p("* Note that the effectiveness is expressed in effect size. A short explanation on the possible range of effect size.") #,
              #  p("A quantile plot that fits your specification best is generated, showing that......")
               
               ), 
             
             # sidebarLayout(
               sidebarPanel(
                 
                 sliderInput(inputId = "mode1", 
                             label = "Best guess (mode):", 
                             min = -1, max = 2, value = 0.5, step= 0.05),
                 
                 sliderInput(inputId = "lower1", 
                             label = "Almost worst-case scenario (2.5th quantile):", 
                             min = -1, max = 2, value = 0.5, step = 0.05),
                 
                 sliderInput(inputId = "upper1", 
                             label = "Almost best-case scenario (97.5th quantile):", 
                             min = -1, max = 2, value = 0.5, step = 0.05)
                 # error message to make sure that median is in the middle of lower bound and upper bound of the prior credible interval
                 
               ),
               
              # mainPanel(
              #   plotOutput("quantilePlot1")
                 
              # )
             #),
             
             mainPanel(
               
               hr(),
               h5("Control"),   
               p("Your assistant provides you some evidence on the effectiveness of Reading Now. 
                 A recent evaluation study estimates the effect size of Reading Now to be 0.25, 
                 indicating that at the evaluation setting, students would score 0.25 standard deviation higher on average if they receive Reading Now, 
                 compared to what they would have scored without the program."), 
               
               h5("Treatment 1"),  
               p("Your assistant provides you some evidence on the effectiveness of Reading Now. 
                 A recent evaluation study estimates the effect size of Reading Now to be 0.25 with a credible interval of (-0.1, 0.6), 
                 indicating that at the evaluation setting, students would score 0.25 standard deviation higher on average if they receive Reading Now, 
                 compared to what they would have scored without the program. While the best guess for the true effect size is 0.25, there is some uncertainty around it. 
                 The probability that the true effect size lies between -0.1 and 0.6 is 95%. 
                 "), 
               
               h5("Treatment 2"),
               p("Your assistant finds some evidence on the effectiveness of Reading Now. 
                 A recent evaluation study estimates the effect size of Reading Now to be 0.25 with a credible interval of (0.1, 0.4), 
                 indicating that at the evaluation setting, students would score 0.25 standard deviation higher on average if they receive Reading Now, 
                 compared to what they would have scored without the program. While the best guess for the true effect size is 0.25, there is some uncertainty around it. 
                 The probability that the true effect size lies between 0.1 and 0.4 is 95%. 
                 "), 
               
               p(strong("Based on this information, please specify your best guess (i.e., mode of your belief), your guess for the almost worst-case scenario (i.e., 2.5th percentile of your belief), 
                        and your guess for the best-case scenario (i.e., 97.5th percentile of  your belief) of the effectiveness of the program on the sliders below "))
               
               ),  
             
             
            # sidebarLayout(
               sidebarPanel(
                 
                 sliderInput(inputId = "mode2", 
                             label = "Best guess (mode):", 
                             min = -1, max = 2, value = 0.5, step= 0.01),
                 
                 sliderInput(inputId = "lower2", 
                             label = "Almost worst-case scenario (2.5th quantile):", 
                             min = -1, max = 2, value = 0.5, step = 0.01),
                 
                 sliderInput(inputId = "upper2", 
                             label = "Almost best-case scenario (97.5th quantile):", 
                             min = -1, max = 2, value = 0.5, step = 0.01)
                 # error message to make sure that median is in the middle of lower bound and upper bound of the prior credible interval
               ),
               
             #  mainPanel(
            #     plotOutput("quantilePlot2")
            #  )
            # ), 
         
           
           mainPanel(
             hr(), 
             
             p(strong("Based on your previous experience and the program description, do you have a subjective belief on the average cost per participant if Reading Now is replicated?")), 
             p("* As the principal, you care about the value of all the resources contributed by the school, including personnel, facilities and licence fee."), 
             
             checkboxGroupInput("prior_cost_RN", 
                                
                                label = h5("Prior Belief on the average cost of Reading Now"), 
                                
                                choices = list("No, I don't have a belief on the average cost." = 1, 
                                               "Yes, I have a belief on the average cost." = 2),
                                selected = 0), 
             
             p(strong("If you do have a belief, please specify your best guess (i.e., mode of your belief), your guess for the almost least-costly scenario (i.e., 2.5th percentile of your belief), 
                      and your guess for the almost most-costly scenario (i.e., 97.5th percentile of your belief) of the average cost of the program on the sliders below. "))
           ),
           
           # sidebarLayout(position = "left",
                         
             sidebarPanel(
               
               sliderInput(inputId = "mode3", 
                           label = "Best guess (mode):", 
                           min = 200, max = 500, value = 50, step= 10),
               
               sliderInput(inputId = "lower3", 
                           label = "Almost least-costly scenario (2.5th quantile):", 
                           min = 200, max = 500, value = 50, step = 10),
               
               sliderInput(inputId = "upper3", 
                           label = "Almost most-costly scenario (97.5th quantile):", 
                           min = 200, max = 500, value = 50, step = 10)
               # error message to make sure that median is in the middle of lower bound and upper bound of the prior credible interval
               
             ),
             
           #  mainPanel(
          #     plotOutput("quantilePlot3")
           #  )
          # ), 
           
           
           mainPanel(
             
             hr(),
             h5("Control"),   
             p("Your assistant reports the findings of a recent cost analysis on Reading Now to you. The study was conducted in three schools. 
               In School 1, the best guess for the true average cost per participant contributed by the school is $450, while the probability that the true value falls between $390 and $500 is 95%.
               In School 2, the best guess for the true average cost per participant contributed by the school is $380, while the probability that the true value falls between $345 and $420 is 95%.
               In School 3, the best guess for the true average cost per participant contributed by the school is $400, while the probability that the true value falls between $355 and $450 is 95%.
               Across the three schools, the best guess for the true average cost per participant contributed by the school is $410, while the probability that the true value falls between $360 and $460 is 95%.
               "), 
             
             h5("Treatment 1"),  
             p("Your assistant reports the findings of a recent cost analysis on Reading Now to you. The study was conducted in three schools. The point estimate and the 95% credible interval
              of the average cost per participant contributed by the school are reported for each school and across the schools in the following table. The point estimate is the best guess for the true value, 
               while the probability that the true value falls between the 2.5th quantile and the 97.5th quantile is 95%."),
             
             img(src = "cost_RN_table.png", align = "center", height = 200, width = 500),

             
             h5("Treatment 2"),
             p("Your assistant reports the findings of a recent cost analysis on Reading Now to you. The study was conducted in three schools. The point estimate and the 95% credible interval
              of the average cost per participant contributed by the school are reported for each school and across the schools in the following figure. The point estimate is the best guess for the true value, 
               while the probability that the true value falls between the 2.5th quantile and the 97.5th quantile is 95%."), 
             
             img(src = "cost_RN_figure.png", align = "center", height = 500, width = 600),
             
             p(strong("Based on this information, please specify your best guess (i.e., mode of your belief), your guess for the almost least-costly scenario (i.e., 2.5th percentile), 
                      and your guess for the almost most-costly scenario (i.e., 97.5th percentile) of the effectiveness of the program on the sliders below."))
             
           ),  
           
           
           # sidebarLayout(position = "left",
                         
             sidebarPanel(
               
               sliderInput(inputId = "mode4", 
                           label = "Best guess (mode):", 
                           min = 200, max = 500, value = 50, step= 10),
               
               sliderInput(inputId = "lower4", 
                           label = "Almost least-costly scenario (2.5th percentile):", 
                           min = 200, max = 500, value = 50, step = 10),
               
               sliderInput(inputId = "upper4", 
                           label = "Almost most-costly scenario (97.5th percentile):", 
                           min = 200, max = 500, value = 50, step = 10)
               # error message to make sure that median is in the middle of lower bound and upper bound of the prior credible interval
               
             ) #,
             
             #mainPanel(
              # plotOutput("quantilePlot4")
            # )
         #  )
        )
               ),
  
  
  tabPanel(title = "Literacy Ladders",
           
           verticalLayout(
             
             mainPanel(
  
               h3("Literacy Ladders"), 
               
               p("Literacy Ladders is a computer-based reading program targeting struggling readers. It runs 30 mins per day, 4 days per week over 10 weeks.
                  Every 20 Students are grouped in a computer lab to use adaptive software to improve their reading skills, supervised by a teaching aid 
                  to check the progress and answer questions from time to time."),
               
               
               p("The reading program license costs $100 per student. Other resources required to implement the program include the working time of teaching aids,
                 computers, and computer labs."), 
               
               hr(), 
               
               p(strong("Based on your previous experience and the program description, do you have a subjective belief on the effectiveness of Literacy Ladders if it is replicated?")), 
               
               checkboxGroupInput("prior_effect_LL", 
                                  
                                  label = h5("Prior Belief on the effect of Literacy Ladders"), 
                                  
                                  choices = list("No, I don't have a belief on the effect." = 1, 
                                                 "Yes, I have a belief on the effect." = 2),
                                  selected = 0), 
               
               p(strong("If you do have a belief, please specify your best guess (i.e., mode of your belief), your guess for the almost worst-case scenario (i.e., 2.5th percentile of your belief), 
                        and your guess for the almost best-case scenario (i.e., 97.5th percentile of your belief) of the program effectiveness on the sliders below.")),
               p("* Note that the effectiveness is expressed in effect size. A short explanation on the possible range of effect size.") #,
              #  p("A quantile plot that fits your specification best is generated, showing that......")
               
               ), 
             
             # sidebarLayout(
               sidebarPanel(
                 
                 sliderInput(inputId = "mode5", 
                             label = "Best guess (mode):", 
                             min = -1, max = 2, value = 0.5, step= 0.05),
                 
                 sliderInput(inputId = "lower5", 
                             label = "Almost worst-case scenario (2.5th quantile):", 
                             min = -1, max = 2, value = 0.5, step = 0.05),
                 
                 sliderInput(inputId = "upper5", 
                             label = "Almost best-case scenario (97.5th quantile):", 
                             min = -1, max = 2, value = 0.5, step = 0.05)
                 # error message to make sure that median is in the middle of lower bound and upper bound of the prior credible interval
                 
               ),
               
               #mainPanel(
                # plotOutput("quantilePlot1")
                 
              # )
           #),
             
             mainPanel(
               
               hr(),
               h5("Control"),   
               p("Your assistant provides you the research findings of a recent impact evaluation on Literacy Ladders. The study was conducted in three schools. 
               In School 1, the best guess for the true average treatment effect is 0.24, while the probability that the true value falls between 0.13 and 0.35 is 95%.
               In School 2, the best guess for the true average treatment effect is 0.14, while the probability that the true value falls between 0.01 and 0.28 is 95%.
               In School 3, the best guess for the true average treatment effect is 0.16, while the probability that the true value falls between 0.01 and 0.31 is 95%.
               Across the three schools, the best guess for the true average treatment effect is 0.18, while the probability that the true value falls between 0.03 and 0.33 is 95%."), 
               
               h5("Treatment 1"),  
               p("Your assistant provides you the research findings of a recent impact evaluation on Literacy Ladders. The study was conducted in three schools. The point estimate and the 95% credible interval
               of the average treatment effect are reported for each school and across the schools in the following table. The point estimate is the best guess for the true value, 
               while the probability that the true value falls between the 2.5th quantile and the 97.5th quantile is 95%.
                 "), 
               
               img(src = "effect_LL_table.png", align = "center", height = 200, width = 500),
               
               h5("Treatment 2"),
               p("Your assistant provides you the research findings of a recent impact evaluation on Literacy Ladders. The study was conducted in three schools. The point estimate and the 95% credible interval
               of the average treatment effect are reported for each school and across the schools in the following figure. The point estimate is the best guess for the true value, 
               while the probability that the true value falls between the 2.5th quantile and the 97.5th quantile is 95%. 
                 "), 
               
               img(src = "effect_LL_figure.png", align = "center", height = 500, width = 600),
               
               p(strong("Based on this information, please specify your best guess (i.e., mode of your belief), your guess for the almost worst-case scenario (i.e., 2.5th percentile of your belief), 
                        and your guess for the best-case scenario (i.e., 97.5th percentile of  your belief) of the effectiveness of the program on the sliders below."))
               
               ),  
             
             
             # sidebarLayout(
               sidebarPanel(
                 
                 sliderInput(inputId = "mode6", 
                             label = "Best guess (mode):", 
                             min = -1, max = 2, value = 0.5, step= 0.05),
                 
                 sliderInput(inputId = "lower6", 
                             label = "Almost worst-case scenario (2.5th quantile):", 
                             min = -1, max = 2, value = 0.5, step = 0.05),
                 
                 sliderInput(inputId = "upper6", 
                             label = "Almost best-case scenario (97.5th quantile):", 
                             min = -1, max = 2, value = 0.5, step = 0.05)
                 # error message to make sure that median is in the middle of lower bound and upper bound of the prior credible interval
               ),
               
               # mainPanel(
               #   plotOutput("quantilePlot2")
              #  )
             #  ), 
           
           mainPanel(
             hr(), 
             
             p(strong("Based on your previous experience and the program description, do you have a subjective belief on the average cost per participant if Literacy Ladders is replicated?")), 
             p("* As the principal, you care about the value of all the resources contributed by the school, including personnel, facilities and licence fee."), 
             
             checkboxGroupInput("prior_cost_LL", 
                                
                                label = h5("Prior Belief on the average cost of Literacy Ladders"), 
                                
                                choices = list("No, I don't have a belief on the average cost." = 1, 
                                               "Yes, I have a belief on the average cost." = 2),
                                selected = 0), 
             
             p(strong("If you do have a belief, please specify your best guess (i.e., mode of your belief), your guess for the almost least-costly scenario (i.e., 2.5th percentile of your belief), and your guess for the almost most-costly scenario (i.e., 97.5th percentile of your belief) of the average cost of the program on the sliders below."))
           ),
           
           # sidebarLayout(position = "left",
                         
                         sidebarPanel(
                           
                           sliderInput(inputId = "mode7", 
                                       label = "Best guess (mode):", 
                                       min = 100, max = 400, value = 50, step= 10),
                           
                           sliderInput(inputId = "lower7", 
                                       label = "Almost least-costly scenario (2.5th quantile):", 
                                       min = 100, max = 400, value = 50, step = 10),
                           
                           sliderInput(inputId = "upper7", 
                                       label = "Almost most-costly scenario (97.5th quantile):", 
                                       min = 100, max = 400, value = 50, step = 10)
                           # error message to make sure that median is in the middle of lower bound and upper bound of the prior credible interval
                           
                         ),
                         
                         #mainPanel(
                          # plotOutput("quantilePlot3")
                        # )
          # ), 
           
           
           mainPanel(
             
             hr(),
             h5("Control"),   
             p("Your assistant reports the findings of a recent cost analysis on Literacy Ladders to you. In the experimental setting, the best guess for the true average cost per participant contributed by the school is $180, 
               while the probability that the true value falls between $120 and $240 is 95%. 
               However, students who received Literacy Ladders in the experiment are quite different from the students at your school in terms of demographic characteristics, as shown in the following table. 
               Previous research shows that costs of a program are positively correlated with these students characteristics.
               "), 
             
             img(src = "stuchar_LL_1.png", align = "center", height = 100, width = 600),
             
             h5("Treatment 1"),  
             p("Your assistant reports the findings of a recent cost analysis on Literacy Ladders to you. In the experimental setting, the best guess for the true average cost per participant contributed by the school is $180, 
               while the probability that the true value falls between $120 and $240 is 95%. 
               However, students who received Literacy Ladders in the experiment are quite different from the students at your school in terms of demographic characteristics. 
               Previous research shows that costs of a program are positively correlated with these students characteristics. So your assistant runs a model and estimates a localized average cost, as reported in the following table. 
               The point estimate is the best guess for the true value, while the probability that the true value falls between the 2.5th quantile and the 97.5th quantile is 95%."),
             
             img(src = "stuchar_LL_2.png", align = "center", height = 200, width = 600),
             
             
             h5("Treatment 2"),
             p("Your assistant reports the findings of a recent cost analysis on Literacy Ladders to you. In the experimental setting, the best guess for the true average cost per participant contributed by the school is $180, 
               while the probability that the true value falls between $120 and $240 is 95%. 
               However, students who received Literacy Ladders in the experiment are quite different from the students at your school in terms of demographic characteristics. 
               Previous research shows that costs of a program are positively correlated with these students characteristics. So your assistant runs a model and estimates a localized average cost, as reported in the following table. 
               The point estimate is the best guess for the true value, while the probability that the true value falls between the 2.5th quantile and the 97.5th quantile is 95%."), 
             
             img(src = "stuchar_LL_3.png", align = "center", height = 200, width = 600),
             
             p(strong("Based on this information, please specify your best guess (i.e., mode of your belief), your guess for the almost least-costly scenario (i.e., 2.5th percentile), 
                      and your guess for the almost most-costly scenario (i.e., 97.5th percentile) of the average cost of the program on the sliders below."))
             
             ),  
           
           # sidebarLayout(position = "left",
                         
                         sidebarPanel(
                           
                           sliderInput(inputId = "mode8", 
                                       label = "Best guess (mode):", 
                                       min = 200, max = 500, value = 50, step= 10),
                           
                           sliderInput(inputId = "lower8", 
                                       label = "Almost least-costly scenario (2.5th percentile):", 
                                       min = 200, max = 500, value = 50, step = 10),
                           
                           sliderInput(inputId = "upper8", 
                                       label = "Almost most-costly scenario (97.5th percentile):", 
                                       min = 200, max = 500, value = 50, step = 10)
                           # error message to make sure that median is in the middle of lower bound and upper bound of the prior credible interval
                           
                         )
                         
                        # mainPanel(
                           #plotOutput("quantilePlot4")
                           
                         #)
           )     
  
),
  
  tabPanel(title = "Comparison", 
           
           verticalLayout(
             mainPanel(
               p("Now you need to decide which program to acquire for the 100 struggling readers at your school."), 
               
               p(strong("What is the primary criterion for you to make the decision?")), 
               
               checkboxGroupInput("criterion", 
                                  
                                  label = h5("Primary criterion for decision-making (Choose ONLY ONE)"), 
                                  
                                  choices = list("Increase students' comprehension test score to a certain level" = 1, 
                                                 "Choose an affordable program" = 2, 
                                                 "Minimize the cost-effectiveness ratio (i.e., achieve the most with a certain amount of costs)" = 3),
                                  selected = 0), 
               
               hr(), 
               
               p("The following three diagrams show the comparision of your posterior beliefs (i.e., your updated belief in the light of research evidence) 
                 on the effectiveness, costs, and cost-effectiveness ratios for Reading Now and Literacy Ladders.")
               ), 
             
             mainPanel(
               plotOutput("Plot1")
             ), 
             
             mainPanel(
               plotOutput("Plot2")
             ), 
             
             mainPanel(
               plotOutput("Plot3")
             ), 
             
             mainPanel(
               p(strong("Among the three diagrams, which one helps you the most to make your decision?")), 
               
               checkboxGroupInput("diagrams_for_decision", 
                                  
                                  label = h5("Which diagrams helps the most?"), 
                                  
                                  choices = list("Comparision of the posterior beliefs on EFFECTIVENESS" = 1, 
                                                 "Comparision of the posterior beliefs on AVERAGE COST" = 2, 
                                                 "Comparision of the posterior beliefs on COST-EFFECTIVENESS RATIO" = 3),
                                  selected = 0), 
               
               hr(), 
               
               p(strong("Which program have you decided to acquire?")), 
               
               checkboxGroupInput("program_to_choose", 
                                  
                                  label = h5("Your decision is"), 
                                  
                                  choices = list("Reading Now" = 1, 
                                                 "Literacy Ladders" = 2), 
                                  selected = 0)
               )
            
  
           ) # for verticle layout
     ) # for the tab
    
           
  )

server <- function(input, output) {
  
  # x <- seq(0, 1, by = 0.01)
  nsim <- 10000
  
  func <- function(x, points){
    y <- numeric(3)
    data <- rsn(10000, x[1], x[2], x[3])
    mode <- density(data)$x[which.max(density(data)$y)]
    y[1] <- points[1] - mode
    y[2] <- points[2] - qsn(0.025, x[1], x[2], x[3], solver="RFB")
    y[3] <- points[3] - qsn(0.975, x[1], x[2], x[3], solver="RFB")
    y
  }
  
  data1 <- reactive({
    
    points1 <- c(input$mode1, input$lower1, input$upper1)
    sol1 <- nleqslv(c(0, 1, 2), func, points = points1)
    data1 <- rsn(nsim, sol1$x[1], sol1$x[2], sol1$x[3])   #  data1 <- qsn(x, sol1$x[1], sol1$x[2], sol1$x[3], solver="RFB") (quantile function)
    return(data1)
    # return(c(points1, sol1, data1))
  })
  
  data2 <- reactive({
    
    points2 <- c(input$mode2, input$lower2, input$upper2)
    sol2 <- nleqslv(c(0, 1, 2), func, points = points2)
    data2 <- rsn(nsim, sol2$x[1], sol2$x[2], sol2$x[3])
    return(data2)
    
  })
  
  
  data3 <- reactive({
    
    points3 <- c(input$mode3, input$lower3, input$upper3)
    sol3 <- nleqslv(c(300, 1, 2), func, points = points3)
    data3 <- rsn(nsim, sol3$x[1], sol3$x[2], sol3$x[3])
    return(data3)
  })
  
  data4 <- reactive({
    
    points4 <- c(input$mode4, input$lower4, input$upper4)
    sol4 <- nleqslv(c(300, 1, 2), func, points = points4)
    data4 <- rsn(nsim, sol4$x[1], sol4$x[2], sol4$x[3])
    return(data4)
  })
  
  data5 <- reactive({
    
    points5 <- c(input$mode5, input$lower5, input$upper5)
    sol5 <- nleqslv(c(0, 1, 2), func, points = points5)
    data5 <- rsn(nsim, sol5$x[1], sol5$x[2], sol5$x[3])
    return(data5)
  })
  
  
  data6 <- reactive({
    
    points6 <- c(input$mode6, input$lower6, input$upper6)
    sol6 <- nleqslv(c(0, 1, 2), func, points = points6)
    data6 <- rsn(nsim, sol6$x[1], sol6$x[2], sol6$x[3])
    return(data6)
  })
  
  
  data7 <- reactive({
    
    points7 <- c(input$mode7, input$lower7, input$upper7)
    sol7 <- nleqslv(c(300, 1, 2), func, points = points7)
    data7 <- rsn(nsim, sol7$x[1], sol7$x[2], sol7$x[3])
    
    return(data7)
  })
  
  data8 <- reactive({
    
    points8 <- c(input$mode8, input$lower8, input$upper8)
    sol8 <- nleqslv(c(300, 1, 2), func, points = points8)
    data8 <- rsn(nsim, sol8$x[1], sol8$x[2], sol8$x[3])
    return(data8)
    
  })
  
  
#  output$quantilePlot1 <- renderPlot({
#    plot(x, data1(), type = "l", lty = 1, lwd = 3, col = "red", xlab = "Quantile", ylab = "Effect size", main = "Your Prior Belief on the Effect of Reading Now")
#    points(c(0.025, 0.975), c(input$lower1, input$upper1), col = "red", pch = 19)
#    # put the mode here
    
#  })
  
  # Effect of Reading Now and Literacy Ladders
  output$Plot1 <- renderPlot({
    
    hist1 <- hist(data2(), plot = FALSE)
    hist2 <- hist(data6(), plot = FALSE)
    col1 <- rgb(1, 0, 0, 0.2)
    col2 <- rgb(0, 0, 1, 0.2)
    xlim <- range(hist1$breaks,hist2$breaks)
    ylim <- range(0,hist1$counts,
                  hist2$counts)
    
    plot(hist1, xlim = xlim, ylim = ylim, col = col1, xlab = "Effect size", panel.first = grid(), main = "Your Posterior Beliefs on the Effect of Reading Now and Literacy Ladders")
    # add the second plot here
    opar <- par(new = FALSE)
    plot(hist2, xlim = xlim, ylim = ylim,
         xaxt = 'n', yaxt = 'n', ## don't add axes
         col = col2, add = TRUE) ## relative, not absolute frequency
    ## add a legend in the corner
    legend("topleft", c("Reading Now", "Literacy Ladders"), fill = c(col1, col2), bty = "n", border = NA)
    par(opar)
 
  })

  
  output$Plot2 <- renderPlot({
    
    hist1 <- hist(data4(), plot = FALSE)
    hist2 <- hist(data8(), plot = FALSE)
    col1 <- rgb(1, 0, 0, 0.2)
    col2 <- rgb(0, 0, 1, 0.2)
    xlim <- range(hist1$breaks,hist2$breaks)
    ylim <- range(0,hist1$counts,
                  hist2$counts)
    
    plot(hist1, xlim = xlim, ylim = ylim, col = col1, xlab = "Dollars", panel.first = grid(), main = "Your Posterior Beliefs on the Average Cost of Reading Now and Literacy Ladders")
    # add the second plot here
    opar <- par(new = FALSE)
    plot(hist2, xlim = xlim, ylim = ylim,
         xaxt = 'n', yaxt = 'n', ## don't add axes
         col = col2, add = TRUE) ## relative, not absolute frequency
    ## add a legend in the corner
    legend("topleft", c("Reading Now", "Literacy Ladders"), fill = c(col1, col2), bty = "n", border = NA)
    par(opar)
  })
  
  output$Plot3 <- renderPlot({
    
    ce1 <- data4() / data2()
    ce2 <- data8() / data6()
    ce1_95th <- ce1[ce1 > quantile(ce1, 0.025) & ce1 < quantile(ce1, 0.975)]
    ce2_95th <- ce2[ce2 > quantile(ce2, 0.025) & ce2 < quantile(ce2, 0.975)]
    
    hist1 <- hist(ce1_95th, plot = FALSE)
    hist2 <- hist(ce2_95th, plot = FALSE)
    col1 <- rgb(1, 0, 0, 0.2)
    col2 <- rgb(0, 0, 1, 0.2)
    xlim <- range(hist1$breaks,hist2$breaks)
    ylim <- range(0,hist1$counts,
                  hist2$counts)
    
    plot(hist1, xlim = xlim, ylim = ylim, col = col1, xlab = "Dollars Per Effect Size", panel.first = grid(), main = "Your Posterior Beliefs on the Cost-effectiveness Ratio of Reading Now and Literacy Ladders")
    # add the second plot here
    opar <- par(new = FALSE)
    plot(hist2, xlim = xlim, ylim = ylim,
         xaxt = 'n', yaxt = 'n', ## don't add axes
         col = col2, add = TRUE) ## relative, not absolute frequency
    ## add a legend in the corner
    legend("topleft", c("Reading Now", "Literacy Ladders"), fill = c(col1, col2), bty = "n", border = NA)
    par(opar)
    
  })
  
}

shinyApp(ui = ui, server = server)

