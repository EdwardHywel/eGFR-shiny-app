library(shiny)


textInputRow<-function (inputId, label1, label2="", value = "") 
{
  div(style="display:inline-block",
      tags$label(label1, `for` = inputId), 
      tags$input(id = inputId, type = "numeric", value = value,class="input-mini", style="width: 50px"),
      tags$label(label2, `for` = inputId))
}

textInputRow2<-function (inputId1, inputId2, label1, label2="", label3="", value1 = "", value2 = "") 
{
  div(style="display:inline-block",
      tags$label(label1, `for` = inputId1), 
      tags$input(id = inputId1, type = "numeric", value = value1, class="input-mini", style="width: 50px"),
      tags$label(label2, `for` = inputId1),
      tags$input(id = inputId2, type = "numeric", value = value2, class="input-mini", style="width: 50px"),
      tags$label(label3, `for` = inputId2))
}


shinyUI(fluidPage(tags$head(includeScript("google-analytics.js")),
					
				  theme = "bootstrap.css",

                  
                  
                  titlePanel("Tool to estimate the Glomerular Filtration Rate (GFR)"),
                  withMathJax(),
                  tags$script("
                              MathJax.Hub.Config({
                              tex2jax: {
                              inlineMath: [['$','$'], ['\\(','\\)']],
                              processEscapes: true
                              }
                              });"
  ),
  
  
  sidebarLayout(
    sidebarPanel(width = 5,  h3("Input Data"),
                 radioButtons("CreatUnit", "Choose the unit for serum creatinine and then input it's value", choices = c("mg/dL"="mg", "umol/L"="umol"), inline=T),
                 conditionalPanel(condition = 'input.CreatUnit == "mg"', 
                                  textInputRow("Creat", "Creatinine:", "mg/dL", value="1")),
                 conditionalPanel(condition = 'input.CreatUnit == "umol"', 
                                  textInputRow("Creat", "Creatinine:", "umol/L", value="100")),
                 br(),
                 radioButtons("AgeUnit", "Choose the unit for Age and then input it's value", choices = c("years"="y", "years and months"="ym"), inline=T),
                 conditionalPanel(condition = 'input.AgeUnit == "y"', 
                                  textInputRow("Age", "Age:", "years", value="50")),
                 conditionalPanel(condition = 'input.AgeUnit == "ym"', 
                                  textInputRow2(inputId1 = "Age", inputId2 = "AgeMonths",  "Age:", "years, ", "months", value1=50, value2=0)),
                 br(),
                 radioButtons("HtUnit", "Choose the unit for height and then input it's value", 
                             choices = c("Metric"="met", "Imperial"="imp"),  inline=T),
                 conditionalPanel(condition = 'input.HtUnit == "met"', 
                                  textInputRow("Ht", "Height:", "cm", value="170")),
                 conditionalPanel(condition = 'input.HtUnit == "imp"', 
                                  textInputRow2(inputId1 = "feet", inputId2 = "inch", "Height:", "feet,", "inshes", value1 = 5, value2 = 10)), 
                                  # textInputRow("feet", "Height:", "feet", value = "5"),
                                  # textInputRow("inch", "Height:", "inches", value="10")),
                 br(),
                 radioButtons("WtUnit", "Choose the unit for weight and then input it's value", 
                             choices = c("Metric"="met", "Imperial" = "imp"),  inline=T),
                 conditionalPanel(condition = 'input.WtUnit == "met"', 
                                  textInputRow("Wt", "Weight:", "kg", value="80")),
                 conditionalPanel(condition = 'input.WtUnit == "imp"', 
                                  textInputRow2(inputId1 = "stone", inputId2 = "pounds", "Weight:", "stone,", "pounds", value1 = 12, value2 = 10)), 
                 br(),
                 radioButtons("Sex","Choose the gender",choices = c("Male"="M","Female"= "F"), inline=T),
                 numericInput("Conf", "Choose the confidence level for the prediction interval [%]", value=95),
                 br(),
                 checkboxInput("Hyptest", "Click the box if you wish to estimate a probability that the actual GFR is below or above a threshold value", value = TRUE), 
                 conditionalPanel(condition = 'input.Hyptest',
                                               numericInput("TestValue", "Imput the GFR threshold value",value="50", min=0), 
                                  radioButtons("LesGre", "Do you wish to calculate the probability that the actual GFR is below or above this threshhold value?", 
                                               choices = c("Below"="below", "Above"="above"), inline = T)
                                  )
    ),
    
  
  
    
    
    mainPanel(width=7,
              tabsetPanel(
                tabPanel(h3("Results"),
                         h3("The estimates provided are for guidance only", style="color:red"),
                         h3(paste0("Estimated GFR using the new model:", '\u00B9')),
                         uiOutput("JWpred"),
                         textOutput("JW_CI1"), 
                         div(class="alert alert-info", style="font-size: 20px; width: 250px; text-align: left; font-weight:bold", uiOutput("JW_CI2")),
                         uiOutput('JWprob1'),
                         uiOutput('JWprob2'),
                         uiOutput('JWprob3'),
                         h3(paste0("Estimated GFR using the BSA adjusted CKD-EPI model:", '\u00B2')),
                         uiOutput("CKDpred"), 
                         textOutput("BSA_statment"),
                         # h3("The variables used to predict GFR are:"),
                         # textOutput("text1"),
                         # textOutput("text2"),
                         # textOutput("text3"),
                         # textOutput("text4"),
                         # textOutput("text5"),
                         # textOutput("text6"),
                         #     helpText("This in the model that has been fitted"),
                         #     verbatimTextOutput("modelSummary"),
                         #     h4("The variables used to predict GFR are"),
                         #     verbatimTextOutput("Table"),
                         br(),
                         h5("References:"),
                         p("1. Janowitz T, Williams EH, et al. A new model for estimating glomerular filtration rate in patients with cancer."),
                         p("2. Levey AS, Stevens LA, Schmid CH, Zhang Y, Castro AF, Feldman HI, et al. A New Equation to Estimate Glomerular Filtration Rate. Ann Intern Med. 2009;150:604-612."),
                         p("3. DuBois D, DuBois E. A formula to estimate the approximate surface area if height and weight be known. Arch Intern Med. 1916;17:863–71. ")
                         
                ),
                tabPanel(h3("Histograms"), 
                         br(),
                         p("This tab shows the histogram of the data used to develop the model with the input values shown as a red vertical line. The maximum and minimum of the variables are shown as grey vertical lines. The data input data values should not be outside the range of the data used to develop the model"),
                         h4("Blood serum creatinine"), 
                         plotOutput("hist1"), 
                         h4("Age"), 
                         plotOutput("hist2"), 
                         h4("Body surface area"), 
                         plotOutput("hist3"),
                         h4("Height"), 
                         plotOutput("hist4"),
                         h4("Weight"), 
                         plotOutput("hist5")
                ),
                tabPanel(h3("Equations"), 
                         
                         h3("The new equation used to predict this value is as follows:"), 
                         div(img(src='NewEquation.png'), align = "center"),
                         # sprintf('$$\\begin{align} 
                         # \\sqrt{\\mathrm{GFR}} &= 1.8140 + 0.1914\\mathrm{Age} + 4.7328\\mathrm{BSA} - 3.7162\\log(\\mathrm{Cre}) - 0.9142\\log(\\mathrm{Cre})^2  \\nonumber \\\\
                         # & \\quad + 1.0628\\log(\\mathrm{Cre})^3 - 0.0297\\mathrm{Age}\\times\\mathrm{BSA} + \\left(0.0202 +0.0125\\mathrm{Age}\\right)[\\mathrm{if} \\, \\mathrm{Sex=Male}] \\nonumber
                         # \\end{align}$$'),
                         tags$div("where:"),
                         tags$div(
                           tags$ul(
                             tags$li("GFR is Glomerular filtration rate with units ml/min"),
                             tags$li("Age has the units years"), 
                             tags$li("BSA is body surface area with units m\u00B2 calculated using the DuBois equation"),
                             tags$li("Cre is blood serum creatinine concentration with units mg/dL")
                           )
                         ),
                         tags$div("and the coefficients in the equation have been rounded to 4 decimal places."),
                         h3("The CKD-EPI equation takes the following form:"), 
                        #  sprintf('$$\\mathrm{GFR_{nonadjusted}} = 
                        #   \\begin{cases} 
                        #       141 \\times \\mathrm{min} \\left(\\frac{\\mathrm{Cre}}{0.7}, 1\\right)^{-0.329} \\times \\mathrm{max} \\left(\\frac{\\mathrm{Cre}}{0.7}, 1 \\right)^{-1.209} \\times \\mathrm{Age}^{0.993} \\times 1.018 & \\mathrm{if} \\, \\mathrm{Sex=Female} \\\\
                        #       141 \\times \\mathrm{min} \\left(\\frac{\\mathrm{Cre}}{0.9}, 1\\right)^{-0.411} \\times \\mathrm{max} \\left(\\frac{\\mathrm{Cre}}{0.9}, 1 \\right)^{-1.209} \\times \\mathrm{Age}^{0.993}  & \\mathrm{if} \\, \\mathrm{Sex=Male} 
                        # \\end{cases}
                        # $$'),
                        div(img(src='CKDEquation.png'), align = "center"),
                        tags$div('where GFR now has the units ml/min/1.73m\u00B2 and all other variables have the same units as above. This non adjusted estimated GFR value is then BSA-adjusted by the following equation'), 
                         # sprintf('$$\\mathrm{GFR_{adjusted}} =  \\mathrm{GFR_{non adjusted}} \\times \\frac{1.73}{\\mathrm{BSA}}$$')
                        div(img(src='AdjustmentEquation.png'), align = "center")
                        
                )
              )
    )
  )
                  ))

