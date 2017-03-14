
library(shiny)

textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-mini"))
}



shinyUI(fluidPage(theme = "bootstrap.css",
                  
                  titlePanel("Estimated Glomerular Filtration Rate Calculator"),
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
                 numericInput("Creat", "Enter the patient's serum creatinine level",value="1"),
                 radioButtons("CreatUnit", "Choose the creatinine units", choices = c("mg/dL"="mg", "umol/L"="umol"), inline=T),
                 textInput("Age", "Enter the patient's age in years",value="50"),
                 #       textInput("Ht", "Enter patients Height",value=""),
                 selectInput("HtUnit", "Choose the unit for the patient's height and then enter it's value", 
                             choices = c("Metric"="met", "Imperial"="imp")),
                 conditionalPanel(condition = 'input.HtUnit == "met"', 
                                  textInputRow("Ht", "Height: cm",value="170")),
                 conditionalPanel(condition = 'input.HtUnit == "imp"', 
                                  textInputRow("feet", "Height: ", value = "5"),
                                  textInputRow("inch", "Height: inches", value="10")),
                 br(),
                 selectInput("WtUnit", "Choose the unit for the patient's weight and then enter it's value", 
                             choices = c("Metric"="met", "Imperial" = "imp")),
                 conditionalPanel(condition = 'input.WtUnit == "met"', 
                                  textInputRow("Wt", "Weight: kg",value="80")),
                 conditionalPanel(condition = 'input.WtUnit == "imp"', 
                                  textInputRow("stone", "Weight: stones", value="12"),
                                  textInputRow("pounds", "Weight: pounds", value="10")),
                 br(),
                 radioButtons("Sex","Choose the patient's sex",choices = c("Male"="M","Female"= "F"), inline=T),
                 numericInput("Conf", "Confidence level: %", value=95)
    ),
    
    mainPanel(width=7,
              tabsetPanel(
                tabPanel(h3("Results"),
                         h3(paste0("Estimated GFR using the new model:", '\u00B2')),
                         div(class="alert alert-success", style="font-size: 20px; width: 250px; text-align: left", uiOutput("text7")),
                         textOutput("text8"), 
                         div(class="alert alert-info", style="font-size: 20px; width: 250px; text-align: left", uiOutput("text9")),
                         h3(paste0("Estimated GFR using the BSA adjusted CKD-EPI model:", '\u00B3')),
                         div(class="alert alert-success", style="font-size: 20px; width: 250px; text-align: left", uiOutput("text10")), 
                         textOutput("text6.5"),
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
                         p("1. DuBois D, DuBois E. A formula to estimate the approximate surface area if height and weight be known. Arch Intern Med. 1916;17:863–71. "),
                         p("2. Janowitz T, Williams EH, et al. A new model for estimating glomerular filtration rate in patients with cancer."),
                         p("3. Levey AS, Stevens LA, Schmid CH, Zhang Y, Castro AF, Feldman HI, et al. A New Equation to Estimate Glomerular Filtration Rate. Ann Intern Med. 2009;150:604-612.")
                ),
                tabPanel(h3("Histograms"), 
                         br(),
                         p("This tab shows the histogram of the data used to develop the model with the input values shown as a red vertical line. The maximum and minimum of the variables are shown as grey vertical lines. The data input data values should not be outside the range of the data used to develop the model"),
                         h4("Blood serum creatinine"), 
                         plotOutput("hist1"), 
                         h4("Age"), 
                         plotOutput("hist2"), 
                         h4("Body surface area"), 
                         plotOutput("hist3")
                ),
                tabPanel(h3("Equations"), 
                         
                         h3("The new equation used to predict this value is as follows:"), 
                         sprintf('$$\\begin{align} 
                         \\sqrt{\\mathrm{GFR}} &= 1.8140 + 0.1914\\mathrm{Age} + 4.7328\\mathrm{BSA} - 3.7162\\log(\\mathrm{Cre}) - 0.9142\\log(\\mathrm{Cre})^2  \\nonumber \\\\
                         & \\quad + 1.0628\\log(\\mathrm{Cre})^3 - 0.0297\\mathrm{Age}\\times\\mathrm{BSA} + \\left(0.0202 +0.0125\\mathrm{Age}\\right)[\\mathrm{if} \\, \\mathrm{Sex=Male}] \\nonumber
                         \\end{align}$$'),
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
                         sprintf('$$\\mathrm{GFR_{nonadjusted}} = 
                          \\begin{cases} 
                              141 \\times \\mathrm{min} \\left(\\frac{\\mathrm{Cre}}{0.7}, 1\\right)^{-0.329} \\times \\mathrm{max} \\left(\\frac{\\mathrm{Cre}}{0.7}, 1 \\right)^{-1.209} \\times \\mathrm{Age}^{0.993} \\times 1.018 & \\mathrm{if} \\, \\mathrm{Sex=Female} \\\\
                              141 \\times \\mathrm{min} \\left(\\frac{\\mathrm{Cre}}{0.9}, 1\\right)^{-0.411} \\times \\mathrm{max} \\left(\\frac{\\mathrm{Cre}}{0.9}, 1 \\right)^{-1.209} \\times \\mathrm{Age}^{0.993}  & \\mathrm{if} \\, \\mathrm{Sex=Male} 
                        \\end{cases}
                        $$'),
                         tags$div('where GFR now has the units ml/min/1.73m\u00B2 and all other variables have the same units as above. This non adjusted estimated GFR value is then BSA adjusted by the following equation'), 
                         sprintf('$$\\mathrm{GFR_{adjusted}} =  \\mathrm{GFR_{non adjusted}} \\times \\frac{1.73}{\\mathrm{BSA}}$$')
                )
              )
    )
  )
                  ))

