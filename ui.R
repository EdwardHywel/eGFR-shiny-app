library(shiny)

textInputRow_one<-function (text, inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$b(text),
      tags$input(id = inputId, type = "number", value = value, style="width: 4em"),
      label)
}


textInputRow_two<- function (text, inputId1, inputId2 , label1, label2, 
                                value1 = "", value2 = "") 
{
  div(style="display:inline-block",
      tags$b(text),
      tags$input(id = inputId1, type = "number", value = value1, style="width: 4em"),
      label1, 
      tags$input(id = inputId2, type = "number", value = value2, style="width: 4em"),
      label2) 
  }


################################################################################

## THis need to b included somewhere

# tags$head(includeScript("google-analytics.js")),
# 
# theme = "bootstrap.css",



navbarPage("Tool to estimate GFR",

  
  tabPanel("Single Patient",
           withMathJax(),
           tags$script("
                        MathJax.Hub.Config({
                        tex2jax: {
                        inlineMath: [['$','$'], 
                        processEscapes: true
                        }
                        });"),
           
  sidebarLayout(
    sidebarPanel(width = 5,  h3("Patient data"),
                 radioButtons("CreatUnit", "Creatinine units:", choices = c("mg/dL"="mg", "umol/L"="umol"), inline=T),
                 conditionalPanel(condition = 'input.CreatUnit == "mg"', 
                                  textInputRow_one("Blood serum creatinine:", "Creat", "mg/dl", value="1")),
                 conditionalPanel(condition = 'input.CreatUnit == "umol"', 
                                  textInputRow_one("Blood serum creatinine:", "Creat", "umol/L", value="88")),
                 # numericInput("Creat", "Blood serum creatinine:",value="1", min=0),
                 radioButtons("CreatType", tags$div("Is the creatinine IDMS tracable?", 
                                                    tags$sup("*")), 
                              choices = c("Yes"="IDMS", "No"="Non_IDMS"), inline=T),
                 textInputRow_one("Age:", "Age", "years", value="50"),
                 br(),
                 br(),
                 radioButtons("HtUnit", "Height units:", 
                             choices = c("Metric"="met", "Imperial"="imp"),  inline=T),
                 conditionalPanel(condition = 'input.HtUnit == "met"', 
                                  textInputRow_one("Height:", "Ht", "cm" ,value="170")),
                 conditionalPanel(condition = 'input.HtUnit == "imp"', 
                                  textInputRow_two(text = "Height:", inputId1 = "feet", inputId2 = "inch", 
                                                      label1 = "feet", label2 = "inches", value1 = "5", value2 = "10")),
                 br(),
                 radioButtons("WtUnit", "Weight units:", 
                             choices = c("Metric"="met", "Imperial" = "imp"),  inline=T),
                 conditionalPanel(condition = 'input.WtUnit == "met"', 
                                  textInputRow_one("Weight:", "Wt", "kg",value="80")),
                 conditionalPanel(condition = 'input.WtUnit == "imp"', 
                                  textInputRow_two(text = "Weight:", inputId1 = "stone", 
                                                   inputId2 = "pounds", label1 = "stone", 
                                                   label2 = "pounds", value1 = "12", value2 = "10")),
                 br(),
                 radioButtons("Sex","Gender:",choices = c("Male"="M","Female"= "F"), inline=T),
                 textInputRow_one("Confidence level for the prediction interval:", "Conf", "%", "95"),
                 br(),
                 checkboxInput("Hyptest", "Do you wish to estimate the probability that the true GFR is below or above a threshold value", 
                               value = FALSE), 
                 conditionalPanel(condition = 'input.Hyptest',
                                  textInputRow_one("GFR threshold value", "TestValue", "ml/min", "50"),
                                  radioButtons("LesGre", "Probaility that try value is above or below?", 
                                               choices = c("Below"="below", "Above"="above"), inline = T)
                                  ),
                 radioButtons("UseOld", "Use the original CamGFR for non-IDMS creatinine data:",
                              choices = c("Yes"=T,"No"= F), inline = T)
    ),

    
    
    mainPanel(width=7,
              tabsetPanel(
                tabPanel(h3("Results"),
                         h3("The estimates provided are for guidance only", style="color:red"),
                         h3(HTML("Estimated GFR using CamGFR:<sup>1</sup>")),
                         div(class="alert alert-success", style="font-size: 20px; width: 250px; text-align: left", uiOutput("CamGFR_estimate")),
                         textOutput("text8"), 
                         div(class="alert alert-info", style="font-size: 20px; width: 250px; text-align: left", uiOutput("CamGFR_interval")),
                         uiOutput('ex1'),
                         uiOutput('ex2'),
                         uiOutput('ex3'),
                         h3(paste0("Estimated GFR using the BSA adjusted CKD-EPI model:", '\u00B2')),
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
                         # div(img(src='NewEquation.png'), align = "center"),
                         sprintf('$$\\begin{align}
                         \\sqrt{\\mathrm{GFR}} &= 1.8140 + 0.01914\\mathrm{Age} + 4.7328\\mathrm{BSA} - 3.7162\\log(\\mathrm{Cre}) - 0.9142\\log(\\mathrm{Cre})^2  \\nonumber \\\\
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
                        # div(img(src='CKDEquation.png'), align = "center"),
                        tags$div('where GFR now has the units ml/min/1.73m\u00B2 and all other variables have the same units as above. This non adjusted estimated GFR value is then BSA-adjusted by the following equation'), 
                        sprintf('$$\\mathrm{GFR_{adjusted}} =  \\mathrm{GFR_{non adjusted}} \\times \\frac{1.73}{\\mathrm{BSA}}$$')
                        # div(img(src='AdjustmentEquation.png'), align = "center")
                        
                )
              ))
  )),

  tabPanel("Multiple Patients", 
    sidebarLayout(
      sidebarPanel(width = 5, 
                   fileInput('data_file', 'Patient data'  ),
                    # checkboxInput("header", "Header", TRUE),
                   radioButtons("sep", "Separator",
                                choices = c(Semicolon = ";",
                                            Comma = ",",
                                            Tab = "\t"),
                                selected = ",", inline = T),
                   p(HTML("<b>This file should contain columns:</b><br/> Creatinine (mg/dL)<br/> 
                          Gender (M or F)<br/>  Height (cm)<br/>  Weight (Kg)<br/>  CreatinineType (IDMS or Non_IDMS)")),
                   textInputRow_one("Confidence level for the prediction interval:", "Conf", "%", "95")
                   ), 
    
    mainPanel(width = 7, 
      tabsetPanel(
        tabPanel("Input Data",
                 h4("Data"),
                 tableOutput("input_file")
                 # tableOutput('input_file_xlsx')
                 #this two commands should be one, but it works
                 
        ),
        tabPanel("Output Data",
                 h4("Data"),
                 tableOutput("output_file")
                 # tableOutput('input_file_xlsx')
                 #this two commands should be one, but it works
                 
        ),
        tabPanel("Summary"
                 # tableOutput("summary") #this doesn´t work because the load input Data don´t work in the server
        ),
        tabPanel("New")
    )
    ) # end mainPanel
    ) # end sidebarLayout
    ) # end tabPanel


) # end navbarPage

