library(shiny)

textInputRow_one<-function (text, inputId, label, value = "", ...) 
{
  div(style="display:inline-block",
      tags$b(text),
      tags$input(id = inputId, type = "number", value = value, style="width: 4em", ...),
      label)
}


textInputRow_two<- function (text, inputId1, inputId2 , label1, label2, 
                                value1 = "", value2 = "", ...) 
{
  div(style="display:inline-block",
      tags$b(text),
      tags$input(id = inputId1, type = "number", value = value1, style="width: 4em", ...),
      label1, 
      tags$input(id = inputId2, type = "number", value = value2, style="width: 4em", ...),
      label2) 
  }


################################################################################

## THis need to be included somewhere

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
    sidebarPanel(width = 5,  h4("Patient data"),
                 radioButtons("CreatUnit", "Creatinine units:", choices = c("mg/dL"="mg", "umol/L"="umol"), inline=T),
                 conditionalPanel(condition = 'input.CreatUnit == "umol"', 
                                  textInputRow_one("Blood serum creatinine:", "Creat_umol", "umol/L", value=88, min = 0)),
                 conditionalPanel(condition = 'input.CreatUnit == "mg"', 
                                  textInputRow_one("Blood serum creatinine:", "Creat_mg", "mg/dL", value=1, min = 0)),
                 # numericInput("Creat", "Blood serum creatinine:",value="1", min=0),
                 radioButtons("CreatType", tags$div("Is the creatinine IDMS traceable?", 
                                                    tags$sup("*")), 
                              choices = c("Yes"="IDMS", "No"="Non_IDMS"), inline=T),
                 textInputRow_one("Age:", "Age", "years", value="50", min = 18),
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
                 br(),
                 h4("Model options"),
                 radioButtons("units", "Output units:", 
                              choices = c("ml/min"="ml/min", "ml/min/1.73m\u00B2"="ml/min/1.73m\u00B2"), inline = T),
                 textInputRow_one("Prediction interval level:", "Conf", "%", "95", min = 0, max = 100),
                 br(),
                 checkboxInput("Hyptest", "Do you wish to estimate the probability that the true GFR is below or above a threshold value", 
                               value = FALSE), 
                 conditionalPanel(condition = 'input.Hyptest',
                                  textInputRow_one("GFR threshold value", "TestValue", "ml/min", "50"),
                                  radioButtons("LesGre", "Probaility that try value is above or below?", 
                                               choices = c("Below"="below", "Above"="above"), inline = T)
                                  ),
                 radioButtons("UseOld", tags$div("Use CamGFR v1 for non-IDMS creatinine data:", 
                                                 tags$sup("**")),
                              choices = c("Yes"=T, "No"= F), inline = T, selected = F)
    ),

    
    
    mainPanel(width=7,
              tabsetPanel(
                tabPanel(("Results"),
                         h3("The estimates provided are for guidance only", style="color:red"),
                         h4("This is an updated app for CamGFR v2", style="color:red"),
                         uiOutput("SufA_sentance"),
                         h4(HTML("GFR estimated using CamGFR:<sup>1,2</sup>")),
                         uiOutput("CamGFR_estimate"),
                         # uiOutput('p_below'),
                         # uiOutput('ex2'),
                         # uiOutput('ex3'),
                         # br(),
                         uiOutput("p_below"),
                         h4(HTML("GFR estimated using the BSA adjusted CKD-EPI model:<sup>3</sup>")),
                         uiOutput("CKD_estimate"),
                         h4(HTML("GFR estimated using the BSA adjusted Lund-Malmo model:<sup>4</sup>")),
                         uiOutput("LM_estimate")
                         
                ),
                tabPanel(("Histograms"), 
                         br(),
                         p("This tab shows the histogram of the data used to develop the model with the input values shown as a red vertical line. The maximum and minimum of the variables are shown as grey vertical lines. The data input values should not be outside the range of the data used to develop the model."),
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
                ) # end tabPanel
              ) # end tabSetPanel
              ) # end mainPanel
  ), # end sideBarLayout
  hr(),
  p(HTML("<font color=grey>*IDMS refers to the isotope dilution mass spectrometry reference measurement procedure <br/> **See the Equations tab for further detail</font>"))
  
  ),

  tabPanel("Multiple Patients", 
    sidebarLayout(
      sidebarPanel(width = 5, 
                   radioButtons("sep", "Separator",
                                choices = c(Semicolon = ";",
                                            Comma = ",",
                                            Tab = "\t"),
                                selected = ",", inline = T),
                   fileInput('data_file', 'Upload file', 
                             accept = c('text/csv','text/comma-separated-values,text/plain','.csv','.xlsx')),
                   checkboxInput("Nonident", "This file contains no patient identifiable data", 
                                 value = FALSE), 
                   textInputRow_one("Confidence level for prediction interval:", "ConfMulti", "%", "95"),
                   radioButtons("UseOldMulti", tags$div("Use CamGFR v1 for non-IDMS creatinine data:", 
                                                   tags$sup("*")),
                                choices = c("Yes"=T,"No"= F), inline = T),
                   br(),
                   br(),
                   downloadButton("downloadData", "Download output data")
                   ), 
    
    mainPanel(width = 7, 
      tabsetPanel(
        tabPanel("Instructions",
                 br(),
                 p(HTML("In this section a comma seperated file (.csv) can be uploaded
                        and GFR will be estimated using both the CamGFR model and the CKD model (if 
                        the file has a different separator please specify). 
                        An excel (.xls or .xlsx) will not work so please convert it to a .csv file 
                        before trying to use this app.")),
                 p(HTML("The input file should have the following column names with the unit in the brackets.")),
                 p(HTML("Creatinine (mg/dL)<br/> 
                          Gender (M or F)<br/>  Height (cm)<br/>  Weight (Kg)<br/>  CreatinineType (IDMS or Non_IDMS)")),
                 tags$strong("Example input data:"),
                 div(tableOutput("example_input"), style = "font-size:100%")
                 
                 
        ),
        tabPanel("Input Data",
                 # h4("Data"),
                 tableOutput("input_file")
                 # tableOutput('input_file_xlsx')
                 #this two commands should be one, but it works
                 
        ),
        tabPanel("Output Data",
                 # h4("Data"),
                 tableOutput("output_file")
                 # tableOutput('input_file_xlsx')
                 #this two commands should be one, but it works
                 
        )
        
    )
    ) # end mainPanel
    ), # end sidebarLayout
    
    hr(),
    p(HTML("<font color=grey>*See the Equations tab for further detail</font>"))
    # p(HTML("<font color=grey>*This option allows you to choose whether to use the original CamGFR model for non-IDMS patients or to use the updated CamGFR model which is a refitter version of the origial and valid for both IDMS or non-IDMS patients with the use of different coefficients. Please vie the Equations and references tab for some further detatil.</font>"))
    
    
    ), # end tabPanel
  
  tabPanel(("Equations and references"), 
           h4(HTML("CamGFR model:<sup>1</sup>")),
           p(HTML("The CamGFR v1 model<sup>2</sup> was developed using a non-IDMS traceable 
                  creatinine data set and hence should only be used for non-IDMS traceable creatinine patients. 
                  The CamGFR v2 model<sup>1</sup> is a refitted version of the original model and is suitable for both IDMS-traceable and and non-IDMS-traceable creatinine. 
                  The two equations for non-IDMS-traceable creatinine produce very similar results and either can be used.  </br>
                  The CamGFR v2 model for patients with IDMS traceable creatinine has the following form and coefficients:")),
           p('$$\\begin{align} 
             \\sqrt{\\mathrm{GFR}} &= 1.154 + 0.018\\mathrm{Age} + 4.772\\mathrm{BSA} - 3.499\\ln(\\mathrm{Cre_{IDMS}}) - 0.738\\ln(\\mathrm{Cre_{IDMS}})^2  \\nonumber \\\\
             & \\quad + 0.699\\ln(\\mathrm{Cre_{IDMS}})^3 - 0.028\\mathrm{Age}\\times\\mathrm{BSA} + \\left(0.302 +0.006\\mathrm{Age}\\right)[\\mathrm{if} \\, \\mathrm{Sex=Male}] \\nonumber
             \\end{align}$$'),
           p("For patients who do not have an IDMS serum creatinine measurement some of the coefficients are adjusted to the following:"),
           p('$$\\begin{align} 
             \\sqrt{\\mathrm{GFR}} &= 1.662 + 0.018\\mathrm{Age} + 4.772\\mathrm{BSA} - 4.049\\ln(\\mathrm{Cre}) - 1.162\\ln(\\mathrm{Cre})^2  \\nonumber \\\\
             & \\quad + 1.532\\ln(\\mathrm{Cre})^3 - 0.028\\mathrm{Age}\\times\\mathrm{BSA} + \\left(0.302 +0.006\\mathrm{Age}\\right)[\\mathrm{if} \\, \\mathrm{Sex=Male}] \\nonumber
             \\end{align}$$'),
           p(HTML("If using non-IDMS traceable creatinine the CamGFR v1 model<sup>3</sup> is valid:")),
           p('$$\\begin{align}
             \\sqrt{\\mathrm{GFR}} &= 1.8140 + 0.01914\\mathrm{Age} + 4.7328\\mathrm{BSA} - 3.7162\\ln(\\mathrm{Cre}) - 0.9142\\ln(\\mathrm{Cre})^2  \\nonumber \\\\
             & \\quad + 1.0628\\ln(\\mathrm{Cre})^3 - 0.0297\\mathrm{Age}\\times\\mathrm{BSA} + \\left(0.0202 +0.0125\\mathrm{Age}\\right)[\\mathrm{if} \\, \\mathrm{Sex=Male}] \\nonumber
             \\end{align}$$'),
           p(HTML("For the above equations:
                  <div>
                    <ul>
                      <li>GFR is glomerular filtration rate with units ml/min</li>
                      <li>Age has the units years</li>
                      <li>BSA is body surface area with units m\u00B2 calculated using the DuBois equation</li>
                      <li>Cre is blood serum creatinine concentration with units mg/dL and the subscript IDMS indicates that the measurement is IDMS traceable.</li>
                      <li>ln is the natural logarithm</li>
                    </ul>
                  </div>
                  All coefficients are rounded to 4 decimal places.")),
           
           h4(HTML("The CKD-EPI equation takes the following form:<sup>4</sup>")), 
           sprintf('$$\\mathrm{GFR_{nonadjusted}} =
                   \\begin{cases}
                   141 \\times \\mathrm{min} \\left(\\frac{\\mathrm{Cre}}{0.7}, 1\\right)^{-0.329} \\times \\mathrm{max} \\left(\\frac{\\mathrm{Cre}}{0.7}, 1 \\right)^{-1.209} \\times \\mathrm{Age}^{0.993} \\times 1.018 & \\mathrm{if} \\, \\mathrm{Sex=Female} \\\\
                   141 \\times \\mathrm{min} \\left(\\frac{\\mathrm{Cre}}{0.9}, 1\\right)^{-0.411} \\times \\mathrm{max} \\left(\\frac{\\mathrm{Cre}}{0.9}, 1 \\right)^{-1.209} \\times \\mathrm{Age}^{0.993}  & \\mathrm{if} \\, \\mathrm{Sex=Male}
                   \\end{cases}
                   $$'),
           tags$div('where GFR now has the units ml/min/1.73m\u00B2 and all other variables have the same units as above. This non adjusted estimated GFR value is then BSA-adjusted by the following equation'), 
           sprintf('$$\\mathrm{GFR_{adjusted}} =  \\mathrm{GFR_{non adjusted}} \\times \\frac{1.73}{\\mathrm{BSA}}$$'),
           
           h4(HTML("The Lund-Malmo equation takes the following form:<sup>5</sup>")), 
           sprintf('$$\\mathrm{GFR_{nonadjusted}} = \\exp\\left(X - 0.0158\\text{Age} + 0.438\\ln(\\text{Age})\\right)$$'),
           tags$div('where'), 
           sprintf('$$
                   \\begin{align}
                    X &= 2.56  + 0.0121(150 - \\text{Cre})&&  \\, \\text{if Sex=Female and Cre} < 150 \\\\
                    X &= 2.56 -0.926\\ln\\left(\\frac{\\text{Cre}}{150}\\right) && \\, \\text{if Sex=Female and Cre} \\geq 150 \\\\
                    X &= 2.5 + 0.00968(180 - \\text{Cre}) && \\, \\text{if Sex=Male and Cre} < 180 \\\\
                    X &= 2.5 -0.926\\ln\\left(\\frac{\\text{Cre}}{180}\\right) && \\, \\text{if Sex=Male and Cre} \\geq 180
                   \\end{align}
                   $$'),
           p(HTML("and Cre is blood serum creatinine concentration with units umol/L.")),
           
           h5("References:"),
           p("1. Williams EH, Whitley C, Weaver, JMJ, et al. The CamGFR model for renal function in patients with cancer: Validation and extension for use with data from isotope mass dilution spectrometry creatinine assays. JNCI Cancer Conference Abstracts. 2018"), 
           p("2. Williams EH, Connell CM, Weaver, JMJ, et al. Multicenter Validation of the CamGFR Model for Estimated Glomerular Filtration Rate. JNCI Camcer Spectrum. 2019"),
           p("3. Janowitz T, Williams EH, et al. A new model for estimating glomerular filtration rate in patients with cancer. Jounal of Clinical Oncology. 2016"),
           p("4. Levey AS, Stevens LA, Schmid CH, Zhang Y, Castro AF, Feldman HI, et al. A New Equation to Estimate Glomerular Filtration Rate. Ann Intern Med. 2009"),
           p("5. Björk, J., Grubb, A., Sterner, G., and Nyman, U. Revised equations for estimating glomerular filtration rate based on the Lund-Malmö Study cohort. Scandinavian Journal of Clinical and Laboratory Investigation. 2011"),
           p("6. DuBois D, DuBois E. A formula to estimate the approximate surface area if height and weight be known. Arch Intern Med. 1916")
           
           ) 
  

) # end navbarPage

