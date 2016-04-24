library(shiny)
library(plotly)


shinyUI(fluidPage(

  titlePanel("Disease risk predictor"),
  headerPanel("Input detail"),

   sidebarPanel(
	fileInput('file1', 'Upload a VCF File',
                accept=c('text/csv',
			'text/comma-separated-values,text/plain',
			'.csv/.vcf')),
								 
   textInput("patientid", "Patient id", value = "90435378"),
	
   selectInput("age","Age",
			choices = c("Adult" = "A",
						"Child" = "C",
						"Newborn" = "N")),
	selectInput("sex","Sex",
			choices = c("Male" = "M",
						"Female" = "F")),
	selectInput("race","Location",
			choices = c("Hong Kong" = "HK",
						"World" = "World")),
	br(),
	actionButton("submit", "Go Prediction!"),
	br(),br(),
	selectInput("disease", "Choose a disease",
            choices = c ("Prostate Cancer" = "Ca Prostate", "Colon Cancer" = "Ca colon"))
	
	),

  mainPanel(
	tabsetPanel(
		tabPanel("VCF",tableOutput("viewVCF")),		
		tabPanel("Risk List",br(),column(2,tableOutput("view"))),
		tabPanel("Risk-o-gram",br(),plotlyOutput("disSPlot", "500px","200px"),br(),br(),column(6,tableOutput("view2"))),		
		tabPanel("Disease Risk",br(),plotlyOutput("distPlot","250px","400px"),br(),br(),column(6,tableOutput("view1")))
			)
	)

))
