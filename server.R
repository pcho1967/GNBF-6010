library(shiny)
library(datasets)
library(tidyr)
library(plotly)
library(RMySQL)
library(vcfR)

#connect MySQL PM.db
mydb = dbConnect(MySQL(), user='root', password='sandy', host='localhost', dbname="PM")

shinyServer(function(input, output, session) {
#upload VCF file, ceate trigger event for VCF upload on submit button
  observeEvent(input$file1, {
    inFile <- input$file1
#null if no file was selected
    if (is.null(inFile))
      return(NULL)
#upload file from directory and skip the VCF metadata with header retain
    tempfile <- read.table(inFile$datapath, skip=10, header = FALSE) 
#Prepare latest job id a key	
	max_job_id <- (as.numeric(dbGetQuery(mydb, "Select max(job_id) from FEI_Job")) + 1)
#Porvide insert SQL on FEI_job	
	temp <- paste("INSERT INTO FEI_Job (job_id, patient_id, location_id) VALUES(",max_job_id,",'",input$patientid,"','",input$race,"');")
#Append FEI_vcf with job id, SNP reference number and genotype
#Use vcfR package to import VCF file as vcf class
	inFile1 <- read.vcfR(inFile$datapath)
#Use vcfR method to get genotype from VCF, 0/0= ref homozygous, 0/1= heterozygous, 1/1 = alt homozygous
	DataFile <- extract.gt(inFile1, element ="GT", mask = FALSE, as.numeric = FALSE, return.alleles = TRUE, allele.sep ="/", extract = TRUE)
#Remove "/" in genotype value
	DataFile <- gsub("/","",DataFile)
#Aggregrate job_id, snp_id and genotype
	InsertFEI_vcf <- list(max_job_id,tempfile[,3],DataFile[,1])
#cast as data frame
	InsertFEI_vcf <- as.data.frame(InsertFEI_vcf)
#rename column name of the data frame
	colnames(InsertFEI_vcf) <- c("job_id","snp_id","genotype")
#clean up FEO_Riskogram_Data and FEO_Risk_List_Data table
	dbSendQuery(mydb,"truncate table FEO_Riskogram_Data;")
	dbSendQuery(mydb,"truncate table FEO_Risk_List_Data;")
#Write to MySQL PM database as table FEI_vcf
	dbWriteTable(mydb, "FEI_vcf", InsertFEI_vcf, overwrite =TRUE)
#insert new record with update job_id in FEI_job table to trigger store procedure in MySQL
	dbSendQuery(mydb,temp)

  })


#Do this when Go prediction button is clicked
observeEvent(input$submit, {
#Create view for VCF UI display
	output$viewVCF <- renderTable({dbGetQuery(mydb, "Select * from FEI_vcf")})
#Update table read for FEO_Risk_List_Data 
	a <- as.data.frame(fetch((dbSendQuery(mydb, "select * from FEO_Risk_List_Data"))))
#Update table read for FEO_Riskogram_Data	
	s <- as.data.frame(fetch((dbSendQuery(mydb, "select * from FEO_Riskogram_Data"))))	
	s <- s[order(s$posttest_probability - s$pretest_probability), ]

#Filter FEO_Risk_List_Data with chosen disease
 data <- reactive({
    read <- a
	f <- subset(read, disease_name %in% input$disease, select = c(seq_no, gene_id, snp_id, genotype, LR, posttest_probability))
    return(f)
})

#Update Risk List table
 output$view <- renderTable({
    if(is.null(data())){return()}
    as.data.frame(c(input$patientid,a))
},include.rownames=FALSE)

#update Disease Risk table
  output$view1 <- renderTable({z <- data()},include.rownames=FALSE)
#update Disease risk diagram
  output$distPlot <- renderPlotly({
	plot_ly(z <- data(), x = posttest_probability, y = seq_no, 
	mode = "markers+lines", marker = list(color = "blue", symbol ="square", size = 12)
		)

	layout(
       title = input$disease,
       xaxis = list(title = "Risk (%)"),
	   yaxis = list(title =""),
       margin = list(l = 100),
       paper_bgcolor = "white"
	)

	})

#update risk-o-gram table
  output$view2 <- renderTable({s[,3:5]},include.rownames=FALSE)
#update risk-o-gram graph  
  output$disSPlot <- renderPlotly({
	gather(s, Cat, value, pretest_probability, posttest_probability) %>%
	plot_ly(x = value, y = disease_name, mode = "markers",color = Cat, colors = c("pink", "blue"))%>%
	add_trace(x = value, y = disease_name, mode = "lines", group = disease_name, line = list(color = "gray"))
	layout(
        title = "Disease Risk Summary",
        xaxis = list(title = "Risk (%)"),
		yaxis = list(title=""),
        margin = list(l = 150),
        paper_bgcolor = "white",
		showlegend = FALSE
		)
	})


})

})
