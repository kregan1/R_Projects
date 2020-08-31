library(shiny)
currentAge=33
arrayCash = rep(0,120);arrayCash[currentAge]=20000
arrayRetirement = rep(0,120);arrayRetirement[currentAge]=95000
arrayHouse = rep(0,120);arrayHouse[currentAge]=70000
cashExpenses=0; retirementExpenses=0


# Define UI ----
ui <- fluidPage(
   titlePanel("Can We Quit Yet?"),

  sidebarLayout(
    sidebarPanel(
	"Current Age: ", currentAge,br(), "Current Cash: ",arrayCash[currentAge],br(),
	"Current Retirement: ",arrayRetirement[currentAge],
	sliderInput("ageRetire","Age at Retirement",currentAge+1,65,50),
	numericInput("income","Annual Income",50000),
	numericInput("expenses","Annual Expenses",25000),
	numericInput("retirementSave","Annual Retirement Savings",15000),
	radioButtons("marketGrowth","Annual Market Growth",c("4%"=.04,"5%"=.05,"6%"=.06),inline=TRUE)
			),
    mainPanel(
	plotOutput(outputId = "retireplot")
		)
  )
)

# Define server logic ----
server <- function(input, output) {

   output$retireplot <- renderPlot({

  marketGrowth = as.numeric(input$marketGrowth)
  
  arrayHousingCost = rep(0,120); arrayHousingCost[32:54]=15216; arrayHousingCost[55:120]=160
  
     
	for(i in (currentAge+1):input$ageRetire){       #Years from present to retirement
		arrayCash[i]=arrayCash[i-1]+input$income-input$expenses-input$retirementSave - arrayHousingCost[i]
		arrayRetirement[i]=arrayRetirement[i-1]+(arrayRetirement[i-1]*marketGrowth) + input$retirementSave

	}
     
  
  for(i in input$ageRetire:120){                               #Years retirement to death
    if(arrayCash[i-1]>0){cashExpenses=input$expenses+arrayHousingCost[i]; retirementExpenses=0} 
    else{cashExpenses=0; retirementExpenses=input$expenses+ arrayHousingCost[i]}

    arrayCash[i]=arrayCash[i-1]-cashExpenses
    arrayRetirement[i]=arrayRetirement[i-1]+(arrayRetirement[i-1]*marketGrowth)-retirementExpenses
    
  }

 	plot(currentAge:120,arrayCash[currentAge:120],type='l',lwd=2,xlab="Age",ylab="$",ylim=c(0,min(max(arrayCash,arrayRetirement),2000000)))
 	lines(currentAge:120,arrayRetirement[currentAge:120],type='l',lwd=2,col="red")
 	abline(h=0)
 	abline(h=1000000)
 	abline(h=500000)
 	
 	abline(v=input$ageRetire,col="red")

 
    })

}

# Run the app ----
shinyApp(ui = ui, server = server)




#To Include
#House value and housing cost
