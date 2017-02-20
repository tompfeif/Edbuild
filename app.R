#US Public School Analysis
library(shiny)
library(rsconnect)
library(ggplot2)
ui <- fluidPage(
        headerPanel('US Public School Analysis'),
        tabsetPanel(
                tabPanel('State Analysis',
                         plotOutput("plotFinanceCluster"),
                         tags$br(),
                         tags$br(),
                         plotOutput("plotGradRate"),
                         tags$br(),
                         tags$br(),
                         selectInput(inputId = "financeSort", label='Select data to sort by:', 
                                     choices = c('Revenue','Expenditure','Debt'), selected = "Revenue"),
                         plotOutput("plotFinanceSort"),
                         tags$br(),
                         tags$br(),
                         plotOutput("plotFundingCluster"),
                         tags$br(),
                         tags$br(),
                         selectInput(inputId = "fundingSort",label = 'Select data to sort by:',
                                     choices = c('Federal','State','Local'),selected = "Federal"),
                         plotOutput("plotFundingSort"),
                         tags$br(),
                         tags$br(),
                         tableOutput("table1"),
                         tags$h4('Data citations:'),
                         tags$ul(
                                 tags$a(tags$li('U.S. Census Bureau 2014 Annual Survey of School System Finances'),
                                        href="http://www.census.gov/govs/school/"),
                                 tags$a(tags$li('EDFacts Data Groups 695 and 696, School year 2014–15; September 15, 2016'),
                                        href="https://nces.ed.gov/ccd/tables/ACGR_RE_and_characteristics_2014-15.asp")
                         )
                         ),
                tabPanel('Further Questions and Next Steps',
                         tags$h1('Questions and Comments of Interest'),
                         tags$ol(
                                 tags$li('Why does Washington D.C. have very high funding per student and very low high school graduation?'),
                                 tags$ul(
                                        tags$li('Possibly there is a high transfer rate and transfer students are counted as not graduating?'),
                                        tags$li('Perhaps the schools are newly funded and investing inefficiently or for the future?'),
                                        tags$li('Maybe there was a data collection error?')
                                        ),
                                 tags$br(),
                                 tags$li('The biggest differences in funding is on the local and state levels.'),
                                 tags$ul(
                                         tags$li('It may be worth examining the relationship between local funding and graduation rate'),
                                         tags$li('States with higher local funding percentages appear to have higher high school graduation rates')
                                 ),
                                 tags$br(),
                                 tags$li('What do the factors analyzed look like on a school district level?'),
                                 tags$ul(
                                         tags$li('Although it is possible, R Shiny is not ideal for creating graphics to that level of detail, a different software should be used.'),
                                         tags$ul(
                                                 tags$li('Javascript (D3.js)'),
                                                 tags$li('QGIS & Tableau')
                                         ),
                                         tags$li('To move forward with online mapping, D3.js and QGIS will both be analyzed. Stay tuned for updates!'),
                                         tags$li('QGIS will be used to visualize the situation surrounding Washington, D.C.')
                                        )
                                )
                        )
                )
)

server <- function(input, output) {
        census <- read.csv("www/CensusPerPupilRevExpDebt.csv") # SOURCE: U.S. Census Bureau 2014 Annual Survey of School System Finances
        gradData <- read.csv("www/CommonCoreHighSchoolGradRate.csv",nrows = 52) # SOURCE: EDFacts Data Groups 695 and 696, School year 2014–15; September 15, 2016
        # data
        census[,2:15] <- sapply(census[,2:15], function(x)as.numeric(gsub(",", "", x)))
        clusterData <- data.frame("state"=rep(census$Geographic.area[-1],3), 
                                  "numbers"=c(census$Total.Revenue.Per.Pupil[-1],census$Total.Expenditure.Per.Pupil[-1],census$Total.Debt.Per.Pupil[-1]),
                                  "funding"=c(census$Federal.Funding.Percent.of.Total[-1],census$State.Funding.Percent.of.Total[-1],census$Local.Funding.Percent.of.Total[-1]),
                                  "fundType"=c(rep('Federal',51),rep('State',51),rep('Local',51)),
                                  "numberType"=c(rep('Revenue Per Pupil',51),rep('Expenditure Per Pupil',51),rep('Debt Per Pupil',51)))
        tableData <- cbind(gradData[,1:2], census[,c(13:15,8:10,11)])
        names(tableData) <- c('Geographic Area', 'Total High School Graduation Rate', 'Total Revenue Per Pupil', 'Total Expenditure Per Pupil', 'Total Debt Per Pupil',
                            'Federal Funding Percent', 'State Funding Percent', 'Local Funding Percent','Elementary and Secondary Student Enrollment')
        financeSort <- reactive({
                if(input$financeSort=="Revenue"){
                        census$Total.Revenue.Per.Pupil[-1]
                } else if(input$financeSort=="Expenditure"){
                        census$Total.Expenditure.Per.Pupil[-1]
                } else {
                        census$Total.Debt.Per.Pupil[-1]
                }
        })
        fundingSort <- reactive({
                if(input$fundingSort=="Federal"){
                        census$Federal.Funding.Percent.of.Total[-1]
                }else if(input$fundingSort=="State"){
                        census$State.Funding.Percent.of.Total[-1]
                }else{
                        census$Local.Funding.Percent.of.Total[-1]
                }
        })
        # output
        output$plotFinanceCluster <- renderPlot({
                ggplot(data=clusterData, aes( x = reorder(factor(state), numbers), y = numbers)) + 
                        geom_bar(stat="identity", aes(fill=numberType), position = "dodge") + labs(title='State Analysis', x='State',y='Dollars',fill="") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size=rel(1.5), hjust=0.5), legend.position="top")
        })
        output$plotGradRate <- renderPlot({
                ggplot(data=gradData[-1,], aes( x = reorder(factor(State), Total), y = Total)) + 
                        geom_bar(stat="identity", position = "dodge") + labs(title='State High School Graduation Rate', x='State',y='Percent',fill="") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size=rel(1.5), hjust=0.5), legend.position="top")
        })
        output$plotFinanceSort <- renderPlot({
                ggplot(data=census[-1,], aes( x = reorder(factor(Geographic.area), financeSort()), y = financeSort())) + 
                        geom_bar(stat="identity", fill="light blue") + labs(title=paste(input$financeSort,'Per Pupil'), x='State',y='Dollars') +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size=rel(1.5), hjust=0.5))
        }) 
        output$plotFundingCluster <- renderPlot({
                ggplot(data=clusterData, aes( x = reorder(factor(state), funding), y = funding)) + 
                        geom_bar(stat="identity", aes(fill = fundType)) + labs(title='Public School Funding Sources', x='State',y='Funding Percent of Total',fill="") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size=rel(1.5), hjust=0.5), legend.position="top")
        })
        output$plotFundingSort <- renderPlot({
                ggplot(data=census[-1,], aes( x = reorder(factor(Geographic.area), fundingSort()), y = fundingSort())) + 
                        geom_bar(stat="identity", position = "dodge", fill="purple") + labs(title=paste(input$fundingSort, 'Funding'), x='State',y='Funding Percent of Total',fill="") +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(size=rel(1.5), hjust=0.5), legend.position="top")
        })
        output$table1 <- renderTable({
                tableData
        })
}
shinyApp(ui = ui, server = server)
