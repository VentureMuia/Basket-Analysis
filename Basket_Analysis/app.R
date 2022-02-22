# Importing the packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(arulesViz)
library(arules)
library(plotly)
library(dashboardthemes)

# Loading the files
groceries<-read.transactions('groceries.csv',sep=',')
items<-c("whole milk","other vegetables","rolls/buns","soda","yogurt","bottled water","root vegetables")


# creating the UI
UI<-dashboardPage(skin="red",
    dashboardHeader(title="Basket Analysis",titleWidth = 800),
    dashboardSidebar(
        sidebarMenu(id="sidebarid",
        menuItem("Associated Items",tabName = "items",icon = icon("shopping-cart",
                                                                  class="fa-spin")
                  ),
        conditionalPanel(condition = "input.sidebarid=='items'",
                         sliderInput("num","Select the number of  items to view",
                                     min = 5,max = 20,step = 1,value = 10),
                         selectInput("itm","Select an Item",choices = items,selected = "whole milk")
                         
                         )
        
    )),
    dashboardBody(
      shinyDashboardThemes(theme = "grey_dark"),
        tabItems(
            tabItem(tabName = "items",
                    h3("Select the item that has been bought.   
      Items tab will show the items that would most likely be bought and its number of occurences.   
      Associations Rules Graph shows a graphical representation of the rules for the item.    
      Visualization tab shows how the lift and support that the next item would be bought."
                   ),
                   
                    box(title = "Items Tab",
                        width = 6,
                        plotOutput("tb")
                     ),
                    
                    
                    box(title = "Visualization Tab",
                        width = 6,
                        plotOutput("gp"))
                    )
            
            
            
        )
        
    )
    
)

# server
server<-function(input,output,session){
    # Inspecting the Basket
    
    data<-reactive({
        df<-groceries
    })
    output$sm<-renderPrint({
        inspect(data()[1:5])
    })
    
    # Plotting the popular items
    output$tb<-renderPlot({
        itemFrequencyPlot(data(),topN=input$num)
    })
    
    # creating the rules
    
    rules<-reactive({
        apriori(data(),
                parameter = list(
                    supp=0.001,
                    conf=0.08
                ),
                appearance = list(
                    default='rhs',
                    lhs=input$itm
                ))%>%
            sort(by="lift",decreasing = T)
            
        
    })
    # visual rules
    output$gp<-renderPlot({
        plot(rules()[1:12],method='graph')
    })
    
}

shinyApp(UI,server)