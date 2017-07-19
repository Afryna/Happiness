library(shiny)
library(shinythemes)
library(shinydashboard)
library(googleVis)
library(corrplot)
library(ggplot2)

  fluidPage(
    theme = shinytheme("cerulean"),

  titlePanel("Pursuit of Happiness"),
    
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        
        
        
        
strong("Background:"),p("As per the", a("Merriam-Webster dictionary",href="https://www.merriam-webster.com/dictionary/happiness",target="_blank"), 
  "happiness is defined as a 
state of well-being and contentment. Over the years,
several attempts have been made to quantify happiness in order to assess 
the overall happiness of the world. One such significant survey was conducted 
by", a("Gallup World Poll",href="http://www.gallup.com/opinion/gallup/182843/happiest-people-world-swiss-latin-americans.aspx",target="_blank"),
"and the data from this study was compiled in the", a("World Happiness Report",href="http://worldhappiness.report/",target="_blank"),
"is 
available on" , a("Kaggle",href="https://www.kaggle.com/unsdsn/world-happiness",target="_blank"),
"for the years 2015, 2016 and 2017. As per this study, six key variables play a role in 
determining the overall happiness of a nation. These are--GDP per Capita, Family, 
Life Expectancy, Freedom, Generosity and Government Corruption. The purpose of this application 
is to provide life to the available raw data with visual graphics and to use a bit of statistics to 
assess the importance of these variables on the happiness score.")),   


      # tags$h4("Make a Selection:"),
      # selectInput("theme", "Select a theme", c("cerulean","cosmo","cyborg","darkly","flatly","journal",
      #                                           "lumen","paper","readable","sandstone","simplex","slate",
      #                                           "spacelab","superhero","united","yeti")),

      
    
      selectInput("measure", "Select a variable", c("Happiness_Rank","Happiness_Score","REGION",
                                            "Economy_GDP_Pc","Life_Expectancy",
                                            "Freedom",
                                            "Govt_Corruption","Generosity")),
  
      selectInput("year", "Select a year",
                    choices = as.character(unique(Happiness_Data$Year))),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),
br(),

h6("This application has been created 
by", a("SI Ranganathan",href="https://scholar.google.com/citations?user=Oy4iOeEAAAAJ&hl=en",target="_blank"),". 
   This is a work in progress and will be updated periodically.")






      ),


      mainPanel(
        p("Note: In order to view the motion charts, please change your browser settings 
to run flash unconditionally."),
        
        fluidRow(
          # p("Happiness data blah, blah...")
          ),

      tabsetPanel(
        tabPanel("Geo Map", fluidRow(
                                  tags$h3("Data Visualization"),

                                  p("Select a variable and a year from the sidebar panel and 
                                    
                                    click on any country to read additional information. The 
coloring scheme is from Green (most satisfactory) to Red (least satisfactory).  
                                    "),

                                  htmlOutput("map",
                                             click = "plot_click", height = "auto"),

                                  tableOutput("info"))),
       
         tabPanel("Motion Chart (all countries)", fluidRow(
          tags$h3("Data in Action"),

          p("The motion charts in this tab have been generated using", 
            a("googleVis",href="https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html",target="_blank"), 
"and the inspiration is from Hans Rosling's", a("TED talk",href="https://www.youtube.com/watch?v=hVimVzgtD6w",target="_blank"), "as 
well as the", a("Bubble Charts",href="https://en.wikipedia.org/wiki/Material_selection",target="_blank"), "popularized by Professor Mike Ashby in 
the field of materials science. Click on the x and y axis to create a bubble chart of interest and play the video to view the data in action!"),

          htmlOutput("Motion_Chart",
                     click = "plot_click",click = "plot_click", height = "auto"),

          tableOutput("info1"))),

        tabPanel("Motion Chart (summary)", fluidRow(
          tags$h3("Data in Action"),
          
          p("This motion chart summarizes the data on the top 10 (and bottom 10) happiest countries in the world during the years 2015, 2016 and 2017. Click on the x and y axis to create a bubble chart of interest and play the video to view the data in action!"),
          
          htmlOutput("Top_Bot",
                     click = "plot_click",click = "plot_click", height = "auto"),
          
          tableOutput("info2"))),
        
        tabPanel(
          
          
          "Box and Density",         fluidRow(
            tags$h3("Box plot"),
            p("Select a year from the side panel to view the box and density plots on the happiness score. In these plots, 
              the happiness score is examined for various regions in the world. Higher score, means a happier region. 
              Along similar lines, lower spread in the density plot indicates that all countries in the given region have comparable happiness scores and vice versa!")
          ),
  
          
                
          plotOutput("Box_Plot"),
                 plotOutput("Density_Plot")),
                tabPanel("Correlation", 
                         
                         
                         
                         fluidRow(
                           tags$h3("Correlation plot"),
                           p("This plot highlights the correlation between all the variables. A dark blue color 
                             indicates a strong positive correlation and a dark red color indicates an strong negative correlation. 
                             A white color indicates no correlation. Results based on a significance test with 
                             95 % confidence level have also been superposed on this plot. Cells with 'X' indicate no significance between the variables based on this test.")
                           ),
                         
                         plotOutput("Corr_Plot",height=500, click = "plot_click",
                                           hover = hoverOpts(id = "plot_hover", delayType = "throttle"),
                                           brush = brushOpts(id = "plot_brush")
        )),                 
        tabPanel("Tree Map", 
                 
                 fluidRow(
                   tags$h3("Tree plot"),
                   p("Select a year from the side panel to view the tree plot that lists the top ten 
happiest and least happiest countries in the world. This plot is a work in progress...")
                   ),
                 
                 
                 plotOutput("Misc_Plot1"),plotOutput("Misc_Plot2"))
        
    ))
  )
)

