library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(googleVis)
library(gapminder)
library(corrplot)
library(ggplot2)
library(treemap)
library(RColorBrewer)
shinyServer(function(input, output,session) {
  
  output$choose_measure <- renderUI({
    selectInput("measure", "Statistic", measures)
  })
  
  
                            
    # World map
  output$map <- renderGvis({
    gvisGeoChart(Happiness_Data[Happiness_Data$Year==as.numeric(input$year),],
                 locationvar="Country",
                 input$measure, options=list(region="world", displayMode="regions",
                 resolution="countries",
                 width="auto", height="auto",projection="mercator",
                 colors=ifelse(input$measure %in% c("Govt_Corruption","Economy_GDP_Pc","Happiness_Score","Life_Expectancy","Freedom","Generosity", "REGION"),"['#780000', '#e71c1c', '#f9e407','#34f907','#0f5100']","['#0f5100','#34f907', '#f9e407','#e71c1c', '#780000']"),
  explorer=	"{actions:['dragToZoom', 'rightClickToReset']}"
                 ))
  })
  
  # Motion plot all countries
  output$Motion_Chart <- renderGvis({
    Motion_Chart=Happiness_Data %>% select(.,Country,POP_EST,Economy_GDP_Pc,Family,
                                           Life_Expectancy,Freedom,Govt_Corruption,Year,
                                           Generosity,Dystopia_Residual,Happiness_Score,Happiness_Rank)
  (gvisMotionChart(Motion_Chart, idvar='Country', timevar='Year',
                   sizevar="POP_EST",colorvar ='Happiness_Rank', 
                   xvar = 'Economy_GDP_Pc', yvar = 'Life_Expectancy'))
                  })
  

  # Motion plot top bottom
    output$Top_Bot <- renderGvis({
    Tree_2015_bottom=Happiness_Data[Happiness_Data$Year==2015,]%>% arrange(.,desc(Happiness_Rank)) %>% head(.,10)
    Tree_2015_top=Happiness_Data[Happiness_Data$Year==2015,]%>% arrange(.,Happiness_Rank) %>% head(.,10)
    Tree_2016_bottom=Happiness_Data[Happiness_Data$Year==2016,]%>% arrange(.,desc(Happiness_Rank)) %>% head(.,10)
    Tree_2016_top=Happiness_Data[Happiness_Data$Year==2016,]%>% arrange(.,Happiness_Rank) %>% head(.,10)
    Tree_2017_bottom=Happiness_Data[Happiness_Data$Year==2017,]%>% arrange(.,desc(Happiness_Rank)) %>% head(.,10)
    Tree_2017_top=Happiness_Data[Happiness_Data$Year==2017,]%>% arrange(.,Happiness_Rank) %>% head(.,10)
    merge_tree_bot=rbind(Tree_2015_bottom,Tree_2016_bottom,Tree_2017_bottom)
    merge_tree_top=rbind(Tree_2015_top,Tree_2016_top,Tree_2017_top)
    
    Tree_bottom=merge_tree_bot %>% select(.,Country,POP_EST,Economy_GDP_Pc,Family,
                                    Life_Expectancy,Freedom,Govt_Corruption,Year,
                                    Generosity,Dystopia_Residual,Happiness_Score,Happiness_Rank)
    
    Tree_top=merge_tree_top %>% select(.,Country,POP_EST,Economy_GDP_Pc,Family,
                                        Life_Expectancy,Freedom,Govt_Corruption,Year,
                                        Generosity,Dystopia_Residual,Happiness_Score,Happiness_Rank)

    
    Top_bot=rbind(Tree_top,Tree_bottom)
    (gvisMotionChart(Top_bot, idvar='Country', timevar='Year',
                     sizevar="POP_EST",colorvar ='Happiness_Rank', 
                     xvar = 'Economy_GDP_Pc', yvar = 'Life_Expectancy'))
  })
  

    # Box and Density plots
    output$Box_Plot <- renderPlot({
      
      
      df=Happiness_Data[Happiness_Data$Year==input$year,]
      g <- ggplot(df, aes(reorder(REGION,Happiness_Score),Happiness_Score))
      g + geom_boxplot(aes(fill=REGION), alpha=0.8) +
        labs(title=sprintf("Box plot %d",as.numeric(input$year)),
             y="Happiness Score",
             x="Region")+theme(axis.text=element_text(size=14),
                               axis.title=element_text(size=14,face="bold"))
      
    })
    output$Density_Plot <- renderPlot({
      df=Happiness_Data[Happiness_Data$Year==input$year,]
      g <- ggplot(df, aes(Happiness_Score))
        g + geom_density(aes(fill=REGION)) +
          labs(title=sprintf("Density plot %d",as.numeric(input$year)),
               x="Happiness Score",
               fill="Region")+theme(axis.text=element_text(size=14),
                                   axis.title=element_text(size=14,face="bold"))
    })
    
    
    # Corr plot
    output$Corr_Plot <- renderPlot({
      Corr_Data1=Happiness_Data %>% select(.,Economy_GDP_Pc,
                                           Life_Expectancy,Generosity,Family,
                                           Freedom,Govt_Corruption,Happiness_Score,Happiness_Rank,LAT,LON,Year)
      cor.mtest <- function(Corr_Data1, conf.level = 0.95){
        mat <- as.matrix(Corr_Data1)
        n <- ncol(mat)
        p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
        diag(p.mat) <- 0
        diag(lowCI.mat) <- diag(uppCI.mat) <- 1
        for(i in 1:(n-1)){
          for(j in (i+1):n){
            tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
            p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
            lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
            uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
          }
        }
        return(list(p.mat, lowCI.mat, uppCI.mat))
      }
      
      res1 <- cor.mtest(Corr_Data1,0.95)
      (corrplot(cor(Corr_Data1),method="pie",p.mat = res1[[1]], sig.level=0.05))
    })
  ## Statistics
  ### Plots
    # Misc plot
    output$Misc_Plot1 <- renderPlot({
      
      Tree_2015_top=Happiness_Data[Happiness_Data$Year==as.numeric(input$year),]%>% arrange(.,Happiness_Rank) %>% head(.,10)
      
      Tree_map_2015_top=treemap(Tree_2015_top, #Your data frame object
                                index=c("REGION","Country"),  #A list of your categorical variables
                                vSize = "Happiness_Score",  #This is your quantitative variable
                                vColor="Happiness_Rank",
                                type="value",
                                title=sprintf("Happiest Nations in %d", as.numeric(input$year)),
                                palette=colorRampPalette(rev(brewer.pal(11, "Greens")))(20),
                                lab = c(TRUE, TRUE),
                                fontsize.legend = 14, fontsize.title =14, fontsize.labels=14,
                                format.legend = list(scientific = FALSE, big.mark = " "))
    })
    output$Misc_Plot2 <- renderPlot({
      Tree_2015_bottom=Happiness_Data[Happiness_Data$Year==as.numeric(input$year),]%>% arrange(.,desc(Happiness_Rank)) %>% head(.,10)
      
      Tree_map_2015_top=treemap(Tree_2015_bottom, #Your data frame object
                                index=c("REGION","Country"),  #A list of your categorical variables
                                vSize = "Happiness_Score",  #This is your quantitative variable
                                vColor="Happiness_Rank",
                                type="value",
                                title=sprintf("Least Happy Nations in %d", as.numeric(input$year)),
                                palette=colorRampPalette(rev(brewer.pal(11, "OrRd")))(20),
                                lab = c(TRUE, TRUE),
                                fontsize.legend = 14, fontsize.title =14, fontsize.labels=14,
                                format.legend = list(scientific = FALSE, big.mark = " "))
    })
    
  
  
})

