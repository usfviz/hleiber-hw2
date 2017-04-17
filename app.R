library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)

# read in data
population <- read.csv("pop.csv", stringsAsFactors = FALSE)
life_expectancy <- read.csv("life_exp.csv", stringsAsFactors = FALSE, skip=4)
fertility <- read.csv("fert.csv", stringsAsFactors = FALSE, skip=4)
meta <- read.csv("metadata.csv", header = TRUE)

# change df to long
population <- gather(population, year, pop, X1960:X2014)[,c("Country.Name","year","pop","Country.Code")]
life_expectancy <- gather(life_expectancy, year, life_exp, X1960:X2014)[,c("Country.Name","year","life_exp","Country.Code")]
fertility <- gather(fertility, year, fert, X1960:X2014)[,c("Country.Name","year","fert","Country.Code")]

# join dfs, get remove Xs in year
combo_df <- inner_join(population, inner_join(life_expectancy, fertility))
combo_df$year <- substr(combo_df$year, 2, nchar(combo_df$year))

# use metadata to get region for each country
combo_df <- inner_join(combo_df, meta[,c("Country.Code","Region")])

# get rid of empty regions
combo_df[combo_df == ""] <- NA
combo_df <- combo_df[!is.na(combo_df$Region),]

#change some col names
colnames(combo_df)[3] <- "Population"
colnames(combo_df)[5] <- "Life.Expectancy"
colnames(combo_df)[6] <- "Fertility"

ui <- fluidPage(
  titlePanel("Gapminder World Development Indicators"),
  mainPanel(
    plotlyOutput("scatter"),
    sliderInput("year", "Year", min=1960, max=2014,
                step=1, value = 1960, width='600px',
                animate = animationOptions(interval=300), sep="")
    ),
  sidebarPanel(
    sliderInput("Population", "Population", value=2, min=0, max=5, step=1)
    ),
  fluidRow(
    column(3, 
           helpText("Note: To view only one region, click", 
                    "on all the regions you aren't interested",
                    "in looking at."))
  )
  )

server <- function(input, output){
  output$scatter <- renderPlotly({
    subset_plot <- combo_df[combo_df$year==input$year,]
    plt <- ggplot(subset_plot, aes(x=Life.Expectancy, y=Fertility, key=Country.Name))
    plt <- plt + geom_point(aes(fill=Region, size=Population), alpha=.85, shape=21, colour="gray13")
    plt <- plt + scale_size(range = c(1, 6)*input$Population, guide = 'none')
    plt <- plt + xlim(0, 90) + ylim(0, 9)
    plt <- plt + scale_fill_manual(values = c('royalblue','orangered','orange','forestgreen',
                                     'darkmagenta','lightseagreen','indianred'), name="")
    plt <- plt + theme_bw()
    plt <- plt + xlab("Life Expectancy") + ylab("Fertility")
    plt <- plt + ggtitle("Life Expectancy compared to Fertility Rate")
    plt <- plt + theme(panel.border = element_blank(),
                       axis.line = element_line(colour = "black"),
                       axis.ticks.x = element_blank(),
                       axis.ticks.y = element_blank())
    })
  }

shinyApp(ui = ui, server = server)

