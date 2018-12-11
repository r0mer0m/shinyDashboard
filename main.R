
library(magrittr)
library(tidyverse)
library(grid)
library(gridExtra)
library(scales)
library(shiny)
library(shinydashboard)

# -- Replace ROOT_PATH for the fail where the data is stored.

ROOT_PATH='./'

setwd(ROOT_PATH)

PATH <- "Officer_Traffic_Stops.csv"

OTS <- read_csv(PATH)

OTS_rough <- OTS

OTS <- OTS %>% 
  separate(Month_of_Stop, 
           into=c("Year_of_Stop", "Month_of_Stop"), sep="/", remove = T) %>% 
  select(-c(Year_of_Stop, Creator, CreationDate, EditDate, Editor, ObjectID)) %>% 
  drop_na()

for(col in c("Month_of_Stop", "Officer_Years_of_Service", "Driver_Age")){
  OTS[[col]] <- factor(OTS[[col]], ordered = T)
}

for(col in c("Reason_for_Stop", "Officer_Race", 
             "Officer_Gender", "Driver_Race", "Driver_Ethnicity", 
             "Driver_Gender", "Was_a_Search_Conducted", "Result_of_Stop", 
             "CMPD_Division")){
  OTS[[col]] <- factor(OTS[[col]])
}

amount_na <- list()
for (col in colnames(OTS)){
  amount_na[col] <- round(sum(is.na(OTS_rough[[col]]))/nrow(OTS_rough)*100,2)
}

# Construction of "dictionaries" for dynamic plotting

Label2Name = list("Month_of_Stop" = "Month of Stop", 
                  "Reason_for_Stop" = "Reason for Stop", 
                  "Officer_Race" = "Officer Race", 
                  "Officer_Gender" = "Officer Gender", 
                  "Driver_Race" = "Driver Race", 
                  "Driver_Ethnicity" = "Driver Ethnicity", 
                  "Driver_Gender" = "Driver Gender", 
                  "Was_a_Search_Conducted" = "Was a Search Conducted ?", 
                  "Result_of_Stop" = "Result of Stop", 
                  "CMPD_Division" = "CMPD Division",
                  "Officer_Years_of_Service" = "Officer Years of Service", 
                  "Driver_Age" = "Driver Age")


Name2Label = list("Month of Stop" = "Month_of_Stop", 
                  "Reason for Stop" = "Reason_for_Stop", 
                  "Officer Race" = "Officer_Race", 
                  "Officer Gender" = "Officer_Gender", 
                  "Driver Race" = "Driver_Race", 
                  "Driver Ethnicity" = "Driver_Ethnicity", 
                  "Driver Gender" = "Driver_Gender", 
                  "Was a Search Conducted ?" = "Was_a_Search_Conducted",
                  "Result of Stop" = "Result_of_Stop", 
                  "CMPD Division" = "CMPD_Division", 
                  "Officer Years of Service" = "Officer_Years_of_Service", 
                  "Driver Age" = "Driver_Age")

# App's Text:

ph11 <- "This app is the composition of 4 sections (tabs):"

ph12 <- " This sections goes through the general information and the 'How to ?'-guide."

ph13 <- " This section is to overview the data before and after the pre-processing and explain the different transformation  of the data that we are using with respect the original one."

ph14 <- " In this section the user can choose any variable and observe how the data is distributed through a bar-plot."

ph151 <- " This tab is the 'play ground'! Up to three variables can be choossen to visualize them all together with different graphs. In a first moment I included up to 4 but the amount of data makes it difficult to visualize the information."

ph152 <- " Graphs are order-sensible"

ph153 <- " to the variable selection. i.e. in a standard representation, when variables are choosen they will be displayed as 'x' 'y' 'color'. This allow the user to fix one varaible and easily go through the rest of variables for interesting relationships. In this sections four kind of graphs can be chosen to displayed the informatiion. Those visualization are:" 

ph16 <- " Horizontal bar-char with varaiable's categories in  y-axis and amount of observatins in the x-axis (1d). Distinguis by coloured-dodged bars to scale to 2 variables and a facet grid for the third variable. Axis fixed."

ph17 <- "Usual vertical bar-char for one variable with the variable in the x-axis and the amount of observations in the y-axis. When two variables are selected it scales to a geom_count plot, which plots the two variables in the axes and reflects the amount of obervations in the amount size of the bubbles. When one more variable is introduced this last one is used to make subplots, just as the previous one, by spliting the data according to that last variable. Axis fixed."

ph18 <- "Vertical-Equivalent to the Horizontal bar-char which scales to 2 variables splitting the filling of the columns and scales to the third axis by feceting with variable (count) y-scales. This is really useful to compare patterns, regardless of the scale, over the last introduced variable."

ph19 <- "This plot is a bar-char in radial coordinates, being the radious the amount of observations it scales the the second variable by filling the 'chesses' and scales to the last variable by fixing the radious scale. This plott is good for comparing close amounts and reflects the different amounts of data in the third varaible. Nontheless it is not good to plot, as a third variable, features with several categories."

ph191 <- "At the bottom of this tab, at any moment the user can find a table with the amount of observations per combination of features so we can inspect when we will end up in an important decrease of cases-examples as a result of a feature combination for a posterior modeling."

ph192 <- " This sesction the obtained relations are presented and the settings to reproduce the figures that lead to those  provided so the reader can vissualize and tickle them just by changing the tab."

ph21 <- "In this section I aim to provide a brief justification of the preprocessing preceding the visual analysis. First notice that we sstart with a data set of "

ph22 <- "Prior to plotting the data, after a quick review of this one, we dropped the Creator and Editor because they are constant for the whole data set. We also proeeded the same way with the 'CreationDate', 'EditDate' and the 'ObjectID' because those are data from the experiment configuration and should not be used or interpretated as to be relevant in future steps."

ph23 <- "Regarding data transformation we have inspected the column Month_of_Stop and observed that there is only data of one year. Therefore we spplited this column into month and year and just keep the month."

ph24 <- "The remaining columns were all treated as factors, I had doubts about the 'Driver_Age' and 'Col_Age' however given  that the number of observations (even after the NA treatment) was really large in comparison to the number of values of those varaibles I decided to treat them as ordered factors. Just as I did with Month_of_Stop. The rest of the remainin varaibles were treated as categorical not-ordered factors."

ph25 <- "Regarding the NAs, as shown in the boxes below, the proportion of NA per column with respect to the total number of obsesrvations is not really large. Given that we have a good amount of ovservations we will proceed by removing those observations with some NA instead of corrupting the data by guessing their categories."

ph26 <- "After conductiong all this proceedure the remaining data set is the displayed in the table below:"

ph50 <- "Notice that we have less data in the last month (Novemeber)."

ph51 <- "As a first result notice, by selecting 'Officer Race' or 'Was a search Conducted ?' or 'Driver Race' between others that the amount of observations, the number of stops differe in great measure between categoriess category within a feature is hig of the  the incredible disproportion of examples in which a combination of variables may result. We need to be careful in how we examine the data as we may end up observing a huge amount of samples in a specific combination mainly as a result of those root-like desproportions. For example if we observe that a lot of observations where a serach was conducted in white man it doest not imply that a huge proportion of search was conducted to white people."

ph52 <- "By creating a count plot with the following variables: Month of Stop, Result of Stop and CMPD Divison respectively we can appreciate that first, the general trend is to either end an stop by a Verbal Reasoning or a Citation Issued, second is that the overall trend remains constant over time and third that some Divisions have a special tendency to those with respect the other possible results. For example check the two extreme cases such as the South Division vs the North Tryon Division, while the amount of otters results remain roughly the same there is a clear difference between the amount of Verbal Reasoning or a Citation Issued. Adding up in this point a dodged horizontal bar-char with variables CMPD Division and Result of Stop let us justify that, except for the Univeristy City Division and Steele Creek Division, where most of the stops end up in the issue of a citation, the usual result in the rest of divisions is a Verbal warning. A funny fact arises after knowing that and going back to our previous graph, where we can appreciate that in the University City District in particular the majority of Citations issued are in the spring semmester (first half of the year)."

ph53 <- "Looking now at the relation between Reason for Stop and Result of Stop through a radial plotting we can appreciate that, in proportion, speeding stops results more with a citation issued."

ph54 <- "By plotting the Age of the Driver in any graph one can appreciate weird non-continuous but equally spatiated ages in where more stops occure. By doing an exhaustive comparison one to one of this variable with the rest I have been able to observe that it does not seems to be related to any other feature."

ph55 <- "One interesting fact is the result of plotting, in this order, Driver Age, Result of Stop and Was a search conducted? Which allow us to compare proportions of the second variable by the first one between cases of the third one. Here  we find out that when a inspection is carried out in an important number of cases, close to one of each four, the result of the stop is an arrest. Adding to these we can also appreciate how the mode of the distribution changes from thirty years in the case of no search to twenty-five in the cases were a search was carried out."

ph56 <- "Finally, I rather worring fact is disclosed by plotting, in this order, the Driver Race, the Result of the Stop and Was a Search Conducted? in a Filled Bar-char, which allow us to compare proportions over the first and second features between the categories of the third one. Relatively speaking, notice the clear difference in proportions of white to black people between the cases where a search was conducted. That implies that when a stop happens, if the person is black, they have more probabilities that police will conduct a search than if they happened to were white. On the other hand the proportions of results seems to be in accordance with the size of the amount of observations."

#### FRONT-END #####
ui <- shinydashboard::dashboardPage(
  # DASHBOARD HEADER
  shinydashboard::dashboardHeader(title = "Officer Traffic Stops"),
  
  # DASHBOARD SLIDEBAR
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Information", tabName = "info", icon = icon("info")), 
      shinydashboard::menuItem("Overview", tabName = "ov", icon = icon("table")),
      shinydashboard::menuItem("1 Dimension", tabName = "1d", icon = icon("bar-chart-o")),
      shinydashboard::menuItem("N Dimensions", tabName = "nd", icon = icon("line-chart")),
      shinydashboard::menuItem("Summary of Results", tabName = "sr", icon = icon("file"))
    )
  ),
  
  # DASHBOARD BODY
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      # Tab level, each block is one tab
      shinydashboard::tabItem(tabName = "info",
                              h1("Information", align = "center"),
                              fluidRow(box(mainPanel(h5(ph11),
                                                     h4("Information"),              
                                                     h5(ph12),
                                                     br(),
                                                     h4("Overview"), 
                                                     h5(ph13),
                                                     br(),
                                                     h4("1 Dimension"), 
                                                     h5(ph14),
                                                     br(),
                                                     h4("N Dimensions"), 
                                                     h5(ph151,tags$b(ph152),ph153),
                                                     h5(tags$em("Dodged Horizontal Bar-char:"), ph16),
                                                     h5(tags$em("Count plot (Point):"), ph17),
                                                     h5(tags$em("Filled Bar-char:"), ph18),
                                                     h5(tags$em("Radial plot:"), ph19),
                                                     h5(ph191),
                                                     br(),
                                                     h4("Summary of Results"),
                                                     h5(ph192),
                                                     width = 200, options = list(margin(r=2,l=2))), 
                                           width = 250))
      ),
      shinydashboard::tabItem(tabName = "ov",
                              h1("Overview",align = "center"),
                              mainPanel(h5("The rough data, as presented, can be inspected in the following table."),width = 400,
                                        fluidRow(column(width = 12,dataTableOutput('table1'))),
                                        h5(ph21),
                                        h3("Dropping Columns"),
                                        h5(ph22),
                                        h3("Data Preprocessing"),
                                        h5(ph23),
                                        h3("Data Treatment"),
                                        h5(ph24),
                                        h3("NA's treatment"),
                                        h5(ph25),
                                        br(),
                                        fluidRow(valueBox(paste(amount_na[["Month_of_Stop"]],"%"),
                                                          "Month of Stop", icon = icon("thumbs-up"), color = "green"),
                                                 valueBox(paste(amount_na[["Reason_for_Stop"]],"%"), 
                                                          "Reason for Stop", icon = icon("thumbs-up"), color = "green"),
                                                 valueBox(paste(amount_na[["Officer_Race"]],"%"), 
                                                          "Officer Race", icon = icon("thumbs-down"), color = "orange")),
                                        fluidRow(valueBox(paste(amount_na[["Officer_Gender"]],"%"), 
                                                          "Officer Gender", icon = icon("thumbs-up"), color = "green"),
                                                 valueBox(paste(amount_na[["Officer_Years_of_Service"]],"%"), 
                                                          "Off. Service Years", icon = icon("thumbs-up"), color = "green"),
                                                 valueBox(paste(amount_na[["Driver_Race"]],"%"), 
                                                          "Driver Race", icon = icon("thumbs-up"), color = "green")),
                                        fluidRow(valueBox(paste(amount_na[["Driver_Ethnicity"]],"%"),
                                                          "Driver Ethnicity", icon = icon("thumbs-up"), color = "green"),
                                                 valueBox(paste(amount_na[["Driver_Gender"]],"%"), 
                                                          "Driver Gender", icon = icon("thumbs-up"), color = "green"),
                                                 valueBox(paste(amount_na[["Driver_Age"]],"%"),
                                                          "Driver Age", icon = icon("thumbs-up"), color = "green")),
                                        fluidRow(valueBox(paste(amount_na[["Was_a_Search_Conducted"]],"%"),
                                                          "Search conducted?", icon = icon("thumbs-up"), color = "green"),
                                                 valueBox(paste(amount_na[["Result_of_Stop"]],"%"),
                                                          "Result of Stop", icon = icon("thumbs-up"), color = "green"),
                                                 valueBox(paste(amount_na[["CMPD_Division"]],"%"), 
                                                          "CMPD Division", icon = icon("thumbs-down"), color = "red")),
                                        mainPanel(h5(ph26), width = 400),
                                        fluidRow(column(width = 12,dataTableOutput('table2')))
                              )
      ),
      shinydashboard::tabItem(tabName = "1d",
                              h1("One dimensional visual analysis", align = "center"),
                              fluidRow(box(title = "Discrete Variable",
                                           collapsible = TRUE, status = "warning", solidHeader = T, width = 400,
                                           selectInput("SV12", label = h4("x-axis"),
                                                       choices = Name2Label, 
                                                       selected = 1)
                              )
                              ),
                              fluidRow(box(title = 'Bar-Char for Discrete Variable', solidHeader = TRUE,
                                           width = 400, height = 800,  status = "primary",
                                           plotOutput("plot12", height = 700)
                              )
                              )
      ),
      tabItem(tabName = "nd",
              h1("N-dimensional visual analysis", align = "center"),
              fluidRow(box(title = "Variable Selection", 
                           collapsible = TRUE, status = "warning", solidHeader = T, width = 400,
                           selectInput("GB", 
                                       label = h4("Choose the variables from which we  should count the observations"),
                                       choices = Name2Label, 
                                       multiple = T,
                                       selected = 1))
              ),
              fluidRow(box(title = "Type of Visualization", 
                           collapsible = TRUE, status = "warning", solidHeader = T, width = 400,
                           selectInput("type", label = h4("Choose a Visualization"),
                                       choices = c("Dodged Horizontal Bar-char"=1,
                                                   "Count plot (Point)"=2,
                                                   "Filled Bar-char"=3,
                                                   "Radial plot"=4),
                                       selected = 1))
              ),
              fluidRow(box(title = "Visualization" ,
                           solidHeader = T, width = 500, status = "primary",height = 800,
                           plotOutput("plotCombi", height = 700))),
              fluidRow(column(width = 12,dataTableOutput('tbl_cmb'), align = "center"))
      ),
      shinydashboard::tabItem(tabName = "sr",
                              h1("Summary of Results", align = "center"),
                              fluidRow(box(mainPanel(tags$b("REMARKS: "),ph50,
                                                     h4("First Result"),              
                                                     h5(ph51),
                                                     br(),
                                                     h4("Second Result"), 
                                                     h5(ph52),
                                                     br(),
                                                     h4("Third Result"), 
                                                     h5(ph53),
                                                     br(),
                                                     h4("Fourth Result"), 
                                                     h5(ph54),
                                                     br(),
                                                     h4("Fifth Result"), 
                                                     h5(ph55),
                                                     br(),
                                                     h4("Sixth Result"), 
                                                     h5(ph55),
                                                     br(),
                                                     width = 200, options = list(margin(r=2,l=2))), 
                                           width = 250))
      )
    )
  )
)

#### BACK END ####
server <- function(input, output) { 
  
  ## Overview section
  
  output$table1 <- renderDataTable({OTS_rough},options = list(scrollX = TRUE,pageLength = 5))
  output$table2 <- renderDataTable({OTS},options = list(scrollX = TRUE,pageLength = 5))
  
  ## 1D Tab
  ### PLOTS
  output$plot12 <- renderPlot({
    random_color = sample(1:100, 1)
    ggplot() + geom_bar(aes(OTS[[input$SV12]]), fill = hue_pal()(100)[random_color], 
                        color = hue_pal()(100)[(random_color+1)%%100], 
                        alpha = 0.9) + 
      labs(x = paste("\n",Label2Name[[input$SV12]])) +
      theme(axis.text.x=element_text(angle=60,hjust=1))
  })
  
  ## ND tab
  
  # Tbl at the bottom of the ND section
  output$tbl_cmb <- renderDataTable(OTS %>%
                                      count_(unlist(input$GB)),
                                    options = list(scrollX = TRUE, 
                                                   pageLength = 10, 
                                                   rownames= TRUE))
  
  # Dynamic graph at the ND section  
  output$plotCombi <- renderPlot({
    
    ## Each if block  with $type is for an specific type of graph  
    if(input$type == 1){
      
      random_color = sample(1:100, 1)
      df_combi <- OTS %>% count_(unlist(input$GB))
      VARIABLES <- colnames(df_combi)
      
      if(length(VARIABLES)==1){
        
        random_color = sample(1:100, 1)
        G <- ggplot(OTS %>% count_(unlist(input$GB))) + 
          geom_bar(aes_string(x = 1, y= 'n'), stat = "identity",
                   alpha = 0.7, fill = hue_pal()(100)[random_color]) + xlab("Total") + 
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          )
        
      }else if(length(VARIABLES)==2){
        G <- ggplot(df_combi) + 
          geom_bar(aes_string(x = VARIABLES[1], y = 'n'), 
                   stat = "identity", fill = hue_pal()(100)[random_color], 
                   color = hue_pal()(100)[(random_color+1)%%100], 
                   alpha = 0.7) + xlab(Label2Name[[VARIABLES[1]]]) +
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          ) + coord_flip()
      }else if(length(VARIABLES) == 3){
        G <- ggplot(df_combi) + 
          geom_bar(aes_string(x = VARIABLES[1], y = 'n', fill = VARIABLES[2]), 
                   stat = "identity", position = "dodge",
                   alpha = 0.7) + xlab(Label2Name[[VARIABLES[1]]]) +
          labs(fill = Label2Name[[VARIABLES[2]]]) +
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          ) + coord_flip()
      }else if(length(VARIABLES) >= 4){
        if (length(unique(df_combi[[VARIABLES[3]]])) < 3){
          G <- ggplot(df_combi) + 
            geom_bar(aes_string(x = VARIABLES[1], y = 'n', fill = VARIABLES[2]), 
                     stat = "identity", position = "dodge", 
                     alpha = 0.7) + xlab(Label2Name[[VARIABLES[1]]]) +
            labs(fill = Label2Name[[VARIABLES[2]]]) + 
            facet_grid(reformulate(".",VARIABLES[3])) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom", 
                                axis.text.x=element_text(angle=60,hjust=1)
            ) + coord_flip()
        }else if(length(unique(df_combi[[VARIABLES[3]]])) < 11){
          G <- ggplot(df_combi) + 
            geom_bar(aes_string(x = VARIABLES[1], y = 'n', fill = VARIABLES[2]), 
                     stat = "identity", position = "dodge", 
                     alpha = 0.7) + xlab(Label2Name[[VARIABLES[1]]]) +
            labs(fill = Label2Name[[VARIABLES[2]]]) +
            facet_wrap(reformulate(".",VARIABLES[3]), ncol = 2) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom", 
                                axis.text.x=element_text(angle=60,hjust=1)
            ) + coord_flip()
        }else{
          G <- ggplot(df_combi) + 
            geom_bar(aes_string(x = VARIABLES[1], y = 'n', fill = VARIABLES[2]), 
                     stat = "identity", position = "dodge", 
                     alpha = 0.7) + xlab(Label2Name[[VARIABLES[1]]]) + ylab("n") +
            labs(fill = Label2Name[[VARIABLES[2]]]) +
            facet_wrap(reformulate(".",VARIABLES[3]), ncol = 4) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom", 
                                axis.text.x=element_text(angle=60,hjust=1)
            ) + coord_flip()
        }
      }
      
    }else if(input$type == 2){
      VARIABLES <- input$GB
      if(length(VARIABLES)<1){
        random_color = sample(1:100, 1)
        G <- ggplot(OTS %>% count_(unlist(input$GB))) + 
          geom_bar(aes_string(x = 1, y= 'n'), stat = "identity",
                   alpha = 0.4, fill = hue_pal()(100)[random_color]) + xlab("Total") + 
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          )
      }else if(length(VARIABLES)==1){
        random_color = sample(1:100, 1)
        df_combi <- OTS %>% count_(unlist(input$GB))
        G <- ggplot(OTS) + 
          geom_bar(aes_string(x = VARIABLES[[1]]),
                   fill = hue_pal()(100)[random_color], 
                   color = hue_pal()(100)[(random_color+1)%%100],
                   alpha = 0.4) +
          xlab(Label2Name[[VARIABLES[1]]]) +
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          )
      }else if(length(VARIABLES)==2){
        random_color = sample(1:100, 1)
        G <- ggplot(OTS) + 
          geom_count(aes_string(x = VARIABLES[1], y = VARIABLES[2]), 
                     color = hue_pal()(100)[random_color], 
                     fill = hue_pal()(100)[random_color], 
                     alpha = 0.4) + xlab(Label2Name[[VARIABLES[1]]])+ 
          ylab( Label2Name[[VARIABLES[2]]]) +
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          )
      }else if(length(VARIABLES)>=3){
        if(length(OTS[[VARIABLES[3]]])<3){
          G <- ggplot(OTS) + 
            geom_count(aes_string(x = VARIABLES[1], y = VARIABLES[2], color=VARIABLES[1]), 
                       position = "jitter", 
                       alpha = 0.4) +
            facet_grid(reformulate(".",VARIABLES[3])) + 
            xlab(Label2Name[[VARIABLES[1]]]) + ylab(Label2Name[[VARIABLES[2]]]) +
            labs(color = Label2Name[[VARIABLES[3]]]) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom", 
                                axis.text.x=element_text(angle=60,hjust=1)
            )
        }else if(length(OTS[[VARIABLES[3]]])<11){
          G <- ggplot(OTS) + 
            geom_count(aes_string(x = VARIABLES[1], y = VARIABLES[2], color=VARIABLES[1]), 
                       position = "jitter", 
                       alpha = 0.4) +
            facet_wrap(reformulate(".",VARIABLES[3]), ncol = 2) + 
            xlab(Label2Name[[VARIABLES[1]]]) + ylab(Label2Name[[VARIABLES[2]]]) +
            labs(color = Label2Name[[VARIABLES[3]]]) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom", 
                                axis.text.x=element_text(angle=60,hjust=1)
            )
        }else{
          G <- ggplot(OTS) + 
            geom_count(aes_string(x = VARIABLES[1], y = VARIABLES[2], color=VARIABLES[1]), 
                       position = "jitter", 
                       alpha = 0.4) +
            facet_wrap(reformulate(".",VARIABLES[3]), ncol = 4) + 
            xlab(Label2Name[[VARIABLES[1]]]) + ylab(Label2Name[[VARIABLES[2]]]) + 
            labs( color = Label2Name[[VARIABLES[3]]]) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom", 
                                axis.text.x=element_text(angle=60,hjust=1)
            )
        }
      }
    }else if(input$type == 3){
      VARIABLES <- input$GB
      if(length(VARIABLES)<1){
        random_color = sample(1:100, 1)
        G <- ggplot(OTS %>% count_(unlist(input$GB))) + 
          geom_bar(aes_string(x = 1, y= 'n'), stat = "identity",
                   alpha = 0.4, fill = hue_pal()(100)[random_color]) + xlab("Total") + 
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          )
      }else if(length(VARIABLES)==1){
        random_color = sample(1:100, 1)
        G <- ggplot(OTS) + 
          geom_bar(aes_string(x = VARIABLES[1]),
                   alpha = 0.4, fill = hue_pal()(100)[random_color]) + 
          xlab(Label2Name[[VARIABLES[1]]]) + 
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          )
        
      }else if(length(VARIABLES)==2){
        G <- ggplot(OTS) + 
          geom_bar(aes_string(x = VARIABLES[1], fill = VARIABLES[2]),
                   alpha = 0.4) + xlab(Label2Name[[VARIABLES[1]]]) + 
          labs(fill = Label2Name[[VARIABLES[2]]]) +
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom", 
                              axis.text.x=element_text(angle=60,hjust=1)
          )
      }else if(length(VARIABLES)>=3){
        if(length(OTS[[VARIABLES[3]]])<3){
          G <- ggplot(OTS) + 
            geom_bar(aes_string(x = VARIABLES[1], fill = VARIABLES[2]),
                     alpha = 0.4)  +
            xlab(Label2Name[[VARIABLES[1]]]) + 
            labs(fill = Label2Name[[VARIABLES[2]]]) +
            facet_grid(reformulate(".",VARIABLES[3]), scales ="free_y") +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank() ) +
            theme(legend.position="bottom", axis.text.x=element_text(angle=60,hjust=1))
        }else if(length(OTS[[VARIABLES[3]]])<11){
          G <- ggplot(OTS) + 
            geom_bar(aes_string(x = VARIABLES[1], fill = VARIABLES[2]),
                     alpha = 0.4)  +
            xlab(Label2Name[[VARIABLES[1]]]) + 
            labs(fill = Label2Name[[VARIABLES[2]]]) +
            facet_wrap(reformulate(".",VARIABLES[3]),ncol = 2, scales ="free_y") +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank() ) +
            theme(legend.position="bottom", axis.text.x=element_text(angle=60,hjust=1))
        }else {
          G <- ggplot(OTS) + 
            geom_bar(aes_string(x = VARIABLES[1], fill = VARIABLES[2]),
                     alpha = 0.4)  +
            xlab(Label2Name[[VARIABLES[1]]]) + 
            labs(fill = Label2Name[[VARIABLES[2]]]) +
            facet_wrap(reformulate(".",VARIABLES[3]),ncol = 4, scales ="free_y") +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank() ) +
            theme(legend.position="bottom", axis.text.x=element_text(angle=60,hjust=1))
        }
      }
    }else if(input$type == 4){
      VARIABLES <- input$GB
      
      if(length(VARIABLES)<1){
        random_color = sample(1:100, 1)
        G <- ggplot(OTS %>% count_(unlist(input$GB))) + 
          geom_bar(aes_string(x = 1, y= 'n'), stat = "identity",
                   alpha = 0.4, fill = hue_pal()(100)[random_color]) + xlab("Total") + 
          coord_polar() +
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom"
          )
        
      }else if(length(VARIABLES)==1){
        random_color = sample(1:100, 1)
        G <- ggplot(OTS) + 
          geom_bar(aes_string(x = VARIABLES[1]),
                   alpha = 0.4, fill = hue_pal()(100)[random_color]) + 
          coord_polar() +
          xlab(Label2Name[[VARIABLES[1]]]) +
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom"
          )
      }else if(length(VARIABLES)==2){
        G <- ggplot(OTS) + 
          geom_bar(aes_string(x = VARIABLES[1], fill = VARIABLES[2]),
                   alpha = 0.4) + 
          coord_polar()+
          xlab(Label2Name[[VARIABLES[1]]]) + 
          labs(fill = Label2Name[[VARIABLES[2]]]) +
          theme_bw() + theme( strip.background  = element_blank(),
                              panel.grid.major = element_line(colour = "grey80"),
                              panel.border = element_blank(),
                              axis.ticks = element_blank(),
                              panel.grid.minor.x=element_blank(),
                              panel.grid.major.x=element_blank(),
                              legend.position="bottom"
          )
      }else if(length(VARIABLES)>=3){
        if(length(VARIABLES[3])<3){
          G <- ggplot(OTS) + 
            geom_bar(aes_string(x = VARIABLES[1], fill = VARIABLES[2]),
                     alpha = 0.4) + 
            coord_polar()+
            facet_grid(reformulate(".",VARIABLES[3])) +
            xlab(Label2Name[[VARIABLES[1]]]) + 
            labs(fill = Label2Name[[VARIABLES[2]]]) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom"
            )
        }else if(length(VARIABLES[3])<11){
          G <- ggplot(OTS) + 
            geom_bar(aes_string(x = VARIABLES[1], fill = VARIABLES[2]),
                     alpha = 0.4) + 
            coord_polar()+
            facet_wrap(reformulate(".",VARIABLES[3]), ncol = 2)  +
            xlab(Label2Name[[VARIABLES[1]]]) + 
            labs(fill = Label2Name[[VARIABLES[2]]]) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom"
            )
        }else{
          G <- ggplot(OTS) + 
            geom_bar(aes_string(x = VARIABLES[1], fill = VARIABLES[2]),
                     alpha = 0.4) + 
            coord_polar(theta = "y")+
            facet_wrap(reformulate(".",VARIABLES[3]), ncol = 4)  +
            xlab(Label2Name[[VARIABLES[1]]]) + 
            labs(fill = Label2Name[[VARIABLES[2]]]) +
            theme_bw() + theme( strip.background  = element_blank(),
                                panel.grid.major = element_line(colour = "grey80"),
                                panel.border = element_blank(),
                                axis.ticks = element_blank(),
                                panel.grid.minor.x=element_blank(),
                                panel.grid.major.x=element_blank(),
                                legend.position="bottom"
            )
        }
      }
    }
    G
  })
}

shinyApp(ui, server)