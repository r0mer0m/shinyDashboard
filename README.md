# Shiny Dashboard Application

As part of the course **MSDS694** we were asked to perform an Exploratory Data Analysis on the `Officer_Traffic_Stops.csv`.

I thought that the best way to automatize the iterative process of visualizing relationships between different variables would be by creating an application that allows me to interactively look for those kinds of relationships.

I decided to do some self-learning in [Shiny Dashboard](https://rstudio.github.io/shinydashboard/) during the weekend and developed this app.
## The application

Probably the most interesting part is the one contained in the `N Dimensions` tab. In this section, the user can play with different types of visualizations for up to three different variables simultaneously.

Under `Variable Selection` the user can choose the feature they want to visualize and under `Type of Visualization` the user can choose which kind of plot they want to visualize it.

There are 4 kinds of plots and they scale from 1 to 3 dimensions depending on the number of variables that the user wants to visualize. 

## Running the application

To run the application you need to:

  + Open `main.R`.
  
  + If needed, replace the current `ROOT_PATH` with the path to the directory where you have the `Officer_Traffic_Stops.csv` dataset stored. By default it assumes both the R script and the data are under the same directory.
    
*Note: If you are not visualizing it in the browser you can do so by clicking in the top-left bottom in the emerging window.*


