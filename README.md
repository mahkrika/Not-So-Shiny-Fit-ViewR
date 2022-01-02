# Not-So-Shiny-Fit-ViewR
A basic Shiny app to view .fit file outputs

## About the Not-So-Shiny Fit ViewR 

This little Shiny app is evidently not very pretty, but it is a work in progress so will hopefully develop into something more aesthetically pleasing, and with more features.

The tool came about because although I like the Suunto app for monitoring exercise I am also a glutton for punishment and like to learn new data skills - in this instance R. 
Therefore this is an attempt to port some of the Suunto (and any other exercise monitoring watch/device) app functionality into an online tool.

It runs using the following packages:
<ul>
<li><a href='https://cran.r-project.org/web/packages/shiny/'>shiny</a></li>
<li><a href='https://cran.r-project.org/web/packages/shinythemes/'>shinythemes</a></li>
<li><a href='https://cran.r-project.org/web/packages/shinyBS/'>shinyBS</a></li>
<li><a href='https://cran.r-project.org/web/packages/dplyr/'>dplyr</a></li>
<li><a href='https://cran.r-project.org/web/packages/ggplot2/'>ggplot2</a></li>
<li><a href='https://cran.r-project.org/web/packages/leaflet/'>leaflet</a></li>
<li><a href='https://cran.r-project.org/web/packages/hms/index.html'>hms</a></li>
<li><a href='https://github.com/grimbough/FITfileR'>FITfileR</a></li>
</ul>
                
I want to bring particular attention to ***FITfileR*** which is a lovely package available from the link in the list. I could not for the life of me
work out how to convert a .fit file into something usable in R. This package does all of the conversion of the .fit file into a dataframe within R which 
is then used for all subsequent operations. If you ever see this message, *thank you for developing this package!*

All of the code that underpins this Shiny app is available in this Github repository, as is a test .fit file (in /data), should you wish to try it out.

## About the Output
At present the output of the application provides a variety of basic information.

### Summary
This tab provides details limited to those recorded from distance, duration, speed, and altitude.

Further information is likely recorded in the .fit file depending upon the device used.
For example, Heart Rate information may be recorded but at present is not displayed in
the app. This will be developed at a later point.

### Graphs
A series of line graphs all with the timestamp on the x-axis. Each graph displays  
elements on the y-axis (depending upon availability in the .fit file). These are currently 
limited to altitude, temperature, cadence, and speed.
Again Heart Rate will be developed at a later date. Any further suggestions are welcomed.

### Map
A map of the route as loaded via the Leaflet package.

The map is centred based upon the median latitude and longitude coordinate from 
those recorded in the .fit file. A zoom factor has been applied to the default 
load, but the user can zoom in and out of the map as they wish.

The route taken is marked upon the map and is coloured: red > blue > black,
where red is the start of the recorded route, and black is the end of the 
recorded route.

### Individual row details of .fit

This is a (collapsed as default) table of all of the recordings from the .fit file.

The output is taken directly from the ***FITfileR*** package using the *records* command. This results in multiple tables which are then bound 
together and ordered by the timestamp.

The timestamp is formatted to display correctly though may be susceptible to 
daylight savings time issues. This may be rectified at a later date.
