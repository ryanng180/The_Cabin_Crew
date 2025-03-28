# The Cabin Crew: Queue Optima

![Repository Image](/the%20cabin%20crew.png)

### ESD Term 5 - 2D Project (Engineering Systems Architecture & Manufacturing and Service Operations)

### Problem 9, Challenge Topic: Priority Queue Operation

## Description
Queue Optima is a sophisticated prescriptive analytics tool designed for queue policy management in airports. For this project, we based our case study on the Los Angeles International Airport (LAX). Our software incorporates three queuing policies: Unified Queue, Double Queue, and Priority Queue, all underscored by an M/M/k model.


Queue Optima aims to prioritize and minimize immigration waiting times for U.S. citizens while ensuring fairness for non-U.S. citizens.


- **M** - Poisson Process arrival rate
- **M** - Exponential service rate
- **k** - Finite number of counters, distributed between queues for U.S. citizens and non-U.S. citizens.


By implementing Queue Optima, airports can enhance operational efficiency, reduce wait times, and maintain equitable service for all passengers.

## [Presentation Slides](https://www.canva.com/design/DAGL70KG5sw/Xip9rGFI0BsoF5dIO5NMkw/view?utm_content=DAGL70KG5sw&utm_campaign=designshare&utm_medium=link&utm_source=editor)
The clickable header contains the link to our presentation deck. 

## 1. Install required R libraries
```
install.packages("shiny")
install.packages("shinythemes")
install.packages("queueing")
install.packages("ggplot2")
install.packages("plotly")
install.packages("tidyr")
```

## 2. Launch the webapp
Step 1: Run the file


Step 2: Launching the webapp


2a. Rstudio: Paste the code below in the terminal and run
```
shinyApp(ui = ui, server = server)
```


2b. VScode: Paste the code below in the termnial and run
```
shiny::runApp("path/to/your_shiny_app")
```

### 2.1 Home page (Landing Page)
Based on real world constraints, namely Queue Psychology, we have prescibed a Double Queue model for LAX immigrations. The statistics shown on the Home page reflects the Double Queue.

### 2.2 Help page
Please refer to our Help page for a quick guide on using the software.
