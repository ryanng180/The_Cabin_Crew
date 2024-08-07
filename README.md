# The_Cabin_Crew : Queue Optima
ESD T5 - 2D Project

Problem 9 : Priority Queue Operation

## Description
Queue Optima is a presciptive analytics tool for queue policy management in Airports. For this project, we have based our case study on the Los Angeles International Airport (LAX). Our software comes coded with 3 queuing policies: Unified Queue, Double Queue, Priority Queue. And all are underscored by a M/M/k model. Queue Optima serves to prioritize and minimize immigration waiting time for the U.S. Cititzens, while ensuring fairness for the Non- U.S. citizens.

M - Poisson Process arrival rate <br>
M - Exponential service rate <br>
k - finite number of counters, of which the number of counters for the U.S. citizens queue and that for the Non- U.S. citizens queue. 

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
Run the file to launch the webapp

## 2.1 Help page
Please refer to our Help page for a quick guide on using the software.

## 2.2 Home page (Landing Page)
Based on real world constraints, namely Queue Psychology, we have prescibed a Double Queue model for LAX immigrations. The statistics shown on the Home page reflects the Double Queue.

