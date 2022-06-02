# 2020 NHIS Mental Health Data Analysis Application

This is an R Shiny App using the 2020 National Health Interview Surveillance data to examine which factors contributed to the anxiety and depression among US adults. This application is designed to offer general public who are interested in statistics/data analysis in health segment, health care professionals, administrators, and health survey designers a statistics-educated tool to explore, visualize, model, and understand the collected data in a clearer format.

The application contains 7 tabs, including:

  * **Introduction tab**: explains the general framework of the 2020 National Health Interview Survey and the subset of data used in the app;
  * **EDA tab**: allows users to explore the data type and the distribution of each variable, and the extent to which the question was answered by the more than 30,000 subjects;
  * **Chi-square tab**: visualizes the tests of relationships between variables and their significance in differences of means (ANOVA), dependence (Chi-square), or correlation depending on the class of variable;
  * **Logistic Regression tab**: builds models predicting severe anxiety or depression and observe the summary output of that model;
  * **Classification Tree tab**: builds and visualizes the tree models to predict the mental health severity;
  * **Random Forest tab**: predicts either the anxiety or depression status based on the variable importance;
  * **Dictionary tab**: provides definitions, levels, and transformations for each variable used in this application;
  

## Launching App

The application is currently hosted on ShinySever. Users can access our App directly here: https://arerickson.shinyapps.io/nhisproduction/

Otherwise, the application and coding can also be accessed or launched on local computer via cloning a GitHub repo and Pull request, downloading source zip file via release version **v0.1.0**, or contacting one of the three App Collaborators for further permissions. 

The App Demo presentation is currently not published, but if users face any constraints in exploring or using the App, users can contact one of the 3 App Collaborators to request for the presentation link.

Required software: R, R studio. Recommend **version 4.1.0** onward.

Packages to be installed before launching the application:

  * **shiny** - build interactive web application in R
  * **bslib** - create app theme
  * **tidyverse** - R package designed for data science
  * **ggmosaic** - visualize categorical data
  * **DT** - R interface to js library table
  * **broom** - tidy up messy output into a data frame
  * **forcats** - use factors to handle categorical variables
  * **gridExtra** - provide user-level functions to work with grid graphics
  * **tree** - develop, modify, and process decision trees
  * **reshape2** - restructure and aggregate data
  * **randomForest** - implement Breinman's random forest algorithm
  
0.1. To install these R packages, input the following commands into R Console then press enter.

```{r}
install.packages("<package_name>")
```

0.2. Once the packages are installed, set working path directory or open project in R

0.3. Access the '/NHIS_App/' folder path, open the **app.R** script

0.4. Click **Run App** button at the top tool bar

## App Status

The current version of this app is operational and ready for use. Please report any errors or change recommendations to the three collaborators, whose contact information is listed below.

## App Collaborators and Contact Information

**Names:** Binh Minh An Nguyen, Ben Stano, and Andrew Erickson (American University)

**Emails:** bn6523a@student.american.edu (Binh Minh An Nguyen); bs3654a@student.american.edu (Ben Stano); andrewe@american.edu (Andrew Erickson)

**GitHub IDs:** annguyenbm (Binh Minh An Nguyen); bstano (Ben Stano); andrew-r-erickson (Andrew Erickson)
