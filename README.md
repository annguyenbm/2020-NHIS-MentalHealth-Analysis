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

## App Status

The application is currently hosted on ShinySever. Users can access our App directly here: https://arerickson.shinyapps.io/nhisproduction/

Otherwise, the application and coding can also be accessed or launched on local computer via cloning a GitHub repo and Pull request, downloading source zip file via release version **v0.1.0**, or contacting one of the three App Collaborators for further permissions. 

Check App vignette for examples of how each tab works and how to interpret the results. The App Demo presentation is currently not published, but if users face any constraints in exploring or using the App, users can contact one of the 3 App Collaborators to request for the presentation link.

Last but not least, please report any errors or change recommendations to the 3 collaborators, whose contact information is listed below.

## Launching App on Local Computer

### Prerequisite 

**A. Software**

Install R : https://cran.r-project.org/
Install RStudio: https://www.rstudio.com/products/rstudio/ 
Recommend **version 4.1.0** onward.

**B. Packages**

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

**C. Steps to launch**
  
1. Install the mentioned R packages: You can install these R packages by follow either of the following methods.
  * **Auto Installation**
  
  Step 1: Download the App's R script and open it on RStudio
  
  Step 2: R will compare the packages listed in this App's R script against the existing library in the user's local computer. If there's any new package, R will auto-suggest user to install the new package. Thus, look for this system message on top of RStudio, right below the toolbar.
  
  Step 3: Click the installation link. The new ackage installation process will be run at the Console window.
  
  Step 4: Restart RStudio is highly recommended whenever a new package is installed.
  
  * **Manual Installation**
  
  Step 1: Open RStudio
  
  Step 2: Look for the Console window in RStudio
  
  Step 3: At the Console window, enter the following command

```{r}
install.packages("<package_name>")
```

  Step 4: Restart RStudio once the packages are installed.

2. Set working direction path or open a new project in R

### Launch App 

1. Access the '/NHIS_App/' folder path
2. Open the R file **app.R**
3. Click **Run App** button at the top tool bar


## App Collaborators and Contact Information

**Names:** Binh Minh An Nguyen, Ben Stano, and Andrew Erickson (American University)

**Emails:** bn6523a@student.american.edu (Binh Minh An Nguyen); bs3654a@student.american.edu (Ben Stano); andrewe@american.edu (Andrew Erickson)

**GitHub IDs:** annguyenbm (Binh Minh An Nguyen); bstano (Ben Stano); andrew-r-erickson (Andrew Erickson)

**LinkedIn:**

Binh Minh An Nguyen : www.linkedin.com/in/annguyenbm

Andrew Erickson : https://www.linkedin.com/in/andrew-erickson-01373475/

Ben Stano : https://www.linkedin.com/in/bstano/
