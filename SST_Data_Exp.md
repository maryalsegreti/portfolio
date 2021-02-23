Student\_Support\_Team\_Data\_Exploration
================
Mary Segreti
1/14/2021

## Overview

The purpose of this data anlaysis is to explore student grade and
attendance distribution at a given checkpoint suring the semester. The
data visualations will be used to spark conversations about student
status, support, and needs. There are multiple data sources used
throughout, all containing different information (such as grades,
attendance, interventions, demographic info, etc.). The data was
combined and cleaned throughout the process for easier analysis.

``` r
library(readr)
library(tidyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
#import & preview data
data <-read.csv("SST_Data.csv")
```

``` r
#transform character data into numeric
data$X10.3.Att.Rate <-sub("%","",data$X10.3.Att.Rate)
data <-transform(data,X10.3.Att.Rate=as.numeric(X10.3.Att.Rate))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
data <-transform(data,X10.12.GPA=as.numeric(X10.12.GPA))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

I want to create a status column for each date of GPA and Attendance
data that indicates whether a students is receiving and admin attendance
intervention (attendance\<=80%) or was (hopefully) receiving teacher
interventions because their attendance was good, but GPA was low

``` r
#creating new columns in data to indicate status at dates

data <- data %>%
  mutate(X10_12_Status = case_when(
    X10.3.Att.Rate <= 80 ~ "Low Attendance",
    X10.12.GPA <= 1 ~ "GPA 1 or Below",
    X10.12.GPA <= 2 ~ "GPA Between 1 and 2"
  ))
data$X10_12_Status[is.na(data$X10_12_Status)] <- "On Track"
```

``` r
#Creating a Histogram of X10_12_Status
Hist_10_12 <- data %>%
  ggplot(aes(x= X10_12_Status)) +
  geom_bar()

Hist_10_12
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-7-1.png)<!-- --> The
above graph shows us the number of students in each status on October
12th, 2020. We want to be able to see how those numbers changed over
time as teachers and administrators intervened to try and increase
student engagement and grades. First I need to clean the data

``` r
#changing all attendance columns to numeric without % symbol
data$X11.24.Att <- sub("%","",data$X11.24.Att)
data <-transform(data,X11.24.Att = as.numeric(X11.24.Att))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
data$X12.11.Att <-sub("%","",data$X12.11.Att)
data <-transform(data,X12.11.Att=as.numeric(X12.11.Att))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
data$X1.8.Att <- sub("%","",data$X1.8.Att)
data <-transform(data,X1.8.Att = as.numeric(X1.8.Att))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
#changing all corresponding GPA columns to numeric
data <- transform(data,X11.23.GPA = as.numeric(X11.23.GPA))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
data <- transform(data,X12.14.GPA = as.numeric(X12.14.GPA))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
data <- transform(data,X1.11.21.GPA = as.numeric(X1.11.21.GPA))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
#making more status columns
data <- data %>%
  mutate(X11_24_Status = case_when(
    X11.24.Att <= 80 ~ "Low Attendance",
    X11.23.GPA <= 1 ~ "GPA 1 or Below",
    X11.23.GPA <= 2 ~ "GPA Between 1 and 2"
  ))

data$X11_24_Status[is.na(data$X11_24_Status)] <- "On Track"

data <- data %>%
  mutate(X12_14_Status = case_when(
    X12.11.Att <= 80 ~ "Low Attendance",
    X12.14.GPA <= 1 ~ "GPA 1 or Below",
    X12.14.GPA <= 2 ~ "GPA Between 1 and 2"
  ))

data$X12_14_Status[is.na(data$X12_14_Status)] <- "On Track"

data <- data %>%
  mutate(X1_11_Status = case_when(
    X1.8.Att <= 80 ~ "Low Attendance",
    X1.11.21.GPA <= 1 ~ "GPA 1 or Below",
    X1.11.21.GPA <= 2 ~ "GPA Between 1 and 2"
  ))

data$X1_11_Status[is.na(data$X1_11_Status)] <- "On Track"

#need to make the data into long form
data_long <-gather(data,Date,Status,X10_12_Status:X1_11_Status, factor_key = TRUE)
```

Now we can make a series of histograms comparing the number of students
in each status at monthly intervals.

``` r
over_time_plot <- data_long %>%
  ggplot(aes(
    x= Date,
    fill= Status
  )) +
  geom_bar(position = 'dodge')+
  labs(title = "12th Grade Student Status Over Time") +
  scale_x_discrete(labels = c(
    "X10_12_Status" = "10/12/20",
    "X11_24_Status" = "11/24/20",
    "X12_14_Status" = "12/14/20",
    "X1_11_Status" = "1/11/21"
  ))

over_time_plot
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> Now,
thinking about the most current date, we want to look at the dat from an
equity standpoint: looking at student status based program

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
program_bar <- data %>%
  ggplot(aes(x=Program, fill = X1_11_Status))+
  geom_bar(position = 'dodge') +
  scale_x_discrete(labels = wrap_format(5))+
  labs(fill = "1/11/21 Status")
  

program_bar
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-13-1.png)<!-- --> Now we
want to see student status by GP as well as attendance. This will
involve data from another source as well.

``` r
#import libraries again
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(janitor)
```

    ## 
    ## Attaching package: 'janitor'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
#import most up to date data
new_data <- read_csv("SST_119.csv")
```

    ## 
    ## ── Column specification ───────────────────────────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_character(),
    ##   `Student ID` = col_double(),
    ##   `F Count 10/26` = col_double()
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
new_data <- new_data %>% clean_names()
```

``` r
#change format of attendance data to integers
new_data$x1_15_att <- sub("%","",new_data$x1_15_att)

new_data <- transform(new_data, x1_15_att = as.numeric(x1_15_att))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
new_data$x1_8_att <- sub("%","", new_data$x1_8_att)

new_data <- transform(new_data, x1_8_att = as.numeric(x1_8_att))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
new_data$x12_18_att <- sub("%", "", new_data$x12_18_att)

new_data <- transform(new_data, x12_18_att = as.numeric(x12_18_att))
```

    ## Warning in eval(substitute(list(...)), `_data`, parent.frame()): NAs introduced
    ## by coercion

``` r
#make a 3 week average attendance
new_data <- mutate(new_data,Avg_3_Week_Att = (new_data$x1_15_att + new_data$x1_8_att + new_data$x12_18_att)/3)
```

``` r
#Make an attendance status column with "attendance 80 or below" and "attendance above 80"
new_data <- new_data %>% mutate(Att_Status = case_when(
  Avg_3_Week_Att <= 80 ~ "Attendance 80% or Below",
  Avg_3_Week_Att > 80 ~ "Attendance above 80%"
))
```

``` r
#get rid of  "on track" and add in category of "GPA above 2" 
new_data <- new_data %>% mutate(GPA_Status = case_when(
  x1_11_21_gpa < 1 ~ "GPA Below 1",
  x1_11_21_gpa <= 2 ~ "GPA Between 1 and 2",
  x1_11_21_gpa > 2 ~ "GPA Above 2"))
```

``` r
#make graph with program as x, status as dodge fill, and attendance as stacked fill
graph_program <- new_data %>%
  ggplot(aes(x=program,
             fill = GPA_Status))+
  geom_bar(position = 'dodge') +
  scale_x_discrete(labels = wrap_format(5))

graph_program
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-21-1.png)<!-- --> To add
in attendance to the comparison, I will make a status column based on
GPA and ATT conditions.

``` r
#make Status Column
new_data <- new_data %>% mutate( Status = case_when(
  x1_11_21_gpa < 1 & Avg_3_Week_Att <= 80 ~ "GPA<1 & Att<80%",
  x1_11_21_gpa < 1 & Avg_3_Week_Att > 80 ~  "GPA<1 & Att>80%",
  x1_11_21_gpa <= 2 & Avg_3_Week_Att <= 80 ~ "1<=GPA<=2 & Att<80%",
  x1_11_21_gpa <= 2 & Avg_3_Week_Att > 80 ~ "1<=GPA<=2 & Att>80%",
  x1_11_21_gpa > 2 & Avg_3_Week_Att <= 80 ~ "GPA>2 & Att<80%",
  x1_11_21_gpa >2 & Avg_3_Week_Att > 80 ~ "GPA>2 & Att>80%"
))

graph_prog_att_gpa <- new_data %>%
  ggplot(aes(x= program, fill = factor(Status, levels=c("GPA<1 & Att<80%","GPA<1 & Att>80%","1<=GPA<=2 & Att<80%", "1<=GPA<=2 & Att>80%","GPA>2 & Att<80%", "GPA>2 & Att>80%"))))+
  geom_bar(position = 'dodge')+
  scale_x_discrete(labels = wrap_format(5))+
  labs(fill = "Current Status", y = "Number of Students")

graph_prog_att_gpa
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-22-1.png)<!-- --> Now,
we will explore student status by race, combining our current data with
another data source of student demographic information.

``` r
#import school data
school_data <-read_csv("Full_School_12_20.csv")
```

    ## Warning: Missing column names filled in: 'X37' [37], 'X38' [38], 'X39' [39],
    ## 'X40' [40], 'X41' [41], 'X42' [42], 'X43' [43], 'X44' [44], 'X45' [45],
    ## 'X46' [46], 'X47' [47], 'X48' [48], 'X49' [49], 'X50' [50], 'X51' [51],
    ## 'X52' [52], 'X53' [53], 'X54' [54], 'X55' [55], 'X56' [56], 'X57' [57],
    ## 'X58' [58], 'X59' [59], 'X60' [60], 'X61' [61], 'X62' [62]

    ## 
    ## ── Column specification ───────────────────────────────────────────────────────────────────────────────
    ## cols(
    ##   .default = col_character(),
    ##   `Student ID` = col_double(),
    ##   `Grade Level` = col_double(),
    ##   Section = col_double(),
    ##   `Numeric Average` = col_double(),
    ##   `GPA Points` = col_double(),
    ##   `Live GPA` = col_double(),
    ##   `Core Failures` = col_double(),
    ##   `Non-Core Failures` = col_double(),
    ##   X37 = col_logical(),
    ##   X38 = col_logical(),
    ##   X39 = col_logical(),
    ##   X40 = col_logical(),
    ##   X41 = col_logical(),
    ##   X42 = col_logical(),
    ##   X43 = col_logical(),
    ##   X44 = col_logical(),
    ##   X45 = col_logical(),
    ##   X46 = col_logical(),
    ##   X47 = col_logical(),
    ##   X48 = col_logical()
    ##   # ... with 10 more columns
    ## )
    ## ℹ Use `spec()` for the full column specifications.

``` r
#merge with sst data by joining with id numbers
joined_data <- left_join(new_data,school_data,by = c("student_id" = "Student ID"))
```

``` r
#create a column that has race and sex together "demographic_info"
joined_data <- joined_data %>% unite(Demographic,Eth,Gender, sep = " ", remove = FALSE)
```

``` r
#create similar bar chart as above but with demographic info (race & gender)
Demo_Graph <- joined_data %>%
  ggplot(aes(x=Demographic, fill = factor(Status, levels=c("GPA<1 & Att<80%","GPA<1 & Att>80%","1<=GPA<=2 & Att<80%", "1<=GPA<=2 & Att>80%","GPA>2 & Att<80%", "GPA>2 & Att>80%"))))+
  geom_bar(position = 'dodge')+
  labs(fill = "Status")+
  scale_x_discrete(labels = wrap_format(5)) +
  labs(y= "Number of Students")

Demo_Graph
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-27-1.png)<!-- --> Now,
we’ll look at number of F’s by program and demographic.

``` r
# try to import F data
f_data <- read_csv("F_Data_1_6.csv")
```

    ## Warning: Missing column names filled in: 'X2' [2]

    ## 
    ## ── Column specification ───────────────────────────────────────────────────────────────────────────────
    ## cols(
    ##   Number_F_1_6 = col_double(),
    ##   X2 = col_character(),
    ##   Student = col_character(),
    ##   `Attendance Rate` = col_character()
    ## )

``` r
#join with joined data
double_joined_data <- left_join(joined_data,f_data,by = c("name" = "Student"))

#make NA to 0 in f's column
double_joined_data$Number_F_1_6[is.na(double_joined_data$Number_F_1_6)] <- 0
```

``` r
#remove duplicates
double_joined_data <- distinct(double_joined_data,student_id,.keep_all = TRUE)
#make f's into char
double_joined_data <-transform(double_joined_data,Number_F_1_6 = as.character(double_joined_data$Number_F_1_6))
#make similar graphs but with F's instead of GPAs
program_f_bar <- double_joined_data %>%
  ggplot(aes( x = program, fill = Number_F_1_6))+
  geom_bar(position = 'dodge')+
  scale_x_discrete(labels = wrap_format(5))+
  labs(fill = "Number of F's", x= "Program", y= "Number of Students")

program_f_bar
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
demo_f_bar <- double_joined_data %>%
  ggplot(aes(x=Demographic, fill = Number_F_1_6)) +
  geom_bar(position = 'dodge') +
  scale_x_discrete(labels = wrap_format(5))+
  labs(y= "Number of Students", fill = "Number of F's")

demo_f_bar
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
#make a DL/EL status colum
double_joined_data <- double_joined_data %>%
  unite(DLEL_Status, DL.Status,EL.Status,sep = " ", remove = FALSE)
```

``` r
DLEL_Status_bar <- double_joined_data %>%
  ggplot(aes(DLEL_Status,fill = factor(Status, levels=c("GPA<1 & Att<80%","GPA<1 & Att>80%","1<=GPA<=2 & Att<80%", "1<=GPA<=2 & Att>80%","GPA>2 & Att<80%", "GPA>2 & Att>80%"))))+
  geom_bar(position = 'dodge') +
  labs(fill = "Status", x = "Learner Profile", y = "Number of Students") +
  scale_x_discrete(labels = c("DL EL" = "DL & EL", "DL Not EL" = "DL", "Not Applicable EL" = "EL", "Not Applicable Not EL" = "Not DL or EL"))

DLEL_Status_bar
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-35-1.png)<!-- --> Now,
we’ll look at F’s by students’ Diverse Learner or English Language
Learner Status.

``` r
DLEL_f_bar <- double_joined_data %>%
  ggplot(aes(x = DLEL_Status, fill = Number_F_1_6)) +
  geom_bar(position = 'dodge') +
  labs(fill = "Number of F's", x = "Learner Profile", y = "Number of Students")+
  scale_x_discrete(labels = c("DL EL" = "DL & EL", "DL Not EL" = "DL", "Not Applicable EL" = "EL", "Not Applicable Not EL" = "Not DL or EL"))

DLEL_f_bar
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

Now, we’ll look at F count by subject and course level.

``` r
#import combined f data
combined_f <- read_csv("combined_F.csv")
```

    ## 
    ## ── Column specification ───────────────────────────────────────────────────────────────────────────────
    ## cols(
    ##   Teacher = col_character(),
    ##   Course = col_character(),
    ##   F_Students = col_character(),
    ##   Course_GPA = col_double(),
    ##   Course_N_of_Students = col_double(),
    ##   Num_Students_F = col_double(),
    ##   Perc_of_Students_F = col_double(),
    ##   Core_Subject = col_character(),
    ##   Core = col_character(),
    ##   `Course Level` = col_character(),
    ##   Dept = col_character()
    ## )

``` r
#create dept bar chart
combined_f_grouped <- combined_f %>%
  group_by(Dept,`Course Level`) %>%
  summarize(F_total = sum(Num_Students_F))
```

    ## `summarise()` has grouped output by 'Dept'. You can override using the `.groups` argument.

``` r
dept_F_bar <- combined_f_grouped %>%
  ggplot(aes(x=Dept, y=F_total, fill=`Course Level`))+
  geom_col(position = 'dodge')+
  scale_x_discrete(labels = wrap_format(5))+
  labs(y="Number of Students With F's",x="Department",title = "1/6/21 12th Grade F Count")

dept_F_bar
```

![](SST_Data_Exp_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->
