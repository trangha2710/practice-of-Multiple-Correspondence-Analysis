MCA report
================

#### load libraries

#### load data

``` r
data <- read_csv("data.csv")
```

    ## Rows: 374 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Gender, Occupation, BMI Category, Blood Pressure, Sleep Disorder
    ## dbl (8): Person ID, Age, Sleep Duration, Quality of Sleep, Physical Activity...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
summary(data)
```

    ##    Person ID         Gender               Age         Occupation       
    ##  Min.   :  1.00   Length:374         Min.   :27.00   Length:374        
    ##  1st Qu.: 94.25   Class :character   1st Qu.:35.25   Class :character  
    ##  Median :187.50   Mode  :character   Median :43.00   Mode  :character  
    ##  Mean   :187.50                      Mean   :42.18                     
    ##  3rd Qu.:280.75                      3rd Qu.:50.00                     
    ##  Max.   :374.00                      Max.   :59.00                     
    ##  Sleep Duration  Quality of Sleep Physical Activity Level  Stress Level  
    ##  Min.   :5.800   Min.   :4.000    Min.   :30.00           Min.   :3.000  
    ##  1st Qu.:6.400   1st Qu.:6.000    1st Qu.:45.00           1st Qu.:4.000  
    ##  Median :7.200   Median :7.000    Median :60.00           Median :5.000  
    ##  Mean   :7.132   Mean   :7.313    Mean   :59.17           Mean   :5.385  
    ##  3rd Qu.:7.800   3rd Qu.:8.000    3rd Qu.:75.00           3rd Qu.:7.000  
    ##  Max.   :8.500   Max.   :9.000    Max.   :90.00           Max.   :8.000  
    ##  BMI Category       Blood Pressure       Heart Rate     Daily Steps   
    ##  Length:374         Length:374         Min.   :65.00   Min.   : 3000  
    ##  Class :character   Class :character   1st Qu.:68.00   1st Qu.: 5600  
    ##  Mode  :character   Mode  :character   Median :70.00   Median : 7000  
    ##                                        Mean   :70.17   Mean   : 6817  
    ##                                        3rd Qu.:72.00   3rd Qu.: 8000  
    ##                                        Max.   :86.00   Max.   :10000  
    ##  Sleep Disorder    
    ##  Length:374        
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ## 

``` r
str(data)
```

    ## spc_tbl_ [374 × 13] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Person ID              : num [1:374] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Gender                 : chr [1:374] "Male" "Male" "Male" "Male" ...
    ##  $ Age                    : num [1:374] 27 28 28 28 28 28 29 29 29 29 ...
    ##  $ Occupation             : chr [1:374] "Software Engineer" "Doctor" "Doctor" "Sales Representative" ...
    ##  $ Sleep Duration         : num [1:374] 6.1 6.2 6.2 5.9 5.9 5.9 6.3 7.8 7.8 7.8 ...
    ##  $ Quality of Sleep       : num [1:374] 6 6 6 4 4 4 6 7 7 7 ...
    ##  $ Physical Activity Level: num [1:374] 42 60 60 30 30 30 40 75 75 75 ...
    ##  $ Stress Level           : num [1:374] 6 8 8 8 8 8 7 6 6 6 ...
    ##  $ BMI Category           : chr [1:374] "Overweight" "Normal" "Normal" "Obese" ...
    ##  $ Blood Pressure         : chr [1:374] "126/83" "125/80" "125/80" "140/90" ...
    ##  $ Heart Rate             : num [1:374] 77 75 75 85 85 85 82 70 70 70 ...
    ##  $ Daily Steps            : num [1:374] 4200 10000 10000 3000 3000 3000 3500 8000 8000 8000 ...
    ##  $ Sleep Disorder         : chr [1:374] "None" "None" "None" "Sleep Apnea" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   `Person ID` = col_double(),
    ##   ..   Gender = col_character(),
    ##   ..   Age = col_double(),
    ##   ..   Occupation = col_character(),
    ##   ..   `Sleep Duration` = col_double(),
    ##   ..   `Quality of Sleep` = col_double(),
    ##   ..   `Physical Activity Level` = col_double(),
    ##   ..   `Stress Level` = col_double(),
    ##   ..   `BMI Category` = col_character(),
    ##   ..   `Blood Pressure` = col_character(),
    ##   ..   `Heart Rate` = col_double(),
    ##   ..   `Daily Steps` = col_double(),
    ##   ..   `Sleep Disorder` = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
head(data)
```

    ## # A tibble: 6 × 13
    ##   Person.ID Gender   Age Occupation           Sleep.Duration Quality.of.Sleep
    ##       <dbl> <chr>  <dbl> <chr>                         <dbl>            <dbl>
    ## 1         1 Male      27 Software Engineer               6.1                6
    ## 2         2 Male      28 Doctor                          6.2                6
    ## 3         3 Male      28 Doctor                          6.2                6
    ## 4         4 Male      28 Sales Representative            5.9                4
    ## 5         5 Male      28 Sales Representative            5.9                4
    ## 6         6 Male      28 Software Engineer               5.9                4
    ## # ℹ 7 more variables: Physical.Activity.Level <dbl>, Stress.Level <dbl>,
    ## #   BMI.Category <chr>, Blood.Pressure <chr>, Heart.Rate <dbl>,
    ## #   Daily.Steps <dbl>, Sleep.Disorder <chr>

#### data manipulation

``` r
data$BMI.Category <- gsub("Normal Weight", "Normal", data$BMI.Category)
data$BMI.Category <- factor(data$BMI.Category, levels = c("Normal","Overweight","Obese"))

data$sleep.hours <- as.factor(ifelse(data$Sleep.Duration < 7, '< 7 hours',
                                     ifelse(data$Sleep.Duration > 7, '7 - 9 hours', '7 hours')))
data <- data %>%
  mutate(Stress.Level = as.character(Stress.Level)) %>%
  mutate(Quality.of.Sleep = as.character(Quality.of.Sleep))
data$Stress.Level <- factor(data$Stress.Level,levels = c("1","2","3","4","5","6","7","8","9","10"))
data$Quality.of.Sleep <- factor(data$Quality.of.Sleep,levels = c("1","2","3","4","5","6","7","8","9","10"))

data$agecat <- as.factor(ifelse(data$Age < 31, '20 - 30',
                        ifelse(data$Age < 41, '30 - 40', 
                        ifelse(data$Age < 51, '40 - 50', '50 - 60'))))

data$bloodpressure <- as.factor(ifelse(data$Blood.Pressure %in% c("117/76","118/76","115/75","115/78","119/77","118/75"), 'normal',
                                ifelse(data$Blood.Pressure %in% c("121/79"), 'elevated', 
                                ifelse(data$Blood.Pressure %in% c("120/80","125/80","122/80","126/83","132/87","130/86","128/85","131/86","128/84","135/88","129/84","130/85","125/82"), 'high bp stage 1', 'high bp stage 2'))))
data$bloodpressure <- factor(data$bloodpressure, levels = c("normal", "elevated", "high bp stage 1", "high bp stage 2"))

data$dailysteps <- as.factor(ifelse(data$Daily.Steps < 5000, 'sedentary',
                            ifelse(data$Daily.Steps > 4999 & data$Daily.Steps < 7500, 'low active', 
                            ifelse(data$Daily.Steps > 7499 & data$Daily.Steps < 10000, 'somewhat active', 'active'))))
data$dailysteps <- factor(data$dailysteps, levels = c("sedentary","low active","somewhat active","active"))

data$heart.rate <- case_when(
  data$Gender == "Male" & data$Heart.Rate > 70 & data$Heart.Rate < 76 ~ "average",
  data$Gender == "Male" & data$Heart.Rate > 65 & data$Heart.Rate < 72 ~ "above average",
  data$Gender == "Male" & data$Heart.Rate > 74 & data$Heart.Rate < 81 ~ "below average",
  data$Gender == "Male" & data$Heart.Rate > 54 & data$Heart.Rate < 62 ~ "excellent",
  data$Gender == "Male" & data$Heart.Rate > 61 & data$Heart.Rate < 67 ~ "good",
  data$Gender == "Male" & data$Heart.Rate > 81 ~ "poor",
  data$Gender == "Female" & data$Heart.Rate > 72 & data$Heart.Rate < 77 ~ "average",
  data$Gender == "Female" & data$Heart.Rate > 68 & data$Heart.Rate < 74 ~ "above average",
  data$Gender == "Female" & data$Heart.Rate > 76 & data$Heart.Rate < 84 ~ "below average",
  data$Gender == "Female" & data$Heart.Rate > 59 & data$Heart.Rate < 65 ~ "excellent",
  data$Gender == "Female" & data$Heart.Rate > 64 & data$Heart.Rate < 69 ~ "good",
  data$Gender == "Female" & data$Heart.Rate > 83 ~ "poor")
data$heart.rate <- factor(data$heart.rate, levels = c("poor", "below average", "average", "above average","good","excellent"))

data$Sleep.Disorder <- factor(data$Sleep.Disorder, levels = c("None", "Insomnia", "Sleep Apnea"))
str(data)
```

    ## tibble [374 × 18] (S3: tbl_df/tbl/data.frame)
    ##  $ Person.ID              : num [1:374] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Gender                 : chr [1:374] "Male" "Male" "Male" "Male" ...
    ##  $ Age                    : num [1:374] 27 28 28 28 28 28 29 29 29 29 ...
    ##  $ Occupation             : chr [1:374] "Software Engineer" "Doctor" "Doctor" "Sales Representative" ...
    ##  $ Sleep.Duration         : num [1:374] 6.1 6.2 6.2 5.9 5.9 5.9 6.3 7.8 7.8 7.8 ...
    ##  $ Quality.of.Sleep       : Factor w/ 10 levels "1","2","3","4",..: 6 6 6 4 4 4 6 7 7 7 ...
    ##  $ Physical.Activity.Level: num [1:374] 42 60 60 30 30 30 40 75 75 75 ...
    ##  $ Stress.Level           : Factor w/ 10 levels "1","2","3","4",..: 6 8 8 8 8 8 7 6 6 6 ...
    ##  $ BMI.Category           : Factor w/ 3 levels "Normal","Overweight",..: 2 1 1 3 3 3 3 1 1 1 ...
    ##  $ Blood.Pressure         : chr [1:374] "126/83" "125/80" "125/80" "140/90" ...
    ##  $ Heart.Rate             : num [1:374] 77 75 75 85 85 85 82 70 70 70 ...
    ##  $ Daily.Steps            : num [1:374] 4200 10000 10000 3000 3000 3000 3500 8000 8000 8000 ...
    ##  $ Sleep.Disorder         : Factor w/ 3 levels "None","Insomnia",..: 1 1 1 3 3 2 2 1 1 1 ...
    ##  $ sleep.hours            : Factor w/ 2 levels "< 7 hours","7 - 9 hours": 1 1 1 1 1 1 1 2 2 2 ...
    ##  $ agecat                 : Factor w/ 4 levels "20 - 30","30 - 40",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ bloodpressure          : Factor w/ 4 levels "normal","elevated",..: 3 3 3 4 4 4 4 3 3 3 ...
    ##  $ dailysteps             : Factor w/ 4 levels "sedentary","low active",..: 1 4 4 1 1 1 1 3 3 3 ...
    ##  $ heart.rate             : Factor w/ 6 levels "poor","below average",..: 2 3 3 1 1 1 1 4 4 4 ...

#### subset

``` r
dataMCA <- subset(data, select = c(2,4,6,8,9,13:18))
head(dataMCA)
```

    ## # A tibble: 6 × 11
    ##   Gender Occupation    Quality.of.Sleep Stress.Level BMI.Category Sleep.Disorder
    ##   <chr>  <chr>         <fct>            <fct>        <fct>        <fct>         
    ## 1 Male   Software Eng… 6                6            Overweight   None          
    ## 2 Male   Doctor        6                8            Normal       None          
    ## 3 Male   Doctor        6                8            Normal       None          
    ## 4 Male   Sales Repres… 4                8            Obese        Sleep Apnea   
    ## 5 Male   Sales Repres… 4                8            Obese        Sleep Apnea   
    ## 6 Male   Software Eng… 4                8            Obese        Insomnia      
    ## # ℹ 5 more variables: sleep.hours <fct>, agecat <fct>, bloodpressure <fct>,
    ## #   dailysteps <fct>, heart.rate <fct>

#### mca

``` r
res.mca <- MCA(dataMCA, quali.sup = c(1,2,8), graph = FALSE)
summary(res.mca)
```

    ## 
    ## Call:
    ## MCA(X = dataMCA, quali.sup = c(1, 2, 8), graph = FALSE) 
    ## 
    ## 
    ## Eigenvalues
    ##                        Dim.1   Dim.2   Dim.3   Dim.4   Dim.5   Dim.6   Dim.7
    ## Variance               0.623   0.422   0.381   0.304   0.245   0.229   0.177
    ## % of var.             19.948  13.512  12.188   9.716   7.830   7.325   5.650
    ## Cumulative % of var.  19.948  33.460  45.648  55.365  63.195  70.519  76.170
    ##                        Dim.8   Dim.9  Dim.10  Dim.11  Dim.12  Dim.13  Dim.14
    ## Variance               0.170   0.128   0.115   0.082   0.052   0.044   0.035
    ## % of var.              5.430   4.109   3.688   2.619   1.673   1.411   1.110
    ## Cumulative % of var.  81.599  85.708  89.396  92.016  93.689  95.100  96.210
    ##                       Dim.15  Dim.16  Dim.17  Dim.18  Dim.19  Dim.20  Dim.21
    ## Variance               0.027   0.024   0.022   0.013   0.009   0.008   0.006
    ## % of var.              0.865   0.771   0.712   0.427   0.289   0.248   0.190
    ## Cumulative % of var.  97.075  97.846  98.558  98.986  99.275  99.522  99.712
    ##                       Dim.22  Dim.23  Dim.24  Dim.25
    ## Variance               0.004   0.003   0.002   0.000
    ## % of var.              0.143   0.088   0.056   0.000
    ## Cumulative % of var.  99.856  99.944 100.000 100.000
    ## 
    ## Individuals (the 10 first)
    ##                         Dim.1    ctr   cos2    Dim.2    ctr   cos2    Dim.3
    ## 1                    |  0.557  0.133  0.039 |  0.769  0.375  0.075 |  0.782
    ## 2                    |  0.675  0.195  0.163 |  0.610  0.235  0.133 | -0.592
    ## 3                    |  0.675  0.195  0.163 |  0.610  0.235  0.133 | -0.592
    ## 4                    |  1.384  0.822  0.086 |  0.371  0.087  0.006 |  3.697
    ## 5                    |  1.384  0.822  0.086 |  0.371  0.087  0.006 |  3.697
    ## 6                    |  1.358  0.791  0.083 |  0.511  0.166  0.012 |  3.637
    ## 7                    |  1.273  0.695  0.119 |  0.555  0.195  0.023 |  2.724
    ## 8                    | -0.833  0.298  0.299 |  0.553  0.194  0.132 |  0.031
    ## 9                    | -0.833  0.298  0.299 |  0.553  0.194  0.132 |  0.031
    ## 10                   | -0.833  0.298  0.299 |  0.553  0.194  0.132 |  0.031
    ##                         ctr   cos2  
    ## 1                     0.430  0.078 |
    ## 2                     0.246  0.125 |
    ## 3                     0.246  0.125 |
    ## 4                     9.593  0.616 |
    ## 5                     9.593  0.616 |
    ## 6                     9.286  0.596 |
    ## 7                     5.210  0.547 |
    ## 8                     0.001  0.000 |
    ## 9                     0.001  0.000 |
    ## 10                    0.001  0.000 |
    ## 
    ## Categories (the 10 first)
    ##                          Dim.1     ctr    cos2  v.test     Dim.2     ctr
    ## Quality.of.Sleep_4   |   1.571   0.662   0.033   3.532 |   0.619   0.152
    ## Quality.of.Sleep_5   |   1.206   0.546   0.028   3.216 |   1.412   1.104
    ## Quality.of.Sleep_6   |   1.092   6.708   0.465  13.171 |   0.630   3.303
    ## Quality.of.Sleep_7   |  -0.152   0.096   0.006  -1.498 |   0.230   0.322
    ## Quality.of.Sleep_8   |  -1.067   6.648   0.468 -13.211 |   0.141   0.171
    ## Quality.of.Sleep_9   |  -0.041   0.006   0.000  -0.385 |  -1.580  14.036
    ## Stress.Level_3       |  -0.078   0.023   0.001  -0.731 |  -1.544  13.392
    ## Stress.Level_4       |  -0.250   0.234   0.014  -2.312 |  -0.837   3.881
    ## Stress.Level_5       |  -1.128   4.570   0.278 -10.176 |   0.835   3.695
    ## Stress.Level_6       |  -0.736   1.335   0.076  -5.321 |   0.697   1.771
    ##                         cos2  v.test     Dim.3     ctr    cos2  v.test  
    ## Quality.of.Sleep_4     0.005   1.392 |   4.223   7.825   0.242   9.494 |
    ## Quality.of.Sleep_5     0.038   3.766 |   2.052   2.587   0.080   5.474 |
    ## Quality.of.Sleep_6     0.155   7.607 |  -0.614   3.478   0.147  -7.413 |
    ## Quality.of.Sleep_7     0.014   2.260 |   0.204   0.281   0.011   2.006 |
    ## Quality.of.Sleep_8     0.008   1.742 |  -0.061   0.036   0.002  -0.758 |
    ## Quality.of.Sleep_9     0.585 -14.775 |   0.282   0.494   0.019   2.633 |
    ## Stress.Level_3         0.558 -14.432 |   0.280   0.487   0.018   2.614 |
    ## Stress.Level_4         0.161  -7.756 |  -0.204   0.255   0.010  -1.888 |
    ## Stress.Level_5         0.152   7.532 |   0.373   0.820   0.030   3.369 |
    ## Stress.Level_6         0.068   5.045 |   0.005   0.000   0.000   0.035 |
    ## 
    ## Categorical variables (eta2)
    ##                        Dim.1 Dim.2 Dim.3  
    ## Quality.of.Sleep     | 0.731 0.645 0.448 |
    ## Stress.Level         | 0.698 0.915 0.077 |
    ## BMI.Category         | 0.628 0.030 0.753 |
    ## Sleep.Disorder       | 0.551 0.066 0.038 |
    ## sleep.hours          | 0.677 0.130 0.035 |
    ## bloodpressure        | 0.343 0.360 0.022 |
    ## dailysteps           | 0.660 0.369 0.754 |
    ## heart.rate           | 0.699 0.863 0.920 |
    ## 
    ## Supplementary categories (the 10 first)
    ##                          Dim.1    cos2  v.test     Dim.2    cos2  v.test  
    ## Female               |   0.284   0.079   5.431 |  -0.732   0.525 -13.991 |
    ## Male                 |  -0.278   0.079  -5.431 |   0.717   0.525  13.991 |
    ## Accountant           |  -0.583   0.037  -3.731 |  -0.693   0.053  -4.435 |
    ## Doctor               |  -0.210   0.010  -1.965 |   0.699   0.114   6.531 |
    ## Engineer             |  -0.665   0.090  -5.782 |  -0.465   0.044  -4.045 |
    ## Lawyer               |  -1.146   0.189  -8.388 |   0.826   0.098   6.047 |
    ## Manager              |   0.198   0.000   0.198 |   0.565   0.001   0.565 |
    ## Nurse                |   0.948   0.218   9.015 |  -0.597   0.086  -5.674 |
    ## Sales Representative |   1.753   0.017   2.482 |   0.572   0.002   0.809 |
    ## Salesperson          |   1.156   0.125   6.830 |   0.707   0.047   4.178 |
    ##                        Dim.3    cos2  v.test  
    ## Female                -0.068   0.004  -1.294 |
    ## Male                   0.066   0.004   1.294 |
    ## Accountant            -0.244   0.007  -1.563 |
    ## Doctor                -0.039   0.000  -0.363 |
    ## Engineer               0.055   0.001   0.475 |
    ## Lawyer                 0.272   0.011   1.990 |
    ## Manager               -0.311   0.000  -0.311 |
    ## Nurse                 -0.090   0.002  -0.855 |
    ## Sales Representative   5.990   0.193   8.482 |
    ## Salesperson           -0.659   0.041  -3.891 |
    ## 
    ## Supplementary categorical variables (eta2)
    ##                        Dim.1 Dim.2 Dim.3  
    ## Gender               | 0.079 0.525 0.004 |
    ## Occupation           | 0.621 0.418 0.285 |
    ## agecat               | 0.198 0.491 0.086 |

``` r
head(res.mca$eig, 10)
```

    ##        eigenvalue percentage of variance cumulative percentage of variance
    ## dim 1   0.6233726              19.947922                          19.94792
    ## dim 2   0.4222628              13.512409                          33.46033
    ## dim 3   0.3808776              12.188082                          45.64841
    ## dim 4   0.3036306               9.716178                          55.36459
    ## dim 5   0.2446913               7.830120                          63.19471
    ## dim 6   0.2288948               7.324633                          70.51934
    ## dim 7   0.1765763               5.650440                          76.16978
    ## dim 8   0.1696769               5.429662                          81.59945
    ## dim 9   0.1284029               4.108893                          85.70834
    ## dim 10  0.1152500               3.688000                          89.39634

``` r
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
```

![](MCA_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

#### dimdesc

``` r
res.desc <- dimdesc(res.mca, axes = c(1,2))
# Description of dimension 1
res.desc[[1]]
```

    ## 
    ## Link between the variable and the categorical variable (1-way anova)
    ## =============================================
    ##                         R2       p.value
    ## Quality.of.Sleep 0.7313756 1.092084e-102
    ## heart.rate       0.6989925  8.153482e-95
    ## Stress.Level     0.6982972  1.941657e-93
    ## sleep.hours      0.6771560  2.361179e-93
    ## dailysteps       0.6596168  3.245445e-86
    ## BMI.Category     0.6276335  2.600498e-80
    ## Occupation       0.6206108  2.884421e-70
    ## Sleep.Disorder   0.5511991  2.857371e-65
    ## bloodpressure    0.3427098  1.744512e-33
    ## agecat           0.1977578  1.368243e-17
    ## Gender           0.0790840  3.159717e-08
    ## 
    ## Link between variable and the categories of the categorical variables
    ## ================================================================
    ##                                        Estimate      p.value
    ## sleep.hours=< 7 hours                0.65943486 2.361179e-93
    ## BMI.Category=Overweight              0.38573264 8.908534e-67
    ## Quality.of.Sleep=Quality.of.Sleep_6  0.51863571 1.730777e-52
    ## heart.rate=average                   0.52236872 1.656668e-48
    ## bloodpressure=high bp stage 2        0.88148130 3.578849e-33
    ## Stress.Level=Stress.Level_8          0.88944384 7.935059e-30
    ## dailysteps=active                    0.89573926 7.075456e-26
    ## Sleep.Disorder=Sleep Apnea           0.46101363 1.999587e-24
    ## Occupation=Nurse                     0.54802299 1.236079e-21
    ## Stress.Level=Stress.Level_7          0.84395210 1.925434e-17
    ## Sleep.Disorder=Insomnia              0.32991538 3.027057e-16
    ## agecat=40 - 50                       0.41414086 1.479638e-14
    ## Occupation=Salesperson               0.71247303 1.852561e-12
    ## Gender=Female                        0.22204602 3.159717e-08
    ## dailysteps=sedentary                 0.51242286 1.266781e-06
    ## heart.rate=below average             0.52677330 1.175969e-04
    ## Quality.of.Sleep=Quality.of.Sleep_4  0.89722869 3.779768e-04
    ## heart.rate=poor                      0.49576906 7.480394e-04
    ## BMI.Category=Obese                   0.48795983 7.480394e-04
    ## Quality.of.Sleep=Quality.of.Sleep_5  0.60870647 1.229285e-03
    ## Occupation=Teacher                   0.17223299 1.531019e-03
    ## Occupation=Sales Representative      1.18369218 1.285727e-02
    ## Occupation=Doctor                   -0.36627440 4.928212e-02
    ## Stress.Level=Stress.Level_4         -0.19781476 2.055857e-02
    ## Occupation=Accountant               -0.66060423 1.708529e-04
    ## dailysteps=low active               -0.18701032 2.799658e-05
    ## bloodpressure=normal                -0.44883183 2.749718e-07
    ## Stress.Level=Stress.Level_6         -0.58169601 6.107827e-08
    ## Gender=Male                         -0.22204602 3.159717e-08
    ## Occupation=Engineer                 -0.72546205 3.507989e-09
    ## bloodpressure=high bp stage 1       -0.07227977 7.047335e-12
    ## agecat=30 - 40                      -0.41541864 1.447213e-15
    ## Occupation=Lawyer                   -1.10481788 1.225755e-18
    ## Stress.Level=Stress.Level_5         -0.89133248 4.176828e-28
    ## heart.rate=above average            -1.10991350 5.194995e-50
    ## Quality.of.Sleep=Quality.of.Sleep_8 -1.18531277 6.524358e-53
    ## dailysteps=somewhat active          -1.22115179 4.412422e-55
    ## Sleep.Disorder=None                 -0.79092901 3.470501e-66
    ## BMI.Category=Normal                 -0.87369248 1.019898e-81
    ## sleep.hours=7 - 9 hours             -0.65943486 2.361179e-93

``` r
# Description of dimension 2
res.desc[[2]]
```

    ## 
    ## Link between the variable and the categorical variable (1-way anova)
    ## =============================================
    ##                          R2       p.value
    ## Stress.Level     0.91506245 1.495723e-194
    ## heart.rate       0.86314988 6.950882e-158
    ## Quality.of.Sleep 0.64483476  1.876348e-80
    ## Gender           0.52481680  4.482166e-62
    ## agecat           0.49082418  6.369816e-54
    ## Occupation       0.41821254  3.038693e-37
    ## dailysteps       0.36910395  9.218727e-37
    ## bloodpressure    0.35993051  1.315556e-35
    ## sleep.hours      0.12973213  6.727162e-13
    ## Sleep.Disorder   0.06596588  3.178984e-06
    ## BMI.Category     0.03032279  3.306311e-03
    ## 
    ## Link between variable and the categories of the categorical variables
    ## ================================================================
    ##                                         Estimate       p.value
    ## Gender=Male                          0.470782286  4.482166e-62
    ## bloodpressure=high bp stage 1        0.435875177  7.541679e-37
    ## heart.rate=above average             0.296871591  1.222765e-26
    ## dailysteps=somewhat active           0.299679771  4.888648e-25
    ## Quality.of.Sleep=Quality.of.Sleep_6  0.252450430  2.491858e-15
    ## Stress.Level=Stress.Level_5          0.485691969  4.937514e-15
    ## sleep.hours=< 7 hours                0.237557575  6.727162e-13
    ## heart.rate=average                   0.181017021  1.576057e-12
    ## Occupation=Doctor                    0.297910747  1.853708e-11
    ## Occupation=Lawyer                    0.380571789  5.984814e-10
    ## Stress.Level=Stress.Level_7          0.459599463  6.769524e-10
    ## agecat=20 - 30                       0.606969080  3.700047e-09
    ## Stress.Level=Stress.Level_8          0.318708461  5.003609e-08
    ## Stress.Level=Stress.Level_6          0.396478738  2.997391e-07
    ## agecat=40 - 50                       0.173591793  6.657651e-06
    ## Occupation=Salesperson               0.303512990  2.445236e-05
    ## heart.rate=below average             0.508694398  1.230330e-04
    ## Quality.of.Sleep=Quality.of.Sleep_5  0.760208136  1.478388e-04
    ## dailysteps=sedentary                 0.305122873  2.275316e-04
    ## agecat=30 - 40                       0.110770153  2.078226e-03
    ## BMI.Category=Normal                  0.030140907  3.320669e-03
    ## Sleep.Disorder=Insomnia              0.190568341  1.765824e-02
    ## Sleep.Disorder=None                  0.091687680  4.159457e-02
    ## Quality.of.Sleep=Quality.of.Sleep_7 -0.007854915  2.359389e-02
    ## BMI.Category=Overweight             -0.191447374  8.913786e-04
    ## Occupation=Teacher                  -0.487436330  6.071754e-04
    ## Occupation=Engineer                 -0.458465374  4.456876e-05
    ## Occupation=Accountant               -0.606427831  7.244278e-06
    ## Sleep.Disorder=Sleep Apnea          -0.282256021  9.876672e-07
    ## Occupation=Nurse                    -0.543724154  6.999699e-09
    ## bloodpressure=normal                -0.483251791  3.581627e-11
    ## sleep.hours=7 - 9 hours             -0.237557575  6.727162e-13
    ## Stress.Level=Stress.Level_4         -0.600581402  6.307709e-16
    ## bloodpressure=high bp stage 2       -0.313529651  3.947233e-17
    ## dailysteps=low active               -0.554815121  2.284358e-36
    ## agecat=50 - 60                      -0.891331027  3.037939e-50
    ## Gender=Female                       -0.470782286  4.482166e-62
    ## Stress.Level=Stress.Level_3         -1.059897229  5.168908e-68
    ## Quality.of.Sleep=Quality.of.Sleep_9 -1.184208542  4.317918e-73
    ## heart.rate=good                     -1.006561416 2.499133e-155

#### categories variables

``` r
var <- get_mca_var(res.mca)
var
```

    ## Multiple Correspondence Analysis Results for variables
    ##  ===================================================
    ##   Name       Description                  
    ## 1 "$coord"   "Coordinates for categories" 
    ## 2 "$cos2"    "Cos2 for categories"        
    ## 3 "$contrib" "contributions of categories"

``` r
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, ggtheme = theme_minimal(),
             geom = c("arrow", "text"), arrow.size = 0.02, arrow.length = unit(0.1, "inches"))
```

![](MCA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#coord var
var$coord
```

    ##                          Dim 1       Dim 2       Dim 3       Dim 4        Dim 5
    ## Quality.of.Sleep_4  1.57110235  0.61931197  4.22301567 -1.05115498  1.585645424
    ## Quality.of.Sleep_5  1.20567132  1.41183177  2.05229902  2.92294952 -2.039368955
    ## Quality.of.Sleep_6  1.09159119  0.63044669 -0.61437465 -0.29165786  0.109197764
    ## Quality.of.Sleep_7 -0.15238232  0.22986446  0.20398395  0.59737240 -0.027085198
    ## Quality.of.Sleep_8 -1.06656374  0.14067774 -0.06122083 -0.02208717  0.687966251
    ## Quality.of.Sleep_9 -0.04117465 -1.58041860  0.28161229 -0.39677422 -1.098889729
    ## Stress.Level_3     -0.07818749 -1.54373704  0.27963131 -0.40209340 -1.111867703
    ## Stress.Level_4     -0.24950519 -0.83689886 -0.20375076  0.86704450  1.418997440
    ## Stress.Level_5     -1.12788778  0.83476027  0.37339857 -0.47660036  0.004014782
    ## Stress.Level_6     -0.73571427  0.69747060  0.00479661  0.11348921 -0.526092277
    ## Stress.Level_7      1.06995623  0.79460668 -0.06396399  1.52554618 -0.576324349
    ## Stress.Level_8      1.12757430  0.57779042 -0.39473456 -1.16728674  0.462289399
    ## Normal             -0.67734417  0.12954443 -0.08686405 -0.07114751  0.061133423
    ## Overweight          0.91779475 -0.21145635 -0.22585946  0.17107046 -0.195311772
    ## Obese               1.04727173  0.33139427  5.21898361 -0.99505659  1.570132295
    ## None               -0.62297382  0.08868717 -0.14395688 -0.10409321  0.073893104
    ## Insomnia            0.79664412  0.24085399  0.05499067  1.25520871  0.112518924
    ## Sleep Apnea         0.96268808 -0.48677240  0.34990096 -0.94685459 -0.318545472
    ## < 7 hours           0.97813899  0.42813426 -0.22117309  0.21513822  0.140761605
    ## 7 - 9 hours        -0.69229015 -0.30301740  0.15653803 -0.15226677 -0.099625794
    ## normal             -0.74666100 -0.95071459 -0.26125724  0.57170796  1.751219196
    ## elevated           -0.63461830  0.34835483 -0.49690547 -0.05550943  0.401865097
    ## high bp stage 1    -0.26973479  0.46372401 -0.04655487  0.11480648 -0.356101414
    ## high bp stage 2     0.93826191 -0.68953028  0.22009182 -0.50019621  0.104136760
    ## sedentary           1.06958841  0.81919841  3.63637828  0.67521456 -0.045470038
    ## low active          0.18371353 -0.50415466 -0.21850545  0.40536903  0.063905267
    ## somewhat active    -1.12608987  0.81082205  0.04817402 -0.37440464 -0.282600679
    ## active              1.55508207  0.27272129 -0.74599818 -1.71736411  0.449118415
    ## poor                1.04727173  0.33139427  5.21898361 -0.99505659  1.570132295
    ## below average       1.08654049  1.08347619  1.47323594  1.92250436 -1.441926931
    ## average             1.08096182  0.57921557 -0.66796318 -0.24858902  0.123617962
    ## above average      -0.98642340  0.75750342  0.01917998 -0.21209423 -0.231808189
    ## good               -0.13160112 -1.24834128 -0.03030046  0.28230086  0.123834351

``` r
fviz_mca_var(res.mca, col.var="black", shape.var = 15,
             repel = TRUE,ggtheme = theme_minimal(),linecolor = "black")
```

![](MCA_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
head(round(var$coord, 2),15)
```

    ##                    Dim 1 Dim 2 Dim 3 Dim 4 Dim 5
    ## Quality.of.Sleep_4  1.57  0.62  4.22 -1.05  1.59
    ## Quality.of.Sleep_5  1.21  1.41  2.05  2.92 -2.04
    ## Quality.of.Sleep_6  1.09  0.63 -0.61 -0.29  0.11
    ## Quality.of.Sleep_7 -0.15  0.23  0.20  0.60 -0.03
    ## Quality.of.Sleep_8 -1.07  0.14 -0.06 -0.02  0.69
    ## Quality.of.Sleep_9 -0.04 -1.58  0.28 -0.40 -1.10
    ## Stress.Level_3     -0.08 -1.54  0.28 -0.40 -1.11
    ## Stress.Level_4     -0.25 -0.84 -0.20  0.87  1.42
    ## Stress.Level_5     -1.13  0.83  0.37 -0.48  0.00
    ## Stress.Level_6     -0.74  0.70  0.00  0.11 -0.53
    ## Stress.Level_7      1.07  0.79 -0.06  1.53 -0.58
    ## Stress.Level_8      1.13  0.58 -0.39 -1.17  0.46
    ## Normal             -0.68  0.13 -0.09 -0.07  0.06
    ## Overweight          0.92 -0.21 -0.23  0.17 -0.20
    ## Obese               1.05  0.33  5.22 -1.00  1.57

#### cos2

``` r
head(round(var$cos2, 2),15)
```

    ##                    Dim 1 Dim 2 Dim 3 Dim 4 Dim 5
    ## Quality.of.Sleep_4  0.03  0.01  0.24  0.01  0.03
    ## Quality.of.Sleep_5  0.03  0.04  0.08  0.16  0.08
    ## Quality.of.Sleep_6  0.47  0.16  0.15  0.03  0.00
    ## Quality.of.Sleep_7  0.01  0.01  0.01  0.09  0.00
    ## Quality.of.Sleep_8  0.47  0.01  0.00  0.00  0.19
    ## Quality.of.Sleep_9  0.00  0.59  0.02  0.04  0.28
    ## Stress.Level_3      0.00  0.56  0.02  0.04  0.29
    ## Stress.Level_4      0.01  0.16  0.01  0.17  0.46
    ## Stress.Level_5      0.28  0.15  0.03  0.05  0.00
    ## Stress.Level_6      0.08  0.07  0.00  0.00  0.04
    ## Stress.Level_7      0.18  0.10  0.00  0.36  0.05
    ## Stress.Level_8      0.29  0.08  0.04  0.31  0.05
    ## Normal              0.63  0.02  0.01  0.01  0.01
    ## Overweight          0.55  0.03  0.03  0.02  0.02
    ## Obese               0.03  0.00  0.75  0.03  0.07

``` r
fviz_mca_var(res.mca, col.var = "cos2",  alpha.var="cos2",invisible=c("quali.sup","ind"),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())
```

![](MCA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
library("corrplot")
```

    ## corrplot 0.92 loaded

``` r
corrplot(var$cos2, is.corr=FALSE,method = "number",
         tl.col = "brown",tl.cex = 0.8, tl.srt=45, number.cex = 0.5)
```

![](MCA_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

#### contrib

``` r
head(round(var$contrib, 2),15)
```

    ##                    Dim 1 Dim 2 Dim 3 Dim 4 Dim 5
    ## Quality.of.Sleep_4  0.66  0.15  7.82  0.61  1.72
    ## Quality.of.Sleep_5  0.55  1.10  2.59  6.58  3.98
    ## Quality.of.Sleep_6  6.71  3.30  3.48  0.98  0.17
    ## Quality.of.Sleep_7  0.10  0.32  0.28  3.02  0.01
    ## Quality.of.Sleep_8  6.65  0.17  0.04  0.01  7.05
    ## Quality.of.Sleep_9  0.01 14.04  0.49  1.23 11.71
    ## Stress.Level_3      0.02 13.39  0.49  1.26 11.99
    ## Stress.Level_4      0.23  3.88  0.26  5.79 19.25
    ## Stress.Level_5      4.57  3.70  0.82  1.68  0.00
    ## Stress.Level_6      1.33  1.77  0.00  0.07  1.74
    ## Stress.Level_7      3.07  2.50  0.02 12.81  2.27
    ## Stress.Level_8      4.77  1.85  0.96 10.50  2.04
    ## Normal              5.31  0.29  0.14  0.12  0.11
    ## Overweight          6.68  0.52  0.66  0.48  0.77
    ## Obese               0.59  0.09 23.90  1.09  3.37

``` r
fviz_mca_var(res.mca, col.var = "contrib", invisible=c("quali.sup","ind"),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # avoid text overlapping (slow)
             ggtheme = theme_minimal())
```

![](MCA_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->
