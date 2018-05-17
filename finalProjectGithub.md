Final Project
================
Nina Cohen
5/17/2018

1.  Introduction

I chose to work with the Global Terrorism Database because terrorism and terrorist attacks are a very relevant threat and topic in the news and socially. Growing up with my father working for the Department of Homeland Security and counterterrorism, I have always been fascinated with the the topic and learning more. Terrorism is such a major problem globally right now and using data science to possibly predict or support new policies that help protect against future attacks or situations.

In this script I will cover: 1) Data Curation 2) Parsing Data 3) Management of Data 4) Exploratory Data Analysis (EDA) 5) Hypothesis Testing and Machine Learning

1.1 Load Data

To load the data set or data of interest, first download the csv file from a website, such as <https://www.kaggle.com/datasets> and save it to the same folder as the file you will be using it in. Then you want to read the csv file and save it to a variable so you can later access and manipulate the data.

``` r
library(tidyverse)
knitr::opts_chunk$set(echo = TRUE)
db <- read_csv("globalterrorism.csv")
db[1:10,1:7]
```

    ## # A tibble: 10 x 7
    ##         eventid iyear imonth  iday approxdate extended resolution
    ##           <dbl> <int>  <int> <int> <chr>         <int> <chr>     
    ##  1 197000000001  1970      7     2 <NA>              0 <NA>      
    ##  2 197000000002  1970      0     0 <NA>              0 <NA>      
    ##  3 197001000001  1970      1     0 <NA>              0 <NA>      
    ##  4 197001000002  1970      1     0 <NA>              0 <NA>      
    ##  5 197001000003  1970      1     0 <NA>              0 <NA>      
    ##  6 197001010002  1970      1     1 <NA>              0 <NA>      
    ##  7 197001020001  1970      1     2 <NA>              0 <NA>      
    ##  8 197001020002  1970      1     2 <NA>              0 <NA>      
    ##  9 197001020003  1970      1     2 <NA>              0 <NA>      
    ## 10 197001030001  1970      1     3 <NA>              0 <NA>

1.  Data Curation

In this code, there is a lot of repeated information, or unneccesary information for what I plan to do with the information. For example, the eventid number is just a combination of the year, month, day, extended value and some count value of somesort. For future information, having a distinct eventid or reference id for each even will not be useful.

The approxdate gives a range of dates the attack took place over, resolution I assume provides a date where a resolution to the attack was created, though in the description of the data set there is no index that describes what each column represents. In all, the main columns that will be important to us will be the year, month, day, country/country id number, provstate, city, type of attack (we will only be focusing on the primary attack of each individual attack, thus will only get the information from attacktype1 and such) and target type.

Below is the code where I select only these column from the database. I also only select rows where the year is after 2001 so I can use more recent data to calcuate future predictions or information.

``` r
tidy_db <- db %>% 
  filter(iyear >= 2001) %>%
  dplyr::select(iyear,imonth,iday,country,country_txt,region,region_txt,provstate,city,attacktype1,
                attacktype1_txt,targtype1,targtype1_txt) 

tidy_db %>% head(10)
```

    ## # A tibble: 10 x 13
    ##    iyear imonth  iday country country_txt  region region_txt   provstate  
    ##    <int>  <int> <int>   <int> <chr>         <int> <chr>        <chr>      
    ##  1  2001      1     0      45 Colombia          3 South Ameri… Valle del …
    ##  2  2001      1     1     228 Yemen            10 Middle East… Adan       
    ##  3  2001      1     1      45 Colombia          3 South Ameri… Bogota     
    ##  4  2001      1     1     209 Turkey           10 Middle East… Istanbul   
    ##  5  2001      1     1       6 Algeria          10 Middle East… Djelfa     
    ##  6  2001      1     2     228 Yemen            10 Middle East… Adan       
    ##  7  2001      1     2     235 Yugoslavia        9 Eastern Eur… Vranje (Mu…
    ##  8  2001      1     2     217 United Stat…      1 North Ameri… Oregon     
    ##  9  2001      1     3     209 Turkey           10 Middle East… Istanbul   
    ## 10  2001      1     3     199 Switzerland       8 Western Eur… Zurich     
    ## # ... with 5 more variables: city <chr>, attacktype1 <int>,
    ## #   attacktype1_txt <chr>, targtype1 <int>, targtype1_txt <chr>

1.  Parsing Data

Here, I am selecting and graphin the number of attacks in each city per year for my ten cities of choice: DC, NYC, London, Jerusalem, Paris, Baghdad, New Delhi, Grozny, Gaza and Barrancabermeja. I chose these cities becasue the majority of them are major cities in countries where terrorism is a major threat, or as I was scrolling through the database, I constantly saw the name reappear. Here I will get a database that contains the number of attacks for all of the cities in the database, numAttacks and numAttacksPerCity, and for just my specified cities, numAttacksMainCities and numAttacksPerCity2, so I can analyze the data differently later on. Column x represents the number of attacks for that year in that specific city.

``` r
numAttacks <- tidy_db %>%
  group_by(iyear, country_txt, city) %>%
  dplyr::select(iyear,country_txt,city,attacktype1) %>%
  dplyr::count(attacktype1)

numAttacksMainCities <- numAttacks %>%
  filter(city == "District of Columbia" || city == "New York City" || city == "London"||city == "Jerusalem"
         || city == "Paris"||city == "Baghdad"||city == "New Delhi"||city == "Grozny"
         ||city == "Gaza"||city == "Barrancabermeja") 


numAttacksPerCity <- aggregate(numAttacks$n, by=list(city=numAttacks$city,iyear=numAttacks$iyear), FUN=sum) 
numAttacksPerCityMain <- aggregate(numAttacksMainCities$n,
                      by=list(city=numAttacksMainCities$city,iyear=numAttacksMainCities$iyear), FUN=sum) 

numAttacksPerCity %>% head(10)
```

    ##                  city iyear x
    ## 1            Aadaisse  2001 1
    ## 2                Abia  2001 1
    ## 3             Abidjan  2001 1
    ## 4               Aboud  2001 1
    ## 5    Acacias district  2001 1
    ## 6                Aceh  2001 1
    ## 7  Aceh Besar Regency  2001 4
    ## 8             Achabal  2001 1
    ## 9                Aden  2001 2
    ## 10           Adjumani  2001 2

``` r
numAttacksPerCityMain %>% head(10)
```

    ##               city iyear  x
    ## 1          Baghdad  2001  2
    ## 2  Barrancabermeja  2001  6
    ## 3             Gaza  2001  6
    ## 4           Grozny  2001 38
    ## 5        Jerusalem  2001 32
    ## 6           London  2001  2
    ## 7        New Delhi  2001  4
    ## 8    New York City  2001  9
    ## 9            Paris  2001  3
    ## 10          Grozny  2002 32

Here I will look at the number of attacks for all of the cities globally to see which city had the most attacks overall. Column x represents the number of attacks for that year in that specific city.

``` r
mostAttacksPerYear2 <- numAttacksPerCity %>% 
  group_by(iyear) %>%
  filter(x == max(x))

mostAttacksPerYear2
```

    ## # A tibble: 16 x 3
    ## # Groups:   iyear [16]
    ##    city    iyear     x
    ##    <chr>   <int> <int>
    ##  1 Unknown  2001    79
    ##  2 Unknown  2002    55
    ##  3 Unknown  2003    48
    ##  4 Baghdad  2004   145
    ##  5 Baghdad  2005   272
    ##  6 Baghdad  2006   454
    ##  7 Baghdad  2007   416
    ##  8 Baghdad  2008   456
    ##  9 Baghdad  2009   458
    ## 10 Baghdad  2010   586
    ## 11 Baghdad  2011   501
    ## 12 Unknown  2012   519
    ## 13 Unknown  2013   725
    ## 14 Baghdad  2014   898
    ## 15 Baghdad  2015  1000
    ## 16 Baghdad  2016   986

Now, I wil calculate the number of attacks per country and divide it by type of attack. The numbers, 1 - 9, each represent a different type of attack. The legend for what each number stands for can be seen in the console. 1 is an Assasination, 2 is an Armed Assault, 3 is a Bombing/Explosion, 4 is a Hijacking, 5 is a Hostage Taking (Barricade), 6 is a Hostage Taking (Kidnapping), 7 is a Facility/Infastructure Attack, 8 is an Unarmed Assault and finally, 9 is Unknown.

``` r
numAttacksCountryType <- tidy_db %>%
  group_by(iyear, country_txt) %>%
  dplyr::select(iyear,country_txt,attacktype1,attacktype1_txt) %>%
  dplyr::count(attacktype1) 

numAttacksCountryType %>% head(10)
```

    ## # A tibble: 10 x 4
    ## # Groups:   iyear, country_txt [3]
    ##    iyear country_txt attacktype1     n
    ##    <int> <chr>             <int> <int>
    ##  1  2001 Afghanistan           2     2
    ##  2  2001 Afghanistan           3    10
    ##  3  2001 Afghanistan           7     2
    ##  4  2001 Albania               3     1
    ##  5  2001 Algeria               1     4
    ##  6  2001 Algeria               2    80
    ##  7  2001 Algeria               3    17
    ##  8  2001 Algeria               6     3
    ##  9  2001 Algeria               7     2
    ## 10  2001 Algeria               9     7

``` r
numAttacksPerCountry <- aggregate(numAttacksCountryType$n,
            by = list(country=numAttacksCountryType$country_txt,year=numAttacksCountryType$iyear), FUN=sum) 

numAttacksPerCountry %>% head(10)
```

    ##        country year   x
    ## 1  Afghanistan 2001  14
    ## 2      Albania 2001   1
    ## 3      Algeria 2001 113
    ## 4       Angola 2001  40
    ## 5    Argentina 2001   2
    ## 6      Armenia 2001   2
    ## 7    Australia 2001   2
    ## 8   Azerbaijan 2001   4
    ## 9   Bangladesh 2001  15
    ## 10     Belarus 2001   1

``` r
numAttacksCountryType %>%
  ggplot(aes(x=iyear, y=n)) +
    geom_point() +
    labs(title="Number of Attacks a Year Per Country",
         x = "Year",
         y = "Number of Attacks") + facet_grid(.~attacktype1) 
```

![](finalProjectGithub_files/figure-markdown_github/dataParsing3-1.png)

``` r
  labs(caption = "1: Assassination, 2: Armed Assault, 3: Bombing/Explosion, 4: Hijacking, 
       5: Hostage Taking (Barricade), 6: Hostage Taking (Kidnapping), 7: Facility/Infastructure Attack,
       8: Unarmed Assault, 9: Unknown")
```

    ## $caption
    ## [1] "1: Assassination, 2: Armed Assault, 3: Bombing/Explosion, 4: Hijacking, \n       5: Hostage Taking (Barricade), 6: Hostage Taking (Kidnapping), 7: Facility/Infastructure Attack,\n       8: Unarmed Assault, 9: Unknown"
    ## 
    ## attr(,"class")
    ## [1] "labels"

As you can see, looking at all of the countries globally creates a graph that does not give us much information. Thus, I will look at the data for the countries where my specified cities are located.

``` r
numAttacksCountryType <- tidy_db %>%
  group_by(iyear, country_txt,city) %>%
  dplyr::select(iyear,country_txt,city,attacktype1,attacktype1_txt) %>%
  filter(country_txt == "United States" || country_txt == "United Kingdom"||country_txt == "Israel"
  || country_txt == "France"||country_txt == "Iraq"||country_txt == "India"||country_txt == "Russia"
  ||country_txt == "West Bank and Gaza Strip"||country_txt == "Colombia") %>%
  dplyr::count(attacktype1) 

numAttacksCountryType %>% head(10)
```

    ## # A tibble: 10 x 5
    ## # Groups:   iyear, country_txt, city [10]
    ##    iyear country_txt city              attacktype1     n
    ##    <int> <chr>       <chr>                   <int> <int>
    ##  1  2001 Colombia    Acacias district            6     1
    ##  2  2001 Colombia    Aguachica                   7     1
    ##  3  2001 Colombia    Alejandria                  2     1
    ##  4  2001 Colombia    Algeciras                   2     1
    ##  5  2001 Colombia    Alto de Mayo                2     1
    ##  6  2001 Colombia    Alto de San Juan            2     1
    ##  7  2001 Colombia    Amalfi                      2     1
    ##  8  2001 Colombia    Araquita district           3     1
    ##  9  2001 Colombia    Aratoca district            3     1
    ## 10  2001 Colombia    Arauca                      6     1

``` r
numAttacksPerCountry <- aggregate(numAttacksCountryType$n,
  by = list(country=numAttacksCountryType$country_txt,iyear=numAttacksCountryType$iyear), FUN=sum) 

numAttacksPerCountry %>% head(10)
```

    ##                     country iyear   x
    ## 1                  Colombia  2001 207
    ## 2                    France  2001  21
    ## 3                     India  2001 234
    ## 4                      Iraq  2001   3
    ## 5                    Israel  2001  79
    ## 6                    Russia  2001 135
    ## 7            United Kingdom  2001  94
    ## 8             United States  2001  41
    ## 9  West Bank and Gaza Strip  2001 123
    ## 10                 Colombia  2002 150

The first table shows us the number of attacks per country per type of attack. Seeing the data broken down like this can give governments or officials better information about how to protect their citizens against types of attacks if they know which type is the most common. The second table shows the total number of attacks per country. Seeing all the types of terrorist attacks grouped together allows one to interpret which country is at the biggest risk for attacks in the years to come.

1.  Management of Data

Having null or unknown rows, as seen in the chart with the max number of attacks per year in a city, is not useful and does not give the user or reader any information. Removing unknowns or NA's will give more information that is accurate and critical to future analysis. Here, we will remove any city that is Unknown and use this table for any analysis that involves solely cities, then use the previous table, numAttacksPerCountry, when doing analysis based on country.

``` r
cleaned_db <- filter(numAttacksPerCity,numAttacksPerCity$city != "Unknown") 

cleaned_db %>% head(10)
```

    ##                  city iyear x
    ## 1            Aadaisse  2001 1
    ## 2                Abia  2001 1
    ## 3             Abidjan  2001 1
    ## 4               Aboud  2001 1
    ## 5    Acacias district  2001 1
    ## 6                Aceh  2001 1
    ## 7  Aceh Besar Regency  2001 4
    ## 8             Achabal  2001 1
    ## 9                Aden  2001 2
    ## 10           Adjumani  2001 2

``` r
mostAttacksPerYear <- cleaned_db %>% 
  group_by(iyear) %>%
  filter(x == max(x))

mostAttacksPerYear
```

    ## # A tibble: 16 x 3
    ## # Groups:   iyear [16]
    ##    city    iyear     x
    ##    <chr>   <int> <int>
    ##  1 Grozny   2001    38
    ##  2 Grozny   2002    32
    ##  3 Baghdad  2003    40
    ##  4 Baghdad  2004   145
    ##  5 Baghdad  2005   272
    ##  6 Baghdad  2006   454
    ##  7 Baghdad  2007   416
    ##  8 Baghdad  2008   456
    ##  9 Baghdad  2009   458
    ## 10 Baghdad  2010   586
    ## 11 Baghdad  2011   501
    ## 12 Baghdad  2012   302
    ## 13 Baghdad  2013   639
    ## 14 Baghdad  2014   898
    ## 15 Baghdad  2015  1000
    ## 16 Baghdad  2016   986

Now, we can see that two of the ten major cities we were looking at are the two cities that had the most attacks each year from 2001 - 2016.

1.  Exploratory Data Analysis (EDA)

First, lets graph the number of attacks since 2001 in the United States to find which states had the most attacks.

``` r
USattacks <- filter(tidy_db,tidy_db$country_txt == "United States") %>%
  group_by(provstate, attacktype1,attacktype1_txt) %>%
  dplyr::select(provstate,attacktype1, attacktype1_txt) %>%
  dplyr::count(attacktype1) 

USattacks2 <- aggregate(USattacks$n, by=list(state=USattacks$provstate), FUN=sum) 

USattacks2 <- USattacks2[order(-USattacks2$x),] %>% head(10)
USattacks2
```

    ##                   state  x
    ## 4            California 56
    ## 27             New York 34
    ## 40           Washington 24
    ## 8               Florida 20
    ## 37                Texas 18
    ## 39             Virginia 14
    ## 7  District of Columbia 13
    ## 18             Michigan 12
    ## 26           New Mexico 12
    ## 32               Oregon 12

``` r
USattacks2 %>%
  ggplot(aes(x=state, y=x, color = state)) +
    geom_point() +
    labs(title="Number of Attacks a Year Per State From 2001 - 2016",
         x = "State",
         y = "Number of Attacks")
```

![](finalProjectGithub_files/figure-markdown_github/EDA1-1.png) From this graph and table, we can conclude that since 2001, California and New York have had the most attacks. Knowing this, the Government and police forces should take this into account when training or putting funds within Homeland Security because these are the two areas/states where peopel are at the most risk for being affected by a terrorist attack

Now, we will graph the number of total attacks per year for each of the ten cities we are analyzing.

``` r
numAttacksPerCityMain %>%
  ggplot(aes(x=iyear, y=x, color = city)) +
    geom_point() +
    labs(title="Number of Attacks a Year Per City",
         x = "Year",
         y = "Number of Attacks") 
```

![](finalProjectGithub_files/figure-markdown_github/EDA2-1.png) As you can see, Baghdad has a significantly higher number of attacks than all of the other cities. This could be due to the fact that the US invaded Iraq in 2003 and was fighting a war there till 2011.

Next, we will graph the number of attacks per contry by type of attack for each of the countries where the ten cities we are focusing on is located.

``` r
numAttacksCountryType
```

    ## # A tibble: 14,173 x 5
    ## # Groups:   iyear, country_txt, city [11,235]
    ##    iyear country_txt city              attacktype1     n
    ##    <int> <chr>       <chr>                   <int> <int>
    ##  1  2001 Colombia    Acacias district            6     1
    ##  2  2001 Colombia    Aguachica                   7     1
    ##  3  2001 Colombia    Alejandria                  2     1
    ##  4  2001 Colombia    Algeciras                   2     1
    ##  5  2001 Colombia    Alto de Mayo                2     1
    ##  6  2001 Colombia    Alto de San Juan            2     1
    ##  7  2001 Colombia    Amalfi                      2     1
    ##  8  2001 Colombia    Araquita district           3     1
    ##  9  2001 Colombia    Aratoca district            3     1
    ## 10  2001 Colombia    Arauca                      6     1
    ## # ... with 14,163 more rows

``` r
numAttacksCountryType %>%
  ggplot(aes(x=iyear, y=n, color = country_txt)) +
    geom_point() +
    labs(title="Number of Attacks a Year Per Country",
         x = "Year",
         y = "Number of Attacks") + facet_grid(attacktype1~.)
```

![](finalProjectGithub_files/figure-markdown_github/EDA3-1.png)

1.  Hypothesis Testing and Machine Learning

As you can see from the graph that displays the number of attacks per year in the ten cities we are focusing on, overall Baghdad, Jerusalem and Gaza have the most attacks. Using this knowledge, we will predict the number of attacks for the following year.

``` r
newCities <- numAttacksCountryType %>% 
  filter(country_txt == "Iraq" || country_txt == "Israel" || country_txt == "West Bank and Gaza Strip")

lmPlot2 <- lm(n ~ country_txt*iyear, data = newCities)
lmPlot <- lmPlot2 %>% broom::tidy()
lmPlot
```

    ##                                        term      estimate   std.error
    ## 1                               (Intercept) -122.55324568 259.3498469
    ## 2                         country_txtIsrael  -48.50231724 589.7767959
    ## 3       country_txtWest Bank and Gaza Strip   82.02551102 462.7997940
    ## 4                                     iyear    0.06377830   0.1289020
    ## 5                   country_txtIsrael:iyear    0.02264021   0.2935339
    ## 6 country_txtWest Bank and Gaza Strip:iyear   -0.04278054   0.2302127
    ##     statistic   p.value
    ## 1 -0.47254027 0.6365620
    ## 2 -0.08223843 0.9344604
    ## 3  0.17723757 0.8593290
    ## 4  0.49478149 0.6207763
    ## 5  0.07712979 0.9385234
    ## 6 -0.18583051 0.8525853

``` r
increaseIraq <- lmPlot$estimate[4]
increaseIsrael <- lmPlot$estimate[4] + lmPlot$estimate[5]
increaseWBGaza <- lmPlot$estimate[4] + lmPlot$estimate[6]

increaseIraq
```

    ## [1] 0.0637783

``` r
increaseIsrael
```

    ## [1] 0.08641851

``` r
increaseWBGaza
```

    ## [1] 0.02099776

From this table, we learn that the intercept is the attacks per year for Iraq, which means each year the number of attacks will increase by about .06, or increaseIraq. The number of attacks in Israel, increaseIsrael, will increase by .022 attacks per year. Finally, the number of attacks in the West Bank and Gaza will decrease by .0209 attcks per year.
