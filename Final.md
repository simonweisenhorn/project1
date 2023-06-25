Final
================
Simon Weisenhorn
2023-06-25

``` r
library(httr) #Here I am loading the required library for the following steps
library(tidyverse)
library(stringr)#str_to_title()
library(jsonlite)
```

# First Function

``` r
recipesByCuisine <- function(likedCuisine, dislikedCuisine=NULL, number=10) {
  
  if((number > 100) | (number <= 0)){
    stop("ERROR: Argument for number must be between 0 and 100!")
  }
  
  likedCuisine <- tolower(likedCuisine)
  dislikedCuisine <- tolower(dislikedCuisine)
  likedCuisine <- paste(likedCuisine, collapse=',')
  dislikedCuisine <- paste(dislikedCuisine, collapse=',')
  if(length(dislikedCuisine)>0){
  recipes <- GET(
    paste("https://api.spoonacular.com/recipes/complexSearch?cuisine=",
                       likedCuisine,"&excludeCuisine=",dislikedCuisine,
                       "&number=",number,
                       "&apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", sep=""))
  } else {
  recipes <- GET(
    paste("https://api.spoonacular.com/recipes/complexSearch?cuisine=",
                         likedCuisine,"&number=",number,
                         "&apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", sep=""))
  }
  parsed <- fromJSON(rawToChar(recipes$content))
  recipeInfo <- parsed$results 
  data <- as_tibble(recipeInfo[,c("id", "title")])
  data <- data %>% rename(ID = id, Recipe = title)
  return(data)
}
```

# Second Function

``` r
recipesByDiet <- function(diet, number=10) {
  
  if((number > 100) | (number <= 0)){
    stop("ERROR: Argument for number must be between 0 and 100!")
  }
  
  diet <- tolower(diet)
  diet <- paste(diet, collapse=',')
  recipes <- GET(
    paste("https://api.spoonacular.com/recipes/complexSearch?diet=",
                       diet,"&number=",number,
                       "&apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", sep=""))
  parsed <- fromJSON(rawToChar(recipes$content))
  recipeInfo <- parsed$results 
  data <- as_tibble(recipeInfo[,c("id", "title")])
  data <- data %>% rename(ID = id, Recipe = title)
  return(data)
}
```

# Third Function

``` r
recipesByIntolerances <- function(intolerance, number=10) {
  
  if((number > 100) | (number <= 0)){
    stop("ERROR: Argument for number must be between 0 and 100!")
  }
  
  intolerance <- tolower(intolerance)
  intolerance <- paste(intolerance, collapse=',')
  recipes <- GET(
    paste("https://api.spoonacular.com/recipes/complexSearch?intolerances=",
          intolerance,"&number=",number,
          "&apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", sep=""))
  parsed <- fromJSON(rawToChar(recipes$content))
  recipeInfo <- parsed$results 
  data <- as_tibble(recipeInfo[,c("id", "title")])
  data <- data %>% rename(ID = id, Recipe = title)
  return(data)
}
```

# Fourth Function

``` r
recipesByType = function(type, number=10) {
  
  if((number > 100) | (number <= 0)){
    stop("ERROR: Argument for number must be between 0 and 100!")
  }
  
  type = tolower(type)
  type = paste(type, collapse=',')
  type = sub(" ", "_", type)
  recipes <- GET(
    paste("https://api.spoonacular.com/recipes/complexSearch?type=",type,
          "&number=",number,"&apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", sep=""))
  parsed <- fromJSON(rawToChar(recipes$content))
  recipeInfo <- parsed$results 
  data = as_tibble(recipeInfo[,c("id", "title")])
  data = data %>% rename(ID = id, Recipe = title)
  return(data)
}
```

# Fifth Function

``` r
taste <- function(id, type=NULL) {
  tastes <- GET(
    paste("https://api.spoonacular.com/recipes/",id,
          "/tasteWidget.json?apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", 
          sep=""))
  parsed <- fromJSON(rawToChar(tastes$content))
  data <- data.frame(parsed)
  data <- data %>% mutate(ID=id) %>% select(ID, everything())
  if (length(type)>0){
    type <- unlist(lapply(type, tolower))
    return(data[c("ID", type)])
  } else{
    return(data)
  }
}
```

# Sixth Function

``` r
multipleTastes <- function(foodids, type=NULL) {
  data <- data.frame()
  for(i in 1:length(foodids)) {
    for(j in foodids[i]){
      data <- bind_rows(data, taste(j, type=type))
    }
  }
  return(data)
}
```

# Seventh Function

``` r
getNutrientsPerServing <- function(id, type=NULL) {
  nutrients <- GET(
    paste0("https://api.spoonacular.com/recipes/",id,
           "/information?includeNutrition=true&apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b",
           sep=""))
  parsed <- fromJSON(rawToChar(nutrients$content))
  dataframe <- as.data.frame(parsed$nutrition[1])
  dataframe <- pivot_wider(dataframe[1:2], names_from=nutrients.name, 
                           values_from=nutrients.amount)
  dataframe <- dataframe %>% mutate(ID=id) %>% select(ID, everything())
  names(dataframe) <- c(make.names(names(dataframe), unique=TRUE))
  if (length(type)>0) {
    type <- unlist(lapply(type, str_to_title))
    return(dataframe[c("ID",type)])
  } else{
    return(dataframe)
  }
}
```

# Eighth Function

``` r
multipleNutrients <- function(foodids, type=NULL) {
  data <- data.frame()
  for(i in 1:length(foodids)) {
    for(j in foodids[i]){
      data <- bind_rows(data, getNutrientsPerServing(j, type=type))
    }
  }
  return(data)
}
```

# Ninth Function

``` r
ingredientsList <- function(id) {
  ingredients <- GET(
    paste("https://api.spoonacular.com/recipes/",id,
          "/ingredientWidget.json?apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", 
          sep=""))
  parsed <- fromJSON(rawToChar(ingredients$content))
  names <- parsed$ingredients$name
  values <- parsed$ingredients$amount$us$value
  units <- parsed$ingredients$amount$us$unit
  data <- data.frame(Measurements=values, Units = units, Ingredients = names)
  return(data)
}
```

# Tenth Function

``` r
equipment <- function(id) {
  equipmentData <- GET(
    paste("https://api.spoonacular.com/recipes/",id,
          "/equipmentWidget.json?apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", 
          sep=""))
  parsed <- fromJSON(rawToChar(equipmentData$content))
  equipmentList <- parsed[[1]]$name
  data <- data.frame(Equipment = equipmentList)
  return(data)
}
```

# Eleventh Function

``` r
recipeInstructions <- function(id) {
  instructions <- GET(
    paste("https://api.spoonacular.com/recipes/",id,
          "/analyzedInstructions?apiKey=f8ceed6cf4f74d56b1584d84ee70ec9b", 
          sep=""))
  parsed <- fromJSON(rawToChar(instructions$content))
  stepnum <- parsed$steps[[1]]$number
  instructionList <- parsed$steps[[1]]$step
  data <- data.frame(Step=stepnum, Instructions = instructionList)
  return(data)
}
```

# Twelvth Function

``` r
foodAPI <- function(func, ...){

  if (func == "recipesByCuisine"){
    output <- recipesByCuisine(...)
  }
  else if (func == "recipesByDiet"){
    output <- recipesByDiet(...)
  }
  else if (func == "recipesByIntolerances"){
    output <- recipesByIntolerances(...)
  }
  else if (func == "recipesByType"){
    output <- recipesByType(...)
  }
  else if (func == "taste"){
    output <- taste(...)
  }
  else if (func == "multipleTastes"){
    output <- multipleTastes(...)
  }
  else if (func == "getNutrientsPerServing"){
    output <- getNutrientsPerServing(...)
  }
  else if (func == "multipleNutrients"){
    output <- multipleNutrients(...)
  }
  else if (func == "ingredientsList"){
    output <- ingredientsList(...)
  }
  else if (func == "equipment"){
    output <- equipment(...)
  }
  else if (func == "recipeInstructions"){
    output <- recipeInstructions(...)
  }
  else {
    stop("ERROR: Function arguement is invalid!")
  }
  
  return(output)
}
```

# First Visual

``` r
americanRecipeNutrition <- foodAPI("multipleNutrients", 
                                   foodids=foodAPI("recipesByCuisine", 
                                                   "American", number=100)$ID)

plot1 <- ggplot(americanRecipeNutrition, aes(x=Carbohydrates, y=Calories, 
                                              color= Sugar))
plot1 + geom_point() + scale_color_gradient(low="blue", high="red") +
  geom_smooth(method=lm, formula=y~x, color="black") +
  labs(x = "Grams of Carbohydrates Per Serving", 
       title = "Calories Vs Carbohydrates in American Cuisine") +
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](Final_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

# Second Visual

``` r
plot2 <- ggplot(americanRecipeNutrition, aes(x=Fat, y=Calories, 
                                              color= Saturated.Fat))
plot2 + geom_point() + scale_color_gradient(low="blue", high="red", 
                                            name="Saturated Fat") +
  geom_smooth(method=lm, formula=y~x, color="black") +
  labs(x = "Grams of Fat Per Serving", 
       title = "Calories Vs Fat in American Cuisine") +
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](Final_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

# Third Visual

``` r
indianRecipeNutrition <- foodAPI("multipleNutrients", 
                                 foodids=foodAPI("recipesByCuisine", 
                                                 "Indian", number=100)$ID)

europeanRecipeNutrition <- foodAPI("multipleNutrients", 
                                   foodids=foodAPI("recipesByCuisine", 
                                                   "European", number=100)$ID)

mediterraneanRecipeNutrition <- foodAPI("multipleNutrients", 
                                        foodids=foodAPI("recipesByCuisine", 
                                                        "Mediterranean", 
                                                        number=100)$ID)


americanRecipeNutrition <- americanRecipeNutrition %>% 
  mutate(SodiumIndicator = case_when(Sodium >= 1150 ~ "High Sodium", 
                                     Sodium < 1150 ~ "Low Sodium"), 
         Cuisine = "American")

indianRecipeNutrition <- indianRecipeNutrition %>% 
  mutate(SodiumIndicator = case_when(Sodium >= 1150 ~ "High Sodium", 
                                     Sodium < 1150 ~ "Low Sodium"), 
         Cuisine = "Indian")

europeanRecipeNutrition <- europeanRecipeNutrition %>% 
  mutate(SodiumIndicator = case_when(Sodium >= 1150 ~ "High Sodium", 
                                     Sodium < 1150 ~ "Low Sodium"), 
         Cuisine = "European")

mediterraneanRecipeNutrition <- mediterraneanRecipeNutrition %>% 
  mutate(SodiumIndicator = case_when(Sodium >= 1150 ~ "High Sodium", 
                                     Sodium < 1150 ~ "Low Sodium"), 
         Cuisine = "Mediterranean")

allCuisineNutrition <- bind_rows(americanRecipeNutrition, 
                                 indianRecipeNutrition, 
                                 europeanRecipeNutrition, 
                                 mediterraneanRecipeNutrition)

table(allCuisineNutrition$Cuisine, allCuisineNutrition$SodiumIndicator)
```

    ##                
    ##                 High Sodium Low Sodium
    ##   American               24         76
    ##   European               33         67
    ##   Indian                 11         89
    ##   Mediterranean          35         65

# Fourth Visual

``` r
allCuisineNutrition %>% group_by(Cuisine) %>% summarise(Min = min(Calories),
                                              firstQuartile = quantile(Calories, 
                                                                       0.25),
                                              Avg=mean(Calories),
                                              Med=median(Calories),
                                              thirdQuartile = quantile(Calories, 
                                                                       0.75),
                                              max = max(Calories),
                                              stdDev = sd(Calories))
```

    ## # A tibble: 4 × 8
    ##   Cuisine         Min firstQuartile   Avg   Med thirdQuartile   max stdDev
    ##   <chr>         <dbl>         <dbl> <dbl> <dbl>         <dbl> <dbl>  <dbl>
    ## 1 American       27.3          308.  490.  449.          660. 1124.   241.
    ## 2 European       98.5          390.  523.  479.          651. 1286.   222.
    ## 3 Indian         53.1          256.  397.  413.          542.  877.   187.
    ## 4 Mediterranean  95.6          359.  514.  479.          634. 1286.   216.

# Fifth Visual

``` r
maincourseRecipeNutrition <- foodAPI("multipleNutrients", 
                                     foodids=foodAPI("recipesByType", 
                                                     "Main Course", 100)$ID)
dessertRecipeNutrition <- foodAPI("multipleNutrients", 
                                  foodids=foodAPI("recipesByType", 
                                                  "Dessert", 100)$ID)

maincourseRecipeNutrition <- maincourseRecipeNutrition %>% 
  mutate(Type="Main Course")
dessertRecipeNutrition <- dessertRecipeNutrition %>% 
  mutate(Type="Dessert")

fullmeal <- bind_rows(maincourseRecipeNutrition, dessertRecipeNutrition)

plot3 <- ggplot(fullmeal, aes(x=Calories, fill=Type))
plot3 + geom_histogram(data=subset(fullmeal, Type == 'Main Course'),
                 bins=20, alpha = 0.75) +
  geom_histogram(data=subset(fullmeal, Type == 'Dessert'),
                 bins=20, alpha = 0.75) +
  labs(x = "Calories Per Serving", y = "Frequency",
       title = "Histogram of Calories Per Serving") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Meal Course")
```

![](Final_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

# Sixth Visual

``` r
maincourseRecipeNutrition <- maincourseRecipeNutrition %>% 
  mutate(merger=seq(1:100))
dessertRecipeNutrition <- dessertRecipeNutrition %>% 
  mutate(merger=seq(1:100))

totalMeal <- inner_join(maincourseRecipeNutrition, 
                        dessertRecipeNutrition, by='merger')

totalMeal <- totalMeal %>% mutate(totCalories=Calories.x+Calories.y)

plot4 <- ggplot(totalMeal, aes(x=totCalories))
plot4 + geom_histogram(color="black", fill="wheat", bins=20) +
  geom_vline(aes(xintercept=1250, color="Male"), linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=1000, color="Female"), linetype="dashed", size=1) +
  labs(x = "Total Calories Per Meal Serving", y = "Frequency", 
       title = "Histogram of Total Calories Per One Serving of Main Course and 
       One Serving of Dessert") +
  theme(plot.title = element_text(hjust = 0.5), title= element_text(size=10), 
        legend.title=element_text(size=9)) + 
  scale_color_manual(name = "Half of Daily Reccomended Calories", 
                     values = c(Male = "blue", Female = "hotpink"))
```

![](Final_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

# Seventh Visual

``` r
ketoNutrients <- foodAPI("multipleNutrients", 
                         foodids=foodAPI("recipesByDiet", "Ketogenic", 
                                         number=100)$ID)
vegetarianNutrients <- foodAPI("multipleNutrients", 
                               foodids=foodAPI("recipesByDiet", "Vegetarian", 
                                                number=100)$ID)
veganNutrients <- foodAPI("multipleNutrients", 
                          foodids=foodAPI("recipesByDiet", "Vegan", 
                                          number=100)$ID)
pescetarianNutrients <- foodAPI("multipleNutrients", 
                                foodids=foodAPI("recipesByDiet", 
                                                "Pescetarian", number=100)$ID)
primalNutrients <- foodAPI("multipleNutrients", 
                           foodids=foodAPI("recipesByDiet", 
                                           "Primal", number=100)$ID)
whole30Nutrients <- foodAPI("multipleNutrients", 
                            foodids=foodAPI("recipesByDiet", 
                                            "Whole30", number=100)$ID)

ketoNutrients <- ketoNutrients %>% mutate(Diet = "Ketogenic")
vegetarianNutrients <- vegetarianNutrients %>% mutate(Diet = "Vegetarian")
veganNutrients <- veganNutrients %>% mutate(Diet = "Vegan")
pescetarianNutrients <- pescetarianNutrients %>% mutate(Diet = "Pescetarian")
primalNutrients <- primalNutrients %>% mutate(Diet = "Primal")
whole30Nutrients <- whole30Nutrients %>% mutate(Diet = "Whole30")

allDietNutrition <- bind_rows(ketoNutrients, vegetarianNutrients, 
                              veganNutrients, pescetarianNutrients, 
                              primalNutrients, whole30Nutrients)

allDietNutrition <- allDietNutrition %>% 
  mutate(proteinToCarbs=Protein/Carbohydrates)

allDietNutrition2 <- allDietNutrition %>% group_by(Diet) %>% 
  summarise(avgProteinToCarbs = mean(proteinToCarbs)) 

plot5 <- ggplot(allDietNutrition2, aes(x=Diet, y=avgProteinToCarbs, fill=Diet))
plot5 + geom_col() + labs(x = "Diet Type", 
                          y = "Average Ratio of Proteins to Carbs (Grams)",
       title="Bar Chart of Average Ratio of Proteins to Carbs in Grams by Diet Type") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Diet Type")
```

![](Final_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

# Eighth Visual

``` r
allDietNutrition <- allDietNutrition %>% mutate(proteinToFats=Protein/Fat)

allDietNutrition3 <- allDietNutrition %>% group_by(Diet) %>% 
  summarise(avgProteinToFats = mean(proteinToFats)) 

plot6 <- ggplot(allDietNutrition3, aes(x=Diet, y=avgProteinToFats, fill=Diet))
plot6 + geom_col() + labs(x = "Diet Type", 
                          y = "Average Ratio of Proteins to Fats (Grams)",
       title = "Bar Chart of Average Ratio of Proteins to Fats in Grams by Diet Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Diet Type")
```

![](Final_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

# Nineth Visual

``` r
allDietNutrition %>% group_by(Diet) %>% summarise(Min = min(Calories),
                                              firstQuartile = quantile(Calories, 
                                                                       0.25),
                                              Avg=mean(Calories),
                                              Med=median(Calories),
                                              thirdQuartile = quantile(Calories, 
                                                                       0.75),
                                              max = max(Calories),
                                              stdDev = sd(Calories))
```

    ## # A tibble: 6 × 8
    ##   Diet          Min firstQuartile   Avg   Med thirdQuartile   max stdDev
    ##   <chr>       <dbl>         <dbl> <dbl> <dbl>         <dbl> <dbl>  <dbl>
    ## 1 Ketogenic    90.9          407.  557.  518.          695. 1086.   223.
    ## 2 Pescetarian  57.9          355.  497.  479.          627. 1116.   209.
    ## 3 Primal       53.9          192.  365.  309.          516. 1163.   228.
    ## 4 Vegan        30.4          198.  318.  294.          439.  778.   163.
    ## 5 Vegetarian   30.4          203.  342.  317.          455. 1029.   188.
    ## 6 Whole30      25.0          187.  383.  358.          509. 1163.   234.

# Tenth Visual

``` r
allDietNutrition <- allDietNutrition %>% 
  mutate(B12Indicator = case_when(Vitamin.B12 >= 1.2 ~ "High B12",
                           (Vitamin.B12 < 1.2 | is.na(Vitamin.B12)) ~ 
                             "Low B12"))

table(allDietNutrition$Diet, allDietNutrition$B12Indicator)
```

    ##              
    ##               High B12 Low B12
    ##   Ketogenic         53      47
    ##   Pescetarian       80      20
    ##   Primal            32      68
    ##   Vegan              0     100
    ##   Vegetarian         5      95
    ##   Whole30           29      71

# Eleventh Visual

``` r
glutenTastes <- foodAPI("multipleTastes", 
                        foodids=foodAPI("recipesByIntolerances", 
                                        NULL, number=100)$ID)
glutenFreeTastes <- foodAPI("multipleTastes", 
                            foodids=foodAPI("recipesByIntolerances", 
                                            "Gluten", number=100)$ID)

glutenTastes <- glutenTastes %>% mutate(Type = "Gluten")
glutenFreeTastes <- glutenFreeTastes %>% mutate(Type = "Gluten Free")
allTastes <- bind_rows(glutenTastes, glutenFreeTastes)

set.seed(558)
plot7 <- ggplot(allTastes, aes(x = Type, y = sweetness))
plot7 + geom_boxplot() + geom_point(aes(color=Type), position="jitter") +
  labs(x = "Recipe Contents", y = "Sweetness Level",
       title = "Sweetness Level of Recipe by Ingredient Contents", 
       color="Recipe Contents") +
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](Final_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->