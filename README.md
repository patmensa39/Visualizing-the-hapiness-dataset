# Visualizing-the-hapiness-dataset
# Case study on R 
install.packages("productplots")
install.packages("RColorBrewer")

### loading all the required libraries using pacman 
install.packages("pacman")
pacman::p_load(pacman, magrittr, productplots, psych, RColorBrewer, tidyverse)

?happy

data <- happy %>% as_tibble() %>% print()
view(data)

names(data)
na.omit(data)
view(data)

### removing the first column (id)
# happy <- happy %>% select(-id) %>% print() 

### Checking the level of happiness
levels(data$happy)

### Reversing the levels of happiness 
data %<>% mutate(happy = fct_rev(happy))  
### Checking the level of happiness
levels(data$happy)
attach(data)
glimpse(data)
summary(data)

### Outcome variable 
### Bar Chart for happiness 
ggplot(data = data, mapping = aes(x = happy, fill = happy)) + geom_bar() + 
  theme(axis.title.x = element_blank(),legend.position = "none")

### Frequencies for happiness 
summary(data$happy)
data %>% count(happy)

### Removing the NA values from the happiness column
data %<>% filter(!is.na(happy))

### Bar Chart for happiness 
ggplot(data = data, mapping = aes(x = happy, fill = happy)) + geom_bar() + 
  theme(axis.title.x = element_blank(),legend.position = "none")

### Bar chart for Gender
ggplot(data = data, mapping = aes(x = sex, fill = sex)) + geom_bar() + 
  theme(axis.title.x = element_blank(),legend.position = "none")

### Frequencies for Gender 
summary(data$sex)
data %>% count(sex)

### Stacked barchart for gender
ggplot(data = data, mapping = aes(x = sex, fill = happy)) + 
  geom_bar(position = "fill")

### Bar chart for Marital Status 
ggplot(data = data, mapping = aes(x = marital, fill = marital)) + geom_bar() + 
  theme(axis.title.x = element_blank(),legend.position = "none")

### Frequencies for Marital status 
summary(data$marital)
data %>% count(marital)

### Removing the NA values from the marital status  column
data %<>% filter(!is.na(marital))

### Bar chart for Marital Status 
ggplot(data = data, mapping = aes(x = marital, fill = marital)) + geom_bar() + 
  theme(axis.title.x = element_blank(),legend.position = "none")

### Stacked barchart for marital status 
ggplot(data = data, mapping = aes(x = marital, fill = happy)) + 
  geom_bar(position = "fill")

### Creating a dichotomous married 
data %<>% mutate(married = ifelse(test = marital == "married", yes = "Yes",
                                  no = "No")) %>% print()

### Frequencies for Married and Unmarried 
summary(data$married)
data %>% count(married)

### Bar chart for  Married and Unmarried 
ggplot(data = data, mapping = aes(x = married, fill = happy)) + geom_bar() + 
  theme(axis.title.x = element_blank(),legend.position = "none")

### Bar chart for level of education 
ggplot(data = data, mapping = aes(x = degree, fill = degree)) + geom_bar() + 
  theme(axis.title.x = element_blank(),legend.position = "none")

### Frequencies for Married and Unmarried 
summary(data$degree)
data %>% count(degree)



### Stacked Bar chart for level of education 
data %>% filter(!is.na(degree)) %>% 
  ggplot(mapping = aes(x = degree, fill = happy)) +
  geom_bar(position = "fill") 


### Categorizing junior college, barchelor and graduate in one group and all
### others in another group 
data %<>% mutate(college = ifelse(degree %in% 
                                    c("junior college", "barchelor", "graduate"),
                                  yes = "College", no = "No College")) %>%
  print()

view(data)

### Frequencies for College and No college
summary(data$college)
data %>% count(college)


### Stacked Bar chart for level of education 
data %>% 
  ggplot(mapping = aes(x = college, fill = happy)) +
  geom_bar(position = "fill") 

### Bar chart for Financial status  
data %>% 
  ggplot(mapping = aes(x = finrela, fill = finrela)) +
  geom_bar() + theme(axis.title.x = element_blank(), 
                     legend.position = "none")

### Frequencies for Financial status
summary(data$finrela)
data %>% count(finrela)

### Stacked Bar chart for Financial status in connection to happiness
data %>% filter(!is.na(finrela)) %>% 
  ggplot(mapping = aes(x = finrela, fill = happy)) +
  geom_bar(position = "fill") 


### Bar chart for Health  
data %>% 
  ggplot(mapping = aes(x = health, fill = health)) +
  geom_bar() + theme(axis.title.x = element_blank(), 
                     legend.position = "none")

### Frequencies for health status
summary(data$health)
data %>% count(health)

### Stacked Bar chart for Financial status in connection to happiness
data %>% filter(!is.na(health)) %>% 
  ggplot(mapping = aes(x = health, fill = happy)) +
  geom_bar(position = "fill") 

### histogram for the year
hist(data$year, col = terrain.colors(6), ylim = c(0,8000))

### Another way
data %>% qplot(year, binwidth = 5, data = .)

### Statistics for the year
summary(data$year)
data %>% select(year) %>% summary()


### Density plot for year by group 
data %>% ggplot(aes(x= year, fill = happy)) + 
  geom_density(alpha = 0.5) + facet_grid(happy~.) +
  theme(legend.position = "none")

### Boxplot for year by group 
data %>% ggplot(aes(x = happy, y = year, fill = happy)) + 
  geom_boxplot() + coord_flip() + xlab("") +
  theme(legend.position = "none")

### Happiness and age 
data %>% qplot(age, binwidth = 5, data = .)
hist(data$age, col = heat.colors(6))

### Statistics for the age
summary(data$age)
data %>% select(age) %>% summary()

### Density plot for age by group in connection to hapiness
data %>% ggplot(aes(x= age , fill = happy)) + 
  geom_density(alpha = 0.5) + facet_grid(happy~.) +
  theme(legend.position = "none")



### Boxplot for age by group 
data %>% ggplot(aes(x = age, y = year, fill = happy)) + 
  geom_boxplot() + coord_flip() + xlab("") +
  theme(legend.position = "none")


### Happiness and year born 
### calculating the year of birth 
data %<>% mutate(born = year - age)
summary(data$born)
data %>% select(born) %>% summary()
data %>% qplot(born, binwidth = 5, data = .)


### Density plot for  in connection to hapiness
data %>% ggplot(aes(x= born , fill = happy)) + 
  geom_density(alpha = 0.5) + facet_grid(happy~.) +
  theme(legend.position = "none")
