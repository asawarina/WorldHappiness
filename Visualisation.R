train <- read.csv("C:/Users/asawari/Desktop/WorldHappiness/train_data.csv")
test <- read.csv("C:/Users/asawari/Desktop/WorldHappiness/test_data.csv")
str(train)

# Creating a new column for continents
train$Continent <- NA

train$Continent[which(train$country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                           "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                           "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                           "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                           "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                           "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                           "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
train$Continent[which(train$country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                           "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                           "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                           "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                           "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                           "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                           "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                           "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                           "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
train$Continent[which(train$country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                           "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                           "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                           "Haiti"))] <- "North America"
train$Continent[which(train$country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                           "Colombia", "Ecuador", "Bolivia", "Peru",
                                           "Paraguay", "Venezuela"))] <- "South America"
train$Continent[which(train$country %in% c("New Zealand", "Australia"))] <- "Australia"
train$Continent[which(is.na(train$Continent))] <- "Africa"

# Moving the continent column's position in the dataset to the second column
train<- train%>% select(country,Continent, everything())
# Changing Continent column to factor
train$Continent <- as.factor(train$Continent)

str(train)


#health_life_expectancy
ggplot(subset(train, train$Continent != "Australia"), aes(x = health_life_expectancy, y = happiness_score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

#economy_gdp_per_capita
ggplot(subset(train, train$Continent != "Australia"), aes(x = economy_gdp_per_capita, y = happiness_score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

#freedom
ggplot(subset(train, train$Continent != "Australia"), aes(x = freedom, y = happiness_score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

#trust_government
ggplot(subset(train, train$Continent != "Australia"), aes(x = trust_government, y = happiness_score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

#generosity
ggplot(subset(train, train$Continent != "Australia"), aes(x = generosity, y = happiness_score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")

#Family
ggplot(subset(train, train$Continent != "Australia"), aes(x = family, y = happiness_score)) + 
  geom_point(aes(color=Continent), size = 3, alpha = 0.8) +  
  geom_smooth(aes(color = Continent, fill = Continent), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~Continent) +
  theme_bw() + labs(title = "Scatter plot with regression line")



