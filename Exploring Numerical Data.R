library(readr)
cars <- read_csv("cars.csv")
View(cars)

# Load package
library(ggplot2)

# Learn data structure
str(cars)

# Create faceted histogram
ggplot(cars, aes(x = city_mpg)) +
  geom_histogram() +
  facet_wrap(~ suv)
unique(cars$ncyl)
# Filter cars with 4, 6, 8 cylinders
common_cyl <- filter(cars, ncyl %in% c(4, 6, 8))

# Create box plots of city mpg by ncyl
ggplot(data = common_cyl, aes(x = as.factor(ncyl), y = city_mpg)) +
  geom_boxplot()

# Create overlaid density plots for same data
ggplot(common_cyl, aes(x = city_mpg, fill = as.factor(ncyl))) +
  geom_density(alpha = .3)
library(magrittr)
# Create hist of horsepwr
cars %>%
  ggplot(aes(x=horsepwr)) +
  geom_histogram() +
  ggtitle("Distribution of Horsepower")

# Create hist of horsepwr for affordable cars
cars %>% 
  filter(msrp < 25000) %>%
  ggplot(aes(x=horsepwr)) +
  geom_histogram() +
  xlim(c(90, 550)) +
  ggtitle("Distribution of Affordable Horsepower")
# Create hist of horsepwr with binwidth of 3
cars %>%
  ggplot(aes(x=horsepwr)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Horsepower Histogram Jagged")

# Create hist of horsepwr with binwidth of 30
cars %>%
  ggplot(aes(x=horsepwr)) +
  geom_histogram(binwidth = 30) +
  ggtitle("Horsepower Histogram Normal")

# Create hist of horsepwr with binwidth of 60
cars %>%
  ggplot(aes(x=horsepwr)) +
  geom_histogram(binwidth = 60) +
  ggtitle("Horsepower Histogram Smooth")
# Construct box plot of msrp
cars %>%
  ggplot(aes(x = 1, y = msrp)) +
  geom_boxplot()

# Exclude outliers from data
cars_no_out <- cars %>%
  filter(msrp < 100000)

# Construct box plot of msrp using the reduced dataset
cars_no_out %>%
  ggplot(aes(x=1, y=msrp)) +
  geom_boxplot()
#Create plot of city_mpg
cars %>%
  ggplot(aes(x=1, y=city_mpg)) +
  geom_boxplot()
cars %>%
  ggplot(aes(x=city_mpg)) +
  geom_density()
cars %>%
  ggplot(aes(x=1, y=width)) +
  geom_boxplot()
cars %>%
  ggplot(aes(x=width)) +
  geom_density()
# Facet hists using hwy mileage and ncyl
common_cyl %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_grid(ncyl ~ suv) +
  ggtitle("Histogram of SUVs by Number of Cylinders")
