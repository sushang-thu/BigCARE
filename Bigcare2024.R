install.packages('BiocManager')
BiocManager::install('remotes')
BiocManager::install('seandavi/2024-r-intro-bigcare')

die <- 1:6
die %*% die
die %o% die


roll2 <- function(bones) {
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}

roll2(bones= 1:4)

source("path/to/your/script.R", local = env)

insurance_url <- "https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv"
insurance <- readr::read_csv(insurance_url)

# How many rows does thd dataset have?
#   How many columns?
#   What are the column names for the dataset?
#   What are the data types in each column?

nrow(insurance)
ncol(insurance)
dim(insurance)
colnames(insurance)
head(insurance)
# OR
summary(insurance)
# OR
class(insurance)
sapply(insurance,class)
lapply(insurance, class)
insurance$obese <- ifelse(insurance$bmi > 30, "obese", "not obese")
unique(insurance$region)
table(insurance$children)
hist(insurance$charges)
boxplot(insurance$age)
plot(insurance$bmi, insurance$charges)
table(insurance$sex, insurance$smoker)
mosaicplot(~ sex + smoker, data = insurance)

mosaicplot(~ smoker + sex, data = insurance)
boxplot(charges ~ smoker, data = insurance)
ggplot(insurance, aes(x = sex, y = charges)) +
  geom_point() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "red") +
  labs(title = "Scatter Plot of Charges by Sex", x = "Sex", y = "Charges")

ggplot(insurance, aes(x = sex, y = charges)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "red") +
  labs(title = "Scatter Plot of Charges by Sex", x = "Sex", y = "Charges")

boxplot(charges ~ sex, data = insurance, 
        main = "Boxplot of Charges by Sex with Mean Points", 
        xlab = "Sex", ylab = "Charges")
t_test_result <- t.test(charges ~ sex, data = insurance)
p_value <- t_test_result$p.value
boxplot(charges ~ sex, data = insurance, 
        main = "Boxplot of Charges by Sex with Mean Points", 
        xlab = "Sex", ylab = "Charges")


ggplot(insurance, aes(x = sex, y = charges)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "red") +
  labs(title = "Scatter Plot of Charges by Sex", x = "Sex", y = "Charges")
text(x = 1.5, y = max(insurance$charges) * 0.9, 
     labels = paste("p-value:", format(p_value, digits = 3)), col = "blue")

boxplot(charges ~ sex, data = insurance)


# Load necessary library
library(ggplot2)

# Perform t-test
t_test_result <- t.test(charges ~ sex, data = insurance)
p_value <- t_test_result$p.value

# Create boxplot with mean points and p-value annotation
ggplot(insurance, aes(x = sex, y = charges)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, color = "red") +
  annotate("text", x = 1.5, y = max(insurance$charges) * 0.9,
           label = paste("p-value:", format(p_value, digits = 3)), color = "blue") +
  labs(title = "Boxplot of Charges by Sex with Mean Points and p-value", x = "Sex", y = "Charges")


library(ggplot2)
ggplot(insurance, aes(x = smoker, y = charges)) +
  geom_violin()

# specify dataset and mapping
# add points and a best fit line to the plot
ggplot(
  data = insurance,
  mapping = aes(x = age, y = charges)
) +
  geom_point(
    color = "blue",
    alpha = 0.3
  ) +
  geom_smooth(method = "lm")

# add points to the plot, colored by the smoker variable
ggplot(
  insurance,
  aes(age,charges, color = smoker)
) +
  geom_point()+
 geom_smooth(method = "lm")



# add points to the plot, colored by the smoker variable, and faceted by the obese variable


ggplot(
  insurance,
  aes(age,charges,color = smoker)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~obese)

ggplot(
  insurance,
  aes(age,charges,color = smoker)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~obese)

# not much difference here

ggplot(
  insurance,
  aes(age,charges,color = smoker)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~obese)

ggplot(
  insurance,
  aes(age,charges,color = smoker)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(~region)

ggplot(
  insurance,
  aes(age,charges,color = smoker)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~region)


# add points to the plot, colored by the smoker variable, faceted by the obese variable, and add labels
ggplot(
  data = insurance,
  mapping = aes(x = age, y = charges, color = smoker)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~obese) +
  labs(
    title = "Medical Charges as a function of patient characteristics",
    subtitle = "US Census Bureau 2013 data",
    caption = "Source: https://github.com/stedy/Machine-Learning-with-R-datasets",
    x = "Age",
    y = "Annual Medical Charges",
    color = "Smoker?"
  )

# add points to the plot, colored by the smoker variable, faceted by the obese variable, add labels, and apply a minimal theme
ggplot(
  data = insurance,
  mapping = aes(x = age, y = charges, color = smoker)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~obese) +
  labs(
    title = "Medical Charges as a function of patient characteristics",
    subtitle = "US Census Bureau 2013 data",
    caption = "Source: https://github.com/stedy/Machine-Learning-with-R-datasets",
    x = "Age",
    y = "Annual Medical Charges",
    color = "Smoker?"
  ) +
  theme_minimal()

# save the plot to a file
ggsave("insurance_plot.png")

paste(c("X","Y"),1:10,sep="_")
paste0(c("X","Y"),1:10,sep="_")

grep('bcd',c('abcdef','abcd','bcde','cdef','defg'))
grepl('bcd', c('abcdef', 'abcd', 'bcde', 'cdef', 'defg'))
#grepl returns logic while grep returns the indices.


# Create a numeric vector called temperatures containing the following values: 72, 75, 78, 81, 76, 73, 93.

temperatures <- c(72, 75, 78, 81, 76, 73, 93)

# Create a character vector called days containing the following values: “Monday”, “Tuesday”, “Wednesday”, “Thursday”, “Friday”, “Saturday”, “Sunday”.

days <- c("Monday","Tuesday", "Wednesday", "Thursday","Friday","Saturday", "Sunday")

# Calculate the average temperature for the week and store it in a variable called average_temperature.

average_temperature <- mean(temperatures)

# Create a named vector called weekly_temperatures, where the names are the days of the week and the values are the temperatures from the temperatures vector.
weekly_temperatures <- temperatures
names(weekly_temperatures) <- days
# Create a numeric vector called ages containing the following values: 25, 30, 35, 40, 45, 50, 55, 60.

ages <- c(25, 30, 35, 40, 45, 50, 55, 60)
# Create a logical vector called is_adult by checking if the elements in the ages vector are greater than or equal to 18.
is_adult <- ages >= 18
# Calculate the sum and product of the ages vector.
sum_ages <- sum(ages)
product_ages <- prod(ages)
# Extract the ages greater than or equal to 40 from the ages vector and store them in a variable called older_ages.
older_ages <- subset(ages, ages >= 40)

## matrix exercises
data(sunspots)
class(sunspots) #ts
sunspot_mat <- matrix(as.vector(sunspots),ncol=12,byrow = TRUE)
colnames(sunspot_mat) <- as.character(1:12)
rownames(sunspot_mat) <- as.character(1749:1983)

dim(sunspot_mat)
summary(sunspot_mat)
class(sunspot_mat)

sunspot_mat[1:10,]
max(sunspot_mat)
hist(sunspot_mat, breaks = c(4*0:5, 10*3:5, 70, 100, 140))

# Error in hist.default(sunspot_mat, breaks = c(4 * 0:5, 10 * 3:5, 70, 100,  : 
#                                                 some 'x' not counted; maybe 'breaks' do not span range of 'x'

range(sunspot_mat) # 0.0 253.8

head(sunspot_mat)
colMeans(sunspot_mat)
apply(sunspot_mat,2,mean)
monthmeans <- colMeans(sunspot_mat)
summary(monthmeans)
plot(
  as.numeric(colnames(sunspot_mat)), 
  monthmeans, 
  type = "l",
  main = "Average Monthly Sunspots",
  xlab = "Month",
  ylab = "Average Sunspots"
) + axis(1, at = 1:12, labels = colnames(sunspot_mat))

yearmeans <- rowMeans(sunspot_mat)
plot(
  as.numeric(rownames(sunspot_mat)), 
  yearmeans, 
  type = "l",
  main = "Average Yearly Sunspots",
  xlab = "Year",
  ylab = "Average Sunspots"
)

plot(yearmeans)
my_list <- list(
  name = "John Doe",
  age = 30,
  scores = c(90, 85, 88),
  address = list(street = "123 Main St", city = "Anytown", zip = 12345),
  is_student = FALSE
)

# Apply the summary function to the list
summary(my_list)

# Apply the class function to the list
class(my_list)

insurance

insurance2 <- as.data.frame(insurance)