#TidyR data wrangling workshop
#27 april 2023

library(tidyverse)
library(readxl)
getwd()

iris <- as_tibble(iris) #views the data frame 'iris' as a tibble for efficiency, but doesn't replace
                        #the original data frame
head(iris)

#drop_na(data, ...) drops rows that contain NA's in columns

iris_NA <- iris
iris_NA[c(3,7,23), 1] <- NA  #[ ] tell R to take a specific subset
iris_NA[c(10,14,24), 2] <- NA

#%>% means "and then...", allows for chaining multiple functions in one line of text


#drop rows from your dataset with missing values
 iris_drop_na <- iris_NA %>%
   drop_na() #having the empty () says you want R to look at the whole data tibble
 
 #ways to reshape data wide to long
 
 #extract specific columns by their header
 iris_long <- iris %>%
   pivot_longer(cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                names_to = "Variable",
                values_to = "Value")
 
 #reshape dataset based on column position
 iris_long <- iris %>%
   pivot_longer(cols = (1:4),
                names_to = "Variable",
                values_to = "Value")
 
 #pivot columns excluding Species from the pivot 
 iris_long <- iris %>%
   pivot_longer(cols = -Species,
                names_to = "Variable",
                values_to = "Value")
 
 #reshape data to go from long to wide
 
 #reshape (pivot) the iris data from wide to long format
 iris_long <- iris %>%
   pivot_longer(cols = (1:4),
                names_to = "Variable",
                values_to = "Value")
 #reshape the iris dataset from long to wide and find the mean of each measurement
 iris_wide <- iris_long %>%
   pivot_wider(id_cols = "Species",
               names_from = "Variable",
               values_from = "Value",
               values_fn = (Value = mean))

 
 #unite mean Length and Width columns into a new column named Length_Width
 iris_unite <- iris_wide %>%
  unite(col = "Sepal_Length_Width", Sepal.Length, Sepal.Width,
        sep = "_")

 #separate the Variable column back into the Feature and Measurement column
 iris_sep <- iris_long %>%
   separate(col = Variable,
            into = c("Feature",
                     "Measurement"),
            sep = "\\.") #tells tidyR that the period is a character

 #gather Sepal and Petal measurements into key-value pairs
 #gather(data, key =, value =, x, y, z)

 iris_gathered <- iris %>%
   gather(key = "Variable",
          value = "Value",
          "Sepal.Length", 
          "Sepal.Width", 
          "Petal.Length", 
          "Petal.Width")
 
 #subset the dataset by column names (species, sepal.length, sepal.width)
 iris_sepal <- iris %>%
   select(Species, Sepal.Length, Sepal.Width)
 head(iris_sepal)

 #subset the dataset by excluding columns (species, sepal.length, petal.length)
 iris_length <- iris %>%
   select(-2,-4) #excludes columns 2 and 4
 view(iris_length)
 
 #subset columns based on name patterns using starts_with(), ends_with(), contains(), and matches()
 iris_width <- iris %>%
   select(Species, contains("Width"))
view(iris_width) 

#subset iris data using column positions, with species in 1st col, petal.length in 2nd col, petal.width in 3rd col
 iris_petal <- iris %>%
   select(5, 3:4)
 view(iris_petal)

 #filter the iris dataset for only the versicolor species
 iris_versicolor <- iris %>%
   filter(Species == "versicolor")

 #group the dataset by Species and summarize the mean Petal.Length
 iris_summ <- iris %>%
   group_by(Species) %>%
   summarise(mean_Petal_Length = mean(Petal.Length))

 #creating new variables with mutate()
 #create new variable to define the petal area
 petal_area <- iris_petal %>%
   mutate(Petal.Area = Petal.Length*Petal.Width)
head(petal_area) 
 
#mutate a new variable for sepal_area
sepal_area <- iris_sepal %>%
  mutate(Sepal.Area = Sepal.Length*Sepal.Width)
head(sepal_area)

#combining tables
#combine petal_area with the iris dataset using left_join()
iris_left <- petal_area %>%
  select(1,4) %>%
  left_join(iris)
head(iris_left) 
#combine sepal_area with the iris dataset using full_join()
iris_full <- sepal_area %>%
  full_join(iris)
head(iris_full)

#combine multiple dataframes using bind_cols()
iris_select <- iris %>%
  select(Sepal.Length, Sepal.Width)
iris_mutate <- iris_select %>%
  mutate(sepal_ratio = Sepal.Length/Sepal.Width)
iris_species <- iris %>%
  select(Species)
iris_col <- iris_mutate %>%
  select(3)
bind_cols(iris_species, iris_select)
head(iris_col)


