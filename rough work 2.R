library(palmerpenguins)
library(sloop)
otype(mtcars)
is.object(1:10)
is.object(mtcars)
attr(1:10,"class")

attr(mtcars,"class")
attributes(mtcars)

new_dog <- function(name,age,sleep_status){
  structure(
    list(
      name = name,
      age = age,
      sleep_status = sleep_status
    ),
    class = "dog"
  )
}

d <- new_dog(name = "Milo",age = 4, sleep_status = "asleep")
d

print(d)
d$name

is_awake <- function(x){UseMethod(is_awake)}

write.csv(results,"project3.csv")

df <- read_csv("project3.csv")
str(df)
glimpse(df)












