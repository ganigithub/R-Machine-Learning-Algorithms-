#install.packages("Rserve")
library(Rserve)
Rserve()

#install.packages('tidyverse')

# The 'tidyverse' is a set of packages that work in harmony because they share common data representations and 'API' design.
#This package is designed to make it easy to install and load multiple 'tidyverse' packages in a single step

library(tidyverse)

mpg

view(mpg)

head(mpg)

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, size = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, shape = class))

ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) +facet_wrap(~ class, nrow = 4)

ggplot(data = mpg) +geom_point(mapping = aes(x = displ, y = hwy)) +facet_grid(drv ~ cyl)

ggplot(data = mpg) +geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) +geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))


ggplot(data = mpg) +geom_smooth(mapping = aes(x = displ, y = hwy, color = drv))

#multiple geom functions:

ggplot(data = mpg) +geom_point(mapping = aes(x = displ, y = hwy)) + geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) + geom_smooth()

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_point(mapping = aes(color = class)) +
        geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

#bar plot:

ggplot(data = diamonds) + geom_bar(mapping = aes(x = cut))


# install.packages('mvoutlier')
