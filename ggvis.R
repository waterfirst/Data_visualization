
set_lib_paths <- function(lib_vec) {
  
  lib_vec <- normalizePath(lib_vec, mustWork = TRUE)
  
  shim_fun <- .libPaths
  shim_env <- new.env(parent = environment(shim_fun))
  shim_env$.Library <- character()
  shim_env$.Library.site <- character()
  
  environment(shim_fun) <- shim_env
  shim_fun(lib_vec)
  
}

set_lib_paths("C:/Program Files/R/R-4.3.2/library")
.libPaths()


#install.packages("ggvis")
library(ggvis)
p <- ggvis(mtcars, x = ~wt, y = ~mpg)

layer_points(p)
layer_points(ggvis(mtcars, x = ~wt, y = ~mpg))


mtcars %>%
  ggvis(x = ~wt, y = ~mpg) %>%
  layer_points()


library(dplyr)
mtcars %>%
  ggvis(x = ~mpg, y = ~disp) %>%
  mutate(disp = disp / 61.0237) %>% # convert engine displacment to litres
  layer_points()

mtcars %>% 
  ggvis(~mpg, ~disp, stroke = ~vs) %>% 
  layer_points()

mtcars %>% 
  ggvis(~mpg, ~disp, fill = ~vs) %>% 
  layer_points()
mtcars %>% 
  ggvis(~mpg, ~disp, size = ~vs) %>% 
  layer_points()

mtcars %>% 
  ggvis(~mpg, ~disp, shape = ~factor(cyl)) %>% 
  layer_points()
mtcars %>% 
  ggvis(~wt, ~mpg, fill := "red", stroke := "black") %>% 
  layer_points()

mtcars %>% 
  ggvis(~wt, ~mpg, size := 300, opacity := 0.4) %>% 
  layer_points()
mtcars %>% 
  ggvis(~wt, ~mpg, shape := "cross") %>% 
  layer_points()

mtcars %>% 
  ggvis(~wt, ~mpg, 
        size := input_slider(10, 100),
        opacity := input_slider(0, 1)
  ) %>% 
  layer_points()

mtcars %>% 
  ggvis(~wt) %>% 
  layer_histograms(width =  input_slider(0, 2, step = 0.10, label = "width"),
                   center = input_slider(0, 2, step = 0.05, label = "center"))


keys_s <- left_right(10, 1000, step = 50)
mtcars %>% ggvis(~wt, ~mpg, size := keys_s, opacity := 0.5) %>% layer_points()

mtcars %>% ggvis(~wt, ~mpg) %>% 
  layer_points() %>% 
  add_tooltip(function(df) df$wt)

mtcars %>% ggvis(~wt, ~mpg) %>% layer_points()

df <- data.frame(x = 1:10, y = runif(10))
df %>% ggvis(~x, ~y) %>% layer_paths()

t <- seq(0, 2 * pi, length = 100)
df <- data.frame(x = sin(t), y = cos(t))
df %>% ggvis(~x, ~y) %>% layer_paths(fill := "red")


mtcars %>% 
  ggvis(~wt, ~mpg) %>% 
  layer_smooths(stroke := "red") %>% 
  layer_points()

mtcars %>% ggvis(~wt, ~mpg) %>%
  layer_smooths(span = 1) %>%
  layer_smooths(span = 0.3, stroke := "red")
