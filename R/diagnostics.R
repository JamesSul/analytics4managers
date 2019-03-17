# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

resid.vs.fitted <- function(model) {
  df <- augment(model)
  df %>% ggplot(aes(x = .fitted, y = .resid)) +
    geom_point(size = 2) +
    geom_smooth(method = "loess", color = "blue", se = FALSE) +
    ggtitle("Residuals vs. Fitted") +
    xlab("Fitted values") +
    ylab("Residuals") +
    theme_bw()
}

lin_compare <- function(df, name_x, name_y) {
  c1 <- deparse(substitute(name_x))
  c2 <- deparse(substitute(name_y))
  df %>% ggplot(aes(x = eval(parse(text = c1)), y = eval(parse(text = c2)))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    xlab(c1) + ylab(c2) + ggtitle("Linear/LOESS comparison") +
    theme_bw()
}

lin_compare <- function(df, name_x, name_y, name_z) {
  c1 <- deparse(substitute(name_x))
  c2 <- deparse(substitute(name_y))
  c3 <- deparse(substitute(name_z))
  df %>% ggplot(aes(x = eval(parse(text = c1)), y = eval(parse(text = c2)),
                    color = eval(parse(text = c3)))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    geom_smooth(method = "loess", se = FALSE, color = "blue") +
    xlab(c1) + ylab(c2) + ggtitle("Linear/LOESS comparison") +
    theme_bw()
}
