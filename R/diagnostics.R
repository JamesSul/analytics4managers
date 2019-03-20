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

lin_compare <- function(df, name_x, name_y, name_z = FALSE) {
  c1 <- deparse(substitute(name_x))
  c2 <- deparse(substitute(name_y))
  c3 <- deparse(substitute(name_z))
  if(c3 == "FALSE") {
    df %>% ggplot(aes(x = eval(parse(text = c1)), y = eval(parse(text = c2)))) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      xlab(c1) + ylab(c2) + ggtitle("Linear comparison") +
      theme_bw()
  }
  else {
    df %>% ggplot(aes(x = eval(parse(text = c1)), y = eval(parse(text = c2)),
                      color = eval(parse(text = c3)))) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      geom_smooth(method = "loess", se = FALSE, color = "blue") +
      xlab(c1) + ylab(c2) + ggtitle("Linear comparison") +
      labs(color = c3) +
      theme_bw()
  }
}
model_compare <- function(...) {
  model_list <- list(...)
  df <- glance(model_list[[1]])
  df$mname <- Reduce(paste, deparse(model_list[[1]]$call[[2]]))
  df <- df %>% select(mname, everything())
  for(i in 2:length(model_list)) {
    new_df <- glance(model_list[[i]])
    new_df$mname <- Reduce(paste, deparse(model_list[[i]]$call[[2]]))
    new_df <- new_df %>% select(mname, everything())
    df <- rbind(df, new_df)
  }
  df %>% select(mname, adj.r.squared, sigma, statistic, p.value, AIC, BIC)
}

assign1_test <- function(mdl) {
  test <- read_csv("http://www.jamessuleiman.com/teaching/datasets/boston_housing.csv")
  test2 <- test %>%
    add_predictions(mdl)
  mse <- test2 %>%
    summarize(MSE = mean((medv - pred)^2))
  if(class(mdl)[[1]] == "train")
    mdl = mdl$finalModel
  if(length(mdl$coefficients) > 6)
    stop('You must choose five or fewer predictors as stated in the assignment.')
  df <- glance(mdl)
  df <- df %>% select(adj.r.squared, BIC)
  df <- cbind(df, mse)
  title <- paste("mse: ", round(df$MSE, digits = 4), ", adjusted.r.squared",
                 round(df$adj.r.squared, digits = 4), ", BIC: ",
                 round(df$BIC, digits = 2))
  lin_compare(test2, pred, medv) +
    ggtitle(paste0(title))
}
