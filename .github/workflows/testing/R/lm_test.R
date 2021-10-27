#' Linear Regression Analysis Report
#'
#' @param file_name Enter the name of your dataset in quotes
#'
#' @return
#' @export
#'
#' @examples test1_lm("project.csv")
test1_lm <- function(file_name){
  file_name1 <- read_csv(file_name , col_types = cols())
  test1_hypothesis <- function(file_name){
    cat(green("Null Hypothesis: ") , "β = 0\n" , green("Alternate Hypothesis: ") , "β ≠ 0\n" , sep = "")
  }
  test1_assumption <- function(file_name){
    lmdata <<- lm(weight ~ height , file_name1)
    a1 <- ggplot(data = lmdata)+
      geom_point(aes(x = lmdata$fitted.values, y = lmdata$residuals), color = 'steelblue')+
      labs(x = "Fitted Values", y = "Residual Values", title = "Residuals vs Fitted")
    a2 <- ggplot(data = file_name1)+
      geom_point(aes(x = height, y = weight),color = 'steelblue')+
      labs(title = "Weight vs Height")
    a3 <- ggplot(data = lmdata, aes(x = lmdata$residuals))+
      geom_histogram(fill = 'steelblue', color = 'black', binwidth = 10)+
      labs(title = 'Histogram of Residuals', x = 'residuals')
    print(a2+a1+a3)
  }
  test1_tstat <- function(file_name){
    model_summary <- summary(lmdata)
    modelcoeffs <- model_summary$coefficients
    std.error <- modelcoeffs["height", "Std. Error"]
    CI_lm <<- confint(lmdata, 'height', level = 0.95)
    matrix1 <- matrix(0, ncol = 1, nrow = 6)
    rownames(matrix1) <- c("Beta Estimate" , "CI (2.5%)" ,"CI (97.5%)" , "T-value" , "Degrees of Freedom" , "P-value")
    colnames(matrix1) <- ("Values")
    matrix1[1,1] = matrix1[1,1] + signif((beta.estimate <<- modelcoeffs["height", "Estimate"]),digits = 3)
    matrix1[2,1] = matrix1[2,1] + signif(CI_lm[1,1], digits = 3)
    matrix1[3,1] = matrix1[3,1] + signif(CI_lm[1,2],digits = 3)
    matrix1[4,1] = matrix1[4,1] + signif((t_value_lm <- beta.estimate/std.error),digits = 3)
    matrix1[5,1] = matrix1[5,1] + signif((df_lm <- model_summary$df[2]),digits = 3)
    matrix1[6,1] = matrix1[6,1] + signif((p_value_lm <<- modelcoeffs["height",4]),digits = 7)
    table1 <- as.table(matrix1)
    print(kable(table1, caption = "Necessary values required for linear regression" ))
  }
  test1_decison <- function(file_name){
    print(if(p_value_lm<0.05){
      glue::glue("Since the" , bold(" P-value = ") , bold(signif(p_value_lm , digits = 3)) , " from the liner regression model is very small, " , underline("we reject the Null Hypothesis."), sep = "")
    }
    else{glue::glue("Since the P-value = ", bold(signif(p_value_lm , digits = 3)) ," from the linear regression model is more than 0.05, " , underline("we don't reject the Null Hypothesis.") , sep = "")})
  }
  test1_conclusion <- function(file_name){
    (if((p_value_lm<0.05) && (beta.estimate<0)){
      glue::glue("We reject the null hypothesis at 5% significance level because the p-value = ", signif(p_value_lm , digits = 3)," is less than 0.5. \nThere is a " , underline("negative linear relationship between height and weight") , " because the beta estimate is less than 0. \nGiven the data, we are 95% confident that with an increase in height, the expected loss in weight is between " ,signif(CI_lm["height" , "2.5 %"], digits = 3), " and " ,signif(CI_lm["height" , "97.5 %"] , digits = 3), " kgs.")
    }else{
      if((p_value_lm<0.05) && (beta.estimate>0)){
      glue::glue("We reject the null hypothesis at 5% significance level because the p-value = ", signif(p_value_lm , digits = 3)," is less than 0.5. \nThere is a " , underline("positive linear relationship between height and weight") , " because the beta estimate is greater than 0. \nGiven the data, we are 95% confident that with an increase in height, the expected gain in weight is between " ,signif(CI_lm["height" , "2.5 %"], digits = 3), " and " ,signif(CI_lm["height" , "97.5 %"] , digits = 3), " kgs.")
      }else{
        glue:glue("We do not reject the null hypothesis at 5% significance because the p-value = ", signif(p_value_lm , digits = 3) ," is greater than 5%. \nThere is no linear relationship between Y and X.")}
    }) %>%
      print()
  }
  test1_wrapper <- function(file_name){
    list(cat(bold$red("Q1: Is there a linear relationship between height and weight?\n")) ,
         cat(bold$red("Formulating Hypothesis:\n")) ,
    test1_hypothesis(file_name) ,
    cat("\n") ,
    cat(bold$red("Plotting graphs in Plots section to check assumptions...\n")) ,
    test1_assumption(file_name) ,
    cat("\n") ,
    cat(bold$red("Running necessary statistical analysis and reporting results...\n")) ,
    test1_tstat(file_name) ,
    cat("\n") ,
    cat(bold$red("Announcing Decision:\n")) ,
    test1_decison(file_name) ,
    cat("\n") ,
    cat(bold$red("Presenting Conclusion:\n")) ,
    test1_conclusion(file_name))
    invisible(ftype(print))
  }
  return(test1_wrapper(file_name))
}
test1_lm("project.csv")
