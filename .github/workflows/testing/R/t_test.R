#' 2 sample T-test Report
#'
#' @param file_name Enter the name of your dataset in quotes
#'
#' @return
#' @export
#'
#' @examples
test2_t <- function(file_name){
  file_name1 <- read_csv(file_name , col_types = cols())
  test2_hypothesis <- function(file_name){
    cat(green$bold("Null Hypothesis [H0]:") , " The " , underline("mean height of males and females is the same") , " i.e. the difference between the mean height of males and females is 0.\n" , green$bold("Alternate Hypothesis [H1]:") , "The " , underline("mean height of males and females is different"), " i.e. the difference between the mean height of males and females is not equal to 0.", sep = "")
  }
  test2_assumption <- function(file_name){
    a2 <- ggplot(data = file_name1, aes(sample = height)) +
      geom_qq() +
      geom_qq_line() +
      facet_wrap(~gender)+
      labs(title = "QQPlot to check normality", x = "Height")

    a3 <- ggplot(data = file_name1, aes(y = height, x = gender)) +
      geom_boxplot() +
      coord_flip() +
      labs(x = "Gender", y = "Height" , title = "Clustured boxplot to assume equal spread")
    print(a2+a3)
  }
  test2_tstat <- function(file_name){
    maledataset <<- file_name1 %>%
      pivot_wider( names_from = gender, values_from = height) %>%
      select(Male) %>%
      na.omit()
    maledataset <<- as.vector(maledataset$Male)
    femaledataset <<- file_name1 %>%
      pivot_wider( names_from = gender, values_from = height) %>%
      select(Female) %>%
      na.omit()
    femaledataset <<- as.vector(femaledataset$Female)
    ttest <<- t.test(maledataset, femaledataset,var.equal = TRUE, conf.level = .95)
    CIttest <<- ttest$conf.int
    matrix2 <- matrix(0, ncol = 1, nrow = 5)
    matrix2[1,1] = matrix2[1,1] + round((ttest_statistic <<- ttest$statistic),2)
    matrix2[2,1] = matrix2[2,1] + round((dfttest <<- ttest$parameter),2)
    matrix2[3,1] = matrix2[3,1] + round((p_value_ttest <<- ttest$p.value),2)
    matrix2[4,1] = matrix2[4,1] + round((CIttest[1]),2)
    matrix2[5,1] = matrix2[5,1] + round((CIttest[2]),2)
    rownames(matrix2) <- c("Test-Statistic" , "Degree of Freedom" , "P-Value" , "Lower Confidence Interval" , "Upper Confidence Interval")
    colnames(matrix2) <- "Values"
    table2 <- as.table(matrix2)
    print(kable(table2, caption = "Test-statistic, p-value, degree of freedom and Confidence Interval"))
  }
  test2_decision <- function(file_name){
    (if(p_value_ttest<0.05){glue::glue("Since the P-value =" , signif(p_value_ttest , digit = 3) , " from the t-test is less than 0.05, we can reject the null hypothesis.The mean height of males and females are different")
    }else {glue::glue("Since the P-value =", signif(p_value_ttest , digit = 3) ," from the t-test is more than 0.05, we don't reject the Null Hypothesis. The mean height of males and the mean of females in the same")}) %>%
      print()
  }
  test2_conclusion <- function(file_name){
    (if(p_value_ttest<0.5){
    glue::glue("We can conclude at the 5% level of significance that there is significant evidence against the null hypothesis, i.e. the mean height of males is not the same as the mean height of females. \nWe are 95% confidence that the interval (" ,signif(CIttest[1] , digits = 3), "," ,signif(CIttest[2], digits = 3), ") includes the true \ndifference between the means", sep = "")
  } else{
    glue::glue("We can conclude at the 5% level of significance that there is significant evidence supporting the null hypothesis, i.e. the mean height of males is the same as the mean height of females", sep ="")
  }) %>%
      print()
  }

  test2_wrapper <- function(file_name){
    list(cat(bold$underline$red("Formulating Hypothesis:\n")) , test2_hypothesis(file_name) , cat("\n\n") , cat(red$bold("Plotting graphs to check the assumptions of a 2 sample t-test...\n\n")) , test2_assumption(file_name) , cat(red("Performing Data Wrangling...\n")) , cat(red("creating male and female dataset...\n\n")) , cat(red$bold$underline("Running necessary statistical analysis and reporting results...")) , test2_tstat(file_name) , cat("\n") , cat(red$bold$underline("Announcing Decision:\n\n")) , test2_decision(file_name) , cat("\n") , cat(red$bold$underline("Presenting Conclusion\n\n")) , test2_conclusion((file_name)))
    invisible(ftype(print))
  }
  return(test2_wrapper(file_name))
}
test2_t("project.csv")

