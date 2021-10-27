#' Chi-squared Test Report
#'
#' @param file_name Enter the name of your dataset in quotes
#'
#' @return
#' @export
#'
#' @examples
test3_chi <- function(file_name){
  file_name1 <- read_csv(file_name , show_col_types = FALSE)
  test3_hypothesis <- function(file_name){
    cat(green("Null Hypothesis [H0]:") , "Gender and amount of physical activity are independent of each other","\n" ,green("Alternate Hypothesis [H1]:") , "Gender and amount of Physical activity are not independent of each other.", sep = "")
  }
  test3_tstat <- function(file_name){
    file3 <- file_name1 %>%
      select(gender, phys) %>%
      mutate(freqneeded =case_when((gender == 'Male') & (phys == 'None') ~  "MaleNone",
                                   (gender == 'Female') & (phys == 'None') ~  "FemaleNone",
                                   (gender == 'Male') & (phys == 'Moderate') ~  "MaleModerate",
                                   (gender == 'Female') & (phys == 'Moderate') ~  "FemaleModerate",
                                   (gender == 'Male') & (phys == 'Intense') ~  "MaleIntense",
                                   TRUE ~  "FemaleIntense"))
    count1 <- count(file3, freqneeded)
    count2 <- as.vector(count1$n , mode = "numeric")
    FemaleInt <- count2[1]
    FemaleMod <- count2[2]
    FemaleNon <- count2[3]
    MaleInt <- count2[4]
    MaleMod <- count2[5]
    MaleNon <- count2[6]
    matrix4 <- matrix(0, nrow = 2, ncol = 3)
    matrix4[1,1] = matrix4[1,1] + MaleNon
    matrix4[1,2] = matrix4[1,2] + MaleMod
    matrix4[1,3] = matrix4[1,3] + MaleInt
    matrix4[2,1] = matrix4[2,1] + FemaleNon
    matrix4[2,2] = matrix4[2,2] + FemaleMod
    matrix4[2,3] = matrix4[2,3] + FemaleInt
    rownames(matrix4) <- c("Male" , "Female")
    colnames(matrix4) <- c("None" , "Moderate" , "Intense")
    chisq_test <- chisq.test(matrix4, correct = FALSE)
    matrix5 <- matrix(, nrow = 3, ncol =1)
    rownames(matrix5) <- c("Test-statistic" , "Degrees of Freedom" , "P-Value")
    colnames(matrix5) <- "Values"
    matrix5[1,1] = statchi <- signif(chisq_test$statistic, digits = 3)
    matrix5[2,1] = dfchi <- signif(chisq_test$parameter, digits = 3)
    matrix5[3,1] = p_value_chi <<- signif(chisq_test$p.value, digits = 3)
    print(kable(matrix5, caption = "X-Squared Test Statistic, Degree of Freedom, P-Value"))
  }
  test3_decision <- function(file_name){
    (if(p_value_chi<0.05){glue::glue("Since the P-value = " , p_value_chi ," from the chi-squared test is very small, we reject the Null Hypothesis")
    }else{glue::glue("Since the P-value = ",p_value_chi ," from the chi-squared test is more than 0.05, we don't reject the Null Hypothesis" )}) %>%
      print()
  }
  test3_conclusion <- function(file_name){
    (if(p_value_chi<0.05){
      glue::glue("As P-value is less than 0.05 at " ,p_value_chi, ", we have  significant evidence against the Null Hypothesis.\nThere is strong evidence to suggest that gender and amount of physical activity are not independent of each other")
    } else{
      glue::glue("As P-value is larger than 0.05 at " ,p_value_chi, ", we have significant evidence in support of the Null Hypothesis.\nThere is strong evidence to suggest that gender and amount of physical activity are independent of each other")
    }) %>%
      print()
  }
  test3_wrapper <- function(file_name){
    list(cat(red$bold$underline("Formulating Hypothesis:\n")) , test3_hypothesis(file_name) , cat("\n\n") , cat(red$underline$bold("Running necessary statistical analysis and reporting the results...")) , test3_tstat(file_name) , cat("\n\n") , cat(red$underline$bold("Announcing Decision:\n\n")) , test3_decision(file_name) , cat("\n\n") , cat(red$underline$bold("Presenting Conclusion:\n\n")) , test3_conclusion(file_name))
    invisible(ftype(print))
  }
  return(test3_wrapper(file_name))
}
test3_chi("project.csv")


