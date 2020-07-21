## ss3diags


The R package `ss3diags` enables users of Stock Synthesis with [`r4ss`](https://github.com/r4ss/r4ss) to reproduce the key model diagnostics plots that presented 'A Cookbook for Using Model Diagnostics in Integrated Stock Assessments' with [`ss3diags_CookbookPlots_v2.R`](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/ss3diags_CookbookPlots_v2.R). 

In addition, the ss3diags Github respository provides fully commented step-by-step R script recipes on how to:  

- [Do log-likelood  profiling for R0](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/Likelihood_profile_R0_example.R)
- [Setup an ASPM diagnostic run](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/Setup_ASPM_example.R)
- [Setup and run a restrospective analysis in Stock Synthesis](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/Run_Retrospective_example.R)
- [Do Jitter tests](https://github.com/jabbamodel/ss3diags/blob/master/Cookbook/Jitter_test_example.R)

..with Stock Synthesis by making of a comprehensive collection of R functions available in [`r4ss`](https://github.com/r4ss/r4ss)

## Installation 

Installing `ss3diags` requires the `librabry(devtools)`, which can be install by 'install.packages('devtools')' and a R version >= 3.5. Then simply install `ss3diags` from github with the command:

`install_github("jabbamodel/ss3diags")`

`library(ss3diags)`
