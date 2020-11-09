create_jpr_options <- function(Model = "ln",
                               Maximum_joinpoints = 3,
                               Num_Cores = 1,
                               Models = "best fit",
                               Line_delimeter = "unix",
                               Missing_character = "period",
                               Field_delimeter = "tab",
                               Output_by_group_headers = "false",
                               By_var_format = "numeric",
                               APC_Precision = 12,
                               AAPC_Precision = 12,
                               P_Value_Precision = 5,
                               X_Values_Precision = 5,
                               Y_Values_Precision = 12,
                               Remove_best_fit_flags = "true",
                               AAPC_Last_obs = "true",
                               Estimated_Joinpoints_Precision = 12,
                               ...){ 
  
  argg <- c(as.list(environment()), list(...))
  names(argg)[[8]] <- "Output by-group headers"
  names(argg)[[9]] <- "By-var format"
  parameters <- names(argg)
  parameters <- gsub("_", " ", parameters)
  parameter_values <- as.character(argg)
  parameter_values %<>% paste0(c(rep("\n", length(parameters) - 1), ""))
  parameters %<>% paste(parameter_values, sep = "=")
  jpoptions_ini_file = paste0(output_name, ".JPOptions.ini")
  
  purrr::walk2(parameters, c(FALSE, rep(TRUE, length(parameters) - 1)),
               ~ cat(file = jpoptions_ini_file,
                     .x,
                     append = .y))
  }

joinpoint_regression <- function(file, 
                                 ind_var, 
                                 AAR, 
                                 SE) { 
  
  
}
