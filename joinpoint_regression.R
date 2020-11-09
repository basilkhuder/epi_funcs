create_jpr_options <- function(output_name,
                               Model = "ln",
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
                               Estimated_Joinpoints_Precision = 12){ 
  
  fn_formals <- rlang::fn_fmls()
  names(fn_formals)[8:9] <- c("Output by-group headers", "By-var format")
  parameters <- names(fn_formals)
  parameters <- gsub("_", " ", parameters)
  parameter_values <- as.character(fn_formals)
  parameter_values %<>% paste0(c(rep("\n", length(parameters) - 1), ""))
  parameters %<>% paste(parameter_values, sep = "=")
  jpoptions_ini_file <- paste0(output_name, "_JPOptions_")
  jpoptions_ini_file %<>% tempfile(fileext = ".ini")
  
  purrr::walk2(parameters, c(FALSE, rep(TRUE, length(parameters) - 1)),
               ~ cat(file = jpoptions_ini_file, .x, append = .y))
  
  return(jpoptions_ini_file)
  
  
}

create_jpr_run <- function(output_name, 
                           file){ 
  
  jprun_ini_file = paste0(output_name, "_JPRun_")
  jprun_ini_file %<>% tempfile(fileext = ".ini")
  file_input <- c("[Joinpoint Input Files]\n",
                  "Session File=User.Created.Session.ini\n",
                  paste0("Output File=",file, ".jpo\n"), 
                  paste0("Export Options File=", file, ".JPOptions.ini\n"),
                  paste0("Run Options File=", file, "JPOptions.ini"))
  
  purrr::walk2(file_input, c(FALSE, rep(TRUE, 4)), 
               ~ cat(file = jprun_ini_file, .x, append = .y))
  
  return(jprun_ini_file)
}
