#' @title Reconstruction Pipeline
#' @description
#' Run the baseline reconstruction pipeline for the
#' \eqn{i^{th}}
#'
#' @param i the index of a district to pull from
#' @param base_model_name the stem name of a model
#' @param wd_name the working directory name
#' @param impute_ty a text string to dispatch `impute_baseline_ty`
#' @param trust_ty a text string to dispatch `get_trust_ty`
#'
#' @returns a
#' @export
pipeline = function(i, base_model_name, wd_name, impute_ty="mean", trust_ty = "unmodified"){
  # useRDS
  model_file <- paste(wd_name, base_model_name, ".rds", sep="")
  print(model_file)
  model <- readRDS(model_file)

  # pull a time series
  prts <- get_district_pfpr_i(i)
  district_name <- prts$district_name[1]
  dir_name <- prts$dir_name[1]
  print(c(i=i, district_name=district_name))

  # set up the fitting
  model$location <- district_name
  model <- setup_fitting(model, prts$pfpr, prts$jdate)
  model <- pr2history(prts$pfpr, prts$jdate, model)
  filename1 <- paste(base_model_name, "_", dir_name, "_history.rds", sep="")
  saveRDS(model, paste(wd_name, filename1, sep=""))
  print(filename1)

  irs_history <- make_irs_history(district_name)
  bednet_history <- make_bednet_history(district_name)

  model <- reconstruct_baseline(model, prts$pfpr, prts$jdate,
                                irs_history, bednet_history,
                                impute_ty, trust_ty)

  filename2 <- paste(base_model_name, "_", dir_name, "_baseline.rds", sep="")
  saveRDS(model, paste(wd_name, filename2, sep=""))
  print(filename2)

  return(list(district_name = district_name,
              dir_name = dir_name,
              prts = prts,
              base_model = model,
              naive_history = filename1,
              reconstructed_history = filename2
              ))
}

#' @title Reconstruction Pipeline
#' @description
#' Run the baseline reconstruction pipeline for the
#' \eqn{i^{th}}
#'
#' @param i the index of a district to pull from
#' @param base_model_name the stem name of a model
#' @param wd_name the working directory name
#' @param impute_ty a text string to dispatch `impute_baseline_ty`
#' @param trust_ty a text string to dispatch `get_trust_ty`
#'
#' @returns a
#' @export
half_pipe = function(i, base_model_name, wd_name, impute_ty="mean", trust_ty = "unmodified"){
  # useRDS
  prts <- get_district_pfpr_i(i)
  district_name <- prts$district_name[1]
  dir_name <- prts$dir_name[1]
  print(c(i=i, district_name=district_name))

  filename1 <- paste(base_model_name, "_", dir_name, "_history.rds", sep="")
  model <- readRDS(paste(wd_name, filename1, sep=""))
  print(filename1)

  irs_history <- make_irs_history(district_name)
  bednet_history <- make_bednet_history(district_name)

  model <- reconstruct_baseline(model, prts$pfpr, prts$jdate,
                                irs_history, bednet_history,
                                impute_ty, trust_ty)

  filename2 <- paste(base_model_name, "_", dir_name, "_baseline.rds", sep="")
  saveRDS(model, paste(wd_name, filename2, sep=""))
  print(filename2)

  return(list(district_name = district_name,
              dir_name = dir_name,
              prts = prts,
              base_model = model,
              naive_history = filename1,
              reconstructed_history = filename2
  ))
}
