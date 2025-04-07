
write_model_to_loc = function(model, location){
  UseMethod("write_model_to_loc", model$frame)
}

write_model_to_loc.cohort = function(model, location){
  filename <- paste(location, model$location, "_eir_", model$model_name, ".rds", sep="")
  saveRDS(model, filename)
}

write_model_to_loc.full = function(model, location){
  filename <- paste(location, model$location, "_Lambda_", model$model_name, ".rds", sep="")
  saveRDS(model, filename)
}
