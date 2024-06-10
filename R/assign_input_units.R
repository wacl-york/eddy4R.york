assign_input_units = function(eddy.data,
                              units){


  # temperature
  base::attr(x = eddy.data$tempAir, which = "unit") <- units$temp

}
