createProgramFeature = function(dfPartial){
  return(
    paste(dfPartial$school, dfPartial$department, dfPartial$day_night)
  )
}
summariseNumericFeature = function(targetFeature){
  return(
    list(
      mean = mean(targetFeature, na.rm=T),
      median = median(targetFeature, na.rm=T),
      range = range(targetFeature, na.rm=T)
    )
  )
}
summariseOneProgram <- function(dfPartial, oneProgram, numericalFeatures, summariseNumericFeature) {
  ## It's observations:
  dfPartial |> subset(dfPartial$program == oneProgram) -> oneProgramsObservations
  ## For each numerical feature
  summaryContainer <- vector("list", length(numericalFeatures))
  for (i in seq_along(numericalFeatures))
  {
    targetNumericalFeature <- numericalFeatures[[i]]
    oneProgramsObservations[[targetNumericalFeature]] -> targetFeature
    # here we only use oneProgramObservations
    summaryContainer[[i]] <- summariseNumericFeature(targetFeature)
    
    # This LINE is additional for referring convenience
    names(summaryContainer)[[i]] <- numericalFeatures[[i]]
  }
  return(summaryContainer)
}
summariseNumericFeature2 = function(targetFeature){
  targetFeatureWithNOmissingValue <- na.omit(targetFeature)
  
  if(length(targetFeatureWithNOmissingValue)==0) return(list(mean=NA, median=NA, range=c(NA,NA)))
  
  return(
    list(
      mean = mean(targetFeature, na.rm=T),
      median = median(targetFeature, na.rm=T),
      range = range(targetFeature, na.rm=T)
    )
  )
}