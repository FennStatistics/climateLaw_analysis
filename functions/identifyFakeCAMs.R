############################################################################
##### identifyFakeCAMs()
# save graphic object as png file
############################################################################
identifyFakeCAMs <- function(CAMdrawn, verbose = FALSE){
  vec_fakeCAMs <- rep(x = FALSE, times = length(CAMdrawn))
  vec_IDs_fakeCAMs <- rep(x = NA, times = length(CAMdrawn))
  for(i in 1:length(CAMdrawn)){
    tmpSingleWords <- stringr::str_split(string = V(CAMdrawn[[i]])$label , pattern = " ", simplify = TRUE)
    tmpSingleWords <- tmpSingleWords[tmpSingleWords != ""]
    tmpSingleWords <- unique(tmpSingleWords)
    tmpSingleWords <- tolower(tmpSingleWords)

    if(any(tmpSingleWords %in% dict_german)){
      if(verbose){
        cat('\nI am a real CAM - words found:\n')
        cat(tmpSingleWords[tmpSingleWords %in% dict_german])
      }
    }else{
      if(verbose){
        print("I am a fake CAM")
      }
      vec_fakeCAMs[i] <- TRUE
      vec_IDs_fakeCAMs[i] <- names(CAMdrawn)[i]
    }
  }
  vec_IDs_fakeCAMs <- vec_IDs_fakeCAMs[!is.na(vec_IDs_fakeCAMs)]


  return(list(bool_fakeCAMs = vec_fakeCAMs, ID_fakeCAMs = vec_IDs_fakeCAMs))
}