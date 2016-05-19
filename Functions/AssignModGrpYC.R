# modified so that when the remaining physicians to randomly sample from is less than sample, stop.

AssignModGrpYC = function(ID, df, size) {

  pid = df[[ID]]
  df$ModGrp = 1
  for (i in 1:floor(length(pid)/size)) {
    if (length(pid) >= size){
      id = sample(pid, size)
      df$ModGrp[df[[ID]] %in% id] = i
      pid = setdiff(pid, id)
    } else 
      df$ModGrp[df[[ID]] %in% pid] = i
    
  }
  return(df)
} 

