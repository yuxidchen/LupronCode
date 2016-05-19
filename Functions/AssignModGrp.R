
AssignModGrp = function(ID, df, size) {

  pid = df[[ID]]
  df$ModGrp = 1
  for (i in 1:floor(length(pid)/size)) {
    id = sample(pid, size)
    df$ModGrp[df[[ID]] %in% id] = i
    pid = setdiff(pid, id)
  }
  return(df)
} 

