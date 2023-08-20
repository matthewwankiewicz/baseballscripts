ip_to_outs <- function(ip){
  
  ip_split <- strsplit(as.character(ip),"\\.")
  
  if(length(ip_split[[1]]) == 1){
    as.numeric(ip_split[[1]][1])*3
  }
  
  else{
    as.numeric(ip_split[[1]][1])*3 + as.numeric(ip_split[[1]][2])
  }
  
  
}
