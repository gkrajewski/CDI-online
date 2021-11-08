prepareOutputAdaptative <- function(design, items, subject, lang, group, endDate){
  
  answered=design$person$items_answered[!is.na(design$person$items_answered)]
  
  
  a = data.frame("q_id" = 1:length(answered),
                 "items" = items[answered],
                 "answers" = design$person$responses[answered],
                 "theta" = design$person$thetas_history[2:length(design$person$thetas_history)],
                 "se_theta" = design$person$thetas_SE_history[2:length(design$person$thetas_SE_history)])
  
  a["final"] = 0
  a[a$q_id==length(answered), "final"] = 1
  
  a["idx"] = subject$id
  a["gender"] = subject$gender
  a["birth"] = subject$birth
  a["test"] = subject$test
  a["comment"] = subject[[paste0(group, "Comment")]]
  a["start_theta"] = subject[[paste0(group, "Theta")]]
  a["filler"] = paste0(subject$filler, subject$fillerTxt)
  a["lang"] = lang
  a["group"] = group
  a["start_date"] = as.POSIXct(subject[[paste0(group, "Start")]])
  a["end_date"] = endDate
  
  return(a)
}