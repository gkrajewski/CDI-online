chooseTheta <- function(startThetas, subjectGender, subjectAge, group) {
  if (!is.data.frame(get("startThetas"))) {
    return(0)
  } 
  
  if (!("group" %in% colnames(startThetas)) & !("age" %in% colnames(startThetas)) & !("gender" %in% colnames(startThetas))) {
    return(0)
  }
  
  if ("group" %in% colnames(startThetas)) {
    if (group %in%  startThetas$group) {
      groupFile = startThetas[startThetas$group==group, ]
    } else {
      return(0)
    }
  } else {
    groupFile=startThetas
  }
  
  if ("gender" %in% colnames(groupFile)) {
    if (subjectGender %in% groupFile$gender) {
      startThetasFile = groupFile[groupFile$gender==subjectGender, ]
    } else {
      if ("age" %in% colnames(groupFile)) {
        startThetasFile = groupFile %>% group_by(age) %>% summarise(theta=mean(theta))
      } else {
        return(0)
      }
    }
  } else {
    if ("age" %in% colnames(groupFile)) {
      startThetasFile = groupFile %>% group_by(age) %>% summarise(theta=mean(theta))
    } else {
      return(0)
    }
  }
  
  if ("age" %in% colnames(startThetasFile)) {
    #choose the closest age provided in StartThetasFile to the subject age
    ageDiff=abs(startThetasFile$age-subjectAge)
    idAge = which.min(ageDiff) 
    startTheta <- startThetasFile$theta[idAge]
  } else {
    startTheta <- startThetasFile[startThetasFile["gender"]==subjectGender, "theta"][1]
  }
  
  return(as.numeric(startTheta))
}