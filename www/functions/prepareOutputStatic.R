prepareOutputStatic <- function(answers, id, lang, form, run, endDate, typeItems, stringLimit){
  
  startDate = answers[answers$type=="none", "answer"]
  startDate = as.POSIXct(startDate)
  answers = answers[answers$type!="none", ]
  
  split_by_type <- function(answer_type, answer) {
    if (answer_type=="oneCheckboxGroup") {
      answer = strsplit(answer, " ")
    }
    else if(answer_type=="sentences" | answer_type=="demographic") {
      answer = strsplit(answer, "#")
    }
    else if(answer_type=="radio" | answer_type=="radioAlt" | answer_type=="checkboxAlt" |
            answer_type=="manyCheckboxGroups" ) {
      answer = strsplit(answer, ",")
    }
    return(answer)
  }
  
  answers$answer = mapply(split_by_type, answers$answer_type, answers$answer)
  
  #reversing answers order
  answers <- answers[nrow(answers):1, ]
  
  a = answers %>% mutate(question_id = row_number()) %>% unnest(answer)
  
  a = a %>% group_by(question_id) %>% mutate(answer_id = row_number())
  
  a$answer1 = a$answer
  a$answer2 = NA
  
  a$answer2[(a$answer_type=="manyCheckboxGroups" | a$answer_type=="checkboxAlt" ) & 
              (a$answer1=="1 2" | a$answer1=="2")] = 1
  a$answer2[(a$answer_type=="manyCheckboxGroups" | a$answer_type=="checkboxAlt" ) & 
              (a$answer1=="1" | a$answer1=="0") ] = 0
  a$answer1[(a$answer_type=="manyCheckboxGroups" | a$answer_type=="checkboxAlt" ) & 
              (a$answer1=="1 2" | a$answer1=="1")] = 1
  a$answer1[(a$answer_type=="manyCheckboxGroups" | a$answer_type=="checkboxAlt" ) & 
              (a$answer1=="2")] = 0
  
  a$answer=NULL
  
  a$answer_id = as.character(a$answer_id)
  a[a$answer_id==1 & a$answer_type=="demographic", "answer_id"] = "birth_date"
  a[a$answer_id==2 & a$answer_type=="demographic", "answer_id"] = "sex"
  a[a$answer_id==3 & a$answer_type=="demographic", "answer_id"] = "guardian"
  if (nrow(a[a$answer_id==4 & a$answer_type=="demographic", "answer_id"])>0){
    a[a$answer_id==4 & a$answer_type=="demographic", "answer_id"] = "guardian other"
  }

  
  a$answer1 = substr(a$answer1, 1, stringLimit)
  
  a["id"] = id
  a["lang"] = lang
  a["form"] = form
  a["run"] = run
  a["start_date"] = startDate
  a["end_date"] = endDate
  
  return(a)
  
}