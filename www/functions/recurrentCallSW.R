recurrentCallSW <- function(done = "false", i = 1, maxI = 3){
  
  callSuccess <- callSW(done)
  if (!callSuccess & i < maxI){
    i <- i + 1
    delay(3000, recurrentCallSW(done, i))
  } else if (i == maxI) {
    print("ERROR: Cannot make successfull call")
  }
  
}