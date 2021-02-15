doPOST <- function(){
  
  url <- "https://apps.multilada.pl/graphql"
  
  body <- "
          curl --location --request POST '{URL}' \
          --header 'Content-Type: application/json' \
          --data-raw '{
          	"query":
              "mutation{
                upsertCdi(input:{
                  token:"{TOKEN}", 
                  cdi:{
                    done:false,
                    cdiTypeId:"wg",
                    childHash:"{HASH}", 
                    score: 0.0}}
                  ){
                  cdi{
                    pk,
                    cdiTypeId,
                    childHash,
                    score
                  }
                }
              }    "
          }
          '
          "
  
  
}