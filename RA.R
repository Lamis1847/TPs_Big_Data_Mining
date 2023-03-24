library(arules)
data=list(c("Po","L","Pa","F"),c("L","Pa","F"),c("Pa","F"),c("Po","Pa","F"))
names(data)=paste("tr",c(1:4),sep = " ")
data
///////
$`tr 1`
[1] "Po" "L"  "Pa" "F" 

$`tr 2`
[1] "L"  "Pa" "F" 

$`tr 3`
[1] "Pa" "F" 

$`tr 4`
[1] "Po" "Pa" "F" 
//////////////
transactions =as(data,"transactions")
transactions
/////////
transactions in sparse format with
 4 transactions (rows) and
 4 items (columns)
//////
itemFrequencyPlot(transactions)
rules<-apriori(transactions,parameter=list(support=0.3,confidence=0.7))
summary(rules)
inspect(rules)
///////////
  lhs         rhs  support confidence coverage lift count
[1]  {}       => {F}  1.0     1          1.0      1    4    
[2]  {}       => {Pa} 1.0     1          1.0      1    4    
[3]  {L}      => {F}  0.5     1          0.5      1    2    
[4]  {L}      => {Pa} 0.5     1          0.5      1    2    
[5]  {Po}     => {F}  0.5     1          0.5      1    2    
[6]  {Po}     => {Pa} 0.5     1          0.5      1    2    
[7]  {F}      => {Pa} 1.0     1          1.0      1    4    
[8]  {Pa}     => {F}  1.0     1          1.0      1    4    
[9]  {F, L}   => {Pa} 0.5     1          0.5      1    2    
[10] {L, Pa}  => {F}  0.5     1          0.5      1    2    
[11] {F, Po}  => {Pa} 0.5     1          0.5      1    2    
[12] {Pa, Po} => {F}  0.5     1          0.5      1    2  
//////////////
image(transactions)
inspect(transactions)


