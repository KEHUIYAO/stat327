how.many=function(item,n.max){
  sign=TRUE
  cat("How many ",item,"?",'\n',sep = '')
  input=scan(nmax = 1,quiet = TRUE)
  while (sign){
    if (input>n.max){
      cat("ERROR: too many for the budget \n")
      cat("How many ",item,"?",'\n',sep = '')
      input=scan(nmax = 1,quiet = TRUE)
      next()
    }
    sign=FALSE
  }
  return (input)
  
}

grocery.list=function(file,budget){
  price_list=read.csv(file,header = F)
  
  colnames(price_list)=c("item","price")
  print(price_list)
  price_list$item=as.character(price_list$item)
  price_list$quantity=0
  for (i in 1:nrow(price_list)){
    tmp=price_list[i,]
    if (tmp$price>budget){
      next()
    }else{
      n.max=floor(budget/tmp$price)
      price_list$quantity[i]=how.many(tmp$item,n.max)
      budget=budget-price_list$quantity[i]*tmp$price
    }
  }
  
  return (price_list[price_list$quantity!=0,])
  
}

shopping_list=grocery.list("groceries.csv",10)
total_bill=t(shopping_list$price)%*%shopping_list$quantity
print(shopping_list)
cat("Your bill is ",total_bill,sep = '')
