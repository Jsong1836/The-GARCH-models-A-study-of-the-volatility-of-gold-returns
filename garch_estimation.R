library(rugarch)
library(quantmod)
library(timetk)
library(dplyr)

data = function(){
  
  getSymbols("GOLDAMGBD228NLBM", src = "FRED", from = "1860-01-01", to = "2019-06-30")
  gold_atb =  timetk::tk_tbl(GOLDAMGBD228NLBM)
  gold_atb = na.omit(gold_atb)
  
  gold = gold_atb %>%
    dplyr::mutate(returns = log(GOLDAMGBD228NLBM) - log(dplyr::lag(GOLDAMGBD228NLBM))) %>%
    na.omit() %>%
    dplyr::filter(index < "2019-06-30")
  
  gold$index = as.Date(gold$index, format = "%Y-%m-%d")
  gold = dplyr::arrange(gold, index)
  colnames(gold) = c("date", "gold", "returns")
  
  sample = round(dim(gold)[1] * 0.8)
  train = gold[1:sample, ]
  test = gold[sample:dim(gold)[1], ]  
  
  return(list(train, test))
}

train = as.data.frame(data()[1])
test = as.data.frame(data()[2])
df = as.data.frame(dplyr::bind_rows(train, test))

garch = function(distribution){
  
    lists = list("eGARCH", "gjrGARCH", "apARCH", "IGARCH")
    for (i in 1:length(lists))
    {
       spec = rugarch::ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                                  variance.model = list(garchOrder = c(1, 1), model = as.character(lists[i]))
                                  , distribution.model = as.character(distribution))
       fit = rugarch::ugarchfit(spec = spec, data = train$returns)
       return(fit)
  }
}
  
backtests = function(distribution){
    
  lists = list("eGARCH", "gjrGARCH", "apARCH", "IGARCH")
    for (i in 1:length(lists))
      {
      par(mfrow = c(2, 2))
      spec = rugarch::ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                                 variance.model = list(garchOrder = c(1, 1), model = as.character(lists[i]))
                                 , distribution.model = as.character(distribution))
      
      roll = rugarch::ugarchroll(spec = spec, df$returns, n.start = (dim(train)[1] - 1), refit.every = 100, 
                                 refit.window = "moving", solver = "hybrid", 
                                 calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.05), keep.coef = TRUE)
      roll.output = roll@forecast$VaR
      roll.output = roll.output[-1, ]
      date = as.Date(test$date)
      colnames(roll.output) = c("alpha", "alpha1", "real")
      var.xts = xts::xts(roll.output, order.by = date)
      VaRplot(alpha = quantile(0.99), actual = var.xts$real, VaR = var.xts$alpha1)
      }
}
 

