

peso_btc = function(btc_weight,eth_weight,btc_pct_change,eth_pct_change){ #função1
  new_btc_value = btc_weight*(1 + btc_pct_change)
  new_eth_value = eth_weight*(1 + eth_pct_change)
  
  (new_btc_value/(new_btc_value + new_eth_value))*100
  
}

peso_btc(0.7,0.3,0.12,0.05) #retorno peso btc

peso_eth = function(btc_weight,eth_weight,btc_pct_change,eth_pct_change) { #função2
  new_btc_value = btc_weight*(1 + btc_pct_change)
  new_eth_value = eth_weight*(1 + eth_pct_change)
  
  (new_eth_value/(new_btc_value + new_eth_value))*100
}
peso_eth(0.7,0.3,0.12,0.05) #retorno peso eth


df = data.frame("Weights" = c(0.33, 0.33,0.33),
                  "Changes" =c(0.15, 0.08,0.2))

#pesos_novos = function( for (i in 1:length(df)) {
  
})
  
new_value = matrix(ncol = 1, nrow = nrow(df))  
novo_peso = matrix(ncol=1, nrow = nrow(new_value))
  
for (i in 1:nrow(df)) {
    new_value[i,] = df[i,1]*(1+df[i,2])
  }
  new_value
 
   for (j in 1:nrow(df)) {
    novo_peso[j,] = new_value[j,]/sum(new_value[,1])
  }
  novo_peso
#############################
  funcao_bonus = function(x) {
    
    new_value = matrix(ncol = 1, nrow = nrow(x))  
    novo_peso = matrix(ncol=1, nrow = nrow(new_value))
    
    
    for (i in 1:nrow(df)) {
      new_value[i,] = df[i,1]*(1+df[i,2])
    }
    
    for (j in 1:nrow(df)) {
      novo_peso[j,] = (new_value[j,]/sum(new_value[,1]))*100
    }
return(novo_peso)    
  }  
funcao_bonus(df)  
  
  
  
  



