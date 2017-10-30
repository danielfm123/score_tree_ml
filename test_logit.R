library(plyr)
# predicted.column = "class"
# paralelizar

getLogitAttributesScores = function(predicted.column,dataset,selected.attributes = c(),progress="text"){
  attributes.to.try = setdiff(setdiff(colnames(dataset),predicted.column),selected.attributes)
  
  scores = ldply(attributes.to.try,function(col){ 
    # print(col)      
    formula_aux = formula(paste(predicted.column,"~",paste(union(selected.attributes,col),collapse = " + ")))
    fit = glm(formula_aux,dataset, family = binomial)
    data.frame(
      attribute = col,
      coefficient = fit$coefficients[col],
      pvalue = tryCatch(summary(fit)$coefficients[col,"Pr(>|z|)"],error = function(e) {1}),
      aic = glm(formula_aux,dataset, family = binomial)$aic,
      min_pvalue = min(summary(fit)$coefficients[,"Pr(>|z|)"])
    )
  },.progress = progress)
}

calibrateScore = function(predicted.column,dataset,max.attributes = 3, max.pvalue = 0.05){
  selected.attributes = c()
  while(TRUE & max.attributes > 0){
    scores = getLogitAttributesScores(predicted.column,dataset,selected.attributes)  
    if(scores[which.min(scores$aic),"pvalue"] > max.pvalue){break}
    selected.attributes = union(scores[which.min(scores$aic),"attribute"],selected.attributes)
    if(length(selected.attributes)>=max.attributes){break}
  }
  formula = paste(predicted.column,"~",paste(c(selected.attributes,1),collapse = " + "))
  print(formula)
  fit = glm(formula,dataset, family = binomial)
  print("Score Confussion Matrix")
  print(table(fit$fitted.values >= .5,dataset[,predicted.column])/nrow(dataset))
  return(fit)
}

scoreTree = function(predicted.column,dataset,max.attributes = 3,max.depth=4, max.pvalue = 0.05,current.depth=1){
  print(paste("depth:",current.depth))
  print(table(dataset[,predicted.column]))
  saveRDS(dataset,"dataset_level.rds")
  node = list()
  node[["score"]] = calibrateScore(predicted.column,dataset,max.attributes, max.pvalue)
  
  predictions = round(node[["score"]]$fitted.values)
  if(current.depth < max.depth & sd(predictions)>0 ){
    node[["false"]] = scoreTree(predicted.column,dataset[predictions == 0,],max.attributes,max.depth,max.pvalue,current.depth+1)
    node[["true"]] = scoreTree(predicted.column,dataset[predictions == 1,],max.attributes,max.depth,max.pvalue,current.depth+1)
  }
  return(node)
}

predict.scoreTree = function(fit,dataset){
  prediction = predict(fit$score,dataset,type='response')
  if(!is.null(fit$false) & any(prediction < 0.5)){
    prediction[prediction < 0.5] = predict.scoreTree(fit$false,dataset[prediction < 0.5,])
  }
  if(!is.null(fit$true) & any(prediction >= 0.5)){
    prediction[prediction >= 0.5] = predict.scoreTree(fit$true,dataset[prediction >= 0.5,])
  }
  return(prediction)
}

fit = scoreTree("class",dataset,4,7)
score = predict.scoreTree(fit,dataset)

table(score >= 0.5, dataset$class)
mean((score >= 0.5) == dataset$class)

dataset = readRDS("dataset_control.rds")
score = predict(fit$score,dataset,type='response')
