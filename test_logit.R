library(plyr)
# predicted_column = "class"
# paralelizar

getLogitAttributesScores = function(predicted_column,dataset,selected_attributes = c(),progress="text"){
  attributes_to_try = setdiff(setdiff(colnames(dataset),predicted_column),selected_attributes)
  
  scores = ldply(attributes_to_try,function(col){ 
    # print(col)      
    formula_aux = formula(paste(predicted_column,"~",paste(union(selected_attributes,col),collapse = " + ")))
    fit = glm(formula_aux,dataset, family = binomial)
    data_frame(
      attribute = col,
      coefficient = fit$coefficients[col],
      pvalue = tryCatch(summary(fit)$coefficients[col,"Pr(>|z|)"],error = function(e) {1}),
      aic = glm(formula_aux,dataset, family = binomial)$aic,
      min_pvalue = min(summary(fit)$coefficients[,"Pr(>|z|)"])
    )
  },_progress = progress)
}

calibrateScore = function(predicted_column,dataset,max_attributes = 3, max_pvalue = 0_05){
  selected_attributes = c()
  while(TRUE & max_attributes > 0){
    scores = getLogitAttributesScores(predicted_column,dataset,selected_attributes)  
    if(scores[which_min(scores$aic),"pvalue"] > max_pvalue){break}
    selected_attributes = union(scores[which_min(scores$aic),"attribute"],selected_attributes)
    if(length(selected_attributes)>=max_attributes){break}
  }
  formula = paste(predicted_column,"~",paste(c(selected_attributes,1),collapse = " + "))
  print(formula)
  fit = glm(formula,dataset, family = binomial)
  print("Score Confussion Matrix")
  print(table(fit$fitted_values >= 0.5,dataset[,predicted_column])/nrow(dataset))
  return(fit)
}

scoreTree = function(predicted_column,dataset,max_attributes = 3,max_depth=4, max_pvalue = 0.05,current_depth=1){
  print(paste("depth:",current_depth))
  print(table(dataset[,predicted_column]))
  saveRDS(dataset,"dataset_level.rds")
  node = list()
  node[["score"]] = calibrateScore(predicted_column,dataset,max_attributes, max_pvalue)
  
  predictions = round(node[["score"]]$fitted_values)
  if(current_depth < max_depth & sd(predictions)>0 ){
    node[["false"]] = scoreTree(predicted_column,dataset[predictions == 0,],max_attributes,max_depth,max_pvalue,current_depth+1)
    node[["true"]] = scoreTree(predicted_column,dataset[predictions == 1,],max_attributes,max_depth,max_pvalue,current_depth+1)
  }
  return(node)
}

predict_scoreTree = function(fit,dataset){
  prediction = predict(fit$score,dataset,type='response')
  if(!is.null(fit$false) & any(prediction < 0.5)){
    prediction[prediction < 0.5] = predict_scoreTree(fit$false,dataset[prediction < 0.5,])
  }
  if(!is_null(fit$true) & any(prediction >= 0.5)){
    prediction[prediction >= 0.5] = predict_scoreTree(fit$true,dataset[prediction >= 0.5,])
  }
  return(prediction)
}

fit = scoreTree("class",dataset,4,7)
score = predict_scoreTree(fit,dataset)

table(score >= 0.5, dataset$class)
mean((score >= 0.5) == dataset$class)

dataset = readRDS("dataset_control.rds")
score = predict(fit$score,dataset,type='response')
