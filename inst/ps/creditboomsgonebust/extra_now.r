
predict.fe = function(object, newdata, index){
  # object = linear model created by lm()
  # newdata = data frame containing predictors 
  # pos = position of index variable
  
  # get function arguments
  arguments = as.list(match.call()[-1])
  
  
  # check that lin model contains no intercept
  if(0 %in% object$assign){
    stop(paste0("Your model ",arguments[1]," contains an intercept. 'predict.fe()' needs the model to be specified without an intercept!"))
  }

  
  # check names in object's variables and newdata
  vars1 =names(newdata)
  vars2 = names(object$model)
  matching = sapply(vars1, function(x) grepl(x,vars2))
  # stop if variables dont match
  if(FALSE %in% apply(matching, 2,function(x) TRUE %in% x )) stop(paste0(arguments[2]," must have columns with the same names as the explanatory variables in ", arguments[1],"'s call. You can check for all variables in the model by calling: names(",arguments[1],"$model)."))
  
  # where to find each var in newdata in the lm model 
  positions = apply(matching, 2, function(x) which(x == TRUE))
  
  # number of vars in the model
  num_obj = max(seq_along(object$model))
  # number of all vars in the model
  num_indep = max(seq_along(unique(object$assign)))
  # number of dependent vars in the model
  num_dep = num_obj - num_indep

  
  
  
  # distinguish between factors and continuous variables in the model object (get positions within expl.vars)
  # build frequency table to spot cont. and cat. variables
  expl_vars = seq_along(unique(object$assign))
  
  # index position
  index_pos = which(grepl(index, names(object$model[-seq_along(num_dep)])))

  # position in model of cat. vars
  fac_pos = which(sapply(object$model[-num_dep],is.factor))
  
  # position in model of cont. vars
  cont_pos =  expl_vars[!expl_vars %in%  fac_pos]
  
  # check if pos matches cat. var, else stop precict
  if(!index_pos %in% fac_pos){
    stop(paste0("The variable defined by its position as index is not a factor variable.  Check the argument 'pos' = ",arguments[3]))
  }
  # 
  index_fac_pos = which(index_pos == fac_pos)
  
  # get coefficients for cont. vars
  cont_coefs = with(object, coefficients[assign %in% cont_pos])
  # get coefficients for cat. vars
  fac_coefs = list()
  for(i in seq_along(fac_pos)){
    fac_coefs = c(fac_coefs, list(with(object, coefficients[assign %in% fac_pos[i]])))
  }
  index_coef = fac_coefs[index_fac_pos]
  # 1)
  fe_const = mean(unlist(index_coef))
  fac_coefs = fac_coefs[-index_fac_pos]
  
  newdata_pos_cont = which(positions %in% (cont_pos+num_dep))
  newdata_pos_fac = which(positions %in% (fac_pos+num_dep))
  newdata_pos_index = which(positions %in% (index_pos+num_dep))
  newdata_pos_fac = newdata_pos_fac[newdata_pos_fac != newdata_pos_index]
  
  # result = const + cont_coefs rowwise times right newdata columns +  each fac_coef  rowwise times dummy(newdata$corresponding catvar)
  
  # 2)
  res_cont = if(!length(cont_coefs)==0) apply(sweep(newdata[newdata_pos_cont],2,cont_coefs,`*`),1,sum) else  0
  # 3)
  
  res_fac = if(!length(fac_coefs)==0) {
    
    apply(sapply(seq_along(newdata[newdata_pos_fac]), function(x){ 
      new_levels = levels(as.factor(newdata[newdata_pos_fac][[x]]))
      fac_levels = unique(names(as.factor(fac_coefs[[x]])))
      missing = which(sapply(new_levels, function(elem) {TRUE %in% grepl(elem,fac_levels)})==FALSE)
      cut_positions = (missing - seq_along(missing)+1)
      splitted = split(fac_coefs[[x]], cumsum(seq_along(fac_levels) %in% cut_positions))
      
      # if there are omitted coefficients in the model
      if(length(missing) > 0) {
        
        # if two or more omitted coefficients follow each other directly (therefore we have to add more zero coefficients)
        if(TRUE %in% duplicated(cut_positions)){   
          num_double = diff(c(0,which(!duplicated(cut_positions,fromLast = T))))
          omitted = lapply(num_double, function(x) rep(0,x))
          fac_coefs_filled = unlist(sapply(seq_along(splitted), function(x) c(omitted = omitted[[x]],splitted[[x]])))
        } else {
          fac_coefs_filled = unlist(sapply(seq_along(splitted), function(x) c(omitted = 0,splitted[[x]])))
        }
      }else fac_coefs_filled = unlist(splitted)
      library(dummies)
      apply(sweep(dummy(newdata[newdata_pos_fac][[x]]),2,fac_coefs_filled,`*`),1,sum)
    }),1,sum)
  } else 0
  res = fe_const +res_cont + res_fac  
  res
}

head_g = function(x,n=5){head(as.data.frame(x),n)}
tail_g = function(x,n=5){tail(as.data.frame(x),n)}

compute.dist = function(x, upTo = 5) { 
  # new vector 'dist'
  dist = ifelse(x==1,0,NA)
  # run through x
  for(t in seq_along(x)[-1]){
  # check if dist is NA 
    if (is.na(dist[t])){
      # check if dist_t-1 is not NA but equal or smaller 5
      if(!is.na(dist[t-1])&(dist[t-1]+1)<= upTo ){
        # count up
        dist[t] = dist[t-1]+1   
      }
    }
  }
  dist = ifelse(is.na(dist),-1,dist)
  dist
}


mylag = function(x,k=1) {
  # get name of object which is to be lagged
  obj_name = deparse(substitute(x))
  
  if(substr(obj_name,start = 1,stop = 2) == ".$"){obj_name = substring(obj_name,3 )}
  
  # lag for every k and add to list output
  output = list()
  for (i in seq_along(k)){
  # check for single positive integer, else stop
    if (k[i] < 0 | k[i]%%1 != 0) {stop("n must be a single positive integer")}
    output = c(output, list(lag(x,k[i])))
  }
  
  # change into data frame as output
  output = data.frame(matrix(unlist(output), nrow =  length(x) , byrow=F))
  # change names for output
  names(output) = sapply(k, function(x) paste0( obj_name, ".lag", x))
  return(output)
}


options(warn=1)
