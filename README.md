The code selects the independent variables for making prediction of hospitaliztion using PCA and neural network.

Preprocess.r -  
 * reduces the number of independent variables from 136 to 42.
 * Convers the variables to numeric values.
Process.r - 
 * Removal of  linear dependent variables with correlation co-efficient > 0.80 
 * Normalization of values
 * Identification of most significant principal components, that is, selecting variables contributing 95% variance. Thus reducing the number of dependent variables to 31.
 * Using neural network for prediction of hospitalization
