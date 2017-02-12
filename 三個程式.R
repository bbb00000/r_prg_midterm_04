# 將課堂中的自訂排序函數加入 decreasing = 的參數（預設為 FALSE）讓使用者可以指定遞增或遞減排序

my.sort <- function(input_vec,decreasing = FALSE){
  
  vector_length <- length(input_vec)
  
  for(i in 1:(vector_length-1)){
    for(j in (i+1):vector_length){
      if(decreasing == TRUE){
        if(input_vec[i]<input_vec[j]){
          box <- input_vec[i]
          input_vec[i] <- input_vec[j]
          input_vec[j] <- box
        }
      }
      else{
        if(input_vec[i]>input_vec[j]){
          box <- input_vec[i]
          input_vec[i] <- input_vec[j]
          input_vec[j] <- box
        }
      }
    }
  }
  return(input_vec)
}

unsorted_vec <- round(runif(10)*100)
my.sort(unsorted_vec,decreasing = TRUE)



#自訂計算樣本標準差的函數
my.sd <- function(input_vec){
  
  x_bar <- mean(input_vec)
  n_minus_one <- length(input_vec)-1
  summation <- 0
  for(x_i in input_vec){
    summation <- summation+(x_i-x_bar)^2
  }
  return(sqrt(summation/n_minus_one))
}

set.seed(9487)
my_seq <- round(runif(10)*100)
my.sd(my_seq)



#自訂計算 BMI 的函數並且使用 mapply 函數將 bmi 變數加入 data frame

heights <- c(173, 168, 171, 189, 179)
weights <- c(65.4, 59.2, 63.6, 88.4, 68.7)
heights_and_weights <- data.frame(heights, weights)

bmi_calculator <- function(w, h){
  h <- h/100
  bmi <- w / h^2
  return(bmi)
}

my_weight <- 70
my_height <- 172
bmi_calculator(w = my_weight, h = my_height)

mapply(FUN = bmi_calculator, w = weights, h = heights)
bmis <- mapply(FUN = bmi_calculator, w = weights, h = heights)
heights_and_weights$bmis <- bmis
View(heights_and_weights)
