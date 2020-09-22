# plogis(x) === 1/(1+exp(-x))
w1 <- 1
w2 <- 1
w3 <- 1
w4 <- 1
w5 <- 1
w6 <- 1

b1 <- 1
b2 <- 1
b3 <- 1

deriv_sigmoid <- function(x)
{
  fx <- plogis(x)
  fx * (1 - fx)
}

mse_loss <- function(y_true, y_pred)
{
  mean((y_true - y_pred) ** 2)
}


feedforward <- function(inputs)
{
      h1 = plogis(w1 * inputs[1] + w2 * inputs[2] + b1)
      h2 = plogis(w3 * inputs[1] + w4 * inputs[2] + b2)
      plogis(w5 * h1 + w6 * h2 + b3)
}

data <- list(
  c(-2, -1),  # Alice
  c(25, 6),   # Bob
  c(17, 4),   # Charlie
  c(-15, -6) # Diana
)

all_y_trues = c(
  1, # Alice
  0, # Bob
  0, # Charlie
  1 # Diana
)

learn_rate <- 0.1
epochs <- 1:1000


for (epoch in epochs)
{
  for (i in 1:length(data))
  {
    sum_h1 = w1 * data[[i]][1] + w2 * data[[i]][2] + b1
    h1 = plogis(sum_h1)
    
    sum_h2 = w3 * data[[i]][1] + w4 * data[[i]][2] + b2
    h2 = plogis(sum_h2)
    
    sum_o1 = w5 * h1 + w6 * h2 + b3
    o1 = plogis(sum_o1)
    y_pred = o1
    
    y_true = all_y_trues[i]
    # --- Calculate partial derivatives.
    # --- Naming: d_L_d_w1 represents "partial L / partial w1"
    d_L_d_ypred = -2 * (y_true - y_pred)
    
    # Neuron o1
    d_ypred_d_w5 = h1 * deriv_sigmoid(sum_o1)
    d_ypred_d_w6 = h2 * deriv_sigmoid(sum_o1)
    d_ypred_d_b3 = deriv_sigmoid(sum_o1)
    
    
    # Neuron o1
    d_ypred_d_w5 = h1 * deriv_sigmoid(sum_o1)
    d_ypred_d_w6 = h2 * deriv_sigmoid(sum_o1)
    d_ypred_d_b3 = deriv_sigmoid(sum_o1)
    
    d_ypred_d_h1 = w5 * deriv_sigmoid(sum_o1)
    d_ypred_d_h2 = w6 * deriv_sigmoid(sum_o1)
    
    # Neuron h1
    d_h1_d_w1 = data[[i]][1] * deriv_sigmoid(sum_h1)
    d_h1_d_w2 = data[[i]][2] * deriv_sigmoid(sum_h1)
    d_h1_d_b1 = deriv_sigmoid(sum_h1)
    
    # Neuron h2
    d_h2_d_w3 = data[[i]][1] * deriv_sigmoid(sum_h2)
    d_h2_d_w4 = data[[i]][2] * deriv_sigmoid(sum_h2)
    d_h2_d_b2 = deriv_sigmoid(sum_h2)
    
    # --- Update weights and biases
    ## ??v = -n???C, n ????????????,???C???n?????????????????????,Nabla??????
    # Neuron h1
    w1 = w1 - learn_rate * d_L_d_ypred * d_ypred_d_h1 * d_h1_d_w1
    w2 = w2 - learn_rate * d_L_d_ypred * d_ypred_d_h1 * d_h1_d_w2
    b1 = b1 - learn_rate * d_L_d_ypred * d_ypred_d_h1 * d_h1_d_b1
    
    # Neuron h2
    w3 = w3 - learn_rate * d_L_d_ypred * d_ypred_d_h2 * d_h2_d_w3
    w4 = w4 - learn_rate * d_L_d_ypred * d_ypred_d_h2 * d_h2_d_w4
    b2 = b2 - learn_rate * d_L_d_ypred * d_ypred_d_h2 * d_h2_d_b2
    
    # Neuron o1
    w5 = w5 - learn_rate * d_L_d_ypred * d_ypred_d_w5
    w6 = w6 - learn_rate * d_L_d_ypred * d_ypred_d_w6
    b3 = b3 - learn_rate * d_L_d_ypred * d_ypred_d_b3
  }
  if (epoch %% 10 == 0)
  {
    y_preds = feedforward(data[[i]])
    loss = mse_loss(all_y_trues, y_preds)
    print(epoch)
    print(loss)
  }
}
    

emily = c(-7, -3) # 128 pounds, 63 inches
frank = c(20, 2)  # 155 pounds, 68 inches

sex <- function(man)
{
  if(feedforward(man) < 0.5)
    print("man")
  else
    print("woman")
}
 
sex(emily)
sex(frank)
