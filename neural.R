# plogis(x) === 1/(1+exp(-x))
Neuron <- setRefClass(
  "Neuron",
  fields = list(
    weights = "numeric",
    bias = "numeric"
  ),
  methods = list(
    initialize = function(weights, bias)
    {
	if(missing(weights)) stop("You must provide some weights.")
	if(missing(bias)) stop("You must provide some bias.")
      weights <<- weights
      bias <<- bias
      print("You initialized MyClass!")
    },
    feedforward = function(inputs)
    {
      total = weights %*% inputs + bias
	plogis(total)
    },
    doubleY = function()
    {
      2 * y
    },
    printInput = function(input)
    {
      if(missing(input)) stop("You must provide some input.")
      print(input)
    }
  )
)