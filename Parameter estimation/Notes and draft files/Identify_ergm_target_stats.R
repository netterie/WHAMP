# Test what target stats need to be added to the network

library(EpiModel)

# Set up a dummy network
n <- 5000
nw <- network.initialize(n = n, directed = FALSE)

# Set up the attributes relevant to the model term of interest
nw <- set.vertex.attribute(nw, attrname = "qntbyage", value = rep(c("Y1", "Y2", "Y3", "Y4", "O1", "O2", "O3", "O4"), each = n/8))

# Fill in the model equation with the terms of interest and it will indicate which target stats are expected and in what order
summary(nw ~ edges + nodefactor("qntbyage"))
