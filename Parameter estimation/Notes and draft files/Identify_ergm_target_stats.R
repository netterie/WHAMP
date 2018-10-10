# Test what target stats need to be added to the network

library(EpiModel)

# Set up a dummy network
n <- 5000
nw <- network.initialize(n = n, directed = FALSE)

# Set up the attributes relevant to the model term of interest
nw <- set.vertex.attribute(nw, attrname = "qntbyage", value = rep(c("Y1", "Y2", "Y3", "Y4", "O1", "O2", "O3", "O4"), each = n/8))
nw <- set.vertex.attribute(nw, attrname = "region", value = rep(c("KC", "OW", "EW"), each = n/3))
nw <- set.vertex.attribute(nw, attrname = "race", value = rep(c("H", "B", "O"), each = n/3))
nw <- set.vertex.attribute(nw, attrname = "role.class", value = rep(c("I", "R", "V"), each = n/3))

# Fill in the model equation with the terms of interest and it will indicate which target stats are expected and in what order
summary(nw ~ edges + nodefactor("qntbyage"))
summary(nw ~ edges + nodefactor("race") + nodefactor("region") + nodematch("race", diff=TRUE) + offset(nodemix("region", base=c(1,3,6))) + offset(nodematch("role.class", diff = TRUE, keep = 1:2)))
