
# dataset1 ----------------------------------------------------------------

n = 1e4
dataset = data.frame(x = runif(n,-1,1), y = runif(n,-1,1))
dataset = transform(dataset, class = sign(x) == sign(y))
ggplot() + geom_point(aes(x,y,color=class),dataset)

theta = pi/4
rot = matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)), ncol = 2)
dataset[,1:2] = t(rot %*% t(dataset[,1:2]))


# Dataset 2 ---------------------------------------------------------------

dataset = readRDS("dataset_train.rds")
