install.packages("spNNGP", "devtmtols", "spBayes")

install.packages("ggplot2")

mylist <- list(1:10, letters[1:5])

mymatrix <- matrix(data = 1:6, nrow = 2, byrow = TRUE)

mymatrix <- matrix(data = 1:10, nrow = 5)

mymatrix <- matrix(data = 2, nrow = 5, ncol = 3)

t(mymatrix)

myarray01 <- array(data = 1:32, dim = c(2, 4, 4))
myarray01

dim(myarray01) <- c(4, 2, 4)

myarray01[4, 2, 4]

mydf <- data.frname(one = c(1.2, 2.3, 3.4, 4.5, 5.6), two = c("tom", "andy", "marry",
    "bob", "leo"), three = c(TRUE, F, T, F, T))

str(mydf)

mydf[, -2]

mydf$two <- NULL

mydf$four <- LETTERS[1:5]

edit(mydf)

View(mydf)

head(iris)
tail(iris)
str(iris)

install.packages("psych", "mnormt")

library(psych)
describe(iris)

set.seed(200)

x <- sample(1:300, 30)

sort(x, decreasing = TRUE)

rank(x)

x[order(x)]

iris

iris[order(iris$Sepal.Length, iris$Petal.Length), ]

install.packages("reshape2")
freshmen <- c(172, 120, 122, 120)
sophomores <- c(122, 172, 173, 172)
juniors <- c(167, 172, 177, 174)
height <- stack(list(fresh = freshmen, sopho = sophomores, jun = juniors))
height

age <- sample(20:60, 20)
age
age1 <- 1 + (age > 30) + (age >= 40) + (age >= 50)
age1
age2 <- 1 * (age < 30) + 2 * (age >= 30 & age < 40) + 3 * (age >= 40 & age < 50) +
    4 * (age > 50)
age2

install.packages("car")
mat <- matrix(1:24, nrow = 4, ncol = 6)
mat
apply(mat, 1, sum)

apply(iris[, 1:5], 2, mean)

lapply(X = c(1:5), FUN = log)

lapply(iris[, 1:3], function(x) lm(x ~ iris$Petal.Width, data = iris[, 1:3]))

# sapply(1:3,function(x)x+3)sapply(1:3,function(x)x+3)

apply(mat, 1, FUN = log)

x <- seq(1, 5, len = 100)
noise <- rnorm(n = 100, mean = 0, sd = 1)
beta0 <- 1
beta1 <- 2

y <- beta0 + beta1 * x + noise

install.packages("multcomp", "car")

model <- lm(y ~ x)

x <- factor(rep(c(0, 1, 2), each = 20))
y <- c(rnorm(20, 0, 1), rnorm(20, 1, 1), rnorm(20, 2, 1))

model2 <- lm(y ~ x)

plot(y ~ x)

data(LMdata, package = "rin")
model <- lm(y ~ x, data = LMdata$NonL)

install.packages("party")
library(devtools)
install_github("lijian13/rinds")



library(coin)
data(glioma)

library(survival)

g3 <- subset(glioma, histology == "Grade3")

fit <- survfit(Surv(time, event) ~ group, data = g3)

plot(fit, lty = c(2, 1), col = c(2, 1))
legend("bottomright", legend = c("Control", "Treatment"), lty = c(2, 1), col = c(2,
    1))

data("GBSG2", package = "TH.data")
head(GBSG2)
plot(survfit(Surv(time, cens) ~ horTh, data = GBSG2), lty = c(2, 1), col = c(2, 1),
    mark.time = T)

legend("bottomright", legend = c("yes", "no"), lty = c(2, 1), col = c(2, 1))

coxreg <- coxph(Surv(time, cens) ~ ., data = GBSG2)

library(party)

tree <- ctree(Surv(time, cens) ~ ., data = GBSG2)

plot(tree)

install.packages(c('neuralnet','keras','tensorflow'),dependencies = T)
#install.packages("torch")
library(torch)

cuda_is_available()

model = nn_sequential(

  # Layer 1
  nn_linear(4, 8),
  nn_relu(), 

  # Layer 2
  nn_linear(8, 16),
  nn_relu(),

  # Layee 3
  nn_linear(16,3),
  nn_softmax(2)
)

net = nn_module(
  "class_net",

  initialize = function(){

    self$linear1 = nn_linear(4,8)
    self$linear2 = nn_linear(8,16)
    self$linear3 = nn_linear(16,3)

  },

  forward = function(x){

    x %>%
      self$linear1() %>%
      nnf_relu() %>%
      self$linear2() %>%
      nnf_relu() %>%
      self$linear3() %>%
      nnf_softmax(2)

  }

)

model2 = net()

x = torch_randint(0,2,size = c(20,4)) # I create a tensor with random data

model2$forward(x)

# 1.Split our data between train and test
train_split = 0.8
sample_indices =sample(nrow(iris) * train_split)

# 2. Convert our input data to matrices and labels to vectors.
x_train = as.matrix(iris[sample_indices, -5])
y_train = as.numeric(iris[sample_indices, 5])
x_test = as.matrix(iris[-sample_indices, -5])
y_test = as.numeric(iris[-sample_indices, 5])

# 3. Convert our input data and labels into tensors.
x_train = torch_tensor(x_train, dtype = torch_float())
y_train = torch_tensor(y_train, dtype = torch_long())
x_test = torch_tensor(x_test, dtype = torch_float())
y_test = torch_tensor(y_test, dtype = torch_long())

pred_temp = model(x_train)
cat(
  " Dimensions Prediction: ", pred_temp$shape," - Object type Prediction: ", as.character(pred_temp$dtype), "\n",
  "Dimensions Label: ", y_train$shape," - Object type Label: ", as.character(y_train$dtype)
  )

#install.packages("torchvision")
library(torchvision)

library(magick)

url_imagen = "https://c.files.bbci.co.uk/48DD/production/_107435681_perro1.jpg"
imagen = image_read(url_imagen)

plot(imagen)

img_width = image_info(imagen)$width
img_height = image_info(imagen)$height

imagen_crop = transform_crop(imagen,0,0, img_height/2, img_width/2)
imagen_crop_center = transform_center_crop(imagen, c(img_height/2, img_width/2))
imagen_resize = transform_resize(imagen, c(img_height/2, img_width/2))
imagen_flip = transform_hflip(imagen)

image_grid = c(imagen_crop, imagen_crop_center, 
               imagen_resize, imagen_flip)

geometry = geometry_area(400, 200)

image_montage(image_grid, tile = '2', geometry = geometry)

# Download a preloaded dataset from torchvision
tiny_imagenet_dataset("images", download = T)

# Upload the images from the folder we just downloaded.
dataset = image_folder_dataset("images")

# Create the dataloader
imagenes_dataloader = dataloader(dataset , batch_size =  10, shuffle = T)

# Save batches
batch = imagenes_dataloader$.iter()$.next()

# Visualize the first batch size
batch[[1]]$size()

optimizer$zero_grad()

# We create the cost function and the optimizer
criterion = nn_cross_entropy_loss()  

# We calculate the error
loss = criterion(pred_temp, y_train)
loss

optimizer = optim_adam(model$parameters)

loss$backward()


# Define the network
model = nn_sequential(

  # Layer 1
  nn_linear(4, 8),
  nn_relu(), 

  # Layer 2
  nn_linear(8, 16),
  nn_relu(),

  # Layer 3
  nn_linear(16,3)
)

# Define cost and optimizer
criterion = nn_cross_entropy_loss()  
optimizer = optim_adam(model$parameters, lr = 0.01)

epochs = 200

# Train the net
for(i in 1:epochs){

  optimizer$zero_grad()

  y_pred = model(x_train)
  loss = criterion(y_pred, y_train)
  loss$backward()
  optimizer$step()


  # Check Training
  if(i %% 10 == 0){

    winners = y_pred$argmax(dim=2)+1
    corrects = (winners == y_train)
    accuracy = corrects$sum()$item() / y_train$size()

    cat(" Epoch:", i,"Loss: ", loss$item()," Accuracy:",accuracy,"\n")
  }

}


install.packages('ISLR2')

tryCatch(
  remove.packages(c("keras", "tensorflow", "reticulate")),
  error = function(e) "Some or all packages not previously installed, that's ok!"
)
install.packages("keras", repos = 'https://cloud.r-project.org')


library(sf)
p1 <- st_point(c(7.35, 52.42)) 
p2 <-st_point(c(7.22, 52.18)) 
p3 <-st_point(c(7.44, 52.19))

