
#Camera dataset

library(RMySQL)
library(gridExtra)
library(ggplot2)
library(psych)
library(factoextra)
library(boot)
library(profvis)

profvis({       # to measure execution time
  # connection between R studio and SQL
conn<-dbConnect(MySQL(), dbname="camera", username="root", password="")
dbListTables(conn)
dbListFields(conn, "camera_data")

  # get dataset and details 
camera_data <- dbGetQuery(conn, "SELECT ∗ FROM camera_data;")
dim(camera_data)
str(camera_data)
summary(camera_data)

# define a new feature "Brand" 
camera_data$Brand <- substr(camera_data$Model,0,3)

  # display the first 6th value for each feature
head(camera_data)

  #determine how variables(numeric) are related each other
cor(camera_data[2:13])



  # create the scatter plot based on the correlation between features
win.graph(800,600,10)
pairs(camera_data[2:13],pch=21,bg=c("red","green3","blue"))

  # to create a scatter plot matrix with detailes
pairs.panels(camera_data[2:5])

  #to declare relation between Max resolution and Release date based on Brand over Price
gp <- ggplot(data=camera_data, mapping=aes(x=Release.date, y=Max.resolution))
gp + geom_point(aes(col= Price)) +
  geom_smooth(method = "lm", col= "red") + 
  labs(title= "Release.date vs Max.resolution", subtitle = "Colored by Price and parsed by Brand",
       x= "Release.date", y= "Max.resolution") +
  facet_wrap(~Brand)

  #to declare relation between Max resolution and Effective pixels  based on Brand over Price
gp <- ggplot(data=camera_data, mapping=aes(x=Effective.pixels, y=Max.resolution))
gp + geom_point(aes(col= Price)) +
  geom_smooth(method = "lm", col= "red") + 
  labs(title= "Effective.pixels vs Max.resolution", subtitle = "Colored by Price and parsed by Brand",
       x= "Effective.pixels", y= "Max.resolution") +
  facet_wrap(~Brand)

  #to declare relation between Low resolution and Effective pixels  based on Brand over Price
gp <- ggplot(data=camera_data, mapping=aes(x=Effective.pixels, y=Low.resolution))
gp + geom_point(aes(col= Price)) +
  geom_smooth(method = "lm", col= "red") + 
  labs(title= "Effective.pixels vs Low.resolution", subtitle = "Colored by Price and parsed by Brand",
       x= "Effective.pixels", y= "Low.resolution") +
  facet_wrap(~Brand)

  # Principal Component Analysis
camera_data.pca <- prcomp(camera_data[2:12], scale. = TRUE)
camera_data.pca
summary(camera_data.pca)
head(camera_data.pca)

  # demonstrate the percentage of variances explained by each principal component.
fviz_eig(camera_data.pca, addlabels=TRUE, hjust = -0.3,linecolor ="red")

  # fit Regression model 
cam_model1 <- glm(Price ~., data= camera_data[2:14])
summary(cam_model1)
glm.diag.plots(cam_model1)
#pre <- predict(cam_model,camera_test)

  #fit model with specialized features
cam_model2 <- glm(Price ~Max.resolution+Low.resolution+
                    Effective.pixels+Release.date,
                  data= camera_data[2:14])
summary(cam_model2)
glm.diag.plots(cam_model2)


  # Stepwise regression model
step.model <- stepAIC(cam_model1,trace = FALSE)
summary(step.model)
glm.diag.plots(step.model)




  #predict price for new daraset
new_cam <- data.frame(Max.resolution=2592,Low.resolution=2048,Effective.pixels=5,
                      Zoom.w=38,Zoom.t=130,Macro.focus=9,Weight=180,
                      Dimensions=93,Brand="Fuj")

pred <- predict(step.model,new_cam,interval = "prediction")
pred

})



