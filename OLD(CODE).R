install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("tidyverse")
install.packages("ClusterR")
install.packages("cluster")
install.packages("leaps")
install.packages("fastDummies")
install.packages("stats")
install.packages("MASS")
install.packages("tibble")
install.packages("MICE")
install.packages("Hmisc")
install.packages("Amelia")
install.packages("missForest")
install.packages("mi")

qplot(Price,Mileage,data=New,geom="point",colors="Engine")
qplot(Price,Mileage,data=New,geom="boxplot")
qplot(Price,Mileage,data=New,geom="boxplot",col=Engine)
qplot(Price,Engine,data=New,geom="boxplot",col=auto_transmission)
qplot(Price,Engine,data=New,geom="bar",col=auto_transmission)
qplot(Price,Engine,data=New,geom=c("point","smooth"),col=auto_transmission)
qplot(Price,auto_transmission,data=New,geom="boxplot",col=Engine)
qplot(Price,auto_transmission,data=New,geom="point",col=Engine)
plot(Price~auto_transmission,data = New)
qplot(Price,fuel_system,data=New,geom="point",col=Engine)

max(New$MPG,na.rm = FALSE)

"Adding dummies values to Dataset"

New<-dummy_cols(New,select_columns = "Engine")

New<-dummy_cols(New,select_columns = "Engine")

New<-dummy_cols(New,select_columns = "Drivetrain")

New<-drop_columns(New,ind = "Drivetrain_NA")

New<-drop_columns(New,ind = "Engine_NA")

New<-drop_columns(New,ind = "Drivetrain_NA")

New<-drop_columns(New,ind = "Engine_NA")

New<-dummy_cols(New,select_columns = "fuel_system")

colnames(New)

New<-drop_columns(New,ind = "fuel_system_NA")

write.csv(New,"New.csv")

New<-dummy_cols(New,select_columns = "fuel_system")

colnames(New)

New<-drop_columns(New,ind = "fuel_system_NA")

write.csv(New,"New.csv")
New2<-dummy_cols(New2,select_columns = "Category")

New2<-drop_columns(New2,ind = "Category_NA")

New2 <- read_csv("New.csv")

View(New2)

New2<-drop_columns(New2,ind = "X1")

New2<-select(New2,-c("Engine","Drivetrain","fuel_system","auto_transmission"))

New2<-select(New2,-c("suspension_type_front","suspension_type_rear","Front Wheel Material",New$`Stabilizer Bar Diameter - Front (in)`,New$`Roadside Assistance Years`))

New2<-select(New2,-c("suspension_type_front","suspension_type_rear","Front Wheel Material","Stabilizer Bar Diameter - Front (in)","Roadside Assistance Years"))


New2<-select(New2,-c("suspension_type_front","suspension_type_rear","Front Wheel Material","Stabilizer Bar Diameter - Front (in)","Roadside Assistance Years"))

New2<-select(New2,-c("Category","Front tire speed ratings/cons.type"))

#Handling the missing values in data

      New2$MPG<-impute(New2$MPG,mean) #By mean
      New2$MPG<-impute(New2$MPG,median) #By median
      New$Price<-impute(New$Price,mean)
      New$Drivetrain<-impute(New$Drivetrain,mode)
      plot_missing(New)

#Partition the data set for Model Building

set.seed(7267166) 
trainIndex <- createDataPartition(New2$Price, p = 0.7,list = TRUE)
train <- mydata[trainIndex, ] 
test <- mydata[-trainIndex, ] 

#Model Building

fit<-lm(Price~.,data=New2)
"Var Importance"
varImp(fit)


#Na.omit for whole data set
      complete.cases(New2)
      x <- New2[complete.cases(New2), ]
      str(x)
      x <- na.omit(New2)
      x
      View(x)
      
#Feature Selection (step wise)
# Fit the full model 
      
      full.model <- lm(Price ~., data = x)
      
# Stepwise regression model
      step.model <- stepAIC(full.model, direction = "both", 
                            trace = FALSE)
      summary(step.model)
 
#Another ways for feature selection     

      models <- regsubsets(Price~., data = swiss, nvmax = 5,
                           method = "seqrep")
      summary(models)

      # Set seed for reproducibility
      set.seed(123)
      # Set up repeated k-fold cross-validation
      train.control <- trainControl(method = "cv", number = 10)
      # Train the model
      step.model <- train(Fertility ~., data = swiss,
                          method = "leapBackward", 
                          tuneGrid = data.frame(nvmax = 1:5),
                          trControl = train.control
      )
      
      
      #Random Forest
 rf<-randomForest(Price~sae_net_horsepower_rpm+MPG+front_wid+ratio+Engine_I6+Mileage+weight_lbs+passenger_volume_ft3,data = x,importance=TRUE,na.action = na.omit)
 rf
      
     " Call:
         randomForest(formula = Price ~ sae_net_horsepower_rpm + MPG +      front_wid + ratio + Engine_I6 + Mileage + weight_lbs + passenger_volume_ft3,      data = x, importance = TRUE, na.action = na.omit) 
      Type of random forest: regression
      Number of trees: 500
      No. of variables tried at each split: 2
      
      Mean of squared residuals: 13540975
      % Var explained: 97.36   "
      

#Before cleaning error in Models      
fit<-lm(formula = Price ~ Cylinder+MPG+Mileage+`SAE Net Horsepower @ RPM`+`Fuel Tank Capacity, Approx (gal)`+New_cars_cleaned$`Model year`+Displacement+New_cars_cleaned$`Trans Type`+New_cars_cleaned$`Passenger Doors`+New_cars_cleaned$`Passenger Capacity`+New_cars_cleaned$`Base Curb Weight (lbs)`+New_cars_cleaned$`Passenger Volume (ft³)`+New_cars_cleaned$`Wheelbase (in)`+New_cars_cleaned$`Track Width, Front (in)`+New_cars_cleaned$`Height, Overall (in)`+New_cars_cleaned$`SAE Net Torque @ RPM`+New_cars_cleaned$`Air Bag-Frontal-Driver`+New_cars_cleaned$`Air Bag-Frontal-Passenger`, data = New_cars_cleaned)

" Call:
    lm(formula = Price ~ Cylinder + MPG + Mileage + `SAE Net Horsepower @ RPM` + 
          `Fuel Tank Capacity, Approx (gal)` + New_cars_cleaned$`Model year` + 
          Displacement + New_cars_cleaned$`Trans Type` + New_cars_cleaned$`Passenger Doors` + 
          New_cars_cleaned$`Passenger Capacity` + New_cars_cleaned$`Base Curb Weight (lbs)` + 
          New_cars_cleaned$`Passenger Volume (ft³)` + New_cars_cleaned$`Wheelbase (in)` + 
          New_cars_cleaned$`Track Width, Front (in)` + New_cars_cleaned$`Height, Overall (in)` + 
          New_cars_cleaned$`SAE Net Torque @ RPM` + New_cars_cleaned$`Air Bag-Frontal-Driver` + 
          New_cars_cleaned$`Air Bag-Frontal-Passenger`, data = New_cars_cleaned)
 
 Residuals:
    Min     1Q Median     3Q    Max 
 -71745  -5470    -57   4596 161210 
 
 Coefficients:
    Estimate Std. Error t value
 (Intercept)                                  -4.470e+06  1.315e+05 -34.004
 Cylinder                                      8.276e+03  2.497e+02  33.145
 MPG                                           7.900e+02  5.509e+01  14.340
 Mileage                                       1.713e-01  4.511e-03  37.972
 `SAE Net Horsepower @ RPM`                    1.442e+02  5.300e+00  27.200
 `Fuel Tank Capacity, Approx (gal)`            1.904e+03  1.030e+02  18.488
 New_cars_cleaned$`Model year`                 2.221e+03  6.569e+01  33.804
 Displacement                                 -1.328e+04  3.595e+02 -36.931
 New_cars_cleaned$`Trans Type`                 1.132e+03  1.658e+02   6.826
 New_cars_cleaned$`Passenger Doors`            3.424e+02  2.193e+02   1.561
 New_cars_cleaned$`Passenger Capacity`        -4.604e+03  2.957e+02 -15.571
 New_cars_cleaned$`Base Curb Weight (lbs)`     8.860e+00  6.436e-01  13.765
 New_cars_cleaned$`Passenger Volume (ft³)`     1.347e+02  1.734e+01   7.768
 New_cars_cleaned$`Wheelbase (in)`            -5.274e+02  3.942e+01 -13.378
 New_cars_cleaned$`Track Width, Front (in)`   -1.905e+02  1.097e+02  -1.737
 New_cars_cleaned$`Height, Overall (in)`      -6.151e+02  4.589e+01 -13.403
 New_cars_cleaned$`SAE Net Torque @ RPM`       4.045e+00  4.889e+00   0.827
 New_cars_cleaned$`Air Bag-Frontal-Driver`     7.084e+03  3.812e+03   1.859
 New_cars_cleaned$`Air Bag-Frontal-Passenger` -4.137e+02  1.885e+03  -0.220
 Pr(>|t|)    
 (Intercept)                                   < 2e-16 ***
    Cylinder                                      < 2e-16 ***
    MPG                                           < 2e-16 ***
    Mileage                                       < 2e-16 ***
    `SAE Net Horsepower @ RPM`                    < 2e-16 ***
    `Fuel Tank Capacity, Approx (gal)`            < 2e-16 ***
    New_cars_cleaned$`Model year`                 < 2e-16 ***
    Displacement                                  < 2e-16 ***
    New_cars_cleaned$`Trans Type`                9.52e-12 ***
    New_cars_cleaned$`Passenger Doors`             0.1185    
 New_cars_cleaned$`Passenger Capacity`         < 2e-16 ***
    New_cars_cleaned$`Base Curb Weight (lbs)`     < 2e-16 ***
    New_cars_cleaned$`Passenger Volume (ft³)`    9.23e-15 ***
    New_cars_cleaned$`Wheelbase (in)`             < 2e-16 ***
    New_cars_cleaned$`Track Width, Front (in)`     0.0824 .  
 New_cars_cleaned$`Height, Overall (in)`       < 2e-16 ***
    New_cars_cleaned$`SAE Net Torque @ RPM`        0.4081    
 New_cars_cleaned$`Air Bag-Frontal-Driver`      0.0631 .  
 New_cars_cleaned$`Air Bag-Frontal-Passenger`   0.8263    
 ---
    Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
 
 Residual standard error: 10200 on 6553 degrees of freedom
 (25744 observations deleted due to missingness)
 Multiple R-squared:  0.7954,	Adjusted R-squared:  0.7948 
 F-statistic:  1415 on 18 and 6553 DF,  p-value: < 2.2e-16 "
 
 #changing double type to numeric
 df<-x[1:64]
 df<-lapply(df,as.numeric)

glimpse(x)
Rows: 5,237
Columns: 63
$ Price                         <dbl> 40600, 45500, 43600, 37400, 42600, ...
$ MPG                           <dbl> 22, 22, 22, 22, 21, 21, 21, 19, 20,...
$ passenger_capacity            <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,...
$ passenger_door                <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,...
$ weight_lbs                    <dbl> 3790, 3829, 3821, 3783, 4026, 4068,...
$ passenger_volume_ft3          <dbl> 104, 104, 104, 104, 104, 104, 104, ...
$ wheelbase_in                  <dbl> 108, 108, 108, 108, 108, 108, 108, ...
$ trach_width_front_in          <dbl> 64, 64, 64, 64, 64, 64, 64, 63, 63,...
$ height_in                     <dbl> 66, 66, 66, 66, 66, 66, 66, 65, 65,...
$ fuel_tank_gal                 <dbl> 17, 17, 17, 17, 17, 17, 17, 16, 16,...
$ sae_net_torque_rpm            <dbl> 280, 280, 280, 280, 280, 280, 280, ...
$ sae_net_horsepower_rpm        <dbl> 272, 272, 272, 272, 272, 272, 272, ...
$ Displacement                  <dbl> 2, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 4,...
$ trans_type                    <dbl> 2, 2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5,...
$ airbag_frontal_driver         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ airbag_frontal_passenger      <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ airbag_passenger_switch_onoff <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ airbag_side_body_front        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ airbag_side_body_rear         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ aibag_side_head_front         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ aibag_side_head_rear          <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ brakes_abs                    <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ child_safety_doorlocks_rear   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ daytime_running_lights        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ traction_control              <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ night_vision                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ rollover_protection_bars      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ fog_lamps                     <dbl> 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0,...
$ parking_aid                   <dbl> 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 1, 0,...
$ tire_pressure_motor           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ backup_camera                 <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ stability_control             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
$ Mileage                       <dbl> 50000, 50000, 50000, 50000, 50000, ...
$ basic_years                   <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,...
$ corrosion_miles_km            <dbl> 150000, 150000, 150000, 150000, 150...
$ corrosion_years               <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,...
$ Drivetrain_Years              <dbl> 70000, 70000, 70000, 70000, 70000, ...
$ Curb_to                       <dbl> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,...
$ road_size                     <dbl> 39, 39, 39, 39, 39, 39, 39, 39, 39,...
$ Make                          <dbl> 50000, 50000, 50000, 50000, 50000, ...
$ front_wid                     <dbl> 2019, 2019, 2019, 2019, 2019, 2019,...
$ rim                           <dbl> 55, 55, 45, 55, 55, 55, 45, 60, 60,...
$ Engine_Electric               <dbl> 19, 19, 20, 19, 19, 19, 20, 18, 18,...
$ Engine_Flat                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_I6                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_l3                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_l4                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_l5                     <dbl> 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0,...
$ Engine_l6                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_V6                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_V8                     <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1,...
$ Engine_V10                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_V12                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_W8                     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Engine_W12                    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Drivetrain_4WD                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Drivetrain_AWD                <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Drivetrain_FWD                <dbl> 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0,...
$ Drivetrain_RWD                <dbl> 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1,...
$ Category_Car                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Category_Pickup               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Category_SUV                  <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
$ Category_Van                  <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
> x<-lapply(x,as.numeric)
> glimpse(x)
List of 63
$ Price                        : num [1:5237] 40600 45500 43600 37400 42600 47500 45600 37500 41000 39700 ...
$ MPG                          : num [1:5237] 22 22 22 22 21 21 21 19 20 20 ...
$ passenger_capacity           : num [1:5237] 5 5 5 5 5 5 5 5 5 5 ...
$ passenger_door               : num [1:5237] 4 4 4 4 4 4 4 4 4 4 ...
$ weight_lbs                   : num [1:5237] 3790 3829 3821 3783 4026 ...
$ passenger_volume_ft3         : num [1:5237] 104 104 104 104 104 104 104 104 104 104 ...
$ wheelbase_in                 : num [1:5237] 108 108 108 108 108 108 108 106 106 106 ...
$ trach_width_front_in         : num [1:5237] 64 64 64 64 64 64 64 63 63 63 ...
$ height_in                    : num [1:5237] 66 66 66 66 66 66 66 65 65 65 ...
$ fuel_tank_gal                : num [1:5237] 17 17 17 17 17 17 17 16 16 16 ...
$ sae_net_torque_rpm           : num [1:5237] 280 280 280 280 280 280 280 252 252 252 ...
$ sae_net_horsepower_rpm       : num [1:5237] 272 272 272 272 272 272 272 279 279 279 ...
$ Displacement                 : num [1:5237] 2 2 2 2 2 2 2 4 4 4 ...
$ trans_type                   : num [1:5237] 2 2 2 2 2 2 2 5 5 5 ...
$ airbag_frontal_driver        : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ airbag_frontal_passenger     : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ airbag_passenger_switch_onoff: num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ airbag_side_body_front       : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ airbag_side_body_rear        : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ aibag_side_head_front        : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ aibag_side_head_rear         : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ brakes_abs                   : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ child_safety_doorlocks_rear  : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ daytime_running_lights       : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ traction_control             : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ night_vision                 : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ rollover_protection_bars     : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ fog_lamps                    : num [1:5237] 0 1 1 0 0 1 1 0 0 0 ...
$ parking_aid                  : num [1:5237] 1 1 1 0 1 1 1 0 0 0 ...
$ tire_pressure_motor          : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ backup_camera                : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ stability_control            : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...
$ Mileage                      : num [1:5237] 50000 50000 50000 50000 50000 50000 50000 50000 50000 50000 ...
$ basic_years                  : num [1:5237] 4 4 4 4 4 4 4 4 4 4 ...
$ corrosion_miles_km           : num [1:5237] 150000 150000 150000 150000 150000 150000 150000 150000 150000 150000 ...
$ corrosion_years              : num [1:5237] 5 5 5 5 5 5 5 5 5 5 ...
$ Drivetrain_Years             : num [1:5237] 70000 70000 70000 70000 70000 70000 70000 70000 70000 70000 ...
$ Curb_to                      : num [1:5237] 6 6 6 6 6 6 6 6 6 6 ...
$ road_size                    : num [1:5237] 39 39 39 39 39 39 39 39 39 39 ...
$ Make                         : num [1:5237] 50000 50000 50000 50000 50000 50000 50000 50000 50000 50000 ...
$ front_wid                    : num [1:5237] 2019 2019 2019 2019 2019 ...
$ rim                          : num [1:5237] 55 55 45 55 55 55 45 60 60 60 ...
$ Engine_Electric              : num [1:5237] 19 19 20 19 19 19 20 18 18 18 ...
$ Engine_Flat                  : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_I6                    : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_l3                    : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_l4                    : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_l5                    : num [1:5237] 1 1 1 1 1 1 1 0 0 0 ...
$ Engine_l6                    : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_V6                    : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_V8                    : num [1:5237] 0 0 0 0 0 0 0 1 1 1 ...
$ Engine_V10                   : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_V12                   : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_W8                    : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Engine_W12                   : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Drivetrain_4WD               : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Drivetrain_AWD               : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Drivetrain_FWD               : num [1:5237] 0 0 0 0 1 1 1 1 0 0 ...
$ Drivetrain_RWD               : num [1:5237] 1 1 1 1 0 0 0 0 1 1 ...
$ Category_Car                 : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Category_Pickup              : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Category_SUV                 : num [1:5237] 0 0 0 0 0 0 0 0 0 0 ...
$ Category_Van                 : num [1:5237] 1 1 1 1 1 1 1 1 1 1 ...

#changing to dataframe
class(x)
[1] "list"
> x<-as.data.frame(x)
> class(x)
[1] "data.frame"

