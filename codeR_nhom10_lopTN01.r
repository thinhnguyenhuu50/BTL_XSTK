library(naniar)
library(lubridate)
library(dplyr)
library(questionr)
library(stringr)
library(xtable)
library(effectsize)
library(data.table)
library(psych)
library(ggplot2)
library(corrplot)
library(nortest)
library(lmtest)
library(car)
library(carData)
library(phia)
library(agricolae)

dirty_data <- read.csv("dirty_data.csv") # Đọc dữ liệu
head(dirty_data, 5) # In 5 giá trị quan trắc cho mỗi biến
str(dirty_data)

#Kiem tra du lieu khuyet
anyNA(dirty_data)

# Bắt đầu Tiền xử lý dữ liệu
unique(dirty_data$date)
unique(dirty_data$season)
unique(dirty_data$nearest_warehouse)

# + Sua dinh dang ngay/thang/nam
df <- data.frame(date_parsed = parse_date_time(dirty_data$date, orders = c("ymd", "dmy", "mdy")))
df$date_parsed <- as.Date(df$date_parsed)

dirty_data$date <- df$date_parsed

# + Sua bien season 
df <- data.frame(season_orginal = c(dirty_data$season))
df$sum <- mday(dirty_data$date) + month(dirty_data$date)*100
df <- df %>%
  mutate(season_fixed = case_when(
    sum %in% 301:530 ~ 'Autumn',
    sum %in% 601:831 ~ 'Winter',
    sum %in% 901:1130 ~ 'Spring',
    TRUE ~ 'Summer'))
dirty_data$season <- df$season_fixed

# + Sua ten warehouse
dirty_data$nearest_warehouse <- str_to_title(dirty_data$nearest_warehouse)

# + Tinh lai tong chi phi
df <- dirty_data %>%
  select(order_price, delivery_charges, coupon_discount, order_total)

df <- df %>%
  mutate(calculated_order_total = order_price*(100 - df$coupon_discount)/100 + delivery_charges)

print(subset(df, calculated_order_total != order_total))
print(sum(df$calculated_order_total != df$order_total))

# + Tinh lai khoang cach toi warehouse
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Differences in coordinates
  delta_lat <- lat2 - lat1
  delta_lon <- lon2 - lon1
  
  # Haversine formula
  a <- sin(delta_lat / 2)^2 + cos(lat1) * cos(lat2) * sin(delta_lon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  # Radius of the Earth in kilometers
  R <- 6371
  
  # Distance in kilometers
  distance <- R * c
  return(distance)
}

# Toa do cua cac warehouse
warehouses <- data.frame(
  name = c("Nickolson", "Thompson", "Bakers"),
  lat = c(-37.818595, -37.8126732, -37.8099961),
  lon = c(144.969551, 144.9470689, 144.99523200000002)
)

df <- dirty_data %>%
  select(customer_lat, 
         customer_long, 
         nearest_warehouse)

# Sua lai vi do
df$customer_lat <- df$customer_lat * ifelse(df$customer_lat > 0, -1, 1)
dirty_data$customer_lat <- dirty_data$customer_lat * ifelse(dirty_data$customer_lat > 0, -1, 1)

df <- df %>%
  mutate(
    distance_to_Nickolson = haversine_distance(customer_lat, customer_long, warehouses$lat[1], warehouses$lon[1]),
    distance_to_Thompson = haversine_distance(customer_lat, customer_long, warehouses$lat[2], warehouses$lon[2]),
    distance_to_Bakers = haversine_distance(customer_lat, customer_long, warehouses$lat[3], warehouses$lon[3]),
    distance_to_nearest_warehouse_fixed = pmin(distance_to_Nickolson, distance_to_Thompson, distance_to_Bakers),
    
    # Assign the name of the nearest warehouse
    nearest_warehouse_fixed = case_when(
      distance_to_nearest_warehouse_fixed == distance_to_Nickolson ~ "Nickolson",
      distance_to_nearest_warehouse_fixed == distance_to_Thompson ~ "Thompson",
      distance_to_nearest_warehouse_fixed == distance_to_Bakers ~ "Bakers"
    )
  )

print(sum(df$nearest_warehouse == df$nearest_warehouse_fixed))

# Dinh dang factor
dirty_data$nearest_warehouse <- as.factor(dirty_data$nearest_warehouse)
dirty_data$season <- as.factor(dirty_data$season)
dirty_data$is_expedited_delivery <- as.factor(dirty_data$is_expedited_delivery)
dirty_data$is_happy_customer <- as.factor(dirty_data$is_happy_customer)
# *Ket thuc* TIEN XU LY SO LIEU  ###############################################

# THống kê mô tả
#chon loc cac bien can su dung va them no vao data_1
data_1<-dirty_data[,c("nearest_warehouse","order_price","delivery_charges",
              "customer_lat","customer_long","coupon_discount","order_total"
              ,"season","is_expedited_delivery","distance_to_nearest_warehouse",
              "is_happy_customer")]
head(data_1,10)
summary(dirty_data,10)

#Main 
# tach ra cac bien lien tuc
data_2<-data_1[,c("order_price","delivery_charges",
                      "customer_lat","customer_long","coupon_discount","order_total"
                      ,"distance_to_nearest_warehouse")]

describe(data_2, fast=TRUE)# ham describle chi cho cac bien lien tuc, khong danh cho bien phan loai

#tach ra cac bien dinh luong
data_3<-data_1[,c("nearest_warehouse"
                    ,"season","is_expedited_delivery",
                      "is_happy_customer")]

#thong ke so luong cho cac bien phan loai
summary(data_3)


                      # De tai hoi quy: phan tich cac anh huong den chi phi don hang


           ##Histogram graph
ggplot(data_2, aes(x=order_total)) + geom_histogram(bins = 15, fill="lightgreen") + 
  labs(title="Order Total histogram plot",x="Order Total (USD)")

ggplot(data_2, aes(x=order_price)) + geom_histogram(bins = 15, fill="lightgreen") + 
  labs(title="Order Price histogram plot",x="Order Price (USD)")

ggplot(data_2, aes(x=distance_to_nearest_warehouse)) + geom_histogram(bins = 15, fill="lightgreen") + 
  labs(title="Distance to nearest warehouse histogram plot",x="Distance to nearest warehouse (KM)")
# 3 bien tren deu chi co 1 cot, se khong thay ro duoc phan phoi cua no. Vi co diem ngoai lai. Do thi co phan phoi lech phai (Do doc nam o ben phai) 
# 3 bien tren can xu li ngoai lai
ggplot(data_2, aes(x=coupon_discount)) + geom_histogram(bins = 15, fill="lightgreen") + 
  labs(title="Coupon Discount histogram plot",x="Coupon Discount (%)")

ggplot(data_2, aes(x=delivery_charges)) + geom_histogram(bins = 15, fill="lightgreen") + 
  labs(title="Delivery Charges histogram plot",x="Delivery Charges (USD)")


# xu li ngoai lai (Q1- 1.5IQR, Q1+1.5IQR)
# PP xoa ngoai lai
data_4 <- data_1

rm.out <- function(x,na.rm = TRUE,...)
{
  qnt <- quantile(x,probs=c(.25, .75), na.rm = na.rm,...)
  H <- 1.5*IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
data_4$order_total = rm.out(data_4$order_total)
data_4$order_price = rm.out(data_4$order_price)
data_4$distance_to_nearest_warehouse = rm.out(data_4$distance_to_nearest_warehouse)

freq.na(data_4)
# thay duoc ti le khuyet nho >> de xuat xoa du lieu khuyet( xoa luon ngoai lai)
data_4 <- na.omit(data_4) # xoa cac diem ngoai lai

ggplot(data_4, aes(x=order_total)) + geom_histogram(bins = 15, fill="lightgreen") + 
  labs(title="Order Total histogram plot",x="Order Total (USD)")

ggplot(data_4, aes(x=order_price)) + geom_histogram(bins = 15, fill="lightgreen") + 
  labs(title="Order Price histogram plot",x="Order Price (USD)")

ggplot(data_4, aes(x=distance_to_nearest_warehouse)) + geom_histogram(bins = 15, fill="lightgreen") + 
  labs(title="Distance to nearest warehouse histogram plot",x="Distance to nearest warehouse (KM)")


    ## Scatter graph( Do thi phan tan)
# giua order price va order total
## khong xoa ngoai lai
  ggplot(data_1, aes(x=order_price, y=order_total)) + geom_point(shape=23, color="lightblue")+
  labs(title="Order Total $ Order Price scatter plot")
## xoa ngoai lai  
  ggplot(data_4, aes(x=order_price, y=order_total)) + geom_point(shape=23, color="darkred")+
  labs(title="Order Total $ Order Price scatter plot")
  
# giua delivery charges va order total
  ## khong xoa ngoai lai
  ggplot(data_1, aes(x=delivery_charges, y=order_total)) + geom_point(shape=23, color="lightblue")+
  labs(title="Order Total $ Delivery Charges scatter plot")
  ## Xoa ngoai lai
  ggplot(data_4, aes(x=delivery_charges, y=order_total)) + geom_point(shape=23, color="darkred")+
    labs(title="Order Total $ Delivery Charges scatter plot")

  ### ket luan duoc delivery charges khong anh huong toi order total (Khong co quan he tuyen tinh )
  
# giua coupon discount va order total
  ## khong xoa ngoai lai
  ggplot(data_1, aes(x=coupon_discount, y=order_total)) + geom_point(shape=23, color="lightblue")+
  labs(title="Order Total $ Coupon Discount scatter plot")
  ## xoa ngoai lai
  ggplot(data_4, aes(x=coupon_discount, y=order_total)) + geom_point(shape=23, color="darkred")+
  labs(title="Order Total $ Coupon Discount scatter plot")
  
#giua distance_to_nearest_warehouse va order total
  ##khong xoa ngoai lai
  ggplot(data_1, aes(x=distance_to_nearest_warehouse, y=order_total)) + geom_point(shape=23, color="lightblue")+
  labs(title="Order Total $ Distance to nearest warehouse scatter plot")
  ## xoa ngoai lai
  ggplot(data_4, aes(x=distance_to_nearest_warehouse, y=order_total)) + geom_point(shape=23, color="darkred")+
    labs(title="Order Total $ Distance to nearest warehouse scatter plot")
  
  ### khong anh huong toi order total
  
  ###boxplot graph
  # nearest warehouse
 ##chua xu li ngoai lai 
  ggplot(data_1, aes(x=nearest_warehouse, y=order_total)) + geom_boxplot()+
  labs(title="Plot of Order total per Nearest warehouse")
  ## xu li ngoai lai
  ggplot(data_4, aes(x=nearest_warehouse, y=order_total)) + geom_boxplot()+
  labs(title="Plot of Order total per Nearest warehouse")
  #season
  ##chua xu li ngoai lai 
  ggplot(data_1, aes(x=season, y=order_total)) + geom_boxplot()+
    labs(title="Plot of Order total per Season")
  ## xu li ngoai lai
  ggplot(data_4, aes(x=season, y=order_total)) + geom_boxplot()+
    labs(title="Plot of Order total per Season")
  #is_expedited_delivery
  ##chua xu li ngoai lai 
  ggplot(data_1, aes(x=is_expedited_delivery, y=order_total)) + geom_boxplot()+
    labs(title="Plot of Order total per is expedited delivery")
  ## xu li ngoai lai
  ggplot(data_4, aes(x=is_expedited_delivery, y=order_total)) + geom_boxplot()+
    labs(title="Plot of Order total per is expedited delivery")
  #is_happy_customer
  ##chua xu li ngoai lai 
  ggplot(data_1, aes(x=is_happy_customer, y=order_total)) + geom_boxplot()+
    labs(title="Plot of Order total per is happy customer")
  ## xu li ngoai lai
  ggplot(data_4, aes(x=is_happy_customer, y=order_total)) + geom_boxplot()+
    labs(title="Plot of Order total per is happy customer")
  
  # Tính ma trận tương quan
  cor_matrix <- cor(data_4)
  
  # Vẽ biểu đồ tương quan
  
  corrplot(cor_matrix, method = "circle", type = "full", 
           col = colorRampPalette(c("blue", "white", "red"))(200),
           tl.col = "red", tl.srt = 45,addCoef.col = "black")
  
##KET THUC THONG KE MO TA
  
# Data_4 đã được xử lý ngoại lai
  
# BAT DAU MO HINH HOI QUY TUYEN TINH
######kiem dinh phan phoi chuan#######
new_data <- dirty_data
{print(shapiro.test(new_data$order_price))
  print(shapiro.test(new_data$order_total))
  print(shapiro.test(new_data$delivery_charges))
  print(shapiro.test(new_data$coupon_discount))
  print(shapiro.test(new_data$distance_to_nearest_warehouse))}
#Kiem tra moi quan he tuyen tinh giua delivery voi is_expedited
model_0<- lm(delivery_charges~is_expedited_delivery+distance_to_nearest_warehouse, data=new_data)
summary(model_0)
# chua xu li ngoai lai :

##model_1
model_1 <-lm(order_total~order_price+delivery_charges+coupon_discount+distance_to_nearest_warehouse ,data= new_data )
summary (model_1)

# xu li ngoai lai : 
new_data_2 <- data_4

# mo hinh 2 : sau khi loai bo ngoai lai 


model_2 <-lm(order_total~order_price+delivery_charges+coupon_discount+distance_to_nearest_warehouse ,data= new_data_2 )
summary (model_2)


# mo hinh 3 : sau khi bo di vai bien ( delivery_charge ,  distance) 

model_3 <-lm(order_total~order_price+coupon_discount ,data= new_data_2 )
summary (model_3)
#tìm khoảng tin cậy cho các hệ số 
confint(model_3)
# Tính giá trị dự đoán từ mô hình hồi quy
predicted_order_total <- predict(model_3, newdata = new_data_2)

# So sánh giữa giá trị dự đoán và giá trị thực tế
comparison <- data.frame(
  Actual = new_data_2$order_total,
  Predicted =predicted_order_total,
  Difference = new_data_2$order_total -predicted_order_total
)



# Tính toán độ chênh lệch trung bình và độ lệch chuẩn của sai số
mean_difference <- mean(comparison$Difference)
sd_difference <- sd(comparison$Difference)

# In kết quả
cat("Sai số trung bình: ", mean_difference, "\n")
cat("Độ lệch chuẩn của sai số: ", sd_difference, "\n")

# Thực hiện kiểm định Durbin-Watson
dw_test_model_3 <- dwtest(model_3)

# In kết quả kiểm định
print(dw_test_model_3)
##Phân phối chuẩn của phần dư
# Trích xuất phần dư
residuals_model_3 <- residuals(model_3)

# Kiểm tra phân phối chuẩn bằng Shapiro-Wilk test
shapiro_test <- shapiro.test(residuals_model_3)
print(shapiro_test)

# Vẽ biểu đồ Histogram và Q-Q plot
par(mfrow = c(1, 2))  # Hiển thị 2 biểu đồ cạnh nhau
hist(residuals_model_3, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", breaks = 20)
qqnorm(residuals_model_3, main = "Q-Q Plot of Residuals")
qqline(residuals_model_3, col = "red")
##Phương sai dồng nhất

bp_test <- bptest(model_3)  # Kiểm định Breusch-Pagan
print(bp_test)
# Vẽ Scale-Location Plot
plot(model_3$fitted.values, sqrt(abs(resid(model_3))),
     main = "Scale-Location Plot",
     xlab = "Fitted values",
     ylab = "√|Residuals|",
     pch = 20,
     col = "blue")
abline(h = 0, col = "red", lwd = 2)

##  Tính tuyến tính 
# Vẽ Residuals vs Fitted Plot
plot(model_3$fitted.values, residuals(model_3),
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals",
     pch = 20,
     col = "blue")
abline(h = 0, col = "red", lwd = 2)  # Đường ngang tại Residual = 0
# KET THUC HOI QUY TUYEN TINH

# BẮT ĐẦU ANOVA1

#anova ảnh hưởng của mùa đến phí giao hàng
#đọc file, đưa ra bảng dữ liệu:
loi1 = data_4 %>%
  select(
    delivery_charges,
    season
  )
# 
#anova:
anova1 <- aov(delivery_charges ~ season, data=loi1)
summary(anova1)
#bội sau ANOVA
lsd1<- LSD.test(anova1, "season", console=TRUE)

#anova ảnh hưởng của yêu cầu giao hàng nhanh đến phí giao hàng
#đọc file, đưa ra bảng dữ liệu:
loi2 = data_4 %>%
  select(
    delivery_charges,
    is_expedited_delivery
  )
#anova:
anova11 <- aov(delivery_charges ~ is_expedited_delivery, data=loi2)
summary(anova11)
#bội sau ANOVA
lsd2<- LSD.test(anova11, "is_expedited_delivery", console=TRUE)

# TODO: is_expedited_delivery & delivery_charges
# TODO: is_expedited_delivery & season
#KẾT THÚC ANOVA1

#BAT DAU ANOVA2 
#doc va chon 3 cot tu file du lieu 
df <- data_4 %>%
  select(delivery_charges,
         is_expedited_delivery,
         season)
#chuyen doi 2 bien coupon_discount va delivery_charges la  thanh cac yeu to factor
df$is_expedited_delivery<-as.factor(df$is_expedited_delivery)
df$season<-as.factor(df$season)
df$delivery_charges<-as.numeric(df$delivery_charges)
#kiem dinh phan phoi chuan hoac gan chuan 
av_residual<-rstandard (aov(df$delivery_charges~df$is_expedited_delivery*df$season))
shapiro.test(av_residual)
#kiem tra phuong sai dong nhat hay ko 
leveneTest(delivery_charges ~ is_expedited_delivery * season, data = df)
#phan tich anova 2 yeu to 
# bang dulieu
xtabs(~is_expedited_delivery + season, data=df)
anova_model <- aov(delivery_charges ~ is_expedited_delivery * season, data=df)
#hien thi ket qua phan tich
summary(anova_model)
#ve do thi tuong tac
interaction.plot(
  x.factor = df$season,  # Biến season (trục x)
  trace.factor = df$is_expedited_delivery,  # Biến is_expedited_delivery (dạng "trace")
  response = df$delivery_charges,  # Biến cần phân tích (delivery_charges)
  fun = mean,  # Hàm tính giá trị trung bình
  type = "b",  # Kiểu đồ (cả điểm và đường nối)
  col = c("red", "blue"),  # Màu sắc cho từng nhóm is_expedited_delivery
  lty = 1:4,  # Kiểu đường (đường liền cho các nhóm is_expedited_delivery)
  pch = 19,  # Dấu chấm cho các điểm
  xlab = "Season",  # Nhãn trục x (mùa)
  ylab = "Mean Delivery Charges",  # Nhãn trục y
  main = "Interaction Plot: Delivery Charges by Season and Expedited Delivery"  # Tiêu đề biểu đồ
)
#phan tich ky hon 
testInteractions(anova_model)
#warning khong anh huong den ket qua 
#KET THUC ANOVA2