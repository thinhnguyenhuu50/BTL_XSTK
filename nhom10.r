library(naniar)
library(lubridate)
library(dplyr)
library(questionr)
library(stringr)
library(xtable)
library(effectsize)
library(data.table)

dirty_data <- read.csv("dirty_data.csv") # Đọc dữ liệu
head(dirty_data, 10) # In 10 giá trị quan trắc cho mỗi biến

# Tiền xử lý dữ liệu
dim(dirty_data)
names(dirty_data)

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
# *Ket thuc* TIEN XU LY SO LIEU  ###############################################
# THống kê mô tả
#lam sach du lieu
#chon loc cac bien can su dung va them no vao data_1
data_1<-dirty_data[,c("nearest_warehouse","order_price","delivery_charges",
              "customer_lat","customer_long","coupon_discount","order_total"
              ,"season","is_expedited_delivery","distance_to_nearest_warehouse",
              "is_happy_customer")]
head(data_1,10)
summary(dirty_data,10)
    #kiem tra du lieu khuyet
library(questionr)
freq.na(data_1)
unique(data_1$nearest_warehouse)
unique(data_1$season)
unique(data_1$is_expedited_delivery)
unique(data_1$is_happy_customer)
#Sua du lieu bi sai
data_1$nearest_warehouse[data_1$nearest_warehouse == "nickolson"] <- "Nickolson"
data_1$nearest_warehouse[data_1$nearest_warehouse == "thompson"] <- "Thompson"

data_1$season[data_1$season == "winter"] <- "Winter"
data_1$season[data_1$season == "autumn"] <- "Autumn"
data_1$season[data_1$season == "summer"] <- "Summer"
data_1$season[data_1$season == "spring"] <- "Spring"
#kiem tra lai du lieu sau khi sua
unique(data_1$season)
unique(data_1$nearest_warehouse)






#Main 
# tach ra cac bien lien tuc
data_2<-data_1[,c("order_price","delivery_charges",
                      "customer_lat","customer_long","coupon_discount","order_total"
                      ,"distance_to_nearest_warehouse")]

library(psych)
describe(data_2, fast=TRUE)# ham describle chi cho cac bien lien tuc, khong danh cho bien phan loai

#tach ra cac bien dinh luong
data_3<-data_1[,c("nearest_warehouse"
                    ,"season","is_expedited_delivery",
                      "is_happy_customer")]
# chuyen sang dang factor
data_3$nearest_warehouse<-as.factor(data_3$nearest_warehouse)
data_3$season<-as.factor(data_3$season)
data_3$is_expedited_delivery<-as.factor(data_3$is_expedited_delivery)
data_3$is_happy_customer<-as.factor(data_3$is_happy_customer)

#thong ke so luong cho cac bien phan loai
summary(data_3)


                      # De tai hoi quy: phan tich cac anh huong den chi phi don hang


           ##Histogram graph

library(ggplot2)
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



# mo hinh hoi qui da bien tuyen tinh 
dirty_data <- read.csv("E:/dirty_data.csv")
head (dirty_data,10)

new_data<-dirty_data [,c("nearest_warehouse" , "order_price" , "delivery_charges" , "order_total" , "season" , "coupon_discount" ,"is_expedited_delivery" , "distance_to_nearest_warehouse"  )]
head (new_data,10) 


unique(new_data$season)
new_data$season[new_data$season =='spring'] <-'Spring' 
new_data$season [new_data$season == 'summer'] <- 'Summer'
new_data$season [new_data$season == 'autumn'] <- 'Autumn'
new_data$season [new_data$season == 'winter'] <- 'Winter'
unique (new_data$season )

unique(new_data$nearest_warehouse)
new_data$nearest_warehouse [new_data$nearest_warehouse == 'thompson'] <- 'Thompson'
new_data$nearest_warehouse [new_data$nearest_warehouse == 'nickolson'] <- 'Nickolson'
unique(new_data$nearest_warehouse)

new_data$is_expedited_delivery[new_data$is_expedited_delivery =='False'] <-'0' 
new_data$is_expedited_delivery[new_data$is_expedited_delivery =='True'] <-'1' 
new_data$is_expedited_delivery <- as.numeric(new_data$is_expedited_delivery)
######kiem dinh phan phoi chuan#######
{print(shapiro.test(new_data$order_price))
  print(shapiro.test(new_data$order_total))
  print(shapiro.test(new_data$delivery_charges))
  print(shapiro.test(new_data$coupon_discount))
  print(shapiro.test(new_data$distance_to_nearest_warehouse))}
#Kiem tra moi quan he tuyen tinh giua delivery voi is_expedited
model_0<- lm(delivery_charges~is_expedited_delivery, data=new_data)
summary(model_0)
# chu xu li ngoai lai :

##model_1
model_1 <-lm(order_total~order_price+delivery_charges+coupon_discount+distance_to_nearest_warehouse ,data= new_data )
summary (model_1)

# xu li ngoai lai : 
new_data_2 <- new_data

rm.out <- function (x , na.rm = TRUE , ...) {
  qnt <- quantile (x , probs =c(.25 , .75) , na.rm = na.rm , ...)
  H <- 1.5 * IQR(x , na.rm = na.rm)
  y <- x
  y [ x < ( qnt [1] - H ) ] <- NA
  y [ x > ( qnt [2] + H ) ] <- NA
  y
}
# ap vao cac bien lien tuc : 
new_data_2$order_price = rm.out (new_data_2$order_price) 
new_data_2$delivery_charges = rm.out (new_data_2$delivery_charges) 
new_data_2$order_total = rm.out (new_data_2$order_total) 
new_data_2$coupon_discount = rm.out (new_data_2$coupon_discount) 
new_data_2$distance_to_nearest_warehouse = rm.out (new_data_2$distance_to_nearest_warehouse) 

library (questionr) 
freq.na(new_data_2)

new_data_2<-na.omit(new_data_2)
# mo hinh 2 : sau khi loai bo ngoai lai 


model_2 <-lm(order_total~order_price+delivery_charges+coupon_discount+distance_to_nearest_warehouse ,data= new_data_2 )
summary (model_2)


# mo hinh 3 : sau khi bo di vai bien ( delivery_charge ,  distance) 

model_3 <-lm(order_total~order_price+coupon_discount ,data= new_data_2 )
summary (model_3)

# ve hinh de kiem dinh mo hinh 33 la mo hinh hoi qui da bo tuyen tinh 

par(mfrow=c(2,2))
plot(model_3)
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

# ANOVA bắt đầu
#đọc file, đưa ra bảng dữ liệu:
sxtk <- read.csv("dirty_data.csv", header=T)
attach(sxtk)
loi = data.frame(coupon_discount)
library(data.table)
df <- data.frame(loi)
dt <- data.table(df)
freq_table <- dt[,.N, by= coupon_discount]
colnames(freq_table) <- c("coupon_discount","count")
freq_table
# ANOVA bắt đầu

#anova ảnh hưởng của chiếc khấu đến giá
#đọc file, đưa ra bảng dữ liệu:
loi1 = data.frame(coupon_discount)
df <- data.frame(loi1)
dt <- data.table(df)
freq_table <- dt[,.N, by= coupon_discount]
colnames(freq_table) <- c("coupon_discount","count")
freq_table

#anova:
thongke=data.frame(coupon_discount, order_price)
anova1nhanto <- aov(order_price ~ coupon_discount, data=thongke)
summary(anova1nhanto)

#thực hiện
eta_squared(anova1nhanto)

#ANOVA1 ket thuc
#ANOVA2 bat dau
