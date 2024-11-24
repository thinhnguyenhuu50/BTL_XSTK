dirty_data <- read.csv("~/Desktop/dirty_data.csv") #code doc du lieu
head(dirty_data,10) #in ket qua ra 10 dong

#lam sach du lieu
#chon loc cac bien can su dung va them no vao data_1
data_1<-dirty_data[,c("nearest_warehouse","order_price","delivery_charges",
              "customer_lat","customer_long","coupon_discount","order_total"
              ,"season","is_expedited_delivery","distance_to_nearest_warehouse",
              "is_happy_customer")]
head(data_1,10)

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
describe(data_2, fast=TRUE)# ham describle chi cho cac bien lien tuc khong danh cho bien phan loai

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
  ggplot(data_1, aes(x=order_price, y=order_total)) + geom_point(shape=23, color="lightblue")
  labs(title="Order Total $ Order Price scatter plot")
## xoa ngoai lai  
  ggplot(data_4, aes(x=order_price, y=order_total)) + geom_point(shape=23, color="darkred")
  labs(title="Order Total $ Order Price scatter plot")
  
# giua delivery charges va order total
  ## khong xoa ngoai lai
  ggplot(data_1, aes(x=delivery_charges, y=order_total)) + geom_point(shape=23, color="lightblue")
  labs(title="Order Total $ Delivery Charges scatter plot")
  ## Xoa ngoai lai
  
  ### ket luan duoc delivery charges khong anh huong toi order total (Khong co quan he tuyen tinh )
  
# giua coupon discount va order total
  ## khong xoa ngoai lai
  ggplot(data_1, aes(x=coupon_discount, y=order_total)) + geom_point(shape=23, color="lightblue")
  labs(title="Order Total $ Coupon Discount scatter plot")
  ## xoa ngoai lai
  ggplot(data_4, aes(x=coupon_discount, y=order_total)) + geom_point(shape=23, color="darkred")
  labs(title="Order Total $ Coupon Discount scatter plot")
  
#giua distance_to_nearest_warehouse va order total
  ##khong xoa ngoai lai
  ggplot(data_1, aes(x=distance_to_nearest_warehouse, y=order_total)) + geom_point(shape=23, color="lightblue")
  labs(title="Order Total $ Distance to nearest warehouse scatter plot")
  ## xoa ngoai lai
  ggplot(data_4, aes(x=distance_to_nearest_warehouse, y=order_total)) + geom_point(shape=23, color="darkred")
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