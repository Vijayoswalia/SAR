home = read.csv("home_data.csv")
home
head(home)
names(home)
df = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+floors+zipcode, data=home)
df
summary(df)
df1 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+zipcode, data=home)
summary(df1)
df2 = lm(price~bedrooms+sqft_living+sqft_lot+zipcode, data=home)
summary(df2)
