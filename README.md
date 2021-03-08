# real_estate_website_scrapping_to_predict_best_rent_price

#1.1 CONTEXT
#We represent an investment fund company which want to create a real estate agency in Toulouse. With the current situation (Covid-19), a lot of
#owners are selling their real estate.

#1.2 PROBLEM
#The company doesn't have any data regarding the Toulouse area and wants to know what is the right price of rent for their last acquired real estate.

#1.3 SOLUTION
#The Business Analytics department designed a new tool which can analyze all the offers on the web in order to create a coherent and precise dataset to predict
#the price with the features of the real estate.

#1.4 METHO
#1.4.1 The Business Analytics department will scrap the different offers' URLs of a website and will store the offers' URLs into a dataframe.
#1.4.2 From these URLs, the department will be able to extract all the features of a real estate (Price, Living Space, Heating Features...)
#1.4.3 All these features will be store into a new dataset
#NB : It's easier to split the scrap of the "Furnished" real estate and "Unfurnished" real estate
#1.4.4 From the last dataset, it's possible to make a multiple linear regression to get the perfect renting price.

