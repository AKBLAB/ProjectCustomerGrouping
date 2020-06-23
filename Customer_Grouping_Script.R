######################################
# Passig customer data sample file
######################################

#####################################
# Libraries 

library(plotrix) # for pie chart
library(purrr)
library(ggplot2)



customer_data=read.csv("D:/Data Science/ProjectCustomerGrouping/customer_grouping_dataset/Customers_data.csv")
str(customer_data)

names(customer_data)
head(customer_data)

## Summary of the data by Age, Annual Income, Spending

summary(customer_data$Age)
summary(customer_data$Annual.Income..k..)

## evaluating standard deviation

sd(customer_data$Annual.Income..k..)
sd(customer_data$Spending.Score..1.100.)


## Analysing Gender distribution (male, Female) in our datsset

    a=table(customer_data$Gender)
    barplot(a,main="Gender Comparision Graph",
            ylab="Count",
            xlab="Gender",
            col=rainbow(2),
            legend=rownames(a))
    
      # Result : number of females are higher than males
    
    ## Trying to see the percentage of females vs males through pie graph
    
    pct=round(a/sum(a)*100)
    lbs=paste(c("Female","Male")," ",pct,"%",sep=" ")
    
    pie3D(a,labels=lbs,
          main="Pie Chart displaying Ratio of Female vs Male")
    
      # RESULT : Female are 56% and Male are 44%

## Analysing how age is distributed in our dataset
    
    summary(customer_data$Age)
    
    ## plotting a histogram to see count of persons within an age grooup
    
    hist(customer_data$Age,
         col="blue",
         main="Histogram for Age count",
         xlab="Age",
         ylab="Frequency",
         labels=TRUE)
    
    # RESULT : Maximum persons are in the age group of 30 to 35. Mimimum age is 18 and max 70.
    

## Analysing annual income of customer
    
    ## plotting a histogram to anlyse the income
    
    summary(customer_data$Annual.Income..k..)
    hist(customer_data$Annual.Income..k..,
         col="#660033",
         main="Histogram for Annual Income",
         xlab="Annual Income",
         ylab="Frequency",
         labels=TRUE)
    
    # RESULT : Average income of customer is 70 with Min as 15 and max as 137
    
    ## plotting density plot ot see the distribution of the income
    
    plot(density(customer_data$Annual.Income..k..),
         col="green",
         main="Density Plot for Annual Income",
         xlab="Annual Income Class",
         ylab="Density")
    polygon(density(customer_data$Annual.Income..k..),
            col="#00ff00")

    # RESULT : Distribution is normal
    
## Analysing spending score of customers in our dataset
    
    ## Plotting Histogram to see the spending trend of customers
    
    hist(customer_data$Spending.Score..1.100.,
         main="HistoGram for Spending Score",
         xlab="Spending Score",
         ylab="Frequency",
         col="#00ff00",
         labels=TRUE)
    
    # RESULT : Customers between spending score 40 & 50 are highest spenders
    
    summary(customer_data$Spending.Score..1.100.)
    
    # Result : Min spend 1 and max spend 99
    

### Using k-mean clusterring to identify the customer group to target as per the product designed.
    
    # Determine the intra-cluster sum of square
    
    set.seed(123)
     
    iss <- function(k) {
      kmeans(customer_data[,3:5],k,iter.max=100,nstart=100,algorithm="Lloyd" )$tot.withinss
    }
    k.values <- 1:10
    iss_values <- map_dbl(k.values, iss)
    plot(k.values, iss_values,
         type="b", pch = 19, frame = FALSE, 
         xlab="No of clusters K",
         ylab="Intra-clusters sum of squares")
    
    # Calculate k-Means using 6 as our optimal cluster
    k_cust_data<-kmeans(customer_data[,3:5],6,iter.max=100,nstart=50,algorithm="Lloyd")
    k_cust_data
    
    # RESULT : Our 2 principal components are 4 & 6

## Visualize as per important components using k-means clustering
    
    imp_components=prcomp(customer_data[,3:5],scale=FALSE) 
    summary(imp_components)
    imp_components$rotation[,1:2]

    set.seed(1)

    #3rd Group : Based on Important Components (PC1, PC2, PC3)
    kCols=function(vec){
      cols=rainbow (length (unique (vec)))
      return (cols[as.numeric(as.factor(vec))])
    }
    cluster_val<-k_cust_data$cluster; 
    cluster_as_str<-as.character(cluster_val);
    
    plot(imp_components$x[,1:2], col =kCols(cluster_val),pch =19,xlab ="K-means",ylab="Important Components")
    legend("bottomleft",unique(cluster_as_str),fill=unique(kCols(cluster_val)))
    
    # RESULT : 1st GROUP :- Medium Principal Component and Low K-means
    #          2nd Group :- Medium Principal Component and High K-means
    #          3rd Group :- Medium Principal Component and Medium K-means
    #          4th Group :- Medium Principal Component and Medium K-means
    #          5th Group :- High Principal Component and Medium K-means
    #          6th Group :- Low Principal Component and variable K-means i.e. minimum to high
    
    
    
    # 2nd Group : Grouping Income vs Spending
    ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
      geom_point(stat = "identity", aes(color = as.factor(k_cust_data$cluster))) +
      scale_color_discrete(name=" ",
                           breaks=c("1", "2", "3", "4", "5","6"),
                           labels=c("1st Group", "2nd Group", "3rd Group", "4th Group", "5th Group","6th Group")) +
      ggtitle("Grouping of  Customers", subtitle = "Using K-means Clustering")
    
    # RESULT : 1st GROUP :- Customer with High Annual Income and High Spend
    #          2nd Group :- Customer with High Annual Income and Low Spend
    #          3rd Group :- Customer with Low Annual Income and Low Spend
    #          4th Group :- Customer with Medium Annual Income and Medium Spend
    #          5th Group :- Customer with Low Annual Income and High Spend
    #          6th Group :- Customer with Medium Annual Income and Medium Spend
    
    
    
    # 3rd Group : Spending vs Age
    ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
      geom_point(stat = "identity", aes(color = as.factor(k_cust_data$cluster))) +
      scale_color_discrete(name=" ",
                           breaks=c("1", "2", "3", "4", "5","6"),
                           labels=c("1st Group", "2nd Group", "3rd Group", "4th Group", "5th Group","6th Group")) +
      ggtitle("Grouping of Customers", subtitle = "Using K-means Clustering")
    
    # RESULT : 1st GROUP :- Customer with 25 - 40 Age group have Highest Spend
    #          2nd Group :- Customer with 35 - 65 Age group have Low Spend
    #          3rd Group :- Customer with 15 - 40 Age group have Medium Spend
    #          4th Group :- Customer with 45 - 70 Age group have Medium Spend
    #          5th Group :- Customer with 20 - 35 Age group have Highest Spend
    #          6th Group :- Customer with 20 - 60 Age group have Low Spend
    
    
