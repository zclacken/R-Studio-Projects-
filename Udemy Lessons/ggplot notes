movies = read.csv("P2-Movie-Ratings.csv")
library(ggplot2)

#-----------------------------------------------------------Exploratory Analysis 
head(movies)
colnames(movies)
colnames(movies) = c("Film","Genre","CriticRating","AudienceRating","BudgetMillions","Year")
colnames(movies)
str(movies)
movies$Film = as.factor(movies$Film)
movies$Genre = as.factor(movies$Genre)
movies$Year = as.factor(movies$Year)
str(movies)
summary(movies)

#--------------------------------------------------------------------Basic ggplot
#Aesthetics 1: axes
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating)) 

#Geometry:adds plot points 
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating)) +
  geom_point()

#Aesthetics 2: Add Color
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, color = Genre)) +
  geom_point()

#Aesthetics 3: Change Size of Plot Points
ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                          color = Genre, size = BudgetMillions)) +
  geom_point()

#-----------------------------------------------------------Plotting with Layers
#Using ggplot, the plot is an object
#thus, for each graphical layer, a + sign is required
#geometry is a layer  

#Create blank plot with axes as object p 
p = ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                              color = Genre, size = BudgetMillions)) 
p # p is now the above portion of the plot
#type ""+ geom_point" to add plot points to the plot p
p = p+ geom_point()
p

#plot lines instead of points
p = ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                              color = Genre, size = BudgetMillions))
p = p + geom_line()
p

#plot lines and points
p = ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                              color = Genre, size = BudgetMillions))
p= p+ geom_line() + geom_point() #in this order, lines are plotted on top of points, switch to plot points on top of lines
p

#---------------------------------------------------------------Overriding Aesthetics
#1. Override the size of plot points
q = ggplot(data = movies, aes(x=CriticRating,y=AudienceRating,
                               color = Genre, size = BudgetMillions))
q= q+geom_point(aes(size = CriticRating)) #override the size of points 
q

#2. Overide the color of plot points
q = ggplot(data = movies, aes(x=CriticRating,y=AudienceRating,
                              color = Genre, size = BudgetMillions))
q= q+ geom_point(aes(color = BudgetMillions)) #override the color of points
q

#3. Override an axis
q = ggplot(data = movies, aes(x=CriticRating,y=AudienceRating,
                              color = Genre, size = BudgetMillions))
q= q+ geom_point(aes(x = BudgetMillions)) +    #override the x-axis
  xlab("Budget Millions $$$")                  #Change the axis label 
q

#4.Multiple Geometries - adjust parameters of each geometry  
q = ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                              color = Genre, size = BudgetMillions ))
q= q+ geom_point() + geom_line(size = 1) #Change line thickness 
q

#------------------------------------------------------------Mapping vs. Setting

r = ggplot(data=movies, aes(x=CriticRating, y = AudienceRating))
r+ geom_point()

#Mapping adds aesthetics according to a  variable 
#Setting adds aesthetics according to a specified parameter 

#Adding color to a chart 
#1. Mapping 
r + geom_point(aes(color = Genre))

#2. Setting 
r + geom_point(color = "DarkGreen")

#Adjusting point size
#1. Mapping
r + geom_point(aes(size = BudgetMillions))

#2. Setting
r + geom_point(size = 10)

#Combining mapping and setting(I figured this out)
r + geom_point(aes(color = BudgetMillions), size =2)

#----------------------------------------------------Histograms & Density Charts

#Histograms 
s = ggplot(data = movies, aes(x = BudgetMillions))
s + geom_histogram(binwidth = 10)
#map color to histogram
s+geom_histogram(binwidth = 10,aes(fill = Genre))
#add a border to histogram bars
s+geom_histogram(binwidth = 10,aes(fill = Genre),color = "Black")

#Density Chart
s + geom_density(aes(fill = Genre), position = "stack")

#-------------------------------------------------------------Starting Layer Tips

#1. 
t = ggplot(data = movies, aes(x=AudienceRating))
t + geom_histogram(binwidth = 10,fill = "White", color = "Blue")

#2. This approach allows you to establish the data set as an object without typing it each time 
t = ggplot(data = movies)
t+geom_histogram(binwidth = 10,aes(x=AudienceRating),fill = "White", 
                 color = "Red")


#-----------------------------------------------------Statistical Transformations

##Add a Smoothing Line
u = ggplot(data = movies,(aes(x = CriticRating, 
                              y = AudienceRating, color = Genre)))
u+ geom_point() + geom_smooth(fill = NA) #fill = NA removes the confidence bands, if undesired

##Box Plots
u = ggplot(data = movies, aes(x = Genre, y=AudienceRating, 
           color = Genre)) 
u + geom_boxplot()
u + geom_boxplot(size=0.6)

#add scatter points to a box plot (points over boxes)
u + geom_boxplot(size=0.6)+ geom_jitter(size=0.5)
#OR (boxes over points; alpha add transparency to boxes)
u + geom_jitter(size= 0.5) + geom_boxplot(size =0.6, alpha = 0.5)

u2 = ggplot(data = movies, aes(x = Genre, y=CriticRating, 
                               color = Genre)) 
u2+ geom_boxplot() + geom_jitter(size = 0.5)

#-----------------------------------------------------------Facets 


##Facet Grid (grid with multiple charts: one for each variable)
v = ggplot(data = movies, aes(x=CriticRating, y= AudienceRating, 
                              color = Genre))
#1. Group Plots by Variable Horizontally  
v + geom_point(size = 1) +
  facet_grid(Genre~.) 
  
#2. Group plots by Variable Vertically 
v + geom_point(size = 1)+ facet_grid(.~Year)

#3. Group plots by two Variables: One Vertical; One Horizontal 
v + geom_point(size = 1)+ facet_grid(Genre~Year)

#4. Add a smoother to each plot 
v + geom_point(size = 1)+ geom_smooth() + facet_grid(Genre~Year)

#5. With aesthetics 
v + geom_point(aes(size = BudgetMillions))+ geom_smooth() + facet_grid(Genre~Year)

#Give each plot its own y-axis scale
w = ggplot(data = movies,aes(BudgetMillions))
w + geom_histogram(binwidth = 10,aes(fill=Genre),color = "Black")+
  facet_grid(Genre~.,scales = "free")

#-------------------------------------------------------Coordinates

#Limit Axes and Zoom into Axes 

m = ggplot(data = movies,aes(x=CriticRating, y= AudienceRating,
                             size = BudgetMillions, color = Genre))
m + geom_point()

#1.Limit Axes - Removes rows of data from data frame not withing the axes range 
n = ggplot(data = movies, aes(x= BudgetMillions))
n + geom_histogram(binwidth = 10,aes(fill = Genre),color = "Black") +ylim(0,50)

#2. Zoom - view chart within an axial range without removing any data (BETTER!!)
n + geom_histogram(binwidth = 10, aes(fill = Genre),color = "Black") +
  coord_cartesian(ylim = c(0,50))

#3. Zoom into charts of a facet grid
v + geom_point(size = 1) + geom_smooth() + 
  facet_grid(Genre~Year) + coord_cartesian(ylim = c(0,100))

#---------------------------------------------------------------Themes

o = ggplot(data = movies, aes(x = BudgetMillions))
h = o + geom_histogram(binwidth = 10,aes(fill = Genre),color = "Black")

#Axes Labels
h
h + xlab("Money Axis")+ ylab("Number of Movies")

#label formatting
h + xlab("Money Axis")+ ylab("Number of Movies")+
  theme(axis.title.x = element_text(color = "DarkGreen", size = 30),
        axis.title.y = element_text(color = "Red", size = 30))

#tick mark formatting
h + 
  xlab("Money Axis")+ 
  ylab("Number of Movies")+
  theme(axis.title.x = element_text(color = "DarkGreen", size = 15),
        axis.title.y = element_text(color = "Red", size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10))


#Legend Formatting
h + 
  xlab("Money Axis")+ 
  ylab("Number of Movies")+
  theme(axis.title.x = element_text(color = "DarkGreen", size = 15),
        axis.title.y = element_text(color = "Red", size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1))

#Plot Title
h + 
  xlab("Money Axis")+ 
  ylab("Number of Movies")+
  ggtitle("Movie Budget Distribution")+  #add a title to the plot 
  theme(axis.title.x = element_text(color = "DarkGreen", size = 15),
        axis.title.y = element_text(color = "Red", size = 15),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size=10),
        #legend theme
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        #title theme
        plot.title = element_text(color = "DarkBlue",size=15, 
                                  family = "Courier", hjust = 0.5),
        )













