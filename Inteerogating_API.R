install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)
install.packages("ggplot2")
install.packages("plotly")
require(devtools)
library(plotly)

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myApplication <- oauth_app(appname = "Interrogating_API",
                   key = "08781ad7126dec1005a2",
                   secret = "5bf84844ed1b27500463c6ca6d4fc071ecb04e62")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- httr::config(token = github_token)
req <- GET("https://api.github.com/users/doryan1998/repos", gtoken)



# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "doryan1998/datasharing", "created_at"] 

-----------------------------------------------------------------------------------
  
  # Step1: Interrogating the GitHub API.  
  
  # The below will return the number of followers and public repositories in my personal GitHub.
myData <- fromJSON("https://api.github.com/users/doryan1998")
myData$followers      
myData$public_repos

myFollowers <- fromJSON("https://api.github.com/users/doryan1998/followers")
myFollowers$login   # the usernames of all users who follow me 
length <- length(myFollowers$login)
length

# The below returns specific details about my repositories.
repositories <- fromJSON("https://api.github.com/users/doryan1998/repos")
repositories$name # names of my public repositories
repositories$created_at # creation dates of my repositories
assignment <- fromJSON("https://api.github.com/repos/doryan1998/Java/commits")
assignment$commit$message # the message I included in each commit to my Java repository

healys10 <- fromJSON("https://api.github.com/users/healys10/following")
healys10$login

# Instead of viewing this information in a dataframe, I can convert it back to a JSON

myDataJSon <- toJSON(myData, pretty = TRUE)
myDataJSon

#  ----- Assignment 5: Visualisation with Github Interrogation 

# Gather data required for visualisations

# usernames that user 'andrew' is following
andrewFollowing = GET("https://api.github.com/users/jtleek/following", gtoken)
andrewFollowingContent = content(andrewFollowing)
andrewFollowingContent
# each of those users' data
andrewFollowing.DF = jsonlite::fromJSON(jsonlite::toJSON(andrewFollowingContent))

# usernames saved in a vector
id = andrewFollowing.DF$login
usernames = c(id)

# creation of empty vectors in a data frame
allusers = c()
allusers.DF = data.frame(
  Username = integer(),
  Following = integer(),
  Followers = integer(),
  Repositories = integer(),
  DateCreated = integer()
)

# loop through all usernames to add 
for (i in 1:length(usernames))
{
  # retrieve an individual users following list
  following_url = paste("https://api.github.com/users/", usernames[i], "/following", sep = "")
  following = GET(following_url, gtoken)
  followingContent = content(following)
  
  # skips user if they do not follow anybody
  if (length(followingContent) == 0)
  {
    next
  }
  
  # add followings to a dataframe and retrieve usernames
  following.DF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = following.DF$login
  
  # loop through 'following' users
  for (j in 1:length(followingLogin))
  {
    # check that the user is not already in the list of users
    if (is.element(followingLogin[j], allusers) == FALSE)
    {
      # add user to list of users
      allusers[length(allusers) + 1] = followingLogin[j]
      
      # retrieve data on each user
      following_url2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(following_url2, gtoken)
      followingContent2 = content(following2)
      following.DF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      # retrieve usernames of each account user is following
      following_number = following.DF2$following
      
      # retrieve each user's followers
      followers_number = following.DF2$followers
      
      # retrieve each user's number of repositories
      repos_number = following.DF2$public_repos
      
      # retrieve year that each user joined Github
      year_created = substr(following.DF2$created_at, start = 1, stop = 4)
      
      # add user's data to a new row in dataframe
      allusers.DF[nrow(allusers.DF) + 1, ] = c(followingLogin[j], following_number, followers_number, repos_number, year_created)
      
    }
    next
  }
  # stop when there are more than 250 users
  if(length(allusers) > 250)
  {
    break
  }
  next
}
Sys.setenv("plotly_username" = "doryan1998")
Sys.setenv("plotly_api_key" = "Afsq4VjAo8Na0czZHyVJ")


# Visual 1: Scatter plot of Followers vs. Repositories for each user, colour coded by year they joined GitHub
plot1 = plot_ly(data = allusers.DF, x = ~Repositories, y = ~Followers, 
                text = ~paste("Followers: ", Followers, "<br>Repositories: ", 
                              Repositories, "<br>Date Created:", DateCreated), color = ~DateCreated)
plot1

api_create(plot1, filename = "Followers vs. Repositories")

# Visual 2: Scatter plot of Followers vs. Following for each user
plot2 = plot_ly(data = allusers.DF, x = ~Following, y = ~Followers,colorscale='Viridis', text = ~paste("Following: ", Following, 
                                                                                                       "<br>Followers: ", Followers))
plot2

api_create(plot2, filename = "Followers vs. Following")

# Data required for visual 3: Bar chart of most popular languages used by each user
languages = c()

for (i in 1:length(allusers.DF))
{
  RepositoriesUrl = paste("https://api.github.com/users/", allusers[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, gtoken)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  RepositoriesNames = RepositoriesDF$name
  
  #Loop through users repos
  for (j in 1: length(RepositoriesNames))
  {
    #add repos to data frame
    RepositoriesUrl2 = paste("https://api.github.com/repos/", allusers[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, gtoken)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    language = RepositoriesDF2$language
    
    #Removes repos with unknown languages
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}


allLanguages = sort(table(languages), increasing=TRUE)

top10Languages = allLanguages[(length(allLanguages)-9):length(allLanguages)] 

#converts to dataframe
languageDF = as.data.frame(top10Languages)

#Plot data frame
plot3 = plot_ly(data = languageDF, x = languageDF$languages, y = languageDF$Freq, type = "bar")
plot3
api_create(plot3, filename = "10 Most Popular Languages")

# Visualisation 4: heatmap of number of followers by geographical location
followers.DF = allusers.DF$Followers
plot4 = plot_ly(allusers.DF, type='choropleth',
                locations=c("UK","Saudi Arabia","Kazakhstan","Iran", "Greenland","USA", "Italy", "Ireland","Norway","Chile","Egypt","Morocco","South Africa", "China", "Germany", "Australia", "Austria", "The Netherlands", "Switzerland", "Brazil", "Russia", "Mexico", "Thailand", "Canada", "India", "Ukraine", "Colombia"),
                locationmode='country names',
                colorscale='Viridis',
                z=c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,600,1000,1050,1200,1800,1400,1100,1250,1300))

plot4
api_create(plot4, filename = "Followers Geographical Locations")


