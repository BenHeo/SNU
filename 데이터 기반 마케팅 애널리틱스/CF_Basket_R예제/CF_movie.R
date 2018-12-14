movies = read.csv('D:\\Teaching\\MarketingAnalytics\\LectureNote\\Examples\\E07_CollaborativeFilteringExample\\movies.csv', stringsAsFactors = F)
str(movies)
#'data.frame':	9125 obs. of  3 variables:
# $ movieId: int  1 2 3 4 5 6 7 8 9 10 ...
# $ title  : Factor w/ 9123 levels "Breathless (\xc3\u0080 bout de souffle) (1960)",..: 8299 4318 3422 8646 2763 3592 6858 8251 7671 3289 ...
# $ genres : Factor w/ 902 levels "(no genres listed)",..: 329 394 687 646 596 242 687 377 2 124 ...


ratings = read.csv('D:\\Teaching\\MarketingAnalytics\\LectureNote\\Examples\\E07_CollaborativeFilteringExample\\ratings.csv')
str(ratings)
#'data.frame':	100004 obs. of  4 variables:
# $ userId   : int  1 1 1 1 1 1 1 1 1 1 ...
# $ movieId  : int  31 1029 1061 1129 1172 1263 1287 1293 1339 1343 ...
# $ rating   : num  2.5 3 3 2 4 2 2 2 3.5 2 ...
# $ timestamp: int  1260759144 1260759179 1260759182 1260759185 1260759205 1260759151 1260759187 1260759148 1260759125 1260759131 ...


library(reshape2)
# Create ratings matrix so that each row is a user and each column is a movie, we will have a 671 by 9066 matrix
ratingmat = dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)  
ratingmat = as.matrix(ratingmat[,-1]) #remove userId variable

library(ggplot2)
image(ratingmat, main = "Raw Ratings")

library(recommenderlab)
#Coerce the rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")
 
#Normalize the data
ratingmat_norm <- normalize(ratingmat)
 
#Create Recommender Model. 
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#        Parameter 'method' decides similarity measure: Cosine or Jaccard

# Here we use UBCF, cosine similarity, nearest neighbors = 30
recommender_model = Recommender(ratingmat_norm, method = "UBCF", param=list(method="Cosine",nn=30))

# other methods
#recommender_model = Recommender(ratingmat_norm, method = "UBCF", param=list(method="Jaccard",nn=30))
#recommender_model = Recommender(ratingmat_norm, method = "IBCF", param=list(method="Jaccard",nn=30))
#recommender_model = Recommender(ratingmat_norm, method = "POPULAR")


recom = predict(recommender_model, ratingmat[1], n=10) #Obtain top 10 recommendations for 1st user in dataset
recom_list = as(recom, "list") #convert recommenderlab object to readable list
 
# Obtain recommendations
recom_num = NULL
recom_title = NULL
recom_genre = NULL
for (i in c(1:10)){
 recom_num = c(recom_num,  movies[as.integer(recom_list[[1]][i]),1])
 recom_title = c(recom_title,  movies[as.integer(recom_list[[1]][i]),2])
 recom_genre = c(recom_genre,  movies[as.integer(recom_list[[1]][i]),3])
 }
myrecom = data.frame(recom_num,recom_title, recom_genre)
cat('Recommendation for the user \n')
print(myrecom)