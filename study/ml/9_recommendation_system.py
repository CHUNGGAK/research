# -*- coding: utf-8 -*-
"""
Created on Mon Mar 16 11:35:10 2020

@author: chgal
"""


import numpy as np

R = np.array([[4, np.NaN, np.NaN, 2, np.NaN],
              [np.NaN, 5, np.NaN, 3, 1],
              [np.NaN, np.NaN, 3, 4, 4],
              [5,2, 1, 2, np.NaN]])

num_users, num_items = R.shape
# 행의 길이는 num_user, 열의 길이는 num_items

K = 3

np.random.seed(1)

P = np.random.normal(scale = 1 / K, size = (num_users, K))
Q = np.random.normal(scale = 1 / K, size = (num_items, K))

from sklearn.metrics import mean_squared_error

def get_rmse(R, P, Q, non_zeros):
    full_pred_matrix = np.dot(P, Q.T)
    # 두 개의 분해된 행렬 P와 Q.T의 내적으로 예측 R 행렬 생성
    
    x_non_zero_ind = [non_zero[0] for non_zero in non_zeros]
    y_non_zero_ind = [non_zero[1] for non_zero in non_zeros]
    
    R_non_zeros = R[x_non_zero_ind, y_non_zero_ind]
    
    full_pred_matrix_non_zeros = full_pred_matrix[x_non_zero_ind, y_non_zero_ind]
    
    mse = mean_squared_error(R_non_zeros, full_pred_matrix_non_zeros)
    rmse = np.sqrt(mse)
    
    return rmse

non_zeros = [(i, j, R[i, j])
             for i in range(num_users)
             for j in range(num_items)
             if R[i, j] > 0]

steps = 1000
learning_rate = 0.01
r_lambda = 0.01

for step in range(steps):
    for i, j, r in non_zeros:
        eij = r - np.dot(P[i, :], Q[j, :].T)
        
        P[i, :] = P[i, :] + learning_rate * (eij * Q[j, :] - r_lambda * P[i, :])
        Q[j, :] = Q[j, :] + learning_rate * (eij * P[i, :] - r_lambda * Q[j, :])
        
        rmse = get_rmse(R, P, Q, non_zeros)
        
        if step % 50 == 0:
            print("### iteration setp : ", step, " rmse : ", rmse)
            
pred_matrix = np.dot(P, Q.T)

print("예측 행렬:\n", np.round(pred_matrix, 3))

import pandas as pd
import numpy as np
import warnings; warnings.filterwarnings("ignore")

movies = pd.read_csv("C:/Users/chgal/workspace/ml/data/tmdb_5000_movies.csv")

print(movies.shape)
movies.head(1)

movies_df = movies[["id", "title", "genres", "vote_average", "vote_count", "popularity",
                    "keywords", "overview"]]

pd.set_option("max_colwidth", 100)

movies_df[["genres", "keywords"]][:1]

from ast import literal_eval

movies_df["genres"] = movies_df["genres"].apply(literal_eval)
movies_df["keywords"] = movies_df["keywords"].apply(literal_eval)

movies_df["genres"] = movies_df["genres"].apply(lambda x: [y["name"] for y in x])
movies_df["keywords"] = movies_df["keywords"].apply(lambda x: [y["name"] for y in x])
movies_df[["genres", "keywords"]][:1]

from sklearn.feature_extraction.text import CountVectorizer

movies_df["genres_literal"] = movies_df["genres"].apply(lambda x: (" ").join(x))
count_vect = CountVectorizer(min_df = 0, ngram_range = (1, 2))
genre_mat = count_vect.fit_transform(movies_df["genres_literal"])
print(genre_mat.shape)

from sklearn.metrics.pairwise import cosine_similarity

genre_sim = cosine_similarity(genre_mat, genre_mat)
print(genre_sim.shape)
print(genre_sim[:1])

genre_sim_sorted_ind = genre_sim.argsort()[:, ::-1]
print(genre_sim_sorted_ind[:1])

def find_sim_movie(df, sorted_ind, title_name, top_n = 10):
    title_movie = df[df["title"] == title_name]
    
    title_index = title_movie.index.values
    similar_indexes = sorted_ind[title_index, :(top_n)]
    
    print(similar_indexes)
    similar_indexes = similar_indexes.reshape(-1)
    
    return df.iloc[similar_indexes]

similar_movies = find_sim_movie(movies_df, genre_sim_sorted_ind, "The Godfather",
                                10)
similar_movies[["title", "vote_average"]]

similar_movies[["title",
                "vote_average",
                "vote_count"]].sort_values("vote_average", ascending = False)[:10]

C = movies_df["vote_average"].mean()
m = movies_df["vote_count"].quantile(0.6)
print("C: ", round(C, 3), "m: ", round(m, 3))

percentile = 0.6
m = movies_df["vote_count"].quantile(percentile)
C = movies_df["vote_average"].mean()

def weighted_vote_average(record):
    v = record["vote_count"]
    R = record["vote_average"]
    
    return((v / (v + m)) * R + (m / (m + v)) * C)

movies["weighted_vote"] = movies.apply(weighted_vote_average, axis = 1)

movies[["title",
        "vote_average",
        "weighted_vote",
        "vote_count"]].sort_values("weighted_vote", ascending = False)[:10]

def find_sim_movie(df, sorted_ind, title_name, top_n = 10):
    title_movie = df[df["title"] == title_name]
    
    title_index = title_movie.index.values
    similar_indexes = sorted_ind[title_index, :(top_n)]
    similar_indexes = similar_indexes.reshape(-1)
    similar_indexes = similar_indexes[similar_indexes != title_index]
    
    return df.iloc[similar_indexes].sort_values("weighted_vote", ascending = False)[:top_n]

similar_movies = find_sim_movie(movies, genre_sim_sorted_ind, "The Godfather", 10)
similar_movies[["title", "vote_average", "weighted_vote"]]

import pandas as pd
import numpy as np

movies = pd.read_csv("C:/Users/chgal/workspace/ml/data/ml-latest-small/movies.csv")
ratings = pd.read_csv("C:/Users/chgal/workspace/ml/data/ml-latest-small/ratings.csv")
print(movies.shape)
print(ratings.shape)

ratings = ratings[["userId", "movieId", "rating"]]
## timestamp 열은 필요없으므로 삭제
ratings_matrix = ratings.pivot_table("rating", index = "userId", columns = "movieId")
ratings_matrix.head(3)

rating_movies = pd.merge(ratings, movies, on = "movieId")

ratings_matrix = rating_movies.pivot_table("rating", index = "userId", columns = "title")
## long format을 wide format으로 변형

ratings_matrix = ratings_matrix.fillna(0)
ratings_matrix.head(3)

ratings_matrix_T = ratings_matrix.transpose()
ratings_matrix_T.head(3)

from sklearn.metrics.pairwise import cosine_similarity

item_sim = cosine_similarity(ratings_matrix_T)

item_sim_df = pd.DataFrame(data = item_sim, index = ratings_matrix.columns, 
                           columns = ratings_matrix.columns)
print(item_sim_df.shape)
item_sim_df.head(3)

item_sim_df["Godfather, The (1972)"].sort_values(ascending = False)[:6]
item_sim_df["Inception (2010)"].sort_values(ascending = False)[1:6]

def predict_rating(ratings_arr, item_sim_arr):
    ratings_pred = ratings_arr.dot(item_sim_arr) / np.array([np.abs(item_sim_arr).sum(axis = 1)])
    
    return ratings_pred

ratings_pred = predict_rating(ratings_matrix, item_sim_df.values)
ratings_pred_matrix = pd.DataFrame(data = ratings_pred,
                                   index = ratings_matrix.index,
                                   columns = ratings_matrix.columns)
ratings_pred_matrix.head(3)

from sklearn.metrics import mean_squared_error

def get_mse(pred, actual):
    pred = pred[actual.nonzero()].flatten()
    actual = actual[actual.nonzero()].flatten()
    
    return mean_squared_error(pred, actual)

print("아이템 기반 모든 최근접 이웃 MSEl ", get_mse(ratings_pred.values, ratings_matrix.values))

def predict_rating_topsim(ratings_arr, item_sim_arr, n = 20):
    pred = np.zeros(ratings_arr.shape)
    
    for col in range(ratings_arr.shape[1]):
        top_n_items = np.argsort(item_sim_arr[:, col])[n-1:-1]
        ## n번째 영화와 타 영화 간의 유사도 값을 추출
        ## 추출한 값을 내림차순으로 정렬하여 인덱스를 top_n_items에 할당
        
        for row in range(ratings_arr.shape[0]):
            pred[row, col] = item_sim_arr[col, :][top_n_items].dot(
                ratings_arr[row, :][top_n_items].T
                )
            ## 유사도 값 * 평점 값 / (유사도 절댓값의 합) = pred
            pred[row, col] /= np.sum(np.abs(item_sim_arr[col, :][top_n_items]))
            
    return pred

ratings_pred = predict_rating_topsim(ratings_matrix.values,  item_sim_df.values, n = 20)
print("아이템 기반 모든 최근접 TOP 20 이웃 MSEl ", get_mse(ratings_pred.values, ratings_matrix.values))

ratings_pred_matrix = pd.DataFrame(data = ratings_pred,
                                   index = ratings_matrix.index,
                                   columns = ratings_matrix.columns)

user_rating_id = ratings_matrix.loc[9, :]
user_rating_id[user_rating_id > 0].sort_values(ascending = False)[:10]

def get_unseen_movies(ratings_matrix, userId):
    user_rating = ratings_matrix.loc[userId, :]
    
    already_seen = user_rating[user_rating > 0].index.tolist()
    
    movies_list = ratings_matrix.columns.tolist()
    
    unseen_list = [movie for movie in movies_list if movie not in already_seen]
    
    return unseen_list

def recomm_movie_by_userid(pred_df, userId, unseen_list, top_n = 10):
    recomm_movies = pred_df.loc[userId, unseen_list].sort_values(ascending = False)[:top_n]
    
    return recomm_movies

unseen_list = get_unseen_movies(ratings_matrix, 9)

recomm_movies = recomm_movie_by_userid(ratings_pred_matrix, 9, unseen_list, top_n = 10)

recomm_movies = pd.DataFrame(data = recomm_movies,
                             index = recomm_movies.index,
                             columns = ["pred_score"])
recomm_movies
