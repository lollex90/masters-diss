import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV
from keras.models import Sequential, Model
from keras.layers import Dense, Conv2D, MaxPooling2D, Flatten, Resizing, BatchNormalization, Dropout
import tensorflow as tf
import random
import os
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import Lasso
import xgboost as xgb

def create_column_names(prefix, general_characteristics):
    general_columns = [prefix + "_" + char for char in general_characteristics]
    log_bin_columns = [prefix + "_log_" + str(i) for i in range(1, 11)] + general_columns
    idr_bin_columns = [prefix + "_idr_" + str(i) for i in range(1, 11)] + general_columns
    
    return general_columns, log_bin_columns, idr_bin_columns

def build_model_and_predict(pre_war_data, prediction_data, selected_columns, model_type, param_grid, log_transform, scale = False, total_metrics = False, diff = False):

    if total_metrics:
        total_mae = 0
        total_mpe = 0

        # initialise a df to store total mae and mpe for each year
        metrics = pd.DataFrame(columns = ["year", "mae", "mpe"])

        if diff:
            min_year = 2013
            denominator = 9
        else:
            min_year = 2012
            denominator = 10
        
        for year in range(min_year, 2022):

            train_data = pre_war_data[pre_war_data['year'] != year]
            test_data = pre_war_data[pre_war_data['year'] == year]     
            mae, mpe, y_pred = build_model(train_data, test_data, selected_columns, model_type, param_grid, log_transform, scale)

            total_mae += mae/denominator
            total_mpe += mpe/denominator

            # add the metrics to the df using concat
            metrics = pd.concat([metrics, pd.DataFrame({"year": [year], "mae": [mae], "mpe": [mpe]})], ignore_index=True)


        gdp_change, y_pred, best_params= predict_with_model(pre_war_data, prediction_data, selected_columns, model_type, param_grid, log_transform, scale, diff)
        
        return total_mae, total_mpe, gdp_change, y_pred, best_params, metrics

    else:

        train_data = pre_war_data[pre_war_data['year'] != 2021]
        test_data = pre_war_data[pre_war_data['year'] == 2021]
        mae, mpe, y_pred = build_model(train_data, test_data, selected_columns, model_type, param_grid, log_transform, scale)
        gdp_change, y_pred, best_params = predict_with_model(pre_war_data, prediction_data, selected_columns, model_type, param_grid, log_transform, scale)

        return mae, mpe, gdp_change, y_pred, best_params

def build_train_test_sets(selected_columns, train_data, test_data, log_transform = False, scale = False):

    # select columns
    train_data_selected = train_data[["real_gdp", "region", "year"] + selected_columns]
    test_data_selected = test_data[["real_gdp", "region", "year"] + selected_columns]

    if log_transform:
        # real_gdp and columns that contain the word"sum" are log transformed
        train_data_selected["real_gdp"] = np.log(train_data_selected["real_gdp"])
        test_data_selected["real_gdp"] = np.log(test_data_selected["real_gdp"])

        for column in selected_columns:
            if "sum" in column:
                train_data_selected[column] = np.log(train_data_selected[column])
                test_data_selected[column] = np.log(test_data_selected[column])

    if scale:
        # scale the data
        scaler = MinMaxScaler()
        train_data_selected[selected_columns] = scaler.fit_transform(train_data_selected[selected_columns])
        test_data_selected[selected_columns] = scaler.transform(test_data_selected[selected_columns])

        # scale the real_gdp
        train_data_selected["real_gdp"] = scaler.fit_transform(train_data_selected[["real_gdp"]])
        test_data_selected["real_gdp"] = scaler.transform(test_data_selected[["real_gdp"]])

    # one hot encode region
    train_data_selected = pd.get_dummies(train_data_selected, columns=["region"])
    test_data_selected = pd.get_dummies(test_data_selected, columns=["region"])

    return train_data_selected, test_data_selected

def build_model(train_data, test_data, selected_columns, model_type, param_grid, log_transform = False, scale = False):

    # build train and test sets
    train_data_selected, test_data_selected = build_train_test_sets(selected_columns, train_data, test_data, log_transform, scale)

    # get input and output data
    X_train = train_data_selected.drop(columns=["real_gdp", "year"])
    y_train = train_data_selected["real_gdp"]

    X_test = test_data_selected.drop(columns=["real_gdp", "year"])
    y_test = test_data_selected["real_gdp"]

    # build model
    if model_type == "xgboost":
        model_test = xgb.XGBRegressor()
    elif model_type == "random_forest":
        model_test = RandomForestRegressor()
    elif model_type == "lasso":
        model_test = Lasso()

    # hyperparameter tuning
    grid_search = GridSearchCV(estimator=model_test, param_grid=param_grid, cv=5, n_jobs=-1)
    grid_search.fit(X_train, y_train)
    best_model = grid_search.best_estimator_
    # random_search = RandomizedSearchCV(estimator=model_test, param_distributions=param_grid, n_iter=10, cv=5, scoring='accuracy', n_jobs=-1)
    # random_search.fit(X_train, y_train)
    # best_model = random_search.best_estimator_

    # make predictions
    y_pred = best_model.predict(X_test)
    
    # calculate mae and mpe
    mae = np.mean(abs(y_pred - y_test))
    mpe = np.mean(abs(100*(y_pred - y_test) / y_test))

    return mae, mpe, y_pred


def predict_with_model(pre_war_data, prediction_data, selected_columns, model_type, param_grid, log_transform = False, scale = False, diff = False):

    # build pre war and prediction sets
    pre_war_data_selected, prediction_data_selected = build_train_test_sets(selected_columns, pre_war_data, prediction_data, log_transform, scale)

    # get input and output data
    X_pre_war = pre_war_data_selected.drop(columns=["real_gdp", "year"])
    y_pre_war = pre_war_data_selected["real_gdp"]
    data_2021 = pre_war_data_selected[pre_war_data_selected["year"] == 2021]

    X_prediction = prediction_data_selected.drop(columns=["real_gdp", "year"])

    # build model, objective: absolute error
    if model_type == "xgboost":
        model_pred = xgb.XGBRegressor()
    elif model_type == "random_forest":
        model_pred = RandomForestRegressor()
    elif model_type == "lasso":
        model_pred = Lasso()

    # hyperparameter tuning
    grid_search = GridSearchCV(estimator=model_pred, param_grid=param_grid, cv=10, n_jobs=-1)
    grid_search.fit(X_pre_war, y_pre_war)
    best_model = grid_search.best_estimator_
    best_params = best_model.get_params()

    # make predictions
    if diff == False:
        y_pred = best_model.predict(X_prediction)

        # calculate the predicted change in the real gdp on the national level
        # if log_transform:
        #     y_pred = np.exp(y_pred)
        pred_gdp_change = 100*(np.sum(y_pred) - np.sum(data_2021["real_gdp"])) / np.sum(data_2021["real_gdp"])
    else:
        y_pred = best_model.predict(X_prediction)

        # calculate the predicted change in the real gdp on the national level
        # if log_transform:
        #     y_pred = np.exp(y_pred)
        pred_gdp_change = 100*(np.sum(y_pred)) / np.sum(data_2021["real_gdp"])

    return pred_gdp_change, y_pred, best_params

def define_cnn(n_features = 10, n_conv = 2, n_dense = 2, input_shape = (765, 1076, 1), res_x = 300, res_y = 440):
    model = Sequential()
    model.add(Resizing(res_x, res_y, input_shape=input_shape))
    for i in range(n_conv):
        model.add(Conv2D(8 * (2**i), (3, 3), activation='relu'))
        model.add(MaxPooling2D((2, 2)))
        model.add(BatchNormalization())
    model.add(Flatten())
    for i in range(n_dense-1, 0, -1):
        model.add(Dense(8 * (2**i), activation='relu'))
    model.add(Dense(n_features))
    return model

def set_seed(seed=42):
    random.seed(seed)
    np.random.seed(seed)
    tf.random.set_seed(seed)
    # Ensuring TensorFlow runs deterministically (may impact performance)
    os.environ['TF_DETERMINISTIC_OPS'] = '1'
    os.environ['PYTHONHASHSEED'] = str(seed)

def extract_features(model, country, country_2021, country_2022, X_train, X_pred, X_test, n_features):

    # get the column names
    column_names = ["region", "year", "real_gdp"] + [f"feature_{i+1}" for i in range(n_features)]

    # get features for 2012-2020
    y_train = model.predict(X_train)
    country_stage_2 = pd.DataFrame(columns = column_names)
    country_train = country[country["year"] < 2021]
    country_train.reset_index(drop=True, inplace=True)
    for i in range(len(country_train)):
        country_stage_2.loc[i, "region"] = country_train["region"][i]
        country_stage_2.loc[i, "year"] = country_train["year"][i]
        country_stage_2.loc[i, "real_gdp"] = country_train["real_gdp"][i]
        for j in range(n_features):
            country_stage_2.loc[i, f"feature_{j+1}"] = y_train[i][j]

    # get the features for 2021
    y_test = model.predict(X_test)
    for i in range(len(country_2021)):
        for j in range(n_features):
            country_2021.loc[i, f"feature_{j+1}"] = y_test[i][j]
    country_2021 = country_2021[column_names]

    # get the features for 2022
    y_pred = model.predict(X_pred)
    for i in range(len(country_2022)):
        for j in range(n_features):
            country_2022.loc[i, f"feature_{j+1}"] = y_pred[i][j]

    country_2022 = country_2022[column_names]

    # change all features to float
    for i in range(n_features):
        country_stage_2[f"feature_{i+1}"] = country_stage_2[f"feature_{i+1}"].astype(float)
        country_2021[f"feature_{i+1}"] = country_2021[f"feature_{i+1}"].astype(float)
        country_2022[f"feature_{i+1}"] = country_2022[f"feature_{i+1}"].astype(float)

    return country_stage_2, country_2021, country_2022

def extract_features_2(model, country, year, X_train, X_test, n_features):

    # get the column names
    column_names = ["region", "year", "real_gdp"] + [f"feature_{i+1}" for i in range(n_features)]

    # get features the training set
    y_train = model.predict(X_train)
    country_stage_2 = pd.DataFrame(columns = column_names)
    country_train = country[country["year"] != year]
    country_train.reset_index(drop=True, inplace=True)
    for i in range(len(country_train)):
        country_stage_2.loc[i, "region"] = country_train["region"][i]
        country_stage_2.loc[i, "year"] = country_train["year"][i]
        country_stage_2.loc[i, "real_gdp"] = country_train["real_gdp"][i]
        for j in range(n_features):
            country_stage_2.loc[i, f"feature_{j+1}"] = y_train[i][j]

    # get the features for the test set
    y_test = model.predict(X_test)
    country_test = country[country["year"] == year]
    country_test.reset_index(drop=True, inplace=True)
    for i in range(len(country_test)):
        for j in range(n_features):
            country_test.loc[i, f"feature_{j+1}"] = y_test[i][j]
    country_test = country_test[column_names]

    # change all features to float
    for i in range(n_features):
        country_stage_2[f"feature_{i+1}"] = country_stage_2[f"feature_{i+1}"].astype(float)
        country_test[f"feature_{i+1}"] = country_test[f"feature_{i+1}"].astype(float)

    return country_stage_2, country_test