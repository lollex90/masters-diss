import pandas as pd
import numpy as np
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import GridSearchCV, RandomizedSearchCV

from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import Lasso
import xgboost as xgb

def create_column_names(prefix, general_characteristics):
    general_columns = [prefix + "_" + char for char in general_characteristics]
    log_bin_columns = [prefix + "_log_" + str(i) for i in range(1, 11)] + general_columns
    idr_bin_columns = [prefix + "_idr_" + str(i) for i in range(1, 11)] + general_columns
    
    return general_columns, log_bin_columns, idr_bin_columns

def build_model_and_predict(pre_war_data, prediction_data, selected_columns, model_type, param_grid, log_transform, scale = False, total_metrics = False):

    if total_metrics:
        total_mae = 0
        total_mpe = 0
        
        for year in range(2012, 2022):

            train_data = pre_war_data[pre_war_data['year'] != year]
            test_data = pre_war_data[pre_war_data['year'] == year]     
            mae, mpe, best_params = build_model(train_data, test_data, selected_columns, model_type, param_grid, log_transform, scale)

            total_mae += mae/10
            total_mpe += mpe/10

        gdp_change, best_params = predict_with_model(pre_war_data, prediction_data, selected_columns, model_type, param_grid, log_transform, scale)
        
        return total_mae, total_mpe, gdp_change, best_params

    else:

        train_data = pre_war_data[pre_war_data['year'] != 2021]
        test_data = pre_war_data[pre_war_data['year'] == 2021]
        mae, mpe, _ = build_model(train_data, test_data, selected_columns, model_type, param_grid, log_transform, scale)
        gdp_change, best_params = predict_with_model(pre_war_data, prediction_data, selected_columns, model_type, param_grid, log_transform, scale)

        return mae, mpe, gdp_change, best_params

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
    best_params = best_model.get_params()

    # random_search = RandomizedSearchCV(estimator=model_test, param_distributions=param_grid, n_iter=10, cv=5, scoring='accuracy', n_jobs=-1)
    # random_search.fit(X_train, y_train)
    # best_model = random_search.best_estimator_

    # make predictions
    y_pred = best_model.predict(X_test)
    
    # calculate mse and mpe
    mae = np.mean(abs(y_pred - y_test))
    mpe = np.mean(abs(100*(y_pred - y_test) / y_test))

    return mae, mpe, best_params, y_pred


def predict_with_model(pre_war_data, prediction_data, selected_columns, model_type, param_grid, log_transform = False, scale = False):

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
    grid_search = GridSearchCV(estimator=model_pred, param_grid=param_grid, cv=5, n_jobs=-1)
    grid_search.fit(X_pre_war, y_pre_war)
    best_model = grid_search.best_estimator_
    best_params = best_model.get_params()

    # make predictions
    y_pred = best_model.predict(X_prediction)

    # calculate the predicted change in the real gdp on the national level
    # if log_transform:
    #     y_pred = np.exp(y_pred)
    pred_gdp_change = 100*(np.sum(y_pred) - np.sum(data_2021["real_gdp"])) / np.sum(data_2021["real_gdp"])

    return pred_gdp_change, best_params