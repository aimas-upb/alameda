"""
This script runs a prediction setup on the prepared EMA dataset which runs in the following manner:
  - Select a train_period day interval which contains the number of days that go into training
  - The next day after the train_period is the one for which we want to do the prediction
  
  - For each train_period we perform time series classification through **aggregate value** feature computation:
    - we compute the mean, 25th - median - 75th percentile values, min and max of the features we have
      collected in the prepared EMA dataset

@author: Alexandru Sorici
"""

import os
import numpy as np
import pandas as pd
import time

from typing import Dict, List
from argparse import ArgumentParser
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
from sklearn.svm import SVC
from sklearn.model_selection import LeaveOneGroupOut, cross_validate
from sklearn.metrics import confusion_matrix
from sklearn.metrics import accuracy_score, make_scorer, precision_score, recall_score
from sklearn.impute import SimpleImputer
from imblearn.combine import SMOTEENN
from imblearn.over_sampling import SMOTE

MAX_DAYS = 14

def percentile(n):
    def percentile_(x):
        return np.nanpercentile(x, n)
    percentile_.__name__ = 'percentile_%s' % n
    return percentile_

def make_ema_train_dataset(dataset_path: str, all_subjects: List[str],
                           nr_train_days: int, sanpar_target_df: pd.DataFrame) -> pd.DataFrame:

    train_df = None

    for subject in all_subjects:
        print("Computing train_df for subject: %s" % subject)
        ema_dataset_file = os.path.join(dataset_path, subject + "_ema_dataset.csv")
        subject_ema_dataset_df = pd.read_csv(ema_dataset_file, index_col=0)

        for day_window_start in range(1, MAX_DAYS - nr_train_days):
            day_window_end = day_window_start + nr_train_days - 1
            prediction_day = day_window_end + 1

            day_window_df = subject_ema_dataset_df[(subject_ema_dataset_df["day"] >= day_window_start) &
                                                   (subject_ema_dataset_df["day"] <= day_window_end)]\
                                                   .drop("day", axis=1)

            window_group = day_window_df.groupby(by=["day_period"])
            window_group_df = window_group.agg(["min", "max", "std", "mean",
                                                                         percentile(25), percentile(50),
                                                                         percentile(75)])
            window_group_df.columns = window_group_df.columns.to_flat_index()
            window_group_df.columns = map(lambda x: "_".join(list(x)) if x[1] else x[0],
                                          window_group_df.columns.to_list())

            subject_training_series = None
            for day_period_name in window_group_df.index.values.tolist():
                day_period_series = window_group_df.loc[day_period_name].copy()
                day_period_series.rename(lambda x: x + "_" + day_period_name, inplace=True)
                if subject_training_series is None:
                    subject_training_series = day_period_series.copy()
                else:
                    subject_training_series = pd.concat([subject_training_series, day_period_series],
                                                        axis=0)

            # add subjectID and target val
            subject_training_series["subject"] = subject

            target = sanpar_target_df[(sanpar_target_df["day_idx"] == prediction_day) &
                                      (sanpar_target_df["ID"] == int(subject))]["target"]
            if target.size == 0:
                continue
            subject_training_series["sanpar_target"] = target.values[0]
            subject_training_series_reset = subject_training_series.reset_index()

            if train_df is None:
                train_df = pd.DataFrame(data=[subject_training_series_reset.loc[:,0].values],
                                        columns=subject_training_series_reset.loc[:, "index"].values.tolist())
            else:
                df = pd.DataFrame(data=[subject_training_series_reset.loc[:,0].values],
                                        columns=subject_training_series_reset.loc[:, "index"].values.tolist())
                train_df = pd.concat([train_df, df], axis=0, ignore_index=True)

    return train_df





def make_ema_and_wearable_train_dataset(dataset_path: str, all_subjects: List[str],
                                        nr_train_days: int, sanpar_target_df: pd.DataFrame) -> pd.DataFrame:
    pass


def confusion_matrix_scorer(clf, X, y):
      y_pred = clf.predict(X)
      cm = confusion_matrix(y, y_pred)
      return {'tn': cm[0, 0], 'fp': cm[0, 1],
              'fn': cm[1, 0], 'tp': cm[1, 1]}

if __name__ == "__main__":
    all_subjects = ["1100" + str(i).zfill(2) for i in range(1, 22) if i != 12]
    # all_subjects = ["110004"]

    parser = ArgumentParser(
        description="""A script to train predictors for the EMA PD dataset.""", add_help=True
    )
    
    parser.add_argument('--dataset-path', metavar='path', type=str, default="data/dataset")
    parser.add_argument('--nr-train-days', metavar="period_length", type=int, default=7)
    parser.add_argument('--use-wearables', default=False, action="store_true")
    parser.add_argument('--config-level', metavar="config_level", type=int, default=0)
    args = parser.parse_args()

    path = args.dataset_path
    nr_train_days = args.nr_train_days
    use_wearables = args.use_wearables

    LABELS = ["mostly_off", "indeterminate", "mostly_on"]

    if args.config_level <= 0:
        # setup the training dataset
        sanpar_target_file = os.path.join(path, "sanpar_target.csv")
        sanpar_target_df = pd.read_csv(sanpar_target_file, index_col=0)

        if not use_wearables:
            train_df = make_ema_train_dataset(path, all_subjects, nr_train_days, sanpar_target_df)
        else:
            train_df = make_ema_and_wearable_train_dataset(path, all_subjects, nr_train_days, sanpar_target_df)

        # save the train_df
        train_df_file = os.path.join(path, "train_dataset_nr_train-%i_use_wearables-%s.csv"
                                     % (nr_train_days, str(use_wearables)))
        train_df.to_csv(train_df_file)

        print("DONE CREATING TRAIN DATASET")

    if args.config_level <= 1:
        # run the actual training
        train_df_file = os.path.join(path, "train_dataset_nr_train-%i_use_wearables-%s.csv"
                                     % (nr_train_days, str(use_wearables)))
        train_df = pd.read_csv(train_df_file, index_col=0)

        # Create our imputer to replace missing values with the mean e.g.
        imp = SimpleImputer(missing_values=np.nan, strategy='mean')

        # train a RandomForest on this stuff
        X = train_df.drop(columns=["subject", "sanpar_target"])
        y = train_df["sanpar_target"].to_numpy()
        groups = train_df["subject"].to_numpy()

        imp.fit(X)
        X_imp = imp.transform(X)

        # Define a RandomForest Classifier
        # clf = RandomForestClassifier(n_estimators=200, max_depth=20, max_features=0.75,
        #                              class_weight="balanced_subsample", max_samples=0.75,
        #                              random_state=42)

        # Define a GradientBoosting Classifier
        clf = GradientBoostingClassifier(loss="deviance", learning_rate=0.75, n_estimators=200, subsample=1.0,
                                         max_depth=20, max_features=0.75,
                                         random_state=42)

        logo = LeaveOneGroupOut()
        for idx, (train_idx, test_idx) in enumerate(logo.split(X_imp, y, groups=groups)):
            # print("TRAIN:", train_idx, "TEST:", test_idx)
            X_train, X_test = X_imp[train_idx], X_imp[test_idx]
            y_train, y_test = y[train_idx], y[test_idx]

            # resample
            sm = SMOTE(random_state=42, k_neighbors=1)
            X_train_smote, y_train_smote = sm.fit_resample(X_train, y_train)
            print(np.unique(y_train_smote, return_counts=True))

            X_train_df = pd.DataFrame(data=X_train_smote, columns=list(X.columns.values))
            X_test_df = pd.DataFrame(data=X_test, columns=list(X.columns.values))

            clf.fit(X_train_df, y_train_smote)
            print("Confusion matrix for split %i " % (idx + 1))
            print(confusion_matrix(y_test, clf.predict(X_test_df), labels=LABELS))
            print("There were %i cases of indeterminate to predict." % np.sum(y_test == "indeterminate"))

            importance_idx_sorted = np.argsort(clf.feature_importances_)
            print("Top 20 important features: ", list(reversed(clf.feature_names_in_[importance_idx_sorted[-20:]])))
            print("Top 20 importance: ", list(reversed(clf.feature_importances_[importance_idx_sorted[-20:]])))

            print("")

        # cv_res = cross_validate(rf_clf, X_imp, y, groups=groups, cv=LeaveOneGroupOut(),
        #                         scoring= {
        #                             # "confusion_matrix": make_scorer(confusion_matrix),
        #                             "accuracy": make_scorer(accuracy_score),
        #                             "precision": make_scorer(precision_score)
        #                         }
        #                         # scoring=confusion_matrix_scorer
        #                         )
