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
from imblearn.combine import SMOTEENN, SMOTETomek
from imblearn.over_sampling import SMOTE, BorderlineSMOTE, KMeansSMOTE
from imblearn.under_sampling import RepeatedEditedNearestNeighbours, EditedNearestNeighbours, TomekLinks
from sklearn.feature_selection import SelectKBest, f_classif

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
            window_group_df = window_group.agg([percentile(25), percentile(50), percentile(75)])
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



def make_ema_and_wearable_train_dataset(ema_dataset_path: str, all_subjects: List[str],
                                        nr_train_days: int, sanpar_target_df: pd.DataFrame) -> pd.DataFrame:
    train_df = None

    for subject in all_subjects:
        print("Computing train_df for subject: %s" % subject)
        ema_dataset_file = os.path.join(ema_dataset_path, subject + "_ema_dataset.csv")
        wearables_dataset_file = os.path.join(ema_dataset_path, subject + "_wearables_dataset.csv")

        subject_ema_dataset_df = pd.read_csv(ema_dataset_file, index_col=0)
        subject_wearables_dataset_df = pd.read_csv(wearables_dataset_file, index_col=0)

        wearable_day_set = set(subject_wearables_dataset_df["day"].tolist())

        for day_window_start in range(1, MAX_DAYS - nr_train_days):
            day_window_end = day_window_start + nr_train_days - 1
            prediction_day = day_window_end + 1

            window_day_set = set(range(day_window_start, day_window_end + 1))

            if window_day_set.issubset(wearable_day_set):
                day_window_ema_df = subject_ema_dataset_df[(subject_ema_dataset_df["day"] >= day_window_start) &
                                    (subject_ema_dataset_df["day"] <= day_window_end)].drop("day", axis=1)
                day_window_wearable_df = subject_wearables_dataset_df[(subject_wearables_dataset_df["day"] >= day_window_start) &
                                        (subject_wearables_dataset_df["day"] <= day_window_end)].drop("day", axis=1)

                # extract EMA series from window
                ema_window_group_df = day_window_ema_df.quantile([0.25, 0.5, 0.75], interpolation="nearest")
                reindexed_window_ema_data = {}
                percentiles = ema_window_group_df.index.tolist()
                for col in ema_window_group_df.columns.tolist():
                    for p in percentiles:
                        reindexed_window_ema_data[(col + "_" + str(p))] = [ema_window_group_df[col].loc[p]]

                window_ema_df = pd.DataFrame.from_dict(reindexed_window_ema_data)

                # extract wearables series from window
                wearable_window_group_df = day_window_wearable_df.quantile([0.25, 0.5, 0.75], interpolation="nearest")
                reindexed_window_wearable_data = {}
                for col in wearable_window_group_df.columns.tolist():
                    for p in percentiles:
                        reindexed_window_wearable_data[(col + "_" + str(p))] = [wearable_window_group_df[col].loc[p]]
                window_wearable_df = pd.DataFrame.from_dict(reindexed_window_wearable_data)

                subject_training_series = pd.concat([window_ema_df, window_wearable_df], axis=1)

                # add subjectID and target val
                subject_training_series["subject"] = subject

                target = sanpar_target_df[(sanpar_target_df["day_idx"] == prediction_day) &
                                          (sanpar_target_df["ID"] == int(subject))]["target"]
                if target.size == 0:
                    continue
                subject_training_series["sanpar_target"] = target.values[0]

                if train_df is None:
                    train_df = subject_training_series.copy()
                else:
                    train_df = pd.concat([train_df, subject_training_series], axis=0, ignore_index=True)

    return train_df


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
    
    parser.add_argument('--dataset-path', metavar='path', type=str, default="data/dataset_simplified")
    parser.add_argument('--nr-train-days', metavar="period_length", type=int, default=7)
    parser.add_argument('--use-wearables', default=False, action="store_true")
    parser.add_argument('--config-level', metavar="config_level", type=int, default=0)
    args = parser.parse_args()

    path = args.dataset_path
    nr_train_days = args.nr_train_days
    use_wearables = args.use_wearables

    LABELS = ["mostly_off", "indeterminate", "mostly_on"]

    if args.config_level == 0:
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

    if args.config_level == 1:
        # run the actual training
        train_df_file = os.path.join(path, "train_dataset_nr_train-%i_use_wearables-%s.csv"
                                     % (nr_train_days, str(use_wearables)))
        train_df = pd.read_csv(train_df_file, index_col=0)

        # train a RandomForest on this stuff
        X = train_df.drop(columns=["subject", "sanpar_target"])
        y = train_df["sanpar_target"].to_numpy()
        groups = train_df["subject"].to_numpy()

        # Create our imputer to replace missing values with the mean e.g.
        imp = SimpleImputer(missing_values=np.nan, strategy='mean')
        imp.fit(X)
        X_imp = imp.transform(X)
        X_imp_df = pd.DataFrame(data=X_imp, columns=list(X.columns.values))

        # filter features that have a very low variance (e.g. more than 80% of features have the same value)
        top_k = 40
        selector = SelectKBest(f_classif, k=top_k)
        X_new_data = selector.fit_transform(X_imp_df, y)
        X_new_df = pd.DataFrame(data=X_new_data, columns=list(selector.get_feature_names_out()))

        # top_k = len(X_imp_df.columns)
        # X_new_df = X_imp_df

        # Define a RandomForest Classifier
        # clf = RandomForestClassifier(n_estimators=200, max_depth=20, max_features=0.75,
        #                              class_weight="balanced_subsample", max_samples=0.75,
        #                              random_state=42)

        # Define a GradientBoosting Classifier
        clf = GradientBoostingClassifier(loss="deviance", learning_rate=0.3, n_estimators=300, subsample=1.0,
                                         max_depth=min(40, int(3 * top_k / 4)), max_features=1.0,
                                         random_state=42)

        logo = LeaveOneGroupOut()
        nr_splits = logo.get_n_splits(X_new_df, y, groups=groups)
        f1_scores_indeterminate = np.zeros(nr_splits)
        f1_scores_mostly_on = np.zeros(nr_splits)
        for idx, (train_idx, test_idx) in enumerate(logo.split(X_new_df, y, groups=groups)):
            # print("TRAIN:", train_idx, "TEST:", test_idx)
            X_train, X_test = X_new_df.iloc[train_idx], X_new_df.iloc[test_idx]
            y_train, y_test = y[train_idx], y[test_idx]

            # resample
            # sm = SMOTE(random_state=42, k_neighbors=2, sampling_strategy={"indeterminate": 100})
            sm = SMOTE(random_state=42, k_neighbors=5, sampling_strategy="not minority")
            # sm = KMeansSMOTE(random_state=42, k_neighbors=3, sampling_strategy="not minority", kmeans_estimator=20,
            #                  cluster_balance_threshold="auto")
            # sm = BorderlineSMOTE(random_state=42, k_neighbors=5, sampling_strategy="not minority")
            # sm = SMOTEENN(random_state=42, sampling_strategy="not minority",
            #               smote=SMOTE(random_state=42, k_neighbors=10, sampling_strategy="not minority"),
            #               enn=RepeatedEditedNearestNeighbours(sampling_strategy="not majority",
            #                                           n_neighbors=5, kind_sel="mode")
            #               )
            # sm = SMOTETomek(random_state=42, sampling_strategy="not minority",
            #               smote=SMOTE(random_state=42, k_neighbors=10, sampling_strategy="not minority"),
            #               tomek=TomekLinks(sampling_strategy="not majority")
            #               )

            X_train_smote, y_train_smote = sm.fit_resample(X_train, y_train)
            print(np.unique(y_train_smote, return_counts=True))

            X_train_df = pd.DataFrame(data=X_train_smote, columns=list(X_new_df.columns.values))
            X_test_df = pd.DataFrame(data=X_test, columns=list(X_new_df.columns.values))

            clf.fit(X_train_df, y_train_smote)
            confusion_mat = confusion_matrix(y_test, clf.predict(X_test_df), labels=LABELS)
            print("Confusion matrix for split %i " % (idx + 1))
            print(confusion_mat)
            print("There were %i cases of indeterminate to predict." % np.sum(y_test == "indeterminate"))

            importance_idx_sorted = np.argsort(clf.feature_importances_)
            print("Top 20 important features: ", list(reversed(clf.feature_names_in_[importance_idx_sorted[-20:]])))
            print("Top 20 importance: ", list(reversed(clf.feature_importances_[importance_idx_sorted[-20:]])))

            prec_indeterminate = 1.0
            if np.sum(confusion_mat, axis=1)[1] > 0:
                prec_indeterminate = confusion_mat[1][1] / np.sum(confusion_mat, axis=1)[1]

            recall_indeterminate = 1.0
            if np.sum(confusion_mat, axis=0)[1]:
                recall_indeterminate = confusion_mat[1][1] / np.sum(confusion_mat, axis=0)[1]

            prec_mostly_on = 1.0
            if np.sum(confusion_mat, axis=1)[2] > 0:
                prec_mostly_on = confusion_mat[2][2] / np.sum(confusion_mat, axis=1)[2]

            recall_mostly_on = 1.0
            if np.sum(confusion_mat, axis=0)[2] > 0:
                recall_mostly_on = confusion_mat[2][2] / np.sum(confusion_mat, axis=0)[2]

            if prec_indeterminate > 0 or recall_indeterminate > 0:
                f1_scores_indeterminate[idx] = 2 * prec_indeterminate * recall_indeterminate / \
                                           (prec_indeterminate + recall_indeterminate)
            else:
                f1_scores_indeterminate[idx] = 0

            if prec_mostly_on > 0 or recall_mostly_on > 0:
                f1_scores_mostly_on[idx] = 2 * prec_mostly_on * recall_mostly_on / (prec_mostly_on + recall_mostly_on)
            else:
                f1_scores_mostly_on[idx] = 0

            print("Indeterminate class F1 Score for split %i = %5.3f " % (idx + 1, f1_scores_indeterminate[idx]))
            print("Mostly On class F1 Score for split %i = %5.3f " % (idx + 1, f1_scores_mostly_on[idx]))
            print("")

        # cv_res = cross_validate(rf_clf, X_new, y, groups=groups, cv=LeaveOneGroupOut(),
        #                         scoring= {
        #                             # "confusion_matrix": make_scorer(confusion_matrix),
        #                             "accuracy": make_scorer(accuracy_score),
        #                             "precision": make_scorer(precision_score)
        #                         }
        #                         # scoring=confusion_matrix_scorer
        #                         )
