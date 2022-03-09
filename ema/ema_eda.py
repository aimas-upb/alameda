import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn
from datetime import date, datetime, timedelta

drop_columns = ["beep_time_start", "beep_time_end", 'soc_who', 'soc_who02', 'soc_who03', 'act_what02',
                'loc_where', 'soc_who', 'soc_who02', 'soc_who03',  'act_what', 'act_what02', 'act_what03']

beep_columns = ['mood_well', 'mood_down', 'mood_fright', 'mood_tense',
               'phy_sleepy', 'phy_tired',
               'mood_cheerf', 'mood_relax', 'thou_concent', 'pat_hallu',
               "act_problemless", "mobility_well", "sit_still", "speech_well", "walk_well",
               "tremor", "slowness", "stiffness", "muscle_tension", "dyskinesia"
               ]

morning_columns = ["mor_sleptwell", "mor_often_awake", "mor_rested", "mor_tired_phys", "mor_tired_ment"]
evening_columns = ["eve_many_offs", "eve_long_offs", "eve_walk_well", "eve_clothing", "eve_eat_well",
                   "eve_personalcare", "eve_household", "eve_tired"]

target_columns = ["sanpar_onoff", "sanpar_medic"]

if __name__ == "__main__":
    all_subjects = ["1100" + str(i).zfill(2) for i in range(1, 22) if i != 12]
    # all_subjects = ["110004"]
    
    path = "."
    eda_output_path = "data" + os.path.sep + "eda"
    if not os.path.exists(eda_output_path):
        os.makedirs(eda_output_path)
    
    ema_data_file = os.path.join(path, "EMA_data.csv")
    ema_df = pd.read_csv(ema_data_file)
    
    for subject in all_subjects:
        subject_out_path = os.path.join(eda_output_path, subject)
        if not os.path.exists(subject_out_path):
            os.makedirs(subject_out_path)
            
        esm_df = ema_df[ema_df["ID"] == int(subject)].copy()
        esm_df.set_index(pd.DatetimeIndex(esm_df["beep_time_start"]), inplace=True)
        esm_df.drop(drop_columns, axis="columns", inplace=True)
        
        year_of_study = esm_df.index[0].year
        
        esm_beep_df = esm_df[beep_columns].copy()
        esm_beep_grp = esm_beep_df.groupby(esm_beep_df.index.dayofyear).agg(["count", "min", "max", "median", "std"])
        
        esm_morning_df = esm_df[morning_columns].copy()
        esm_morning_grp = esm_morning_df.groupby(esm_morning_df.index.dayofyear).agg(["count", "min", "max", "median",
                                                                                      "std"])

        esm_evening_df = esm_df[evening_columns].copy()
        esm_evening_grp = esm_evening_df.groupby(esm_evening_df.index.dayofyear).agg(["count", "min", "max", "median",
                                                                                      "std"])
        
        beep_col_rankings = []
        for col in esm_beep_df.columns:
            beep_col_rankings.append((col, esm_beep_grp[col]["median"].std()))
        beep_col_rank_df = pd.DataFrame(beep_col_rankings, columns=["var_name", "median_std"])
        beep_col_rank_df.sort_values(by=["median_std"], inplace=True, ascending=False)
        
        morning_col_rankings = []
        for col in esm_morning_df.columns:
            morning_col_rankings.append((col, esm_morning_grp[col]["median"].std()))
        morning_col_rank_df = pd.DataFrame(morning_col_rankings, columns=["var_name", "median_std"])
        morning_col_rank_df.sort_values(by=["median_std"], inplace=True, ascending=False)

        evening_col_rankings = []
        for col in esm_evening_df.columns:
            evening_col_rankings.append((col, esm_evening_grp[col]["median"].std()))
        evening_col_rank_df = pd.DataFrame(evening_col_rankings, columns=["var_name", "median_std"])
        evening_col_rank_df.sort_values(by=["median_std"], inplace=True, ascending=False)
        
        beep_col_rank_df.to_csv(os.path.join(subject_out_path, "beep_col_rank_df.csv"))
        morning_col_rank_df.to_csv(os.path.join(subject_out_path, "morning_col_rank_df.csv"))
        evening_col_rank_df.to_csv(os.path.join(subject_out_path, "evening_col_rank_df.csv"))
        
        # create plots
        top_beep_cols = beep_col_rank_df.iloc[:20]["var_name"].values.tolist()
        day_names = [
            (date(int(year_of_study), 1, 1) + timedelta(days=int(day_num) - 1)).strftime("%a")
            for day_num in esm_beep_grp.index.values.tolist()
        ]
        
        fig, axes = plt.subplots(len(top_beep_cols), 1, figsize=(20, 30))
        for i, col in enumerate(top_beep_cols):
            seaborn.boxplot(esm_beep_df[col].index.dayofyear, esm_beep_df[col], ax=axes[i])
            axes[i].set_title(col)
            axes[i].set_ylabel("")
            
            axes[i].set_xticklabels(
                           ["%s, %s" % (dname, yd) for (dname, yd) in
                            zip(day_names,
                                esm_beep_grp.index.values.tolist())])

        plt.subplots_adjust(hspace=1.0, wspace=0.15)
        fig.tight_layout()
        fig.savefig(os.path.join(subject_out_path, "beep_top_var_ts.png"))
        plt.close(fig)
