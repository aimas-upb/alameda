"""
The script generates the dataset required for the `next week condition' predictive setup on the EMA_PD data of
each person.

@author: Alexandru Sorici
"""

import os
import numpy as np
import pandas as pd
import time

import pyedflib
from scipy.signal import find_peaks_cwt, welch
from scipy.signal import decimate
from mne.filter import filter_data
from datetime import timedelta, datetime
from typing import Dict, List
from argparse import ArgumentParser

# sensor names
left_sensors = ['13797', '13799', '13794', '13806']
right_sensors = ['13805', '13801', '13793', '13795']
chest_sensors = ['13804', '13792', '13803', '13796']

# predictor vars
beep_columns = ['mood_well', 'mood_down', 'mood_fright', 'mood_tense',
               'phy_sleepy', 'phy_tired',
               'mood_cheerf', 'mood_relax', 'thou_concent', 'pat_hallu',
               "act_problemless", "mobility_well", "sit_still", "speech_well", "walk_well",
               "tremor", "slowness", "stiffness", "muscle_tension", "dyskinesia"
                ]
morning_columns = ["mor_sleptwell", "mor_often_awake", "mor_rested", "mor_tired_phys", "mor_tired_ment"]
evening_columns = ["eve_many_offs", "eve_long_offs", "eve_walk_well", "eve_clothing", "eve_eat_well",
                   "eve_personalcare", "eve_household", "eve_tired"]

all_predictors = beep_columns + morning_columns + evening_columns

# constants
BASE_FREQ = 200
DOWNSAMPLING = 4
FEATURE_WINDOW_LENGTH = 60  # in seconds
WINDOW_SIZE = 15            # in minutes

DAY_START_HOUR = 6
DAY_END_HOUR = 24


def get_file_lists(path_to_ema_folder: str, subject: str):
    '''
    Creates a list of files for each sensor (Left/right/chest) located in the indicated patients
    sensor file folder.

    '''
    subj_folder = os.path.join(path_to_ema_folder, subject)
    
    
    edf_files = [f for f in os.listdir(subj_folder) if
                 (os.path.isfile(os.path.join(subj_folder, f))) and (f[0] != '_' and f[-3:] == 'edf')]
    
    left_files = []
    right_files = []
    chest_files = []
    
    for f in edf_files:
        if f[0:5] in left_sensors:
            left_files.append(os.path.join(subj_folder, f))
        elif f[0:5] in right_sensors:
            right_files.append(os.path.join(subj_folder, f))
        elif f[0:5] in chest_sensors:
            chest_files.append(os.path.join(subj_folder, f))
    
    left_files = sorted(left_files)
    right_files = sorted(right_files)
    chest_files = sorted(chest_files)
    
    return left_files, right_files, chest_files


def read_edf_data(filename: str):
    '''
    Reads and .edf file and returns labels, timestamps, signal buffers and samplefrequency.
    '''

    # Extract data
    f = pyedflib.EdfReader(filename)
    fs = f.getSampleFrequencies()[0]
    n = f.signals_in_file
    signal_labels = f.getSignalLabels()
    sig_bufs = np.zeros((n, f.getNSamples()[0]))

    for i in np.arange(n):
        sig_bufs[i, :] = f.readSignal(i)

    # Get starting time
    starting_time = filename[-19:-4]
    starting_time = pd.to_datetime(starting_time, format='%Y%m%d_%H%M%S', errors='ignore')

    sig_bufs = decimate(sig_bufs, DOWNSAMPLING, axis=1)
    fs = fs / DOWNSAMPLING
    freq = '%d ms' % (1000 / fs)
    timestamps = pd.date_range(start=starting_time, periods=sig_bufs.shape[1], freq=freq)

    return signal_labels, timestamps, sig_bufs, fs


def get_min_pos(timediffs):
    prev_val = None
    
    min_pos = 0
    for v in timediffs:
        if prev_val is None:
            prev_val = v
        else:
            if v >= prev_val:
                return min_pos
            else:
                prev_val = v
                min_pos += 1
    
    return min_pos


def extract_raw_trials(left_files, right_files, chest_files, esm_df,
                       esm_window_length=15, feature_window_length=60):
    '''
    Reads the sensor files and aligns the data with the esm data. Then
    data quality is checked.
    Returns cleaned and synced trial data and ESM beeps, as well as the cleaned series of 15 minute windows between
    DAY_START_HOUR and DAY_END_HOUR
    '''
    
    files = [left_files, right_files, chest_files]
    
    # create array of timestamps marking the beginnings of quarter hour extract from the EDF data, between
    # the DAY_START_HOUR and DAY_END_HOUR marks
    esm_start_date = datetime.strptime(esm_df.iloc[0]["beep_time_start"], "%Y-%m-%d %H:%M:%S").replace(hour=0, minute=0, second=0)
    esm_end_date = datetime.strptime(esm_df.iloc[-1]["beep_time_start"], "%Y-%m-%d %H:%M:%S").replace(hour=23, minute=59, second=59)
    n_days = (esm_end_date - esm_start_date).days
    window_end_ts = []
    
    for day in range(n_days):
        esm_date = esm_start_date + timedelta(days=day)
        day_start = esm_date + timedelta(hours=DAY_START_HOUR)
        day_end = esm_date + timedelta(hours=DAY_END_HOUR)
        day_windows = pd.date_range(start=day_start, end=day_end, freq="30 min")
        window_end_ts += day_windows.to_list()[1:]
    
    n_beeps = esm_df.shape[0]
    n_sensors = len(files)
    # trials = [[[]] * n_beeps] * n_sensors
    
    # trials = np.zeros((n_sensors, n_beeps, int(esm_window_length * FEATURE_WINDOW_LENGTH * 100), 6))
    full_trials = np.zeros((n_sensors, len(window_end_ts),
                            int(esm_window_length * FEATURE_WINDOW_LENGTH * BASE_FREQ / DOWNSAMPLING), 6), dtype=np.float32)
    # found_trials = np.zeros((n_beeps, n_sensors))
    found_trials = np.zeros((len(window_end_ts), n_sensors))
    window_beep_alignment: Dict[int, int] = {}
    beep_window_alignment: Dict[int, int] = {}
    
    for i, f in enumerate(files):
        window_idx = 0
        beep_idx = 0

        while window_idx < len(window_end_ts):
            for file in f:
                print(file)
                
                if window_idx == len(window_end_ts):
                    print("Already filled in all the window slots with data from file up to: %s" % file)
                    continue
                
                # Read the data from the filepath, raise error if file is not in the correct format
                try:
                    labels, timestamps, data, fs = read_edf_data(file)  # as input instead: leftFiles
                    if data.shape[1] < fs * feature_window_length:
                        raise ValueError('File too short to proceed.')
                except Exception:
                    print('%s is broken' % file)
                    continue
                
                data = pd.DataFrame(data.T, index=timestamps)
                min_pos = -1
                
                while True:
                    if window_idx == len(window_end_ts):
                        break
                        
                    beep_time = pd.to_datetime(esm_df['beep_time_start'].iloc[beep_idx])
                    window_ts = window_end_ts[window_idx]
                    prev_window_ts = beep_time if window_idx == 0 else window_end_ts[window_idx - 1]
                    beep_selected = False
                    ref_time = window_ts
                    
                    if beep_time < window_ts and beep_time >= prev_window_ts:
                        beep_selected = True
                        ref_time = beep_time

                    # print("window_idx: ", str(window_idx), "; beep_idx: ", str(beep_idx))
                    # print("prev_window_ts:", str(prev_window_ts))
                    # print("beep_time: ", str(beep_time))
                    # print("window_ts:", str(window_ts))
                    # print("")
                    
                    timediffs = np.abs(data.index[(min_pos + 1):] - ref_time)
                    
                    if len(timediffs) == 0:
                        break
                    
                    # curr_min_pos = np.argmin(timediffs)
                    curr_min_pos = get_min_pos(timediffs)
                    min_pos += curr_min_pos + 1
                    timediff = timediffs[curr_min_pos]
                    
                    # Find corresponding moment for reference time in the sensor data by
                    # calculating the difference in sensor and reference time timestamps and
                    # select the index with the smallest difference.
                    if timediff < timedelta(minutes=esm_window_length):
                        # If the time difference is less than the window length,
                        # it means there is data around the point of reference.
                        # Otherwise, we just skip this reference point
                        
                        # For the smallest time difference, find the position in the sensor data
                        if min_pos > esm_window_length * FEATURE_WINDOW_LENGTH * fs:
                            full_trials[i, window_idx, :, :] = data.iloc[min_pos - (int(esm_window_length * FEATURE_WINDOW_LENGTH * fs)):min_pos].values
                            found_trials[window_idx, i] = 1
                            
                            if beep_selected:
                                window_beep_alignment[window_idx] = beep_idx
                                beep_window_alignment[beep_idx] = window_idx
                    
                    window_idx += 1
                    if beep_selected:
                        while pd.to_datetime(esm_df['beep_time_start'].iloc[beep_idx]) < window_ts:
                            beep_idx += 1
                        
    keep = np.sum(found_trials, axis=1) == n_sensors  # Keep trials if all three sensors contain values
    keep_beep = np.array([True if beep_idx in beep_window_alignment and keep[beep_window_alignment[beep_idx]] else False
                          for beep_idx in range(n_beeps)])
    
    kept_window_ts = [
        pd.Timestamp(datetime.strptime(esm_df["beep_time_start"].iloc[window_beep_alignment[w_idx]], "%Y-%m-%d %H:%M:%S"))
        if w_idx in window_beep_alignment else window_end_ts[w_idx]
        for w_idx in range(len(window_end_ts)) if keep[w_idx]
    ]
    
    
    fullTrialData = np.zeros((np.sum(keep), int(esm_window_length * FEATURE_WINDOW_LENGTH * fs), 3 * 6), dtype=np.float32)
    beepSlices = []
    
    counter = 0
    for window_idx in range(len(window_end_ts)):
        if keep[window_idx]:
            temp = np.concatenate((full_trials[0, window_idx, :, :], full_trials[1, window_idx, :, :],
                                   full_trials[2, window_idx, :, :]), axis=1)
            fullTrialData[counter, :, :] = temp
            
            if window_idx in window_beep_alignment:
                beepSlices.append(counter)
            
            counter += 1
    foundESM = esm_df.iloc[keep_beep, :]
    
    return fullTrialData, foundESM, beepSlices, kept_window_ts


# ==================================== Extracting Features ====================================
class FeatureExtractor(object):
    def get_feature_names(self, window_data, sample_rate, window_length=60) -> List[str]:
        raise NotImplementedError("Feature Names retrieval not implemented in base class")
    
    def get_features(self, window_data, sample_rate, window_length=60):
        raise NotImplementedError("Feature Extraction not implemented in base class")
    

class TremorFeatureExtractor(FeatureExtractor):
    def __init__(self):
        self.tremor_channels = [('AccX', 0), ('AccY', 1), ('AccZ', 2), ('GyrX', 3), ('GyrY', 4), ('GyrZ', 5)]
    
    def get_feature_names(self, window_data, sample_rate, window_length=60):
        feature_names = ['TremorPower' + ch[0] for ch in self.tremor_channels]
        return feature_names
    
    def get_features(self, window_data, sample_rate, window_length=60):
        if window_data.shape[0] != sr * window_length:
            print(window_data.shape, sr * window_length)
        
        features = []
        for ch_key, ch_val in self.tremor_channels:
            f, spec = welch(window_data[:, ch_val], fs=sr, nperseg=sr)
            selected = np.logical_and(f > 3.5, f < 7.5)
            spec = np.mean(np.log(spec[selected]))
            features.append(spec)
    
        return features


class BradykinesiaFeatureExtractor(FeatureExtractor):
    def __init__(self):
        self.accelerometer_channels = [('AccX', 0), ('AccY', 1), ('AccZ', 2)]
        
    def get_feature_names(self, window_data, sample_rate, window_length=60):
        feature_names = []
        for ch_key, _ in self.accelerometer_channels:
            feature_names.append('bradyPower' + ch_key)
            feature_names.append('DomFreq' + ch_key)
            feature_names.append('DomEnergyRatio' + ch_key)
            feature_names.append('RMS' + ch_key)
            feature_names.append('AmpRange' + ch_key)
        
        feature_names.append('MaxCC')
        feature_names.append('MaxCCLoc')
        
        return feature_names
        
    def get_features(self, window_data, sample_rate, window_length=60):
        features = []
        windowData = filter_data(window_data[:, [ch[1] for ch in self.accelerometer_channels]].T, sr, 0, 3,
                                 method='iir', verbose='WARNING').T
        freq = np.fft.rfftfreq(window_length * sr, d=1. / sr)
        
        for ch_key, ch_val in self.accelerometer_channels:
            f, spec = welch(windowData[:, ch_val], fs=sr, nperseg=sr)
            selected = np.logical_and(f > 0.5, f < 3.0)
            spec = np.mean(np.log(spec[selected]))
            features.append(spec)
    
            spec = np.abs(np.fft.rfft(windowData[:, ch_val]))
            domFreq = freq[np.argmax(spec)]
            features.append(domFreq)
    
            domEnergyRatio = np.max(spec) / np.sum(spec)
            features.append(domEnergyRatio)
    
            rms = np.sqrt(np.mean(windowData[:, ch_val] ** 2))
            features.append(rms)
    
            ampRange = np.max(windowData[:, ch_val]) - np.min(windowData[:, ch_val])
            features.append(ampRange)

        cCMax = []
        cCLocs = []
        for i, (ch1_key, ch1_val) in enumerate(self.accelerometer_channels):
            for j, (ch2_key, ch2_val) in enumerate(self.accelerometer_channels[i + 1:]):
                crossCorr = np.correlate(windowData[:, ch1_val],
                                         windowData[:, ch2_val],
                                         'same')
                crossCorr = crossCorr / (np.std(windowData[:, ch1_val]) * np.std(windowData[:, ch2_val]))
        
                cCMax.append(np.max(crossCorr))
                cCLocs.append(np.argmax(crossCorr))
        
        features.append(np.max(cCMax))
        features.append(cCLocs[np.argmax(cCMax)])
        
        return features
        

def extract_wearable_pd_features(data, data_timestamps, sr=BASE_FREQ / DOWNSAMPLING, windowLength=900):
    numSamples = data.shape[0]
    
    # Set all the extractors
    extractors = [TremorFeatureExtractor(), BradykinesiaFeatureExtractor()]
    
    # Get all unique feature names per extractor
    feature_names = [extr.get_feature_names(data[0, :windowLength * sr, :], sr, window_length=windowLength)
                     for extr in extractors]
    nr_unique_features = sum([len(extr_feat_names) for extr_feat_names in feature_names])
    
    # Getting number and names of features for all sensor positions (left, right, chest)
    cols = []
    for s in ['L', 'R', 'C']:
        for extr_feat_names in feature_names:
            cols.extend([c + s for c in extr_feat_names])
            
    # Getting features for defined extractors
    aligned_df = pd.DataFrame(columns=cols)
    accelerometer_channel = [a + b for a in [0, 6, 12] for b in range(3)]
    for data_idx in np.arange(data.shape[0]):
        t = time.time()
        allFeat = []
        numWindows = int(data.shape[1] / sr / windowLength)
        buff = data[data_idx, :, :]
        buff[:, accelerometer_channel] = (
                buff[:, accelerometer_channel].T - np.mean(buff[:, accelerometer_channel].T, axis=0)).T
        
        for s, sID in enumerate([range(6), range(6, 12), range(12, 18)]):
            features = np.zeros((numWindows, nr_unique_features))
            for i in range(0, numWindows):
                win = i * windowLength * sr
                idx = 0
                for extr in extractors:
                    features[i, idx: idx + len(extr.get_feature_names(buff[win:win + windowLength * sr, sID], sr,
                                                                      window_length=windowLength))] = \
                        extr.get_features(buff[win:win + windowLength * sr, sID], sr, window_length=windowLength)
                    idx += len(extr.get_feature_names(buff[win:win + windowLength * sr, sID],
                                                      sr, window_length=windowLength))
            allFeat.append(features)
            
        allFeat = np.concatenate(allFeat, axis=1)
        aligned_df = aligned_df.append(pd.DataFrame(allFeat, columns=cols), ignore_index=True)
        
        print("Extracted features for data_idx %i out of %i" % (data_idx, data.shape[0]))
        
    aligned_df.set_index(pd.to_datetime(data_timestamps), inplace=True)
    return aligned_df


def get_day_period(datetime_str):
    dt = pd.Timestamp(datetime_str)
    if dt.hour >= 6 and dt.hour < 12:
        return "morning"
    elif dt.hour >= 12 and dt.hour < 18:
        return "afternoon"
    else:
        return "evening"
    
    
if __name__ == "__main__":
    # all_subjects = ["1100" + str(i).zfill(2) for i in range(1, 22) if i != 12]
    all_subjects = ["110004"]
    
    parser = ArgumentParser(
        description="""A script to setup the EMA PD dataset in the desired prediction form.""", add_help=True
    )
    
    parser.add_argument('--raw-data-path', metavar='path', type=str, default="/media/alex/05A408EF2467286E/EMA_PD")
    parser.add_argument('--config-level', metavar="config_level", type=int, default=0)
    args = parser.parse_args()

    path = args.raw_data_path
    ema_data_file = os.path.join(path, "EMA_data.csv")
    ema_df = pd.read_csv(ema_data_file)

    pre_processed_data_path = "data" + os.path.sep + "preprocessed"
    extracted_features_data_path = "data" + os.path.sep + "features"
    dataset_data_path = "data" + os.path.sep + "dataset"
    
    if args.config_level <= 0:
        # compute and align the trial data which has values from all 3 sensor locations and which is aligned with
        # the questionnaire beeps
        if not os.path.exists(pre_processed_data_path):
            os.makedirs(pre_processed_data_path)

        for subject in all_subjects:
            esm_df = ema_df[ema_df["ID"] == int(subject)]
            
            left_files, right_files, chest_files = get_file_lists(path, subject)
            trial_data, selected_esm, beep_slices, kept_window_ts = extract_raw_trials(left_files=left_files,
                                                                                       right_files=right_files,
                                                                                    chest_files=chest_files,
                                                                                    esm_df=esm_df)
            beep_trial_data = trial_data[beep_slices]
            
            print("Nr entries in beep_trial_data: %i" % beep_trial_data.shape[0])
            print("Nr entries in selected_esm dataframe: %i" % selected_esm.shape[0])
            
            # np.save(os.path.join(pre_processed_data_path, subject + '_trials.npy'), trial_data.astype(np.float32))
            # np.save(os.path.join(pre_processed_data_path, subject + '_beep_trials.npy'),
            #   beep_trial_data.astype(np.float32))
            
            
            np.savez_compressed(os.path.join(pre_processed_data_path, subject + '_trials_compressed.npz'),
                                trial_data=trial_data,
                                beep_trial_data=beep_trial_data,
                                trial_data_ts=np.array([t.to_numpy() for t in kept_window_ts]))
            selected_esm.to_csv(os.path.join(pre_processed_data_path, subject + '_esm.csv'), index=False)
            
        print(" ######## DONE PRE-PROCESSING ########")
        print("")
        
    if args.config_level <= 1:
        # load the saved trial data and selected esm data for each subject to extract the bradykinesia and
        # tremor features
        sr = int(BASE_FREQ / DOWNSAMPLING)
        win_len = FEATURE_WINDOW_LENGTH * WINDOW_SIZE

        if not os.path.exists(extracted_features_data_path):
            os.makedirs(extracted_features_data_path)
        
        for subject in all_subjects:
            print("Extracting wearable features for subject: ", subject, " ... ")
            subject_data = np.load(os.path.join(pre_processed_data_path, subject + '_trials_compressed.npz'))
            trial_data = subject_data["trial_data"].astype(np.float64)
            beep_trial_data = subject_data["beep_trial_data"].astype(np.float64)
            trial_data_ts = subject_data["trial_data_ts"]
            
            selected_esm = pd.read_csv(os.path.join(pre_processed_data_path, subject + '_esm.csv'))
            
            wearables_pd_feature_df = extract_wearable_pd_features(trial_data, trial_data_ts,
                                                                   sr=sr, windowLength=win_len)
            
            wearables_pd_feature_df.to_csv(os.path.join(extracted_features_data_path, subject+'_wearable_features.csv'))

        print(" ######## DONE FEATURE EXTRACTION ########")
        print("")
        
    if args.config_level <= 2:
        if not os.path.exists(dataset_data_path):
            os.makedirs(dataset_data_path)
        
        # set up the prediction problem
        all_subject_ids = ema_df["ID"].unique().tolist()
        
        # compute sanpar_onoff day-average target
        sanpar_target_df = pd.DataFrame(columns=["ID", "day_idx", "date", "target"])
        for subject_id in all_subject_ids:
            sanpar_target = []

            subject_ema_df = ema_df[ema_df["ID"] == subject_id]
            subject_start_date = datetime.strptime(subject_ema_df.iloc[0]["beep_time_start"],
                                                   "%Y-%m-%d %H:%M:%S").replace(hour=0, minute=0, second=0)
            subject_end_date = datetime.strptime(subject_ema_df.iloc[-1]["beep_time_start"],
                                                 "%Y-%m-%d %H:%M:%S").replace(hour=23, minute=59, second=59)
            n_days = (subject_end_date - subject_start_date).days + 1

            for day in range(n_days):
                start_day = subject_start_date + timedelta(days=day)
                end_day = subject_start_date + timedelta(days=(day + 1))

                ts = subject_ema_df["beep_time_start"].apply(lambda x: pd.Timestamp(x))

                day_sanpar = subject_ema_df[(ts > start_day)
                                            & (ts < end_day)]["sanpar_onoff"]
                counts = day_sanpar.value_counts()

                sanpar_counts = np.zeros(4)
                for sanpar_idx in range(4):
                    if (sanpar_idx + 1) in counts:
                        sanpar_counts[sanpar_idx] = counts[(sanpar_idx + 1)]

                winners = np.flatnonzero(sanpar_counts == np.max(sanpar_counts))

                if winners.size == 1:
                    if winners[0] == 0:
                        sanpar_target.append((subject_id, (day + 1), start_day, "mostly_off"))
                    elif winners[0] == 2:
                        sanpar_target.append((subject_id, (day + 1), start_day, "mostly_on"))
                    else:
                        sanpar_target.append((subject_id, (day + 1), start_day, "indeterminate"))
                else:
                    if sanpar_counts[0] + sanpar_counts[3] < sanpar_counts[1] + sanpar_counts[2]:
                        sanpar_target.append((subject_id, (day + 1), start_day, "mostly_on"))
                    elif sanpar_counts[0] + sanpar_counts[3] > sanpar_counts[1] + sanpar_counts[2]:
                        sanpar_target.append((subject_id, (day + 1), start_day, "mostly_off"))
                    else:
                        sanpar_target.append((subject_id, (day + 1), start_day, "indeterminate"))

            subject_sanpar_df = pd.DataFrame(sanpar_target, columns=["ID", "day_idx", "date", "target"])
            sanpar_target_df = sanpar_target_df.append(subject_sanpar_df, ignore_index=True)

        sanpar_target_df.to_csv(os.path.join(dataset_data_path, 'sanpar_target.csv'))
        print(sanpar_target_df["target"].value_counts())
        
        # compute training dataset
        for subject_id in all_subject_ids:
            subject_ema_df = ema_df[ema_df["ID"] == subject_id].copy()
            subject_ema_df["day"] = (subject_ema_df["beep_time_start"].apply(lambda x: pd.Timestamp(x))
                                     - pd.Timestamp(subject_ema_df["beep_time_start"].iloc[0])).apply(lambda dt: dt.days) + 1
            subject_ema_df["day_period"] = subject_ema_df["beep_time_start"].apply(get_day_period)

            subject_ema_dataset = subject_ema_df[all_predictors + ["day", "day_period"]].\
                groupby(by=["day", "day_period"]).agg(["min", "max", "mean"]).reset_index()
            subject_ema_dataset.columns = subject_ema_dataset.columns.to_flat_index()
            subject_ema_dataset.columns = map(lambda x: "_".join(list(x)) if x[1] else x[0],
                                              subject_ema_dataset.columns.to_list())

            subject_ema_dataset.to_csv(os.path.join(dataset_data_path, str(subject_id) + '_ema_dataset.csv'))
        
        print(" ######## DONE DATASET SETUP ########")
        print("")
