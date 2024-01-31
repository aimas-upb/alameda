"""
Script to extract aggregate metrics from insole data stored in an .slg tab separated file.
"""

import os
import argparse
import pandas as pd
from typing import Dict, Any, Tuple
from datetime import datetime, time

def get_raw_data(filename: str) -> Tuple[str, str, datetime, float, pd.DataFrame]:
    """
    Extract the information from the .mva file returning:
    - the original pdo file name
    - the file creation time
    - the total duration of the recorded data
    - a dataframe of force and force-time-integral recordings for each foot zone

    :param filename:
    :return: A tuple containing the (patientID, original filename, the creation time, total duration,
    dataframe of raw data)
    """
    print(f"Extracting raw data from {filename}...")


    agg_dict = {}
    with open(filename, "r") as f:
        for i, line in enumerate(f):
            # 1. read the first row of the file and separate by tab to get the filename and the timestamp strings
            if i == 0:
                line_elements = line.split("\t")

                # split the filename string by column to get the original pdo file name, as the second argument,
                # by removing leading and trailing whitespace and removing the .pdo extension
                filename_str = os.path.basename(filename)
                # filename_str_split = filename_str.split(":")
                original_pdo_filename = filename_str.strip().replace(".mva", "")

                # split the original pdo filename by underscore to get the patient id, as the first argument,
                # and the timestamp string as the second
                original_pdo_filename_split = original_pdo_filename.split("_")
                patient_id = original_pdo_filename_split[0]
                timestamp_str = original_pdo_filename_split[1]

                # convert the timestamp string to a datetime object - try first "%Y-%m-%d %H-%M-%S-%f" and, if that fails,
                # try "%y-%m-%d %H-%M-%S-%f" and if that fails then try "%Y-%m-%d %H-%M-%S"
                try:
                    timestamp = datetime.strptime(timestamp_str, "%Y-%m-%d %H-%M-%S-%f")
                except Exception:
                    try:
                        timestamp = datetime.strptime(timestamp_str, "%y-%m-%d %H-%M-%S-%f")
                    except Exception:
                        try:
                            timestamp = datetime.strptime(timestamp_str, "%y-%m-%d %H-%M-%S")
                        except Exception:
                            timestamp = datetime.strptime(timestamp_str, "%Y-%m-%d %H-%M-%S")

                
                agg_dict["patient_id"] = patient_id
                agg_dict["original_filename"] = original_pdo_filename
                agg_dict["start_timestamp"] = timestamp

            # 2. read the 6th row of the file and separate by column to get the total duration as the first argument,
            # time_per_frame as the second and sampling frequency as the third
            if i == 5:
                line_elements = line.split("\t")
                total_time_str, time_per_frame_str, sampling_frequency_str = line_elements[0:3]
                total_time = float(total_time_str.split(":")[-1].strip())
                time_per_frame = float(time_per_frame_str.split(":")[-1].strip())
                sampling_frequency = int(sampling_frequency_str.split(":")[-1].strip())
                agg_dict["total_duration"] = total_time
                agg_dict["time_per_frame"] = time_per_frame
                agg_dict["sampling_frequency"] = sampling_frequency

            # for any row greater than 5, break out of the loop
            if i > 5:
                break

    # The rest of the information we extract as a pandas dataframe, reading in rows 13 to end from the tab separated
    # mva file.
    step_df = pd.read_csv(filename, sep="\t", skiprows=12, header=None,
                          names=["timestamp", "time_rel_start", "step_left", "force_left_heel", "fti_left_heel",
                                 "force_left_forefoot", "fti_left_forefoot", "force_left", "fti_left",
                                 "step_right", "force_right_heel", "fti_right_heel", "force_right_forefoot",
                                 "fti_right_forefoot", "force_right", "fti_right", "force_total", "fti_total",
                                 "foib", "ablt"], dtype=str, index_col=False)



    # cast the timestamp column to a time object
    step_df["timestamp"] = step_df["timestamp"].apply(lambda x: datetime.strptime(x, "%H:%M:%S:%f").time())

    # convert all "---" values appearing in columns step_left, step_right and ablt to NaN
    step_df["step_left"] = step_df["step_left"].apply(lambda x: x if isinstance(x, float) else float(x.strip()) if x.strip() != "---" else float("NaN"))
    step_df["step_right"] = step_df["step_right"].apply(lambda x: x if isinstance(x, float) else float(x.strip()) if x.strip() != "---" else float("NaN"))
    step_df["ablt"] = step_df["ablt"].apply(lambda x: x if isinstance(x, float) else float(x.strip()) if x.strip() != "---" else float("NaN"))

    # convert foib column to float by looking at the string value and seeing if it ends in "R" or "L".
    # If it ends in "R" then the value is obtained by splitting the string for space end taking the first element is
    # multiplied with 1.0. If it ends in "L" then the value, obtained in the same way, is multiplied with -1.0.
    # If it does not end in "R" or "L" then the value is converted to float.
    step_df["foib"] = step_df["foib"].apply(lambda x: x if isinstance(x, float) else float("NaN") if x.strip() == "---" else float(x) if x.endswith("R") is False and x.endswith("L") is False
        else float(x.strip().split(" ")[0]) * 1.0 if x.endswith("R") else float(x.strip().split(" ")[0]) * -1.0)

    # convert all columns, except timestamp, to numeric
    step_df[step_df.columns[1:]] = step_df[step_df.columns[1:]].apply(pd.to_numeric, errors="coerce")
    step_df["step_left"] = step_df["step_left"].astype("Int64")
    step_df["step_right"] = step_df["step_right"].astype("Int64")

    return agg_dict["patient_id"], agg_dict["original_filename"], agg_dict["start_timestamp"], \
              agg_dict["total_duration"], step_df


def process_mva_file(mva_file_path: str, root_output_dir: str):
    # 1. get the meta information and aggregate measures per foot zone from the .mva file
    patient_id, original_filename, start_timestamp, total_duration, step_df = get_raw_data(mva_file_path)

    # add patient_id and the date component of start_timestamp to the step_df dataframe
    step_df["patient_id"] = patient_id
    step_df["date"] = start_timestamp.date()

    # print("Patient ID: {}".format(patient_id))
    # print("Original filename: {}".format(original_filename))
    # print("Start timestamp: {}".format(start_timestamp))
    # print("Total duration: {}".format(total_duration))
    # print("Step dataframe describe: {}".format(step_df.describe()))
    # print(step_df.head())

    # the output directory has the root given by args.outdir and a path given by the patient_id and the date component of start_timestamp
    output_dir = os.path.join(root_output_dir, patient_id, str(start_timestamp.date()))
    out_filename = os.path.join(output_dir, original_filename + ".parquet")
    
    # if the output directory does not exist, create it
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # 2. store the step_df dataframe as a parquet file
    # step_df.to_parquet(out_filename, index=False, partition_cols=["patient_id", "date"])
    step_df.to_parquet(out_filename, index=False)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=str, required=True, help="Input .mva file or directory of .mva files")
    parser.add_argument("--outdir", type=str, required=True, help="Output directory for the resulting .parquet file")
    args = parser.parse_args()

    # If the input is a file, then we process the file directly
    if os.path.isfile(args.input):
        process_mva_file(args.input, args.outdir)
    else:
        # If the input is a directory, then we process it recursively until we reach .mva files, which we process
        # The root output directory is the same for all the files.
        for root, dirs, files in os.walk(args.input):
            for file in files:
                if file.endswith(".mva"):
                    mva_file_path = os.path.join(root, file)
                    process_mva_file(mva_file_path, args.outdir)
        

