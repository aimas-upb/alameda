"""
This script collects data from the CSV outputs of the ActivInsights Activity Summary and Sleep Summary R scripts,
as well as the SleepPy sleep metrics library.
It outputs a unified JSON with metrics reported on a per-day basis.

The input to the script is the .bin file containing the raw data.

@author Alexandru Sorici
"""

from argparse import ArgumentParser
import os
import pandas as pd
from typing import Dict, Any, List
import json
from datetime import datetime
import numpy as np

HEADER_FILE_SUFFIX = "_header.json"
ACTIVITY_SUMMARY_SUFFIX = "_activity_summary.csv"
SLEEP_SUMMARY_SUFFIX = "_sleep_summary.csv"


class NumpyEncoder(json.JSONEncoder):
    """ Custom encoder for numpy data types """
    def default(self, obj):
        if isinstance(obj, (np.int_, np.intc, np.intp, np.int8,
                            np.int16, np.int32, np.int64, np.uint8,
                            np.uint16, np.uint32, np.uint64)):

            return int(obj)

        elif isinstance(obj, (np.float_, np.float16, np.float32, np.float64)):
            return float(obj)

        elif isinstance(obj, (np.complex_, np.complex64, np.complex128)):
            return {'real': obj.real, 'imag': obj.imag}

        elif isinstance(obj, (np.ndarray,)):
            return obj.tolist()

        elif isinstance(obj, (np.bool_)):
            return bool(obj)

        elif isinstance(obj, (np.void)):
            return None

        return json.JSONEncoder.default(self, obj)


def collect_report(report_basename: str,
                   activinsights_activity_dir: str,
                   activinsights_sleep_dir: str,
                   sleeppy_dir: str) -> Dict[str, Any]:
    
    # ## We are going to maintain a set of meta data for use internally, as well as for the final JSON payload
    device_id: str = None
    patient_id: str = None
    report_start_time: str = None
    report_start_ts: float = None
    device_location_code: str = None
    measurement_frequency: int = None
    
    report_dict: Dict[str, Any] = {}
    
    # 1. Collect the header information
    header_file = activinsights_activity_dir + os.path.sep \
                  + "Outputs" + os.path.sep + report_basename + HEADER_FILE_SUFFIX
    with open(header_file) as header_fp:
        header = json.load(header_fp)
        
        device_id = header["Device_Unique_Serial_Code"]
        patient_id = str.strip(header["Subject_Code"])
        report_start_ts = header["Start_Time_ts"]
        report_start_time = datetime.strftime(datetime.fromtimestamp(report_start_ts), "%Y-%m-%d %H:%M:%S.%z")
        measurement_frequency = round(float(header["Measurement_Frequency"]))
        
        report_dict["meta"] = {
            "patient_id": patient_id,
            "device_id": device_id,
            "measurement_frequency": measurement_frequency,
            "start_time": report_start_time,
            "start_time_ts": report_start_ts
        }
        
    # 2. Collect the Activinsights activity summary information
    activity_summary_file = activinsights_activity_dir + os.path.sep \
                            + "Outputs" + os.path.sep + report_basename + ACTIVITY_SUMMARY_SUFFIX
    activity_summary = pd.read_csv(activity_summary_file)
    activity_summary.drop(activity_summary.tail(1).index, inplace=True)  # drop the last row since its a `mean` row
    
    report_dict["activity_summary"]: List[Dict[str, Any]] = []
    activinsights_activ_report = {
        "provenance": "Activinsights",
        "data": []
    }
    
    for _, row in activity_summary.iterrows():
        act_rep = {
            "day_start": row["Day_Start"],
            "day_start_ts": datetime.strptime(row["Day_Start"], "%Y-%m-%d %H:%M:%S.%z").timestamp(),
            "day_end": row["Day_End"],
            "day_end_ts": datetime.strptime(row["Day_End"], "%Y-%m-%d %H:%M:%S.%z").timestamp(),
            "steps": row["Steps"],
            "non_wear": row["Non_Wear"],
            "sedentary": row["Sedentary"],
            "sedentary_percent": np.round(row["Sedentary"] / 86400, 2),
            "light_activity": row["Light"],
            "light_activity_percent": np.round(row["Light"] / 86400, 2),
            "moderate_activity": row["Moderate"],
            "moderate_activity_percent": np.round(row["Moderate"] / 86400, 2),
            "vigorous_activity": row["Vigorous"],
            "vigorous_activity_percent": np.round(row["Vigorous"] / 86400, 2)
        }
        activinsights_activ_report["data"].append(act_rep)
        
    report_dict["activity_summary"].append(activinsights_activ_report)
    
    # 3. Collect the sleep summary information
    report_dict["sleep_summary"]: List[Dict[str, Any]] = []
    
    # 3a. from Activinsights
    sleep_summary_file = activinsights_sleep_dir + os.path.sep \
                            + "Outputs" + os.path.sep + report_basename + SLEEP_SUMMARY_SUFFIX
    sleep_summary = pd.read_csv(sleep_summary_file)
    sleep_summary.drop(sleep_summary.tail(1).index, inplace=True)  # drop the last row since its a `mean` row
    
    activinsights_sleep_report = {
        "provenance": "Activinsights",
        "data": []
    }
    
    for _, row in sleep_summary.iterrows():
        sleep_rep = {
            "night_start": row["Night.Start.Datetime"],
            "night_start_ts": datetime.strptime(row["Night.Start.Datetime"], "%Y-%m-%d %H:%M:%S.%z").timestamp(),
            "night_end": row["Night.End.Datetime"],
            "night_end_ts": datetime.strptime(row["Night.End.Datetime"], "%Y-%m-%d %H:%M:%S.%z").timestamp(),
            "sleep_onset": row["Sleep.Onset.Datetime"],
            "sleep_onset_ts": datetime.strptime(row["Sleep.Onset.Datetime"], "%Y-%m-%d %H:%M:%S.%z").timestamp(),
            "rise": row["Rise.Datetime"],
            "rise_ts": datetime.strptime(row["Rise.Datetime"], "%Y-%m-%d %H:%M:%S.%z").timestamp(),
            "total_bed_time": row["Total.Elapsed.Bed.Time"],
            "total_sleep_time": row["Total.Sleep.Time"],
            "waso": row["Total.Wake.Time"],
            "first_waso": row["First.WASO"],
            "num_active_periods": row["Num.Active.Periods"],
            "median_active_len": row["Median.Activity.Length"],
            "sleep_efficiency": row["Sleep.Efficiency"]
        }
        activinsights_sleep_report["data"].append(sleep_rep)
    
    report_dict["sleep_summary"].append(activinsights_sleep_report)
    
    # 3b. from SleepPy
    sleeppy_file = os.path.sep.join([sleeppy_dir, report_basename, "results", "sleep_endpoints_summary.csv"])
    sleeppy_summary = pd.read_csv(sleeppy_file)
    sleeppy_report = {
        "provenance": "SleepPy",
        "data": []
    }

    for _, row in sleeppy_summary.iterrows():
        sleep_rep = {
            "sleep_onset": row["sleep_onset"],
            "sleep_onset_ts": row["sleep_onset_ts"],
            "rise": row["rise"],
            "rise_ts": row["rise_ts"],
            "total_bed_time": row["total_bed_time"],
            "total_sleep_time": row["total_sleep_time"],
            "waso": row["waso"],
            "first_waso": row["first_waso"],
            "num_active_periods": row["num_active_periods"],
            "median_active_len": row["median_active_len"],
            "sleep_efficiency": row["sleep_efficiency"]
        }
        sleeppy_report["data"].append(sleep_rep)
        
    report_dict["sleep_summary"].append(sleeppy_report)
    
    return report_dict


if __name__ == "__main__":
    parser = ArgumentParser(
        description="""A test to extract sleep information from
                GENEACtiv accelerometer bin files using SleepPy.""", add_help=True
    )

    parser.add_argument('--activityReportDir', metavar='activity_report_dir', type=str,
                        default="ActivinsightsActivityReport",
                        help="Path to Activinsights Activity Report parent folder "
                             "from which the R script for activity summary has been run.")
    
    parser.add_argument('--sleepReportDir', metavar='sleep_report_dir', type=str,
                        default="ActivinsightsSleepReport",
                        help="Path to Activinsights Sleep Report parent folder "
                             "from which the R script for sleep summary has been run.")

    parser.add_argument('--sleepPyDir', metavar='sleeppy_report_dir', type=str,
                        default="test_results",
                        help="Path to SleepPy Report parent folder "
                             "from which the Python script for sleep summary has been run.")
    
    parser.add_argument('-f', '--inputFile', metavar='input_file', type=str, required=True,
                        help="Path to GENEActiv .bin file containing the recorded data.")

    parser.add_argument('-o', '--outputDir', metavar='output_file', type=str,
                        default=".",
                        help="Path to output directory where the payload json will be dumped to file.")
    
    args = parser.parse_args()
    
    input_file = args.inputFile
    activinsights_activity_dir = args.activityReportDir
    activinsights_sleep_dir = args.sleepReportDir
    sleeppy_dir = args.sleepPyDir
    
    report_basename = os.path.basename(input_file).split(".")[0]
    
    report = collect_report(str(report_basename),
                            activinsights_activity_dir, activinsights_sleep_dir, sleeppy_dir)
    
    output_file_name = "geneactiv_report_" + datetime.strftime(datetime.fromtimestamp(report["meta"]["start_time_ts"]),
                                                               "%Y_%m_%d") + ".json"
    output_file_path = os.path.sep.join([args.outputDir, output_file_name])
    
    with open(output_file_path, "w") as ofp:
        json.dump(report, ofp, indent=4, cls=NumpyEncoder)
        

