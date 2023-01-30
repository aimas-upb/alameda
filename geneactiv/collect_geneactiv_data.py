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
from typing import Dict, Any, List, Tuple
import json
from datetime import datetime, timedelta
import numpy as np
import actipy
import pytz.tzinfo

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


def process_activinsights_activity_summary(report_basename: str, activinsights_activity_dir: str) \
        -> Tuple[Dict[str, Any], List[Dict[str, Any]]]:
    """
    Collect the activity summary.
    :param report_basename: basename of the original GENEACtiv .bin file
    :param activinsights_activity_dir: root dir of the ActivInsights activity analysis
    :return:
    """
    meta_dict = {}
    data = []

    header_file = activinsights_activity_dir + os.path.sep \
                  + "Outputs" + os.path.sep + report_basename + HEADER_FILE_SUFFIX
    if not os.path.exists(header_file):
        print("Required header_file %s does not exist in activity directory %s. Stopping processing. "
              % (header_file, activinsights_activity_dir))
        return meta_dict, data

    with open(header_file) as header_fp:
        header = json.load(header_fp)

        report_start_ts = header["Start_Time_ts"]
        report_start_time = datetime.strftime(datetime.fromtimestamp(report_start_ts), "%Y-%m-%d %H:%M:%S.%z")
        measurement_frequency = round(float(header["Measurement_Frequency"]))

        meta_dict["meta"] = {
            "measurement_frequency": measurement_frequency,
            "start_time": report_start_time,
            "start_time_ts": report_start_ts
        }

    # Collect the Activinsights activity summary information
    activity_summary_file = activinsights_activity_dir + os.path.sep \
                            + "Outputs" + os.path.sep + report_basename + ACTIVITY_SUMMARY_SUFFIX
    activity_summary = pd.read_csv(activity_summary_file)
    activity_summary.drop(activity_summary.tail(1).index, inplace=True)  # drop the last row since its a `mean` row

    for _, row in activity_summary.iterrows():
        data_dict = {
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
        data.append(data_dict)

    return meta_dict, data


def process_activinsights_sleep_summary(report_basename: str, activinsights_sleep_dir: str) \
        -> Tuple[Dict[str, Any], List[Dict[str, Any]]]:
    """
    Collect the activity summary.
    :param report_basename: basename of the original GENEACtiv .bin file
    :param activinsights_sleep_dir: root dir of the ActivInsights sleep analysis
    :return:
    """
    meta_dict = {}
    data = []

    header_file = activinsights_sleep_dir + os.path.sep \
                  + "Outputs" + os.path.sep + report_basename + HEADER_FILE_SUFFIX

    if not os.path.exists(header_file):
        print("Required header file %s does not exist in activity directory %s. Stopping processing. "
              % (header_file, activinsights_sleep_dir))
        return meta_dict, data

    with open(header_file) as header_fp:
        header = json.load(header_fp)

        report_start_ts = header["Start_Time_ts"]
        report_start_time = datetime.strftime(datetime.fromtimestamp(report_start_ts), "%Y-%m-%d %H:%M:%S.%z")
        measurement_frequency = round(float(header["Measurement_Frequency"]))

        meta_dict["meta"] = {
            "measurement_frequency": measurement_frequency,
            "start_time": report_start_time,
            "start_time_ts": report_start_ts
        }

    sleep_summary_file = activinsights_sleep_dir + os.path.sep \
                         + "Outputs" + os.path.sep + report_basename + SLEEP_SUMMARY_SUFFIX
    sleep_summary = pd.read_csv(sleep_summary_file)
    sleep_summary.drop(sleep_summary.tail(1).index, inplace=True)  # drop the last row since its a `mean` row

    for _, row in sleep_summary.iterrows():
        data_dict = {
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
        data.append(data_dict)

    return meta_dict, data


def process_sleeppy_summary(report_basename: str, sleeppy_dir: str) -> List[Dict[str, Any]]:
    """
    Collect the sleeppy summary
    :param report_basename: basename of the original GENEACtiv .bin file
    :param sleeppy_dir: root dir of the sleeppy analysis
    :return:
    """
    data = []

    sleeppy_file = os.path.sep.join([sleeppy_dir, report_basename, "results", "sleep_endpoints_summary.csv"])
    if not os.path.exists(sleeppy_file):
        print("Required sleeppy summary file %s does not exist in sleeppy directory %s. Stopping processing. "
              % (sleeppy_file, sleeppy_dir))
        return data

    sleeppy_summary = pd.read_csv(sleeppy_file)
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
        data.append(sleep_rep)

    return data


def process_ggir_meta(report_basename: str, ggir_output_dir: str) -> Dict[str, Any] or None:
    # read the part2_summary.csv file from the results subdirectory of the ggir output directory and extract the
    # following information: samplefreq, start_time (parsed as a datetime object), start_time_ts - obtained by
    # converting the start_time to a timestamp
    summary_file = ggir_output_dir + os.path.sep + "results" + os.path.sep + "part2_summary.csv"
    if not os.path.exists(summary_file):
        print("Required ggir summary file %s does not exist in ggir directory %s. "
              "Stopping processing GGIR meta information."
              % (summary_file, ggir_output_dir))
        return None

    summary = pd.read_csv(summary_file)
    meta_dict = {
        "measurement_frequency": summary["samplefreq"][0],
        "start_time": datetime.strptime(summary["start_time"][0], "%Y-%m-%dT%H:%M:%S%z"),
        "start_time_ts": datetime.strptime(summary["start_time"][0], "%Y-%m-%dT%H:%M:%S%z").timestamp()
    }

    return meta_dict


def process_ggir_sleep_summary(report_basename: str, ggir_output_dir: str, tz: pytz.tzinfo.DstTzInfo) -> \
        List[Dict[str, Any]] or None:
    # read the part4_nightsummary_sleep_cleaned.csv file from the results subdirectory of the ggir output directory
    summary_file = ggir_output_dir + os.path.sep + "results" + os.path.sep + "part4_nightsummary_sleep_cleaned.csv"
    if not os.path.exists(summary_file):
        print("Required ggir sleep summary file %s does not exist in ggir directory %s. "
              "Stopping processing GGIR sleep summary."
              % (summary_file, ggir_output_dir))
        return None

    sleep_summary = pd.read_csv(summary_file)
    sleep_data = []
    for _, row in sleep_summary.iterrows():
        # get the sleep onset datetime by parsing the calendar date (column calendar_date) to which
        # we add the number of hours specified in the "sleeponset" column
        sleep_onset_datetime = datetime.strptime(row["calendar_date"], "%d/%m/%Y").replace(tzinfo=tz) + \
                               timedelta(hours=row["sleeponset"])
        sleep_onset_ts = sleep_onset_datetime.timestamp()
        rise_datetime = datetime.strptime(row["calendar_date"], "%d/%m/%Y").replace(tzinfo=tz) \
                        + timedelta(hours=row["wakeup"])
        rise_ts = rise_datetime.timestamp()

        sleep_rep = {
            "sleep_onset": sleep_onset_datetime,
            "sleep_onset_ts": sleep_onset_ts,
            "rise": rise_datetime,
            "rise_ts": rise_ts,
            # convert the total bedtime from hours to seconds by rounding to the nearest second
            "total_bed_time": round(row["SptDuration"] * 3600),
            "total_sleep_time": round(row["SleepDurationInSpt"] * 3600),
            # convert the waso from hours to seconds by rounding to the nearest second
            "waso": round(row["WASO"] * 3600),
            "num_active_periods": row["number_of_awakenings"],
            "sleep_efficiency": round(row["SleepDurationInSpt"] / row["SptDuration"] * 100)
        }
        sleep_data.append(sleep_rep)

    return sleep_data


def process_ggir_activity_summary(report_basename: str, ggir_output_dir: str,
                                  tz: pytz.tzinfo.DstTzInfo) -> List[Dict[str, Any]] or None:
    # read the CSV file from the results subdirectory of the ggir output directory
    # that starts with "part5_daysummary_MM"
    results_dir = ggir_output_dir + os.path.sep + "results"
    activity_summary_file = None
    for file in os.listdir(results_dir):
        if file.startswith("part5_daysummary_MM"):
            activity_summary_file = results_dir + os.path.sep + file
            break

    if activity_summary_file is None:
        print("Required ggir activity summary file does not exist in ggir directory %s. "
              "Stopping processing GGIR activity summary."
              % ggir_output_dir)
        return None

    activity_summary = pd.read_csv(activity_summary_file)
    activity_data = []
    for _, row in activity_summary.iterrows():
        day_start = datetime.strptime(row["calendar_date"], "%Y-%m-%d").replace(tzinfo=tz)
        day_start_ts = day_start.timestamp()
        day_end = day_start + timedelta(days=1)
        day_end_ts = day_end.timestamp()

        activity_rep = {
            "day_start": day_start,
            "day_start_ts": day_start_ts,
            "day_end": day_end,
            "day_end_ts": day_end_ts,
            # compute non-wear time by multiplying the percentage of non-wear time by the number of seconds in a day
            "non_wear": round(row["nonwear_perc_day"] * 86400),
            "sedentary": row["dur_day_total_IN_min"] * 60,
            "sedentary_percent": round(row["dur_day_total_IN_min"] / row["dur_day_min"] * 100),
            "light_activity": row["dur_day_total_LIG_min"] * 60,
            "light_activity_percent": round(row["dur_day_total_LIG_min"] / row["dur_day_min"] * 100),
            "moderate_activity": round(row["dur_day_total_MOD_min"] * 60),
            "moderate_activity_percent": round(row["dur_day_total_MOD_min"] / row["dur_day_min"] * 100),
            "vigorous_activity": round(row["dur_day_total_VIG_min"] * 60),
            "vigorous_activity_percent": round(row["dur_day_total_VIG_min"] / row["dur_day_min"] * 100)
        }
        activity_data.append(activity_rep)

    return activity_data

def collect_report(report_basename: str,
                   activinsights_activity_dir: str,
                   activinsights_sleep_dir: str,
                   sleeppy_dir: str,
                   ggir_dir) -> Dict[str, Any]:

    # We are going to maintain a set of metadata for use internally, as well as for the final JSON payload
    report_dict: Dict[str, Any] = {}

    # 1. Collect the required header information by reading the meta information from the name of the input file
    # split the filename by _ and parse the first element as the patient_id, the second as the device_location_code and
    # the third as the device_id
    report_basename_elements = report_basename.split("_")
    patient_id = report_basename_elements[0]
    device_location_code = report_basename_elements[1]
    device_id = report_basename_elements[2]

    report_dict["meta"] = {
        "patient_id": patient_id,
        "device_id": device_id,
        "device_location_code": device_location_code,
        "measurement_frequency": None,
        "start_time": None,
        "start_time_ts": None
    }

    report_dict["activity_summary"]: List[Dict[str, Any]] = []
    report_dict["sleep_summary"]: List[Dict[str, Any]] = []

    # If activinsights_activity_dir exists, fill in activity information from it
    if activinsights_activity_dir is not None:
        meta_dict, activity_data = process_activinsights_activity_summary(report_basename, activinsights_activity_dir)
        if meta_dict:
            report_dict["meta"]["measurement_frequency"] = meta_dict.get("measurement_frequency", None)
            report_dict["meta"]["start_time"] = meta_dict.get("start_time", None)
            report_dict["meta"]["start_time_ts"] = meta_dict.get("start_time_ts", None)
        if activity_data:
            report_dict["activity_summary"].append({
                "provenance": "ActivInsights",
                "data": activity_data
            })

    # If activinsights_sleep_dir dir exists, fill in the sleep information from it
    if activinsights_sleep_dir is not None:
        meta_dict, sleep_data = process_activinsights_sleep_summary(report_basename, activinsights_sleep_dir)
        if meta_dict:
            report_dict["meta"]["measurement_frequency"] = meta_dict.get("measurement_frequency", None)
            report_dict["meta"]["start_time"] = meta_dict.get("start_time", None)
            report_dict["meta"]["start_time_ts"] = meta_dict.get("start_time_ts", None)
        if sleep_data:
            report_dict["sleep_summary"].append({
                "provenance": "ActivInsights",
                "data": sleep_data
            })

    # If SleepPy directory exists, fill in the sleep information from it
    if sleeppy_dir is not None:
        sleeppy_data = process_sleeppy_summary(report_basename, sleeppy_dir)
        if sleeppy_data:
            report_dict["sleep_summary"].append({
                "provenance": "SleepPy",
                "data": sleeppy_data
            })

    # If GGIR directory exists, fill in the information about activity and sleep from it
    if ggir_dir is not None:
        ggir_meta = process_ggir_meta(report_basename, ggir_dir)
        if ggir_meta:
            report_dict["meta"]["measurement_frequency"] = ggir_meta.get("measurement_frequency", None)
            report_dict["meta"]["start_time"] = ggir_meta.get("start_time", None)
            report_dict["meta"]["start_time_ts"] = ggir_meta.get("start_time_ts", None)

            ggir_activity_data = process_ggir_activity_summary(report_basename, ggir_dir,
                                                               tz=ggir_meta["start_time"].tzinfo)
            if ggir_activity_data:
                report_dict["activity_summary"].append({
                    "provenance": "GGIR",
                    "data": ggir_activity_data
                })

            ggir_sleep_data = process_ggir_sleep_summary(report_basename, ggir_dir,
                                                         tz=ggir_meta["start_time"].tzinfo)
            if ggir_sleep_data:
                report_dict["sleep_summary"].append({
                    "provenance": "GGIR",
                    "data": ggir_sleep_data
                })

    return report_dict


if __name__ == "__main__":
    parser = ArgumentParser(
        description="""A test to extract sleep information from
                GENEACtiv accelerometer bin files using SleepPy, ActivInsights or GGIR.""", add_help=True
    )

    parser.add_argument('--activityReportDir', metavar='activity_report_dir', type=str,
                        default=None,
                        help="Path to Activinsights Activity Report parent folder "
                             "from which the R script for activity summary has been run.")

    parser.add_argument('--sleepReportDir', metavar='sleep_report_dir', type=str,
                        default=None,
                        help="Path to Activinsights Sleep Report parent folder "
                             "from which the R script for sleep summary has been run.")

    parser.add_argument('--sleepPyDir', metavar='sleeppy_report_dir', type=str,
                        default=None,
                        help="Path to SleepPy Report parent folder "
                             "from which the Python script for sleep summary has been run.")

    parser.add_argument('--ggirDir', metavar='ggir_report_dir', type=str, default=None,
                        help="Path to GGIR output parent folder")

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
    gqir_dir = args.ggirDir

    report_basename = os.path.basename(input_file).split(".")[0]

    report = collect_report(str(report_basename),
                            activinsights_activity_dir, activinsights_sleep_dir,
                            sleeppy_dir,
                            gqir_dir)

    output_file_name = "geneactiv_report_" + datetime.strftime(datetime.fromtimestamp(report["meta"]["start_time_ts"]),
                                                               "%Y_%m_%d") + ".json"
    output_file_path = os.path.sep.join([args.outputDir, output_file_name])

    with open(output_file_path, "w") as ofp:
        json.dump(report, ofp, indent=4, cls=NumpyEncoder, default=str)
