"""
Script to extract aggregate metrics from insole data stored in an .slg tab separated file.
"""

import os
import argparse

import numpy as np
import pandas as pd
from typing import Dict, Any, Tuple
from datetime import datetime, time
import json
import yaml
import requests

def get_aggregate_information(filename: str) -> Dict[str, Any]:
    """
    Extract the aggregate information from the .slg file returning:
    - the patient id
    - the original pdo file name
    - the file creation time
    - the patient body weight
    - the cadence aggregation frequency
    - the sampling frequency
    - the total duration of the recorded data
    - the factor of imbalance (foib)
    - the average bodyload over time (ablt)
    - the average cadence

    Apart from the aggregate information, return a pandas dataframe containing per foot zone information about
    step count, average contact time, average peak force, average loading rate, force time integral (fti) and
    percentages of time spent loading the forefoot, heel and whole sole.
    :param filename:
    :return: dictionary of meta and aggregate information per foot zone
    """

    agg_dict = {}

    with open(filename, "r") as f:
        for i, line in enumerate(f):
            # 1. read the first row of the file and separate by tab to get the filename and the timestamp strings
            if i == 0:
                line_elements = line.split("\t")
                filename_str = line_elements[0]

                # split the filename string by column to get the original pdo file name, as the second argument,
                # by removing leading and trailing whitespace and removing the .pdo extension
                filename_str_split = filename_str.split(":")
                original_pdo_filename = filename_str_split[1].strip().replace(".pdo", "")

                # split the original pdo filename by underscore to get the patient id, as the first argument,
                # and the timestamp string as the second
                original_pdo_filename_split = original_pdo_filename.split("_")
                patient_id = original_pdo_filename_split[0]
                timestamp_str = original_pdo_filename_split[1]

                # convert the timestamp string to a datetime object
                timestamp = datetime.strptime(timestamp_str, "%y-%m-%d %H-%M-%S-%f")
                agg_dict["patient_id"] = patient_id
                agg_dict["original_filename"] = original_pdo_filename
                # agg_dict["start_timestamp"] = timestamp.strftime("%Y-%m-%dT%H:%M:%S.%f%zZ")
                agg_dict["start_timestamp"] = timestamp.isoformat()

            # 2. read the 3rd row of the file and separate by column to get the body weight as the second argument,
            # converting to int
            if i == 2:
                line_split = line.split(":")
                body_weight = int(line_split[1].strip())
                agg_dict["body_weight"] = body_weight
            # 3. read the 4th row of the file and separate by ">=" to get the cadence frequency string as the 2nd arg
            if i == 3:
                line_split = line.split(">=")
                cadence_frequency_str = line_split[1].strip()
                cadence_frequency = int(cadence_frequency_str.split(" ")[0])
                agg_dict["cadence_frequency"] = cadence_frequency
            # 4. read the 9th row of the file and separate by tab to get the start time, end time, time per frame
            # and sampling frequency strings
            if i == 8:
                line_elements = line.split("\t")
                start_time_str, end_time_str, time_per_frame_str, sampling_frequency_str = line_elements[0:4]
                start_time = datetime.strptime(start_time_str.split(" ")[-1].strip(), "%H:%M:%S:%f").time()
                end_time = datetime.strptime(end_time_str.split(" ")[-1].strip(), "%H:%M:%S:%f").time()
                time_per_frame = float(time_per_frame_str.split(" ")[-1].strip())
                sampling_frequency = int(sampling_frequency_str.split(" ")[-1].strip())
                agg_dict["start_time"] = start_time.isoformat()
                agg_dict["end_time"] = end_time.isoformat()
                agg_dict["time_per_frame"] = time_per_frame
                agg_dict["sampling_frequency"] = sampling_frequency
                agg_dict["total_duration"] = (datetime.combine(datetime.today(), end_time) - datetime.combine(datetime.today(), start_time)).total_seconds()
            # 5. read the 29th row of the file and separate by tab to get the FOIB, ABLT and avg. cadenece strings
            if i == 28:
                line_elements = line.split("\t")
                foib_str, ablt_str, avg_cadence_str = line_elements[0:3]
                foib = float(foib_str.split(" ")[1].strip())
                ablt = float(ablt_str.split(" ")[-1].strip())

                avg_cadence = avg_cadence_str.split(" ")[-1].strip()
                if avg_cadence == "---":
                    avg_cadence = None
                else:
                    avg_cadence = float(avg_cadence)

                agg_dict["foib"] = foib
                agg_dict["ablt"] = ablt
                agg_dict["avg_cadence"] = avg_cadence

    # The rest of the meta information is we extract as a pandas dataframe.
    # We are first reading in rows 12-18 from the tab separated slg file. The column names are: zones,
    # above_limit_percent, between_limit_percent, below_limit_percent, upper_force_limit, lower_force_limit,
    # drop_percent
    force_limit_df = pd.read_csv(filename, sep="\t", skiprows=11, nrows=7, header=None,
                                 names=["zones", "above_limit_percent", "between_limit_percent", "below_limit_percent",
                                        "upper_force_limit", "lower_force_limit", "drop_percent"])
    # replace the values of the zones column with the zone names: ["left_heel", "left_forefoot", "left",
    # "right_heel", "right_forefoot", "right", "total"]
    force_limit_df["zones"] = ["left_heel", "left_forefoot", "left", "right_heel", "right_forefoot", "right", "total"]

    # drop column drop_percent
    force_limit_df = force_limit_df.drop("drop_percent", axis=1)

    # convert all columns to numeric
    force_limit_df = force_limit_df.apply(pd.to_numeric, errors="ignore")

    # convert all NaN values to None
    force_limit_df = force_limit_df.replace(np.NaN, None)

    # convert the dataframe to a dictionary, by having the zones column as a first level key and
    # the column names as second level keys
    force_limit_dict = force_limit_df.set_index("zones").T.to_dict("index")
    agg_dict.update(force_limit_dict)

    # Then we are reading in rows 21-26 from the tab separated
    # slg file. The column names are: zones, steps, avg_contact_time, avg_peak_force, avg_loading_rate, fti,
    # forefoot_percent, midfoot_percent and heel_percent. All columns are numeric.
    agg_df = pd.read_csv(filename, sep="\t", skiprows=20, nrows=7, header=None,
                          names=["zones", "steps", "avg_contact_time", "avg_peak_force",
                                 "avg_loading_rate", "fti", "forefoot_percent", "midfoot_percent", "heel_percent"])
    # remove the last row, which is the total
    agg_df = agg_df[:-1]

    # replace the values of the zones column with the zone names: ["left_heel", "left_forefoot", "left",
    # "right_heel", "right_forefoot", "right"]
    agg_df["zones"] = ["left_heel", "left_forefoot", "left", "right_heel", "right_forefoot", "right"]

    # convert all "---" values appearing in column avg_loading_rate to nan
    agg_df.loc[agg_df["avg_loading_rate"] == "---", "avg_loading_rate"] = None
    agg_df = agg_df.apply(pd.to_numeric, errors="ignore")

    # convert all NaN values appearing in the dataframe to None
    agg_df = agg_df.replace(np.NaN, None)

    # convert the dataframe to a dictionary, by having the zones column as a first level key and
    # the column names as second level keys
    agg_dict.update(agg_df.set_index("zones").T.to_dict("index"))

    return agg_dict


def iam_login(iam_config_file: str = "config.yaml") -> Tuple[str, str] or Tuple[None, None]:
    """
    Login to the ALAMEDA IAM service to obtain an access_token and a refresh_token
    :param iam_config_file: configuration file storing the IAM endpoint and the username and password under
    keywords "iam_endpoint", "iam_username" and "iam_password"
    :return: access_token, refresh_token or None, None if the login failed
    """
    with open(iam_config_file, "r") as f:
        iam_config = yaml.safe_load(f)
        iam_endpoint = iam_config["iam_endpoint"]
        iam_username = iam_config["iam_username"]
        iam_password = iam_config["iam_password"]
        iam_login_path = iam_config["iam_login_path"]

        # The login endpoint is the iam_endpoint + "/api/v1/auth/login"
        login_endpoint = f"{iam_endpoint}" + f"{iam_login_path}"
        # The login payload is a JSON object with the username and password
        login_payload = {"username": iam_username, "password": iam_password}
        # The login headers are the standard JSON headers
        login_headers = {"Content-Type": "application/json"}
        # The login request is a POST request
        login_request = requests.post(login_endpoint, json=login_payload, headers=login_headers)

        # If the login request failed, return None, None
        if login_request.status_code != 200:
            return None, None
        else:
            # The login response is a JSON object with the access_token and the refresh_token
            login_response = login_request.json()
            access_token = login_response["access_token"]
            refresh_token = login_response["refresh_token"]
            return access_token, refresh_token


def iam_logout(access_token: str, refresh_token: str, iam_config_file: str = "config.yaml"):
    """
    Logout from the ALAMEDA IAM service
    :param iam_config_file: configuration file storing the IAM endpoint and IAM logout path under
    keywords "iam_endpoint" and "iam_logout_path"
    :param access_token: access token to be used for the logout request
    :param refresh_token: refresh token to be used for the logout request
    :return:
    """
    with open(iam_config_file, "r") as f:
        iam_config = yaml.safe_load(f)
        iam_endpoint = iam_config["iam_endpoint"]
        iam_logout_path = iam_config["iam_logout_path"]

        # The logout endpoint is the iam_endpoint + "/api/v1/auth/logout"
        logout_endpoint = f"{iam_endpoint}" + f"{iam_logout_path}"
        # The logout payload is a JSON object with the refresh_token
        logout_payload = {"refresh_token": refresh_token}
        # The logout headers are the standard JSON headers + the Authorization header with the value
        # "Bearer " + access_token
        logout_headers = {"Content-Type": "application/json", "Authorization": f"Bearer {access_token}"}
        # The logout request is a POST request
        logout_request = requests.post(logout_endpoint, json=logout_payload, headers=logout_headers)


def upload_dict_to_semkg(agg_dict: Dict[str, Any], access_token: str):
    """
    Upload the aggregate metrics to the SemKG endpoint configured in the config.yaml file
    :param agg_dict:
    :param access_token:
    :return:
    """
    # get the semkg_endpoint and the semkg_upload_path from the config.yaml file
    with open("config.yaml", "r") as f:
        config = yaml.safe_load(f)
        semkg_endpoint = config["semkg_endpoint"]
        semkg_post_path = config["semkg_post_path"]

        # The upload endpoint is the semkg_endpoint + semkg_post_path
        upload_endpoint = f"{semkg_endpoint}" + f"{semkg_post_path}"
        # The upload headers are the standard JSON headers + the Authorization header with the value
        # "Bearer " + access_token
        # The payload is the agg_dict
        req = requests.post(upload_endpoint, json=agg_dict,
                            headers={"Content-Type": "application/json",
                                     "Authorization": f"Bearer {access_token}"})
        if req.status_code != 200:
            print(f"Upload failed with status code {req.status_code}")
            print(req.text)
        else:
            print(f"Upload succeeded with status code {req.status_code}")


def upload_to_semkg(agg_dict: Dict[str, Any]):
    """
    Upload the aggregate metrics to the SemKG endpoint
    :param agg_dict:
    :return:
    """
    # First, check to see if we have a valid access_token and a refresh_token, which are stored in a json file
    # called "iam_tokens.json". If the file does not exist, then we need to login to the ALAMEDA IAM service
    # to obtain the tokens.
    access_token = None
    refresh_token = None

    if not os.path.exists("iam_tokens.json"):
        access_token, refresh_token = iam_login()
        # If the login failed, return
        if access_token is None or refresh_token is None:
            return
        # Otherwise, store the tokens in the iam_tokens.json file
        else:
            with open("iam_tokens.json", "w") as f:
                json.dump({"access_token": access_token, "refresh_token": refresh_token}, f)

    # Now call the upload_dict_to_semkg function, passing the access_token as argument
    try:
        upload_dict_to_semkg(agg_dict, access_token)
    except Exception as e:
        print(f"Upload failed with exception {e}")
    finally:
        # After the upload, revoke the access_token by calling the logout endpoint of the IAM service
        iam_logout(access_token, refresh_token)
        # delete the iam_tokens.json file
        os.remove("iam_tokens.json")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=str, required=True, help="Input .slg file or folder of .slg files")
    parser.add_argument("--outdir", type=str, required=True, help="Output directory for the resulting .json file")
    parser.add_argument("--upload", action="store_true", help="Upload the aggregate metrics to the SemKG endpoint")
    args = parser.parse_args()

    # Check if input is a file or a directory
    if os.path.isfile(args.input):
        print("Handling single file {}".format(args.input))

        # 1. get the meta information and aggregate measures per foot zone from the .slg file
        agg_dict = get_aggregate_information(args.input)
        # print(agg_dict)

        # 2. store the dictionary as a json file in the output directory, naming the file as the original pdo filename
        # with the .json extension. Treat nan values as null.
        with open(os.path.join(args.outdir, agg_dict["original_filename"] + ".json"), "w") as f:
            json.dump(agg_dict, f, indent=4)

        # 3. upload the agg_dict to the SemKG endpoint if the --upload flag is set
        if args.upload:
            upload_to_semkg(agg_dict)
    else:
        print("Handling directory {}".format(args.input))

        # parse recursively the directory and process all files ending in .slg
        for root, dirs, files in os.walk(args.input):
            for file in files:
                if file.endswith(".slg"):
                    slg_file_path = os.path.join(root, file)

                    print("Handling file {}".format(slg_file_path))

                    # 1. get the meta information and aggregate measures per foot zone from the .slg file
                    agg_dict = get_aggregate_information(slg_file_path)
                    # print(agg_dict)

                    # 2. store the dictionary as a json file in the output directory, naming the file as the original pdo filename
                    # with the .json extension. Treat nan values as null.
                    with open(os.path.join(args.outdir, agg_dict["original_filename"] + ".json"), "w") as f:
                        json.dump(agg_dict, f, indent=4)

                    # 3. upload the agg_dict to the SemKG endpoint if the --upload flag is set
                    if args.upload:
                        upload_to_semkg(agg_dict)




