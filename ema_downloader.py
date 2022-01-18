from typing import List
from argparse import ArgumentParser
import pandas as pd
import os
import json
from datetime import datetime
import urllib.request as req
from tqdm import tqdm

DATAVERSE_DATAFILE_PATH = "https://dataverse.nl/api/access/datafile/"


class DownloadProgressBar(tqdm):
    def update_to(self, b=1, bsize=1, tsize=None):
        if tsize is not None:
            self.total = tsize
        self.update(b * bsize - self.n)


def download_url(url, output_path):
    with DownloadProgressBar(unit='B', unit_scale=True,
                             miniters=1, desc=url.split('/')[-1]) as t:
        req.urlretrieve(url, filename=output_path, reporthook=t.update_to)


def get_dataset_df(metadata_filepath: str) -> pd.DataFrame:
    dataset_files_info = []
    
    with open(metadata_filepath, "r") as f:
        metadata_dict = json.load(f)
        
        files_data = metadata_dict["datasetVersion"]["files"]
        
        for f_data in files_data:
            if "directoryLabel" not in f_data:
                continue
            
            file_id = f_data["dataFile"]["id"]
            file_name = f_data["dataFile"]["filename"]
            patient_id = f_data["directoryLabel"]
            download_uri = DATAVERSE_DATAFILE_PATH + str(file_id)
            
            file_elems = os.path.splitext(file_name)[0].split("_")
            sensor_id = file_elems[0]
            file_date = datetime.strptime(file_elems[1], "%Y%m%d").date()
            file_time = datetime.strptime(file_elems[2], "%H%M%S").time()
            
            dataset_files_info.append({
                "file_id": file_id,
                "name": file_name,
                "patient_id": patient_id,
                "download_uri": download_uri,
                "sensor_id": sensor_id,
                "date": file_date,
                "time": file_time
            })
            
    return pd.DataFrame(dataset_files_info)
    

def download(metadata_filepath: str, download_path: str, patient_ids: List[str] = None):
    download_path_exists = os.path.exists(download_path)
    dataset_df = None
    
    if not download_path_exists:
        os.makedirs(download_path)

    dataset_df_file = os.path.join(download_path, "dataset_df.pkl")
    if os.path.exists(dataset_df_file):
        dataset_df = pd.read_pickle(dataset_df_file)
    else:
        # create the dataset_df
        dataset_df = get_dataset_df(metadata_filepath)
        pd.to_pickle(dataset_df, dataset_df_file)
    
    if patient_ids:
        for p_id in patient_ids:
            patient_folder = os.path.join(download_path, p_id)
            if not os.path.exists(patient_folder):
                os.makedirs(patient_folder)
                
            df_files = dataset_df[dataset_df["patient_id"] == p_id].sort_values(["date", "time"],
                                                                                ascending=(True, True))
            
            for _, row in df_files.iterrows():
                print("Downloading file: " + row["name"])
                download_uri = row["download_uri"]
                filepath = os.path.join(patient_folder, row["name"])
                download_url(download_uri, filepath)
                
    
if __name__ == "__main__":
    arg_parser = ArgumentParser()
    
    arg_parser.add_argument('--dataset_metadata', default="dataset_export_metadata.json", type=str,
                            help='Path to EMA dataset JSON metadata file.')
    arg_parser.add_argument('--download_dir', default="./data", type=str, help='Path to download dir.')
    arg_parser.add_argument('--patient_ids',  type=str, nargs="+", help='Video resolution.')

    args = arg_parser.parse_args()
    
    download(args.dataset_metadata, args.download_dir, patient_ids = args.patient_ids)
