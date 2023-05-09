import nextcloud_client
import json
import os
import argparse
from datetime import datetime, timedelta, time, date
from nextcloud_client import FileInfo
from tqdm import tqdm

SERVER_PUBLIC_URI = "server_public_uri"
REMOTE_BRACELET_ROOT_PATH = "remote_bracelet_root_path"
REMOTE_INSOLES_ROOT_PATH = "remote_insoles_root_path"
REMOTE_DATASET_ROOT_PATH = "remote_dataset_root_path"

PILOT_RELATIVE_PATHS = "pilot_relative_paths"
STROKE_PILOT = "stroke"
PD_PILOT = "pd"
MS_PILOT = "ms"
PILOTS = [STROKE_PILOT, PD_PILOT, MS_PILOT]

LOCAL_ROOT_DATA_PATH = "local_root_data_path"
LOCAL_RAW_DIR = "local_raw_dir"
LOCAL_STAGING_DIR = "local_staging_dir"
SMART_BRACELET_PATH = "smart_bracelet_path"
SMART_INSOLE_PATH = "smart_insole_path"
SMART_WATCH_PATH = "smart_watch_path"
SMART_BELT_PATH = "smart_belt_path"

START_DATE = "start_date"
END_DATE = "end_date"

BRACELET = "bracelet"
INSOLES = "insoles"
DATA_TYPES = [BRACELET, INSOLES]


class DatasetManager(object):
    """
    Class for managing the datasets in the ALAMEDA project which are hosted on a Nextcloud server.
    It has two main functions: 
      - (i) download all raw data files between start_date and end_date (if provided) 
      - (ii) upload all processed datasets to the server.
    
    Raw data files are downloaded in the raw_staging directory.
    Dataset files are uploaded from the dataset_staging directory.
    """
    def __init__(self, config_file_path: "config.json") -> None:
        """
        Args:
            config_file_path (str): Path to the config.json file.
        """
        self.config_file_path = config_file_path
        self.config = self._read_config()
        self._init_nextcloud_client()
        

    def _read_config(self) -> dict:
        """
        Reads the config.json file.
        """
        with open(self.config_file_path, "r") as f:
            config = json.load(f)
        return config
    
    def _init_nextcloud_client(self) -> None:
        """
        Initializes the Nextcloud client.
        """
        self.nc = nextcloud_client.Client.from_public_link(self.config[SERVER_PUBLIC_URI])


    ############################ CONFIG FUNCTIONS ############################
    @property
    def start_date(self) -> date:
        """
        Returns the start date of the dataset as a python datetime object.
        """
        return datetime.strptime(self.config[START_DATE], "%Y-%m-%d").date()

    @property
    def end_date(self) -> date:
        """
        Returns the end date of the dataset as a python datetime object.
        """
        return datetime.strptime(self.config[END_DATE], "%Y-%m-%d").date()
    
    @property
    def local_data_root_dir(self) -> str:
        """
        Returns the root directory of the local dataset.
        """
        return self.config[LOCAL_ROOT_DATA_PATH]
    
    @property
    def local_raw_data_dir(self) -> str:
        """
        Returns the directory of the local raw data.
        """
        return os.path.join(self.local_data_root_dir, self.config[LOCAL_RAW_DIR])
    
    @property
    def local_staged_data_dir(self) -> str:
        """
        Returns the directory of the local staged data.
        """
        return os.path.join(self.local_data_root_dir, self.config[LOCAL_STAGING_DIR])

    @property
    def local_smart_bracelet_raw_path(self) -> str:
        """
        Returns the path to the smart bracelet raw data locally, i.e. the path to the directory based on the root_local_dir.
        """
        return os.path.join(self.local_raw_data_dir, self.config[SMART_BRACELET_PATH])
    
    @property
    def local_smart_insole_raw_path(self) -> str:
        """
        Returns the path to the smart insole raw data locally, i.e. the path to the directory based on the root_local_dir.
        """
        return os.path.join(self.local_raw_data_dir, self.config[SMART_INSOLE_PATH])
    
    @property
    def local_bracelet_staged_path(self) -> str:
        """
        Returns the path to the smart bracelet staged data locally, i.e. the path to the directory based on the root_local_dir.
        """
        return os.path.join(self.local_staged_data_dir, self.config[SMART_BRACELET_PATH])
    
    @property
    def local_insole_staged_path(self) -> str:
        """
        Returns the path to the smart insole staged data locally, i.e. the path to the directory based on the root_local_dir.
        """
        return os.path.join(self.local_staged_data_dir, self.config[SMART_INSOLE_PATH])

    @property
    def remote_bracelet_dataset_path(self) -> str:
        """
        Returns the path to the smart bracelet dataset on the remote server.
        """
        return os.path.join(self.config[REMOTE_DATASET_ROOT_PATH], self.config[SMART_BRACELET_PATH])
    
    @property
    def remote_insole_dataset_path(self) -> str:
        """
        Returns the path to the smart insole dataset on the remote server.
        """
        return os.path.join(self.config[REMOTE_DATASET_ROOT_PATH], self.config[SMART_INSOLE_PATH])


    ############################ DOWNLOAD FUNCTIONS ############################
    def _list_remote_file_paths(self, path: str, start_date: date, end_date: date = None) -> list[FileInfo]:
        """
        List the paths in a remote directory for files (excluding directories) that have a last modified date between start_date and end_date.
        """
        res = list(self.nc.list(path, depth="infinity"))
        res = [r for r in res if not r.is_dir() and r.get_last_modified().date() >= start_date]
        
        if end_date is not None:
            res = [r for r in res if r.get_last_modified().date() <= end_date]
        return res
        
    def _download_file(self, remote_path: str, local_path: str) -> None:
        """
        Downloads a file from the remote_path to the local_path.
        """
        # First, create the local directory if it does not exist
        local_dir = os.path.dirname(local_path)
        if not os.path.exists(local_dir):
            os.makedirs(local_dir)

        print("Downloading file {} to {} ...".format(remote_path, local_path))
        self.nc.get_file(remote_path, local_path)

    def retrieve_files(self, data_type: str, start_date:date, end_date: date = None) -> None:
        """
        Downloads all the raw data files for the given data_type, for all pilot sites, that have a last modified date between start_date and end_date.
        The local directory structure is the following: data_type specific local root path/start_date_end_date/pilot/file_name
        """
        # set the local and remote root paths based on the data_type; assume that by default the data_type is smart_bracelet
        local_raw_path = self.local_smart_bracelet_raw_path
        remote_raw_path = self.config[REMOTE_BRACELET_ROOT_PATH]

        if data_type == INSOLES:
            local_raw_path = self.local_smart_insole_raw_path
            remote_raw_path = self.config[REMOTE_INSOLES_ROOT_PATH]
    
        for pilot in PILOTS:
            remote_path = os.path.join(remote_raw_path, self.config[PILOT_RELATIVE_PATHS][pilot])

            file_path_list = self._list_remote_file_paths(remote_path, start_date, end_date)
            print("Downloading {} files of type {} for pilot {} ...".format(len(file_path_list), data_type, pilot))

            for fp in tqdm([r.path for r in file_path_list]):
                local_path = os.path.join(local_raw_path, 
                                          "{}_{}".format(start_date.isoformat(), end_date.isoformat()), 
                                          pilot, 
                                          fp.split("/")[-1])
                self._download_file(fp, local_path)


    ############################ UPLOAD FUNCTIONS ############################
    def _upload_dataset_file(self, local_path: str, remote_path: str) -> None:
        """
        Uploads a dataset file from local_path to remote_path.
        """
        self.nc.put_file(remote_path, local_path, chunked=False)

    def _upload_dataset_dir(self, local_dir_path: str, remote_dir_path: str) -> None:
        """
        Uploads a dataset directory from local_dir_path to remote_dir_path.
        """
        # get the basename of the local_dir_path
        local_dir_basename = os.path.basename(local_dir_path)
        remote_dataset_dir_path = os.path.join(remote_dir_path, local_dir_basename, "")
        
        # create the remote dataset directory if it does not exist
        # check for existence by listing the remote_dir_path and checking for the remote_dataset_dir_path
        remote_dir_contents = self.nc.list(remote_dir_path)
        if remote_dataset_dir_path not in [r.path for r in remote_dir_contents]:
            self.nc.mkdir(remote_dataset_dir_path)

        # upload the files in the local_dir_path
        for fp in os.listdir(local_dir_path):
            if os.path.isfile(os.path.join(local_dir_path, fp)):
                local_path = os.path.join(local_dir_path, fp)
                self._upload_dataset_file(local_path, remote_dataset_dir_path)
        

    
    def upload_bracelet_dataset(self, start_date: date, end_date: date):
        """
        Uploads the dataset for the given data_type, for all pilot sites, that have a last modified date between start_date and end_date.
        """
        # set the local and remote root paths
        local_staged_path = self.local_bracelet_staged_path
        remote_dataset_path = self.remote_bracelet_dataset_path

        # Create the remote date range directory if it does not exist
        remote_date_range_dir = os.path.join("/", remote_dataset_path, "{}_{}".format(start_date.isoformat(), end_date.isoformat()), "")
        remote_date_range_path_list = [r.path for r in self.nc.list(remote_dataset_path, depth=1)]
        if remote_date_range_dir not in remote_date_range_path_list:
            self.nc.mkdir(remote_date_range_dir)

        for pilot in PILOTS:
            staged_path = os.path.join(local_staged_path, 
                                       "{}_{}".format(start_date.isoformat(), end_date.isoformat()),
                                       pilot)
            
            # The remote directory relative to the remote_dataset_path is based on the start and end dates and the pilot site.
            # We want to check if it already exists (from a previous upload) and if not, create it. To do this, we list the depth=1 entries 
            # in the remote_dataset_path. If the remote_dir_rel_path is not in the list, we create it.
            remote_dir_path = os.path.join("/", remote_dataset_path, "{}_{}".format(start_date.isoformat(), end_date.isoformat()), pilot, "")
            remote_dir_path_list = [r.path for r in self.nc.list(remote_date_range_dir, depth=1)]
            if remote_dir_path not in remote_dir_path_list:
                self.nc.mkdir(remote_dir_path)

            # we know that the staged bracelet data is organized into directories based on the pilot site
            # scan the staged_path directory and upload all directories found there
            print("Searching for data for pilot {} in the staged directory ...".format(staged_path))
            if os.path.exists(staged_path):
                for dir_name in os.listdir(staged_path):
                    dir_path = os.path.join(staged_path, dir_name)
                    if os.path.isdir(dir_path):
                        self._upload_dataset_dir(dir_path, remote_dir_path)
            else:
                print("No data found for pilot {} in the staged directory.".format(pilot))


if __name__ == "__main__":
    # Create an ArgumentParser object by which we parse the command line arguments.
    # We expect to receive the path to the config file, the task to be executed and the data types.
    # The task can be download or upload and is required.
    # The data types are optional, if not provided, all data types are considered. 
    # Data types are provided as a list of strings and accepted values are: ["bracelet", "insoles"]

    parser = argparse.ArgumentParser(description="Dataset manager for the ALAMEDA project.")
    parser.add_argument("--config", type=str, help="Path to the config file.", required=True, default="config.json")
    parser.add_argument("--task", type=str, help="Task to be executed.", required=True, default="download", choices=["download", "upload"])
    parser.add_argument("--dtypes", nargs="*", help="Data types to be retrieved or uploaded.", default=[], choices=["bracelet", "insoles"])
    args = parser.parse_args()

    # Get the config file path and the tasks to be executed. If the tasks are not provided, all tasks are executed.
    config_file_path = args.config
    task = args.task
    dtypes = args.dtypes
    if len(dtypes) == 0:
        dtypes = ["bracelet", "insoles"]

    # create the dataset manager
    dm = DatasetManager(config_file_path)

    if task == "download":
        if "insoles" in dtypes:
            # retrieve smart insole files
            print("Retrieving insole data for the period {} - {} ...".format(dm.start_date, dm.end_date))
            dm.retrieve_files(data_type = INSOLES, start_date=dm.start_date, end_date=dm.end_date)
    
        if "bracelet" in dtypes:
            print("Retrieving bracelet data for the period {} - {} ...".format(dm.start_date, dm.end_date))
            dm.retrieve_files(data_type = BRACELET, start_date=dm.start_date, end_date=dm.end_date)

    elif task == "upload":
        if "insoles" in dtypes:
            print("Skipping upload of insole data because it is not yet implemented.")
        if "bracelet" in dtypes:
            print("Uploading bracelet data for the period {} - {} ...".format(dm.start_date, dm.end_date))
            dm.upload_bracelet_dataset(start_date=dm.start_date, end_date=dm.end_date)

    print("Done.")
    

