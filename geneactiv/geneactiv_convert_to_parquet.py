import os
import json
from glob import glob
import argparse
import numpy as np
from datetime import datetime, timedelta

import actipy
from tqdm import tqdm
import pyarrow as pa
import pandas as pd
from actipy import processing
from actipy.reader import Timer

INPUT_DIR = "./geneactiv/data/ms_pilot/raw"
OUTPUT_DIR = "./geneactiv/data/ms_pilot/parquet"


class NpEncoder(json.JSONEncoder):
    def default(self, obj):
        if isinstance(obj, np.integer):
            return int(obj)
        if isinstance(obj, np.floating):
            return float(obj)
        if isinstance(obj, np.ndarray):
            return obj.tolist()
        return super(NpEncoder, self).default(obj)


def _write_pyarrow_zerocopy(filename: str, table: pa.Table):
    with pa.OSFile(filename, 'wb') as sink:
        with pa.RecordBatchFileWriter(sink, table.schema) as writer:
            writer.write_table(table)

def _extract_patient_id_from_filename(filename: str) -> str:
    """
    Extracts the patient ID from the filename.
    :param filename:
    :return:
    """
    return filename.split("_")[0]


if __name__ == '__main__':
    """
    The script converts the raw GeneActiv bin data to a Pandas and R readable format, parquet.
    """
    # create an ArgumentParser object to parse mandatory command line arguments for input and output directories,
    # and optional arguments for the lowpass filter, resampling frequency, detecting non-wear and
    # whether to compress the output
    parser = argparse.ArgumentParser(description='Convert GeneActiv bin data to parquet format.')
    parser.add_argument('-i', '--input_dir', type=str, required=True, default=INPUT_DIR,
                        help='Path to the input directory containing the raw GeneActiv bin files.')
    parser.add_argument('-o', '--output_dir', type=str, required=True, default=OUTPUT_DIR,
                        help='Path to the output directory where the converted parquet files and their metadata will be stored.')
    parser.add_argument('-l', '--lowpass_hz', type=int, default=None,
                        help='Lowpass filter frequency in Hz to be applied to the raw data.')
    parser.add_argument('-r', '--resample_hz', type=int, default=None,
                        help='Resampling frequency in Hz to be applied to the raw data.')
    parser.add_argument('-n', '--nonwear', action='store_true',
                        help='Detect non-wear periods in the raw data. (default: False)')
    parser.add_argument('-c', '--compress', action='store_true',
                        help='Compress the output parquet files. (default: False)')

    # parse the command line arguments
    args = parser.parse_args()
    input_dir = args.input_dir
    output_dir = args.output_dir
    lowpass_hz = args.lowpass_hz
    resample_hz = args.resample_hz
    detect_nonwear = args.nonwear

    watch_files = glob(os.path.join(input_dir, "*.bin"))

    for file in tqdm(watch_files):
        base_name = os.path.basename(file).split(".bin")[0].replace(" ", "_")
        print(f" -> now at {base_name}")

        # create a folder in the output directory for the current patient, named using the base name of the file
        patient_dir = os.path.join(output_dir, base_name)
        if not os.path.exists(patient_dir):
            os.makedirs(patient_dir)

        patient_id = _extract_patient_id_from_filename(base_name)
        metadata_file = os.path.join(patient_dir, patient_id + "_" + "meta" + ".json")

        # First, just read the plain data, without any processing.
        data, info = actipy.read_device(file, lowpass_hz=None, calibrate_gravity=False, detect_nonwear=False,
                                        resample_hz=False, verbose=True)
        # add the patient ID to the info dict
        info["patient_id"] = patient_id

        # Then, apply the processing steps that were specified in the command line arguments.
        # get the stationary indicator data
        timer = Timer(True)
        timer.start("Getting stationary points...")
        stationary_indicator = processing.get_stationary_indicator(data)
        timer.stop()

        # calibrate the gravity vector
        timer.start("Calibrating gravity vector...")
        data, info_calib = processing.calibrate_gravity(data, stationary_indicator=stationary_indicator)
        info.update(info_calib)
        timer.stop()

        # save the metadata dict as a json file
        with open(metadata_file, "w") as f:
            json.dump(info, f, cls=NpEncoder)

        # The acceleration data is too large to store in a single parquet file, so we split it by days, by
        # looking at the index of the data. We then write each day to a separate parquet file, after having marked
        # the non-wear period in that day with a NaN value.

        # Get the day range of the data, knowing that the start date and end date are given as strings in the info dict.
        start_date = datetime.strptime(info["StartTime"], "%Y-%m-%d %H:%M:%S")
        end_date = datetime.strptime(info["EndTime"], "%Y-%m-%d %H:%M:%S")

        # construct the date range such that it includes the start and end date and the days
        # in between (each starting at 00:00:00 and ending at 23:59:59)

        # get the midnight after the first day
        first_day_midnight = (start_date + timedelta(days=1)).replace(hour=0, minute=0, second=0, microsecond=0)
        # get the midnight before the last day
        last_day_midnight = end_date.replace(hour=0, minute=0, second=0, microsecond=0)

        # get the days in between
        day_range = pd.date_range(start=first_day_midnight, end=last_day_midnight, freq="D")
        # add the start and end date to the day range
        day_range = [start_date] + list(day_range) + [end_date]

        # Iterate over the days and write the data for each day to a separate parquet file.
        for day_idx in range(1, len(day_range)):
            # Get the start and end time of the day.
            day = day_range[day_idx - 1].strftime("%Y-%m-%d")
            start_time = day_range[day_idx - 1]
            end_time = day_range[day_idx]

            # Get the data for the day.
            day_data = data.loc[start_time:end_time]

            # apply the lowpass filter, the resampling and the non-wear detection if the corresponding arguments are set
            if lowpass_hz is not None:
                timer.start("Lowpass filtering for day... %s" % day)
                day_data, _ = processing.lowpass(day_data, info["SampleRate"], lowpass_hz)
                timer.stop()

            if resample_hz is not None:
                timer.start("Resampling for day... %s" % day)
                day_data, _ = processing.resample(day_data, resample_hz)
                timer.stop()

            if detect_nonwear:
                # Use a patience of 60 minutes to detect non-wear periods.
                timer.start("Detecting non-wear for day... %s" % day)
                day_data, _ = processing.detect_nonwear(day_data, patience='60m',
                                                        stationary_indicator=stationary_indicator,
                                                        drop=False)
                timer.stop()

            # Get the filename for the parquet file for the day.
            day_parquet_file = os.path.join(patient_dir, patient_id + "_" + "data" + "_" + day + ".parquet")

            # Save the day data as a parquet file. Use gzip compression if the compress argument is set,
            # otherwise "snappy" is used. Set the index to None to avoid saving the index as a column.
            day_data.to_parquet(day_parquet_file, compression="gzip" if args.compress else "snappy", index=None)

        # Finally, delete the data and info dicts to free up memory.
        del data
        del info
