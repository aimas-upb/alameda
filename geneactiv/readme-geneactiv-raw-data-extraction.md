## General
This readme describes the functionality and usage of the data extraction and visualization scripts for raw info retrieved from GENEActiv bracelets.

There are two scripts:
  - The `geneactiv_convert_to_parquet.py` script: converts `.bin` files to **a set** of `.parquet` files, where each file contains **accelerometer** (x, y and z axes) and **temperature** data for **one day** (each day starts at 00:00:00 and ends at 23:59:59). The number of files cover the duration of recordings from the original `.bin` file.
  - The `geneactiv_display_parquet.py` script: generates an HTML file containing a `plotly` diagram displaying the accelerometer and temperature time series data for an individual .parquet file (a day)

## Requirements
To run the scripts, generate a Python environment using your favorite python package management framework (e.g. `pip` or `conda`) and install the packages in the `requirements.txt` file.

To obtain `.bin` input files from the GENEActiv smart bracelets, follow the provided [extraction tutorial](https://docs.google.com/document/d/1fvJdPKLGes_boVxu5iqTqjMKqbPkyeSInhqqwI_42gM/edit?usp=sharing) that makes use of the [GENEActiv Windows software suite](https://activinsights.com/support/geneactiv-support/).

## Functionality

### The `geneactiv_convert_to_parquet.py` script

The script converts a GENEActiv `.bin` file into a set of per day `.parquet` files. 
The script has the following parameters:
```
  -i INPUT_DIR, --input_dir INPUT_DIR
        (Required) Path to the input directory containing the raw GeneActiv bin 
        files.
  
  -o OUTPUT_DIR, --output_dir OUTPUT_DIR
        (Required) Path to the output directory where the converted parquet files 
        and their metadata will be stored.
  
  -l LOWPASS_HZ, --lowpass_hz LOWPASS_HZ 
        (Optional) Lowpass filter frequency in Hz to be applied to the raw data.
  
  -r RESAMPLE_HZ, --resample_hz RESAMPLE_HZ
        (Optional) Resampling frequency in Hz to be applied to the raw data.
  
  -n, --nonwear         
        (Optional) Detect non-wear periods in the raw data. (default: False)
  
  -c, --compress        
        (Optional) Compress the output parquet files using gzip. (default: False, 
        will compress only using snappy)
```
 
The `INPUT_DIR` can contain one or more `.bin` files. For each input file, the script creates a **subfolder** in the `OUTPUT_DIR` named using the `basename` of the input file. The subfolder contains:
  - the resulting **per day** `.parquet` files, each containing timestamped accelerometer and temperature data.
  - a  `.json` file containing meta information: patient_id, device_id, start and end timestamp of the recordings, sample rate, gravity calibration coefficients.


### The `geneactiv_display_parquet.py` script

The script creates a `plotly` diagram for an individual .parquet file, displaying the x, y and z accelerometer and the temperature time series.
Because of the high default sampling rate (50Hz) of the accelerometer values, the script first *downsamples* the accelerometer values to 1Hz (by averaging the samples within a second).

The output is a plotly HTML file, which can be opened in a browser to interact with the diagram.

The script has the following parameters:
```
-i INPUT_FILE, --input_file INPUT_FILE
	    Path to the parquet input file whose data will be plotted.
```

The output html is generated in the folder containing the `INPUT_FILE`. The output html will be named using the basename of the `INPUT_FILE`.

## Example invocations

#### Data extraction
Considering an input `.bin` (e.g. `PGVjtj_left wrist_064398_2023-01-18 15-19-45.bin`) file residing in the folder `data/ms_pilot/raw`.

The invocation:
```
python geneactiv_convert_to_parquet.py -i ./data/ms_pilot/raw -o ./data/ms_pilot/parquet
```
produces the following output in the `data/ms_pilot/parquet` OUTPUT_FOLDER

```
+ data/ms_pilot/parquet
  + PGVjtj_left_wrist_064398_2023-01-18_15-19-45
    - PGVjtj_meta.json
    - PGVjtj_data_2022-12-23.parquet
    - PGVjtj_data_2022-12-24.parquet
    - PGVjtj_data_2022-12-25.parquet
    - PGVjtj_data_2022-12-26.parquet
    - PGVjtj_data_2022-12-27.parquet
    - ...
```

The total number of files in the `PGVjtj_left_wrist_064398_2023-01-18_15-19-45` subfolder depends on the number of days that the patient has worn the bracelet.

#### Data Visualization
To render a diagram for a .parquet file make a call like:
```
python geneactiv_display_parquet.py -i ./data/ms_pilot/parquet/PGVjtj_left_wrist_064398_2023-01-18_15-19-45/PGVjtj_data_2022-12-25.parquet

```
