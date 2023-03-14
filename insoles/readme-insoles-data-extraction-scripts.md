## General
This readme describes the functionality and usage of the python scripts that extract raw and aggregate data from the Loadsol insoles.

There are two scripts:
  - The `extract_aggregate_insole_metrics` script: extracts aggregate metrics (e.g. number of steps, average load rate, average cadence, average peak force per heel or forefoot) from an Loadsol `.slg` file
  - The `extract_insole_raw_data.py` script: extracts the raw force measurements from a Loadsol `.mva` file and stores them in a `.parquet` file.


## Requirements
To run the scripts, generate a Python environment using your favorite python package management framework (e.g. `pip` or `conda`) and install the packages in the `requirements.txt` file.

To obtain `.slg` and `.mva` files the following are required:
  - Extract `.pdo` files from a smartphone that was connected to Novel Loadsol-1 Insoles. Follow [this data extraction tutorial](https://docs.google.com/document/d/1fvJdPKLGes_boVxu5iqTqjMKqbPkyeSInhqqwI_42gM/edit?usp=sharing).
  - Have a Novel  **License Key** USB Stick which enables opening the `.pdo` file using the Novel Loadpad Analysis software suite (Windows only). Using the Loadpad Analysis programme, obtain the `.slg` and `.mva` files corresponding to a `.pdo`. Both `.slg` and `.mva` files are *tab-separated* files and can be opened as such using tools such as MS Excel or LibreOffice Calc.


## Functionality

### The `extract_aggregate_insole_metrics.py` script

The script extracts aggregate information from the .slg file returning:  
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

Apart from the above, it returns a per foot zone information about: 
  - step count
  - average contact time
  - average peak force
  - average loading rate
  - force time integral (fti) 
  - percentages of time spent loading the forefoot, heel and whole sole

The script arguments are:
```
  --input INPUT    Input .slg file
  --outdir OUTDIR  Output directory for the resulting .json file
  --upload         Upload the aggregate metrics to the SemKG endpoint 
				  (Default False)
```
 
By default the script will just extract the meta information and store it in a json file in the OUTDIR.
If the `--upload` flag is used, the script will use the information in the `config.yaml` file to send the data to the configured ALAMEDA SemKG endpoint.

For the upload to work, the script requires a configuration setup in a `config.yaml` file that **must** be present in the same folder as the script. The config file shoud resemble the following:
```yaml
iam_endpoint: "https://iam-backend.alamedaproject.eu"  
iam_username: "<your-institution-IAM-username>"  
iam_password: "<your-institution-IAM-password>"  
iam_login_path: "/api/v1/auth/login"  
iam_logout_path: "/api/v1/auth/logout"  
iam_verify_path: "/api/v1/auth/verify"  
semkg_endpoint: "https://semkg.alamedaproject.eu:4567"  
semkg_post_path: "/si/post"  
semkg_get_path: "/si/get"
```


### The `extract_insole_raw_data.py` script

The script extracts the information from the `.mva` file returning:  
- the original pdo file name  
- the file creation time  
- the total duration of the recorded data  
- a Pandas dataframe of force and force-time-integral recordings for each foot zone. The columns are: "timestamp", "time_rel_start", "step_left", "force_left_heel", "fti_left_heel",  "force_left_forefoot", "fti_left_forefoot", "force_left", "fti_left",  "step_right", "force_right_heel", "fti_right_heel", "force_right_forefoot",  "fti_right_forefoot", "force_right", "fti_right", "force_total", "fti_total",  "foib", "ablt"

The script has the following parameters:
```
--input INPUT    Input .mva file
--outdir OUTDIR  Output directory for the resulting .parquet file
```



## Example invocations

#### Aggregate data extraction

An invocation such as:
```
python extract_aggregate_insole_metrics.py --input ./data/stroke36_22-10-11_14-30-36-917.slg --outdir ./data/extracted_data --upload
```
produces the following `stroke36_22-10-11_14-30-36-917.json`  output file in the `data/extracted_data` OUTDIR.

The description and measurement units of each JSON key is provided in the [Schema of extracted Novel Loadsol summary metrics](https://docs.google.com/document/d/1YTuEdNpkR3xzA4bpPl6L7SGrbUtVuvgI6hmkvZDwjo8/edit?usp=sharing) document.

```JSON
{  
    "patient_id": "stroke36",  
    "original_filename": "stroke36_22-10-11 15-45-03-680",  
    "start_timestamp": "2022-10-11T15:45:03.680000Z",  
    "body_weight": 706,  
    "cadence_frequency": 10,  
    "start_time": "00:00:00",  
    "end_time": "00:37:04.860000",  
    "time_per_frame": 0.01,  
    "sampling_frequency": 100,  
    "total_duration": 2224.86,  
    "foib": 0.09,  
    "ablt": 543.0,  
    "avg_cadence": null,  
    "above_limit_percent": {  
        "left_heel": 4.26,  
        "left_forefoot": 6.83,  
        "left": 28.2,  
        "right_heel": 14.01,  
        "right_forefoot": 4.85,  
        "right": 31.76,  
        "total": 73.12  
    },  
    "between_limit_percent": {  
        "left_heel": 27.22,  
        "left_forefoot": 17.05,  
        "left": 18.13,  
        "right_heel": 19.97,  
        "right_forefoot": 20.36,  
        "right": 23.65,  
        "total": 2.36  
    },  
    "below_limit_percent": {  
        "left_heel": 68.53,  
        "left_forefoot": 76.12,  
        "left": 53.67,  
        "right_heel": 66.02,  
        "right_forefoot": 74.79,  
        "right": 44.59,  
        "total": 24.53  
    },  
    "upper_force_limit": {  
        "left_heel": 400.0,  
        "left_forefoot": 400.0,  
        "left": 400.0,  
        "right_heel": 400.0,  
        "right_forefoot": 400.0,  
        "right": 400.0,  
        "total": 400.0  
    },  
    "lower_force_limit": {  
        "left_heel": 200.0,  
        "left_forefoot": 200.0,  
        "left": 200.0,  
        "right_heel": 200.0,  
        "right_forefoot": 200.0,  
        "right": 200.0,  
        "total": 200.0  
    },  
    "steps": {  
        "left_heel": null,  
        "left_forefoot": null,  
        "left": 140.0,  
        "right_heel": null,  
        "right_forefoot": null,  
        "right": 151.0  
    },  
    "avg_contact_time": {  
        "left_heel": 962.0,  
        "left_forefoot": 985.0,  
        "left": 1297.0,  
        "right_heel": 1026.0,  
        "right_forefoot": 1227.0,  
        "right": 1431.0  
    },  
    "avg_peak_force": {  
        "left_heel": 281.606,  
        "left_forefoot": 387.422,  
        "left": 501.719,  
        "right_heel": 329.077,  
        "right_forefoot": 382.207,  
        "right": 540.826  
    },  
    "avg_loading_rate": {  
        "left_heel": null,  
        "left_forefoot": null,  
        "left": null,  
        "right_heel": null,  
        "right_forefoot": null,  
        "right": null  
    },  
    "fti": {  
        "left_heel": 283854.31,  
        "left_forefoot": 263271.03,  
        "left": 547125.38,  
        "right_heel": 373857.72,  
        "right_forefoot": 287093.34,  
        "right": 660951.06  
    },  
    "forefoot_percent": {  
        "left_heel": null,  
        "left_forefoot": null,  
        "left": 37.14,  
        "right_heel": null,  
        "right_forefoot": null,  
        "right": 47.68  
    },  
    "midfoot_percent": {  
        "left_heel": null,  
        "left_forefoot": null,  
        "left": 0.0,  
        "right_heel": null,  
        "right_forefoot": null,  
        "right": 0.0  
    },  
    "heel_percent": {  
        "left_heel": null,  
        "left_forefoot": null,  
        "left": 65.0,  
        "right_heel": null,  
        "right_forefoot": null,  
        "right": 56.95  
    }  
}
```

The `--upload` flag sends the data to the configure SemKG endpoint. 

#### Raw data extraction

A call like the following:
```
python extract_insole_raw_data.py --input data/stroke36_22-10-11_15-45-03-680.mva --outdir ./data/extracted_data
```
will create a python `stroke36_22-10-11_15-45-03-680.parquet` file in the selected OUTDIR.