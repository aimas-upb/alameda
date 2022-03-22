from sleeppy.sleep import SleepPy
import pandas as pd
import time
import os
from argparse import ArgumentParser

def run_test(src, run_config=0):
    dst = "test_results"
    dst_subfolder = os.path.basename(src).split(".")[0]
    st = time.time()
    try:
        SleepPy(input_file=src, results_directory=dst, sampling_frequency=50, verbose=True, run_config=run_config)
    except Exception as e:
        print("Error processing: {}\nError: {}".format(src, e))
    stp = time.time()
    print("total run time: {} minutes".format((stp - st) / 60.0))


def collect_endpoints(results_dir, dst_subfolder):
    src = results_dir + os.path.sep + dst_subfolder + os.path.sep + "sleep_endpoints" + os.path.sep \
          + "sleep_endpoints_summary.csv"
    return pd.read_csv(src).values[0]


def test_endpoints(expected, obtained):
    endpoint_names = [
        "day",
        "total_sleep_time",
        "percent_time_asleep",
        "waso",
        "sleep_onset_latency",
        "number_wake_bouts",
    ]
    errors = 0
    for i in range(len(expected)):
        try:
            assert expected[i] == obtained[i]
        except AssertionError:
            print(
                "Error encountered: endpoint at index {} ({}) "
                "does not match expected output".format(i, endpoint_names[i])
            )
            errors += 1
    if errors > 0:
        print("Total of {} errors encountered".format(errors))
    else:
        print("All tests passed")


if __name__ == "__main__":
    parser = ArgumentParser(
        description="""A test to extract sleep information from
                GENEACtiv accelerometer bin files using SleepPy.""", add_help=True
    )

    parser.add_argument('--inputFile', metavar='input_file', type=str, required=True)
    parser.add_argument('--configLevel', metavar="config_level", type=int, default=0)
    args = parser.parse_args()
    
    run_test(args.inputFile, run_config=args.configLevel)
    # run_test("data/asorici_right_wrist_test2.bin", run_config=5)