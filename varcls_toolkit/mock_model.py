#!/usr/bin/env python3

import time
from datetime import datetime
from argparse import ArgumentParser
import json, os

if __name__ == '__main__':
    parser = ArgumentParser()
    parser.add_argument("--model", help="Model name for the VarCls algorithm to be run", type=str)
    parser.add_argument("--disease", help="Disease type for the VarCls algorithm to be run", type=str)
    parser.add_argument("--algID", help="Algorithm Invocation ID to be used for output JSON file name", type=str)
    parser.add_argument("--output_folder", help="Output folder where the JSON result file is to be stored", type=str)
    args = parser.parse_args()

    model = args.model
    disease = args.disease
    alg_id = args.algID
    output_folder = args.output_folder

    # simulate processing duration
    time.sleep(10)

    # write the JSON output
    mock_dict = {
        "meta": {
            "model": model,
            "disease": disease,
            "datetime": datetime.strftime(datetime.now(), "%Y-%m-%dT%H:%M:%S.%f%z")
        },
        "data": {
            "evaluation": {
                "accuracy": 0.86,
                "prec_mostly_off": 0.7,
                "recall_mostly_off": 0.9,
                "prec_indeterminate": 0.5,
                "recall_indeterminate": 0.7,
                "prec_mostly_on": 0.0,
                "recall_mostly_on": 0.0 
            },
        }
    }

    result_file_name = alg_id + "_" + "result" + ".json"

    result_file_path = os.path.join(output_folder, result_file_name)
    with open(result_file_path, 'w') as fp:
        json.dump(mock_dict, fp)



