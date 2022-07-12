import pathlib
import os

class DefaultConfig(object):
    ALLOWED_MODELS = ["PD_aggreg_1", "Stroke_aggreg_1", "MS_aggreg_1"]
    INPUT_DIR = os.path.join(pathlib.Path(__file__).parent.parent.resolve(), "data", "input_dir")
    OUTPUT_DIR = os.path.join(pathlib.Path(__file__).parent.parent.resolve(), "data", "output_dir")

    MAX_UNPROCESSED_INPUT_FILES = 50

    DATETIME_FORMAT = "%Y-%m-%dT%H:%M:%S.%f%z"