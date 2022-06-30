# coding: utf-8

import sys
from setuptools import setup, find_packages

NAME = "varcls_server"
VERSION = "1.0.0"

# To install the library, run the following
#
# python setup.py install
#
# prerequisite: setuptools
# http://pypi.python.org/pypi/setuptools

REQUIRES = [
    "connexion",
    "swagger-ui-bundle>=0.0.2"
]

setup(
    name=NAME,
    version=VERSION,
    description="ALAMEDA Predictor Variable Timeseries Classification Toolkit",
    author_email="alexandru.sorici@upb.ro",
    url="",
    keywords=["Swagger", "ALAMEDA Predictor Variable Timeseries Classification Toolkit"],
    install_requires=REQUIRES,
    packages=find_packages(),
    package_data={'': ['swagger/swagger.yaml']},
    include_package_data=True,
    entry_points={
        'console_scripts': ['server=server.__main__:main']},
    long_description="""\
    API description for the ALAMEDA Predictor Variable Timeseries Classification Toolkit. This description presents the RESTful interface by which the suite of algorithms developed for the classification of the patient disease status based on month-long data collected from wearables and PROs.  Each algorithm receives as input a CSV file containing timestamped results of PROs collected from a patient, as well as _information aggregated at **day** level_ determined from wearables (e.g. sum of steps, percentages of time per day in each type of activity intensity level, detected activties or exercises, objectively measured stiffness, brady / dyskenisia, detected fall).  Algorithms are selectable by a *model* and are applicable per _disease_ (PD, MS, Stroke). The algorithms can be run in **evaluation** or **prediction** mode.  In **evaluation** mode, the input CSV must contain both predictor and target variables, while the result CSV will report on the *performance metrics* for each target variable. In **prediction** mode, the input CSV contains only predictor variables, while the result CSV contains one or more target variables together with the probability distribution for each target variable value.  
    """
)

