from pyexpat import model
import connexion
import six

from server.models.alg_instance import AlgInstance  # noqa: E501
from server.models.alg_invocation import AlgInvocation  # noqa: E501
from server.models.api_response import ApiResponse  # noqa: E501
from server import util
from flask import Response
from flask import current_app
from flask import url_for
import os
import uuid
import json
import subprocess
from datetime import datetime
import shutil


def apply_algorithm_alg_iddelete(*args, **kwargs):  # noqa: E501
    """Stop a running execution or inform the server that the results of the algorithm invocation identified by algID can be safely discarded

     # noqa: E501

    :param algID: ID of algorithm instance to return
    :type algID: string

    :rtype: None
    """
    if "algID" in connexion.request.view_args:
        algID = connexion.request.view_args["algID"]
    elif "alg_id" in kwargs:
        algID = kwargs["alg_id"]
    
    data_input_dir = current_app.config["INPUT_DIR"]
    data_output_dir = current_app.config["OUTPUT_DIR"]
    output_root_folder_path = os.path.join(data_output_dir, algID)
    
    input_file_name = algID + ".csv"
    meta_file_name = algID + "_meta" + ".json"
    result_file_name = algID + "_result" + ".json"

    input_file_path = os.path.join(data_input_dir, input_file_name)
    meta_file_path = os.path.join(output_root_folder_path, meta_file_name)
    result_file_path = os.path.join(output_root_folder_path, result_file_name)

    # if the output root DOES NOT EXIST, return a 404
    if not os.path.exists(output_root_folder_path):
        return Response(
            response=json.dumps(ApiResponse(code=404, type="not found",
                                 message="No algorithm with algID %s is currently running or has finished executing." % algID).to_dict()),
            status=404,
            mimetype="application/json")
    
    # otherwise delete the directory altogether + the input dir
    #  - in the case the folder is deleted earlier than the algorithm process finishes,
    #    the algorithm will fail silently, because the ouput path to write the result file will not be found
    try:
        os.remove(input_file_path)
        shutil.rmtree(output_root_folder_path)
    except OSError as e:
        print("Error: %s : %s" % (output_root_folder_path, e.strerror))
    
    return Response(
            response=json.dumps(ApiResponse(code=204, type="deleted",
                                 message="Algorithm with algID %s has been terminated and files have been removed." % algID).to_dict()),
            status=204,
            mimetype="application/json")


def apply_algorithm_alg_idget(*args, **kwargs):  # noqa: E501
    """Retrieve status and result of a launched algorithm instance identified by algID

     # noqa: E501

    :param algID: ID of algorithm instance to return
    :type algID: string

    :rtype: List[AlgInstance]
    """
    if "algID" in connexion.request.view_args:
        algID = connexion.request.view_args["algID"]
    elif "alg_id" in kwargs:
        algID = kwargs["alg_id"]
    
    data_output_dir = current_app.config["OUTPUT_DIR"]
    output_root_folder_path = os.path.join(data_output_dir, algID)
    
    meta_file_name = algID + "_meta" + ".json"
    result_file_name = algID + "_result" + ".json"

    meta_file_path = os.path.join(output_root_folder_path, meta_file_name)
    result_file_path = os.path.join(output_root_folder_path, result_file_name)

    # if the output root folder exists, but there is NO result file in it => alg still running
    # if the result file EXISTS => alg has finished; load the file and return the result
    if not os.path.exists(output_root_folder_path):
        return Response(
            response=json.dumps(ApiResponse(code=404, type="not found",
                                 message="No algorithm with algID %s is currently running or has finished executing." % algID).to_dict()),
            status=404,
            mimetype="application/json")

    # if the output folder exists, there MUST be a meta file for the algorithm invocation
    meta_fp = open(meta_file_path)
    alg_invocation_dict = json.load(meta_fp)
    alg_instance = AlgInstance.from_dict(alg_invocation_dict)
    alg_instance.status = "running"

    if not os.path.exists(result_file_path):
        # the algorithm is still running, return the meta information
        with open(meta_file_path) as meta_fp:
            return Response(
                response=json.dumps(alg_instance.to_dict(), cls=util.DTEncoder),
                status=200,
                mimetype="application/json")
    
    # if we got here it means the algorithm has finished running and the result exists
    with open(result_file_path) as result_fp:
        result_dict = json.load(result_fp)
        alg_instance.status = "finished"
        alg_instance.result = result_dict

        return Response(
            response=json.dumps(alg_instance.to_dict(), cls=util.DTEncoder),
            status=200,
            mimetype="application/json"
        )


def apply_algorithm_post(config: AlgInvocation):  # noqa: E501
    """Launch a classification algorithm into execution

    This is the main call which launches a classification algorithm into execution. The type of the algorithm is determined by the _model_, _disease_ and *fileID* parameters. The algorithm can be launched in *classification* or *evaluation* mode. The former returns probability distributions over the possible values of each target variable known to the algorithm. The latter returns performance metrics for all the target variables included in the uploaded CSV file. # noqa: E501

    :param config: Config of the call specifying a structure containing model name, disease name and uploaded input file ID
    :type config: dict | bytes

    :rtype: AlgInstance
    """
    if connexion.request.is_json:
        config = AlgInvocation.from_dict(connexion.request.get_json())  # noqa: E501

    data_input_dir = current_app.config["INPUT_DIR"]
    data_output_dir = current_app.config["OUTPUT_DIR"]

    # verify existence of fild_id in data/input_dir
    uploaded_filename = config.file_id + ".csv"
    uploaded_filepath = os.path.join(data_input_dir, uploaded_filename)
    if not os.path.exists(uploaded_filepath):
        return Response(
            response=json.dumps(ApiResponse(code=400, type="bad request",
                                 message="Required CSV file with file id %s not found on server" % config.file_id).to_dict()),
            status=400,
            mimetype="application/json")
    
    if config.disease != "PD":
        return Response(
            response=json.dumps(ApiResponse(code=400, type="bad request",
                                 message="There is currently no model for disease %s" % config.disease).to_dict()),
            status=400,
            mimetype="application/json")
    
    if config.model not in current_app.config["ALLOWED_MODELS"]:
        return Response(
            response=json.dumps(ApiResponse(code=400, type="bad request",
                                 message="There is no model named %s" % config.model).to_dict()),
            status=400,
            mimetype="application/json")

    alg_invocation_id = config.file_id
    output_root_folder_path = os.path.join(data_output_dir, alg_invocation_id)

    if os.path.exists(output_root_folder_path):
        return Response(
            response=json.dumps(ApiResponse(code=400, type="bad request",
                                 message="There is already an instance of the invoked model running for file ID %s" % config.file_id).to_dict()),
            status=400,
            mimetype="application/json")

    # We have a new invocation.
    # Therefore, create the algorithm invocationID to be equal to the fileID of the uploaded CSV file.
    os.makedirs(output_root_folder_path)
    
    # store a json file with META information about the algorithm invocation in the output folder
    # this will be used when checking the status of the algorithm
    meta_file_name = alg_invocation_id + "_meta" + ".json"
    meta_file_path = os.path.join(output_root_folder_path, meta_file_name)
    
    with open(meta_file_path, "w") as fp:
        json.dump(
            AlgInstance(
                id=alg_invocation_id, 
                name=config.model,
                start_time=datetime.strftime(datetime.now(), current_app.config["DATETIME_FORMAT"]),
                status="running",
                results_uri=url_for("/v1.server_controllers_var_cls_controller_apply_algorithm_alg_idget", algID=alg_invocation_id)
            ).to_dict(), 
            fp)

    # launch a sub-process that mocks actual algorithm execution for 10 seconds
    # and then writes the output JSON to the output_root_folder_path
    cp = subprocess.Popen(
        ["python", 
        "mock_model.py", 
        "--model", config.model, 
        "--disease", config.disease, 
        "--algID", alg_invocation_id, 
        "--output_folder", output_root_folder_path
        ],
        text=True
    )
    # print(cp)

    return Response(
            response=json.dumps(
                AlgInstance(
                    id=alg_invocation_id, 
                    name=config.model,
                    start_time=datetime.strftime(datetime.now(), current_app.config["DATETIME_FORMAT"]),
                    status="running",
                    results_uri=url_for("/v1.server_controllers_var_cls_controller_apply_algorithm_alg_idget", algID=alg_invocation_id)
                ).to_dict()),
            status=201,
            mimetype="application/json")



def prepare_algorithm_post(model, disease, file):  # noqa: E501
    """Prepare execution of an ALAMEDA algorithm

    Prepare the execution of an ALAMEDA variable timeseries classification algorithm by uploading the required CSV input file. The call will return an ID of the file for use in the algorithm invocation. The algorithm to be executed is identified through *model* and *disease* parameters. The validity of the CSV input file is checked against these. # noqa: E501

    :param model: Name of the algorithm to be invoked later on
    :type model: str
    :param disease: Neurological Disease for which the algorithm will be applied
    :type disease: str
    :param file: Input CSV file to upload
    :type file: werkzeug.datastructures.FileStorage

    :rtype: object
    """
    if disease != "PD":
        return Response(
            response=json.dumps(ApiResponse(code=400, type="bad request",
                                 message="There is currently no model for disease %s" % disease).to_dict()),
            status=400,
            mimetype="application/json")

    if model not in current_app.config["ALLOWED_MODELS"]:
        return Response(
            response=json.dumps(ApiResponse(code=400, type="bad request",
                                 message="There is no model named %s" % model).to_dict()),
            status=400,
            mimetype="application/json")

    # save the file to the data input directory if there still is space
    max_uploaded_files = current_app.config["MAX_UNPROCESSED_INPUT_FILES"]
    data_input_dir = current_app.config["INPUT_DIR"]

    num_files = len(os.listdir(data_input_dir))
    if num_files == max_uploaded_files:
        return Response(
            response=json.dumps(ApiResponse(code=412, type="precondition failed",
                                 message="Number of input files to process is already "
                                         "at max level of %i" % max_uploaded_files).to_dict()),
            status=412,
            mimetype="application/json")

    # TODO: verify that submitted file is a CSV and that it has the fields required by the selected Model
    upload_file_id = disease + "_" + model + "_" + str(uuid.uuid4().hex)
    upload_filename = upload_file_id + ".csv"
    upload_file_path = os.path.join(data_input_dir, upload_filename)

    file.save(upload_file_path)

    return Response(response=json.dumps({"fileID": upload_file_id}),
                    status=200,
                    mimetype="application/json")
