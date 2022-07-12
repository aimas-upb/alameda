# coding: utf-8

from __future__ import absolute_import
from datetime import date, datetime  # noqa: F401

from typing import List, Dict  # noqa: F401

from server.models.base_model_ import Model
from server import util


class AlgInvocation(Model):
    """NOTE: This class is auto generated by the swagger code generator program.

    Do not edit the class manually.
    """

    def __init__(self, model: str=None, disease: str=None, mode: str=None, file_id: str=None):  # noqa: E501
        """AlgInvocation - a model defined in Swagger

        :param model: The model of this AlgInvocation.  # noqa: E501
        :type model: str
        :param disease: The disease of this AlgInvocation.  # noqa: E501
        :type disease: str
        :param mode: The mode of this AlgInvocation.  # noqa: E501
        :type mode: str
        :param file_id: The file_id of this AlgInvocation.  # noqa: E501
        :type file_id: str
        """
        self.swagger_types = {
            'model': str,
            'disease': str,
            'mode': str,
            'file_id': str
        }

        self.attribute_map = {
            'model': 'model',
            'disease': 'disease',
            'mode': 'mode',
            'file_id': 'fileID'
        }

        self._model = model
        self._disease = disease
        self._mode = mode
        self._file_id = file_id

    @classmethod
    def from_dict(cls, dikt) -> 'AlgInvocation':
        """Returns the dict as a model

        :param dikt: A dict.
        :type: dict
        :return: The AlgInvocation of this AlgInvocation.  # noqa: E501
        :rtype: AlgInvocation
        """
        return util.deserialize_model(dikt, cls)

    @property
    def model(self) -> str:
        """Gets the model of this AlgInvocation.


        :return: The model of this AlgInvocation.
        :rtype: str
        """
        return self._model

    @model.setter
    def model(self, model: str):
        """Sets the model of this AlgInvocation.


        :param model: The model of this AlgInvocation.
        :type model: str
        """
        allowed_values = ["PD_aggreg_1", "Stroke_aggreg_1", "MS_aggreg_1"]  # noqa: E501
        if model not in allowed_values:
            raise ValueError(
                "Invalid value for `model` ({0}), must be one of {1}"
                .format(model, allowed_values)
            )

        self._model = model

    @property
    def disease(self) -> str:
        """Gets the disease of this AlgInvocation.


        :return: The disease of this AlgInvocation.
        :rtype: str
        """
        return self._disease

    @disease.setter
    def disease(self, disease: str):
        """Sets the disease of this AlgInvocation.


        :param disease: The disease of this AlgInvocation.
        :type disease: str
        """
        allowed_values = ["PD", "MS", "Stroke"]  # noqa: E501
        if disease not in allowed_values:
            raise ValueError(
                "Invalid value for `disease` ({0}), must be one of {1}"
                .format(disease, allowed_values)
            )

        self._disease = disease

    @property
    def mode(self) -> str:
        """Gets the mode of this AlgInvocation.


        :return: The mode of this AlgInvocation.
        :rtype: str
        """
        return self._mode

    @mode.setter
    def mode(self, mode: str):
        """Sets the mode of this AlgInvocation.


        :param mode: The mode of this AlgInvocation.
        :type mode: str
        """
        allowed_values = ["evaluate", "classify"]  # noqa: E501
        if mode not in allowed_values:
            raise ValueError(
                "Invalid value for `mode` ({0}), must be one of {1}"
                .format(mode, allowed_values)
            )

        self._mode = mode

    @property
    def file_id(self) -> str:
        """Gets the file_id of this AlgInvocation.

        ID of the CSV input file that has been uploaded prior to algorithm invocation  # noqa: E501

        :return: The file_id of this AlgInvocation.
        :rtype: str
        """
        return self._file_id

    @file_id.setter
    def file_id(self, file_id: str):
        """Sets the file_id of this AlgInvocation.

        ID of the CSV input file that has been uploaded prior to algorithm invocation  # noqa: E501

        :param file_id: The file_id of this AlgInvocation.
        :type file_id: str
        """

        self._file_id = file_id