# coding: utf-8

from __future__ import absolute_import

from flask import json
from six import BytesIO

from server.models.alg_instance import AlgInstance  # noqa: E501
from server.models.alg_invocation import AlgInvocation  # noqa: E501
from server.models.api_response import ApiResponse  # noqa: E501
from server.test import BaseTestCase


class TestVarClsController(BaseTestCase):
    """VarClsController integration test stubs"""

    def test_apply_algorithm_alg_iddelete(self):
        """Test case for apply_algorithm_alg_iddelete

        Stop a running execution or inform the server that the results of the algorithm invocation identified by algID can be safely discarded
        """
        response = self.client.open(
            '/v1/apply_algorithm/{algID}'.format(algID=789),
            method='DELETE')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_apply_algorithm_alg_idget(self):
        """Test case for apply_algorithm_alg_idget

        Retrieve status and result of a launched algorithm instance identified by algID
        """
        response = self.client.open(
            '/v1/apply_algorithm/{algID}'.format(algID=789),
            method='GET')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_apply_algorithm_post(self):
        """Test case for apply_algorithm_post

        Launch a classification algorithm into execution
        """
        config = AlgInvocation()
        response = self.client.open(
            '/v1/apply_algorithm',
            method='POST',
            data=json.dumps(config),
            content_type='application/json')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))

    def test_prepare_algorithm_post(self):
        """Test case for prepare_algorithm_post

        Prepare execution of an ALAMEDA algorithm
        """
        data = dict(model='PD_aggreg_1',
                    disease='PD',
                    file=(BytesIO(b'some file data'), 'file.txt'))
        response = self.client.open(
            '/v1/prepare_algorithm',
            method='POST',
            data=data,
            content_type='multipart/form-data')
        self.assert200(response,
                       'Response body is : ' + response.data.decode('utf-8'))


if __name__ == '__main__':
    import unittest
    unittest.main()
