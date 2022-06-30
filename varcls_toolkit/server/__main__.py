#!/usr/bin/env python3

import connexion

from server import encoder


def main():
    app = connexion.App(__name__, specification_dir='./swagger/')
    app.app.json_encoder = encoder.JSONEncoder
    app.add_api('swagger.yaml', arguments={'title': 'ALAMEDA Predictor Variable Timeseries Classification Toolkit'},
                pythonic_params=True)
    app.app.config.from_object("configs.config.DefaultConfig")
    app.run(port=8080)


if __name__ == '__main__':
    main()
