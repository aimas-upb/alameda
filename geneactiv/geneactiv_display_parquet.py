import os
from glob import glob
import argparse
import json
import plotly.express as px
import plotly.graph_objects as go
import pandas as pd
import pyarrow as pa
from datetime import datetime
from plotly.subplots import make_subplots


def _extract_patient_id_from_filename(filename: str) -> str:
    """
    Extracts the patient ID from the filename.
    :param filename:
    :return:
    """
    return filename.split("_")[0]

def _read_pyarrow_zerocopy(filename: str) -> pa.Table:
    source = pa.memory_map(filename, 'r')
    return pa.ipc.RecordBatchFileReader(source).read_all()


if __name__ == '__main__':
    """
    The script aids to visualize the x, y, z and temperature time series data from a GeneActiv parquet file.
    """
    # Create an ArgumentParser object to parse mandatory command line arguments for the input directory
    parser = argparse.ArgumentParser(description='Visualize GeneActiv parquet data.')
    parser.add_argument('-i', '--input_file', type=str, required=True,
                        help='Path to the parquet input file whose data will be plotted.')
    args = parser.parse_args()
    input_file = args.input_file

    # get the base name of the input file and the directory where it is located
    base_name = os.path.basename(input_file).split(".parquet")[0]
    input_dir = os.path.dirname(input_file)
    patient_id = _extract_patient_id_from_filename(base_name)

    metadata_file = os.path.join(input_dir, patient_id + "_" + "meta" + ".json")

    info = None
    data = None

    # read the metadata json file
    with open(metadata_file, "r") as f:
        info = json.load(f)

    # Print the metadata
    print("Metadata:")
    print(json.dumps(info, indent=4))

    # read the parquet file
    print("Reading parquet file...")
    data = pd.read_parquet(input_file)

    # Before plotting the data, downsample the data to 1 Hz
    print("Downsampling data...")
    data = data.resample("1s").mean()

    print("Plotting data...")
    # Create a plotly figure that displays the x, y, z and temperature time series data in separate subplots.
    # The time axis is given by the index of the data frame.
    # Display the ticks for the time axis every hour and format them in ISO 8601 format.
    # Color the x, y, z and temperature time series data in red, green, blue and orange respectively.
    fig = make_subplots(rows=4, cols=1, shared_xaxes=True, vertical_spacing=0.05,
                        subplot_titles=("acc_x", "acc_y", "acc_z", "temperature"))

    fig.add_trace(go.Scatter(x=data.index, y=data["x"], mode='lines', name='acc_x',
                             line=dict(color='red')), row=1, col=1)
    fig.add_trace(go.Scatter(x=data.index, y=data["y"], mode='lines', name='acc_y',
                             line=dict(color='green')), row=2, col=1)
    fig.add_trace(go.Scatter(x=data.index, y=data["z"], mode='lines', name='acc_z',
                             line=dict(color='blue')), row=3, col=1)
    fig.add_trace(go.Scatter(x=data.index, y=data["T"], mode='lines', name='temperature',
                             line=dict(color='orange')), row=4, col=1)

    # Update the layout of the figure setting the title and the x-axis label.
    # The title is the base name of the input file. The x-axis label is "Time".
    fig.update_layout(
        title=base_name,
        xaxis_title="Time",
    )

    # repeat the same setup of x-axis ticks for all subplots
    fig.update_xaxes(
        # set the format of the time axis ticks to ISO 8601
        # tickformat="%H:%M:%S",
        tickmode="array",
        # set the tick values to be every hour, where we now that the data is a DateTimeIndex
        tickvals=data.index[::3600],

        # rotate the tick labels by 45 degrees
        tickangle=-45,
        tickfont=dict(size=10),

        # set the grid lines every hour
        tick0=data.index[0],
        dtick=3600,
        ticklen=5,
        tickwidth=1,
        tickcolor="#bbb",
        showgrid=True,
        gridwidth=1,
        gridcolor="#000",
    )

    # save the figure as an html file
    fig.write_html(os.path.join(input_dir, base_name + ".html"))




