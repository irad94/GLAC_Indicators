from app import app
import dash
import dash_bootstrap_components as dbc
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output, State
import plotly.graph_objs as go
from numpy import arange, array, ones
import numpy as np
from scipy import stats
import plotly.io as pio
import pandas as pd
import json
import base64
import io
from dash.exceptions import PreventUpdate

body = dbc.Container(
    [
        dbc.Row(
            [html.H1(id="base-title", style={'font-size': '400%'})]),
        dbc.Row([
                html.Div([
                    html.P([html.Strong('Seleccione la variable dependiente', style={'font-size': '120%'})]),
                    dcc.Dropdown(
                        id='yaxis-column'
                    )
                ],
                    style={'width': '50%', 'display': 'inline-block'}),

                html.Div([
                    html.P([html.Strong('Seleccione la variable independiente', style={'font-size': '120%'})]),
                    dcc.Dropdown(
                        id='xaxis-column'
                    )
                ], style={'width': '50%', 'float': 'right', 'display': 'inline-block'})]),
        dbc.Row([
            html.P("")]),
        dbc.Row([
            dbc.Col([
                dbc.FormGroup(
                    [
                        dbc.Label("Introduzca el nombre del eje de las ordenadas",
                                  style={'font-size': '130%', 'margin-top': '15px'}),
                        dbc.Input(id='input-1-state',
                                  placeholder="Introduzca el nombre del eje Y",
                                  type="text", autoComplete='off'),
                    ]
                ),
                dbc.FormGroup(
                    [
                        dbc.Label("Introduzca el nombre del eje de las abscisas",
                                  style={'font-size': '130%'}),
                        dbc.Input(id='input-2-state',
                                  placeholder="Introduzca el nombre del eje X",
                                  type="text",autoComplete='off'),
                    ]
                ),
                html.Hr(style={'width': '100%', 'margin': '0', 'margin-top': '15px', 'margin-bottom': '15px'}),
                html.Div(
                    dbc.Button('ACTUALIZAR EJES', id='btn-1', color="primary", className="mr-2",
                               size="lg", block=True),
                    style={'width': '100%', 'margin': '0', 'margin-top': '15px', 'margin-bottom': '15px'}),
                html.Div(
                    dcc.Upload(
                        id='upload-data',
                        children=html.Div([
                            'Drag and Drop or ',
                            html.A('Select Files', style={'font-size': '115%'})]),
                        style={
                            'width': '100%',
                            'height': '60px',
                            'lineHeight': '60px',
                            'borderWidth': '1px',
                            'borderStyle': 'dashed',
                            'borderRadius': '5px',
                            'textAlign': 'center',
                        },
                        # Allow multiple files to be uploaded
                        multiple=False
                    ),
                    style={'width': '100%', 'margin': '0', 'margin-top': '15px', 'margin-bottom': '15px'})
            ],  md=5),
            dbc.Col([dcc.Graph(id='indicator-graphic')], md=7),
            ]),
        dbc.Row([
            html.P("")]),
    ],
    className="mt-4",
)

layout = html.Div([body])


@app.callback(
    [Output('yaxis-column', 'options'),
     Output('xaxis-column', 'options')],
    [Input("upload-data", 'contents'),
     Input('upload-data', 'filename')])
def update_graph(contents, filename):
    if contents is None:
        raise PreventUpdate
    else:
        content_type, content_string = contents.split(',')

        decoded = base64.b64decode(content_string)
        try:
            if 'csv' in filename:
                # Assume that the user uploaded a CSV file
                df = pd.read_csv(
                    io.StringIO(decoded.decode('utf-8')))
                df = df.select_dtypes(include=np.number).dropna(axis=1, how='all')
                available_indicators = df.columns
                options1 = [{'label': i, 'value': i} for i in available_indicators]
                options2 = [{'label': i, 'value': i} for i in available_indicators]
            elif 'xls' in filename:
                # Assume that the user uploaded an excel file
                df = pd.read_excel(io.BytesIO(decoded))
                df = df.select_dtypes(include=np.number).dropna(axis=1, how='all')
                available_indicators = df.columns
                options1 = [{'label': i, 'value': i} for i in available_indicators]
                options2 = [{'label': i, 'value': i} for i in available_indicators]
        except Exception as e:
            print(e)
            return html.Div([
                'There was an error processing this file.'
            ])
        return options1, options2


@app.callback(
    Output('indicator-graphic', 'figure'),
    [Input('btn-1', 'n_clicks'),
     Input("upload-data", "contents"),
     Input("upload-data", 'filename'),
     Input('xaxis-column', 'value'),
     Input('yaxis-column', 'value')],
    [State('input-1-state', 'value'),
     State('input-2-state', 'value')])
def update_graph(n_clicks1, contents, filename, xaxis_column_name, yaxis_column_name, input_1, input_2):
    if contents is None:
        raise PreventUpdate
    elif xaxis_column_name is None and yaxis_column_name is None:
        raise PreventUpdate
    elif xaxis_column_name is None:
        raise PreventUpdate
    elif yaxis_column_name is None:
        raise PreventUpdate
    else:
        content_type, content_string = contents.split(',')

        decoded = base64.b64decode(content_string)
        try:
            if 'csv' in filename:
                # Assume that the user uploaded a CSV file
                df = pd.read_csv(
                    io.StringIO(decoded.decode('utf-8')))
            elif 'xls' in filename:
                # Assume that the user uploaded an excel file
                df = pd.read_excel(io.BytesIO(decoded))
        except Exception as e:
            print(e)
            return html.Div([
                'There was an error processing this file.'
            ])

        df = df.select_dtypes(include=np.number) \
            .dropna(axis=1, how='all')

        dff = df.dropna(subset=[xaxis_column_name, yaxis_column_name])
        # Construcción del modelo
        slope, intercept, r_value, p_value, std_err = stats.linregress(dff[xaxis_column_name], dff[yaxis_column_name])
        line = slope * dff[xaxis_column_name] + intercept
        return {
            'data': [dict(
                x=dff[xaxis_column_name],
                y=dff[yaxis_column_name],
                mode='markers',
                marker={
                    'size': 7,
                    'opacity': 0.6,
                    'color': 'black'
                },
                hoverinfo='x+y'
            ), dict(
                x=dff[xaxis_column_name],
                y=line, mode='lines',
                marker={
                    'size': 9,
                    'opacity': 0.8,
                    'color': 'grey'},
                hoverinfo="x+y+text")],
            'layout': dict(
                xaxis={
                    'title': input_2
                },
                yaxis={
                    'title': input_1
                },
                margin={'l': 60, 'b': 40, 't': 10, 'r': 10},
                hovermode='closest',
                annotations=[
                    dict(
                        x=dff[xaxis_column_name].max()-(dff[xaxis_column_name].max()*0.1),
                        y=dff[yaxis_column_name].max()-(dff[yaxis_column_name].max()*0.1),
                        text='<b>R^2 = '+str(round(r_value**2, 4))+', p-value = '+str(round(p_value, 4))+'<b>',
                        showarrow=False),
                    dict(
                        x=dff[xaxis_column_name].max()-(dff[xaxis_column_name].max()*0.12),
                        y=dff[yaxis_column_name].max()-(dff[yaxis_column_name].max()*0.15),
                        text='<b>Y = ' + str(round(intercept, 4)) + ' + ' + str(round(slope, 4)) + 'X<b>',
                        showarrow=False)
                ],
                showlegend=False
            )
        }


@app.callback(
    Output("base-title", 'children'),
    [Input("upload-data", 'filename')])
def update_graph(filename):
    if filename is None:
        raise PreventUpdate
    else:
        if 'csv' in filename:
            return str(filename).replace(".csv",'')
        elif 'xls' in filename:
            return str(filename).replace(".xls|.xlsx", '')