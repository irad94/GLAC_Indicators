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

# df = pd.read_spss("C:/Users/GLAC/Downloads/Base de datos FGF21 final.sav")

navbar = dbc.NavbarSimple(
    children=[
        dbc.NavItem(dbc.NavLink("Link", href="#")),
        dbc.DropdownMenu(
            nav=True,
            in_navbar=True,
            label="Menu",
            children=[
                dbc.DropdownMenuItem("Entry 1"),
                dbc.DropdownMenuItem("Entry 2"),
                dbc.DropdownMenuItem(divider=True),
                dbc.DropdownMenuItem("Entry 3"),
            ],
        ),
    ],
    brand="PLATAFORMA DE ANÁLISIS REGRESIONAL UNIVARIADO",
    brand_href="#",
    sticky="top",
    color="primary",
    dark=True
)

body = dbc.Container(
    [
        dbc.Row(
            [html.H1(id="base-title", style={'font-size': '400%'})]),
        dbc.Row([
                html.Div([
                    html.P([html.Strong('Seleccione la variable dependiente', style={'font-size': '120%'})]),
                    dcc.Dropdown(
                        id='yaxis-column',
                        value='FGF21_OFICIAL'
                    )
                ],
                    style={'width': '50%', 'display': 'inline-block'}),

                html.Div([
                    html.P([html.Strong('Seleccione la variable independiente', style={'font-size': '120%'})]),
                    dcc.Dropdown(
                        id='xaxis-column',
                        value='ACTANTIOX_OFICIAL'
                    )
                ], style={'width': '50%', 'float': 'right', 'display': 'inline-block'})]),
        dbc.Row([
            html.P("")]),
        dbc.Row([
            html.Div([
                dcc.Graph(id='indicator-graphic')])],
                style={'float': 'right'}),
        dbc.Row([
            html.P("")]),
        dbc.Row([
            html.P("Introduzca el nombre del eje de las ordenadas",
                   style={'font-size': '120%', 'margin-top': '15px', 'margin-bottom': '15px'})]),
        dbc.Row([
            dcc.Input(id='input-1-state', type='text', value='FGF21_OFICIAL', size='65')]),
        dbc.Row([
            html.P("Introduzca el nombre del eje de las abscisas",
                   style={'font-size': '120%', 'margin-top': '15px', 'margin-bottom': '15px'})]),
        dbc.Row([
            dcc.Input(id='input-2-state', type='text', value='ACTANTIOX_OFICIAL', size='65')]),
        dbc.Row([
            html.Hr(style={'width': '97%', 'margin': '0', 'margin-top': '15px', 'margin-bottom': '15px'})]),
        dbc.Row([
            dbc.Button('ACTUALIZAR EJES', id='btn-1', color="primary", className="mr-2", size="lg", block=True),
        ]),
        dbc.Row([html.Div([
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
                    'margin-left': '25px',
                    'margin-right': '140px',
                    'margin-top': '50px',
                    'font-size': '115%',
                },
                # Allow multiple files to be uploaded
                multiple=True
            )
        ])])
    ],
    className="mt-4",
)
external_stylesheets = [dbc.themes.LUMEN]

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

app.layout = html.Div([navbar, body])


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
    content_type, content_string = contents.split(',')

    decoded = base64.b64decode(content_string)
    try:
        if 'csv' in filename:
            # Assume that the user uploaded a CSV file
            df = pd.read_csv(
                io.StringIO(decoded.decode('utf-8')))
        elif 'sav' in filename:
            # Assume that the user uploaded an excel file
            df = pd.read_spss(decoded)
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
            annotations=[dict(
                x=dff[xaxis_column_name].max()-(dff[xaxis_column_name].max()*0.1),
                y=dff[yaxis_column_name].max()-(dff[yaxis_column_name].max()*0.1),
                text='<b>R^2 = '+str(round(r_value**2, 4))+', p-value = '+str(round(p_value, 4))+'<b>',
                showarrow=False,
                font_size=10,
                font_color="grey"
            ), dict(
                x=dff[xaxis_column_name].max()-(dff[xaxis_column_name].max()*0.12),
                y=dff[yaxis_column_name].max()-(dff[yaxis_column_name].max()*0.15),
                text='<b>Y = ' + str(round(intercept, 4)) + ' + ' + str(round(slope, 4)) + 'X<b>',
                showarrow=False,
                font_size=16,
                font_color="grey"
            )],
            showlegend=False
        )
    }


@app.callback(
    Output("base-title", 'children'),
    [Input("upload-data", 'filename')])
def update_graph(filename):
    return filename


@app.callback(
    [Output('yaxis-column', 'options'),
     Output('xaxis-column', 'options')],
    [Input("upload-data", 'contents'),
     Input('upload-data', 'filename')])
def update_graph(contents, filename):
    content_type, content_string = contents.split(',')

    decoded = base64.b64decode(content_string)
    try:
        if 'csv' in filename:
            # Assume that the user uploaded a CSV file
            df = pd.read_csv(
                io.StringIO(decoded.decode('utf-8')))
        elif 'sav' in filename:
            # Assume that the user uploaded an excel file
            df = pd.read_spss(decoded)
    except Exception as e:
        print(e)
        return html.Div([
            'There was an error processing this file.'
        ])

    df = df.select_dtypes(include=np.number) \
        .dropna(axis=1, how='all')

    available_indicators = df.columns

    return [{'label': i, 'value': i} for i in available_indicators], [{'label': i, 'value': i} for i in available_indicators]


if __name__ == '__main__':
    app.run_server(debug=True)
