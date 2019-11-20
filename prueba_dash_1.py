import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import pandas as pd
import plotly.express as px
import chart_studio.plotly as py
import plotly.graph_objs as go
from numpy import arange,array,ones
from scipy import stats

# Lectura de la base
bdf = pd.read_spss("C:/Users/IOR_C/Downloads/Base de datos FGF21 final.sav")
bdf.dropna(subset=['ACTANTIOX_OFICIAL', 'FGF21_OFICIAL'], inplace=True)

# Construcción del modelo

slope, intercept, r_value, p_value, std_err = stats.linregress(bdf["ACTANTIOX_OFICIAL"],bdf["FGF21_OFICIAL"])
line = slope*bdf["ACTANTIOX_OFICIAL"]+intercept

# Construcción de la gráfica

fig = go.Figure()
fig.add_scatter(x=bdf["ACTANTIOX_OFICIAL"], y=bdf["FGF21_OFICIAL"], mode = 'markers')
fig.add_scatter(x=bdf["ACTANTIOX_OFICIAL"],y=line,mode='lines', marker_color='rgb(31, 119, 180)')

fig.add_annotation(
    go.layout.Annotation(
        x=1000,
        y=3000,
        text='$R^2 = '+str(round(r_value**2,4))+' p value = '+str(round(p_value,4))+', '+'Y = '+str(round(intercept,4))+' + '+str(round(slope,4))+'X$',
        showarrow=False,
        font_size = 18))
fig.update_layout(title = 'Linear Fit in Python', showlegend = False)
fig.show()
