import dash
import dash_bootstrap_components as dbc

FONT_AWESOME = "https://use.fontawesome.com/releases/v5.7.2/css/all.css"
external_stylesheets = [dbc.themes.LUMEN, FONT_AWESOME]

app = dash.Dash(__name__, external_stylesheets=external_stylesheets,
                meta_tags=[
                    {"name": "viewport", "content": "width=device-width, initial-scale=1"}
                ]
                )

server = app.server
app.config.suppress_callback_exceptions = True