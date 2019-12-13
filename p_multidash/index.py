import dash_core_components as dcc
import dash_html_components as html
import dash_bootstrap_components as dbc
from dash.dependencies import Input, Output, State

from app import app
from apps import app1, app2

# the style arguments for the sidebar. We use position:fixed and a fixed width
SIDEBAR_STYLE = {
    "background-color": "#f8f9fa",
    'display': 'flex',
    'min-width': '250px',
    'max-width': '250px',
    'min-height': '100vh'
}

# the styles for the main content position it to the right of the sidebar and
# add some padding.

sidebar = dbc.Collapse(
    dbc.Nav([
        html.Div([html.H2("Sidebar", className="display-4"),
                  html.Hr()]),
        html.Div([html.Ul(html.P("A simple sidebar layout with navigation links", className="lead"))]),
        dbc.NavLink("Page 1", href="/apps/app1", id="page-1-link"),
        dbc.NavLink("Page 2", href="/apps/app2", id="page-2-link")],
        vertical=True,
        pills=True,
        style=SIDEBAR_STYLE,
        id="sidebar"
    ),
    id="left-collapse")

navbar = dbc.Navbar(
    children=[
        html.A(
            dbc.Row([
                dbc.Col(dbc.Button(html.Span([html.I(className="fas fa-align-justify")]),
                                   id="left", color="light", outline=True, size='lg'), md=1),
                dbc.Col(dbc.NavbarBrand("PLATAFORMA DE ANÁLISIS REGRESIONAL UNIVARIADO",
                                        className="ml-2",
                                        style={'font-size': '200%'}), md=7)
            ],
                align="center",
                no_gutters=False
            ),
            href="#",
        )
    ],
    sticky="top",
    color="primary",
    dark=True,
    id="navbar",
    style={"padding": "1.5rem"})

content = html.Div(id="page-content")

app.layout = html.Div([dcc.Location(id="url", refresh=False),
                       sidebar,
                       html.Div([navbar, content],
                                style={'width': '100%'}
                                )],
                      style={'display': 'flex',
                             'width': '100%',
                             'align-items': 'stretch'})


@app.callback(
    [Output(f"page-{i}-link", "active") for i in range(1, 3)],
    [Input("url", "pathname")],
)
def toggle_active_links(pathname):
    if pathname == "/":
        # Treat page 1 as the homepage / index
        return True, False
    return [pathname == f"/apps/app{i}" for i in range(1, 3)]


@app.callback(Output("page-content", "children"), [Input("url", "pathname")])
def render_page_content(pathname):
    if pathname in ["/", '/apps/app1']:
        return app1.layout
    elif pathname == '/apps/app2':
        return app2.layout
    # If the user tries to reach a different page, return a 404 message
    return dbc.Jumbotron(
        [
            html.H1("404: Not found", className="text-danger"),
            html.Hr(),
            html.P(f"The pathname {pathname} was not recognised..."),
        ]
    )


@app.callback(
    Output("left-collapse", "is_open"),
    [Input("left", "n_clicks")],
    [State("left-collapse", "is_open")],
)
def toggle_left(n_left, is_open):
    if n_left:
        return not is_open
    return is_open


if __name__ == '__main__':
    app.run_server(debug=True)
