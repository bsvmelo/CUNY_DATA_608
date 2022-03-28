# Run this app with `python app.py` and
# visit http://127.0.0.1:8050/ in your web browser.

from dash import Dash, dcc, html, Input, Output, dash_table
import plotly.express as px
import pandas as pd
import numpy as np

app = Dash(__name__)

colors = {
    'background': 'white',
    'text': '#00008B'
}

colors1 = {
    'text': '#00008B'
}

# assume you have a "long-form" data frame
# see https://plotly.com/python/px-arguments/ for more options
url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json'
#trees = pd.read_json(url)



# number of trees per species - only Alive trees
soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,status, count(tree_id)' +\
        #'&$where=status=\'Alive\'' +\
        '&$group=spc_common,status').replace(' ', '%20')
df_spc = pd.read_json(soql_url)
df_spc=df_spc[df_spc.status == 'Alive']
df_spc=df_spc.drop(['status'],axis=1)
df_spc= df_spc.dropna()



# health per species per  boro
# i could have used a function here but was running out of time, so decided to do on brute force
#boro = 'Bronx'
soql_url_1 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,status,count(tree_id)' +\
        '&$where=boroname=\'Bronx\'' +\
        '&$group=spc_common,health,boroname,steward,status').replace(' ', '%20')
df_trees1 = pd.read_json(soql_url_1)

soql_url_2 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,status,count(tree_id)' +\
        '&$where=boroname=\'Brooklyn\'' +\
        '&$group=spc_common,health,boroname,steward,status').replace(' ', '%20')
df_trees2 = pd.read_json(soql_url_2)

soql_url_3 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,status,count(tree_id)' +\
        '&$where=boroname=\'Manhattan\'' +\
        '&$group=spc_common,health,boroname,steward,status').replace(' ', '%20')
df_trees3 = pd.read_json(soql_url_3)

soql_url_4 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,status,count(tree_id)' +\
        '&$where=boroname=\'Queens\'' +\
        '&$group=spc_common,health,boroname,steward,status').replace(' ', '%20')
df_trees4 = pd.read_json(soql_url_4)

soql_url_5 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,status,count(tree_id)' +\
        '&$where=boroname=\'Staten Island\'' +\
        '&$group=spc_common,health,boroname,steward,status').replace(' ', '%20')
df_trees5 = pd.read_json(soql_url_5)


df=pd.DataFrame().append([df_trees1,df_trees2,df_trees3,df_trees4,df_trees5])
df=df[df.status == 'Alive']
df=df.drop(['status'],axis=1)
# dropping NAS
df= df.dropna()

# merging to calculate proportion by species by health by steward
df = df.merge(df_spc, on=['spc_common'], how='left')

#aggregating excluding stewards
grouped = df.groupby(['spc_common', 'boroname','health','count_tree_id_y']).sum().reset_index()
grouped['prop'] = round((grouped['count_tree_id_x']/grouped['count_tree_id_y'])*100,2)

#aggregating including stewards
grouped_st = df.drop(['spc_common','boroname','count_tree_id_y'], axis=1)
grouped_st = grouped_st.groupby(['steward','health']).sum().reset_index()
total_trees = grouped_st.count_tree_id_x.sum()
grouped_st['Proportion in %'] = round((grouped_st['count_tree_id_x']/total_trees)*100,2)
grouped_st.drop(['count_tree_id_x'],axis=1,inplace=True)
# grouped_st['steward'] = df['steward'].astype('category')    
# grouped_st['health'] = df['health'].astype('category') 

#heat_map
# heat_map = px.imshow(grouped_st,color_continuous_scale='RdBu_r', origin='lower')
#heat_map =px.density_heatmap(grouped_st, x="health", y="steward",color_continuous_scale='RdBu_r')

#aggregating including stewards
#grouped_2 = df.drop(['spc_common','boroname','count_tree_id_y'], axis=1)
# grouped_2 = df.groupby(['spc_common','boroname','steward','health','count_tree_id_y']).sum().reset_index()
# total_trees = grouped_st.count_tree_id_x.sum()
# grouped_st['prop'] = round((grouped_st['count_tree_id_x']/total_trees)*100,2)

def generate_table(dataframe, max_rows=15):
    return html.Table([
        html.Thead(
            html.Tr([html.Th(col) for col in dataframe.columns])
        ),
        html.Tbody([
            html.Tr([
                html.Td(dataframe.iloc[i][col]) for col in dataframe.columns
            ]) for i in range(min(len(dataframe), max_rows))
        ])
    ])

app.layout = html.Div([
    html.H3(
        children='Data 608 - Spring 2022',
        style={
            'textAlign': 'left',
            'color': colors1['text'],
            'font-family':'colibri',
            'fontSize':40
            
        }

    ),
        html.Div(children='Assignment 4: Tree Census in NYC', style={
        'textAlign': 'left',
        'color': colors1['text']
    }),
    
    html.Div(children='Question 1: Choose a Borough and a Tree Species:', style={
        'textAlign': 'left',
        'color': colors1['text']
    }),    
    
    html.Div([

        html.Div([
            dcc.Dropdown(
                df['boroname'].unique(),
                'Bronx',
                id='xaxis-column'
            ),

        ], style={'width': '48%', 'display': 'inline-block'}),

        html.Div([
            dcc.Dropdown(
                df['spc_common'].unique(),
                'American beech',
                id='yaxis-column'
            ),

        ], style={'width': '48%', 'float': 'center', 'display': 'inline-block'}),
    ])  ,  
    dcc.Graph(id='health-bar'),
    # dcc.Graph(id='heatmap',figure=heat_map)
  
    
    html.H4(children='Question 2: Are stewards having an impact on the health of trees:'),
    html.H4(children='In general, as displayed in the table below, highest proportion of trees in good health have no care at all.'),
    html.H4(children='When stewards are caring for the trees, it looks like less care is better.'),
    html.H4(children='I will enhance this visualization to include interactivity, so that proportions are displayed by Borough and Tree Species.'),
    generate_table(grouped_st)   
    
   
    
])   

@app.callback(
    Output('health-bar', 'figure'),
    Input('xaxis-column', 'value'),
    Input('yaxis-column', 'value'))
    # Input('xaxis-type', 'value'),
    # Input('yaxis-type', 'value'),
    # Input('year--slider', 'value'))
def update_graph(boro_name, spc_name):
    
    dff = grouped[(grouped['boroname'] == boro_name) & (grouped['spc_common'] == spc_name)]
    
    boro = boro_name
    spc = spc_name
        
    fig = px.bar(dff, x='health',y='prop',color = 'health', title= "Health of " f'{spc}' " trees in location: " f'{boro}')

    # fig = px.scatter(x=dff[dff['Indicator Name'] == xaxis_column_name]['Value'],
    #                  y=dff[dff['Indicator Name'] == yaxis_column_name]['Value'],
    #                  hover_name=dff[dff['Indicator Name'] == yaxis_column_name]['Country Name'])

   # fig.update_layout( hovermode='closest')
# margin={'l': 30, 'b': 30, 't': 10, 'r': 0},
    fig.update_xaxes(title="Tree Health")
                     # type='linear' if xaxis_type == 'Linear' else 'log')

    fig.update_yaxes(title="Proportion in %", automargin=True)
                     # type='linear' if yaxis_type == 'Linear' else 'log')
    # Proportion in % of Total Alive Species
    fig.update_layout(hovermode='closest',showlegend=False,
    plot_bgcolor=colors['background'],
    paper_bgcolor=colors['background'],
    font_color=colors['text'], xaxis={'categoryorder':'total descending'})
   
    return fig




if __name__ == '__main__':
    app.run_server(debug=True)

