import graphviz

def draw_node_and_edges(*args, **kwargs):
    nodes = args[0]
    edges = args[1]   

    graph = graphviz.Digraph('karis program', 
    comment='Karis abstract syntax tree visualization', 
    engine='sfdp', node_attr={'color': 'lightblue2', 'style': 'filled',
    'shape': 'record', 'height': '.9','width': '.5'})    

    for node in nodes:
        if node[1] == "NODE(PROGRAM)":
            graph.node(node[0],node[1], style='filled', fillcolor='#36eee0', shape='circle')
        elif node[1] == "NODE(ASSIGN)":
            graph.node(node[0],node[1], style='filled', fillcolor='#f652a0')
        elif node[1] == "NODE(PLUS)" or node[1] == "NODE(MINUS)" \
            or node[1] == "NODE(ASTERISK)" or node[1] == "NODE(SLASH)":
            graph.node(node[0],node[1], style='filled', fillcolor='#fbc740')
        elif "NODE(LET" in node[1]:
            graph.node(node[0],node[1], style='filled', fillcolor='#bd97cb')
        else:
            graph.node(node[0],node[1])

    for edge in edges:
        graph.edge(edge[0], edge[1])

    g = graph.unflatten(stagger=5)
    g.format = 'png' 
    g.render(directory='render').replace('\\', '/')
    