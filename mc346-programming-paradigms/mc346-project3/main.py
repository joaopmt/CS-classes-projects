# Artur Lima RA:166916
# João Pedro Martins RA:176117

from math import inf
from itertools import product, takewhile, dropwhile
import sys


def get_nodes(graph):
    """ returns list of nodes in graph
    """
    nodes = set()
    for v in graph:
        nodes.add(v)
        for u, w in graph[v]:
            nodes.add(u)
    return nodes


def floyd_warshall(graph, n):
    """ builds a path dictionary of given graph where
    the keys are pairs of nodes and the values are
    the path list and the distance between each pair
    """
    path_dict = {}
    rn = range(n)
    dist = [[inf] * n for i in rn]
    nxt  = [[0]   * n for i in rn]
    for i in rn:
        dist[i][i] = 0
    for v in graph:
        for u, w in graph[v]:
            dist[v][u] = w
            nxt[v][u] = u
    nodes = get_nodes(graph)
    for k, i, j in product(nodes, repeat=3):
        sum_ik_kj = dist[i][k] + dist[k][j]
        if dist[i][j] > sum_ik_kj:
            dist[i][j] = sum_ik_kj
            nxt[i][j]  = nxt[i][k]
    for i, j in product(nodes, repeat=2):
        if i != j and dist[i][j] != inf:
            path = [i]
            while path[-1] != j:
                path.append(nxt[path[-1]][j])
            path_dict[(i,j)] = (path,dist[i][j])
    return path_dict


def main():
    graph = {}
    max_v = 0;
    lines = open(sys.argv[1]).read().splitlines()
    graph_input = takewhile(lambda s: s!="", lines)
    passenger_input = dropwhile(lambda s: s!="", lines)
    next(passenger_input)
    for line in graph_input:
        v, u, w = line.split()
        v, u = int(v), int(u)
        if max(int(v), int(u)) > max_v:
            max_v = max(int(v), int(u))
        if v in graph:
            graph[v].append((u, float(w)))
        else:
            graph[v] = [(u, float(w))]
    path_dict = floyd_warshall(graph, max_v+1)
    #Lista que será preenchida com todos os passageiros, com seus respectivos vértices iniciais, finais e, caso tenham, vertices do meio (caso não tenham, recebe -1)
    passengers = []
    i = 1
    for line in passenger_input:
        if len(line.split()) == 2:
            ev, fv = line.split()
            passengers.append((i, int(ev), int(fv), -1))
            i += 1
        else:
            ev, fv, mv = line.split()
            passengers.append((i,int(ev), int(fv), int(mv)))
            i+=1
    i-=1

    #Chamada para tentar formar par entre todos os passageiros
    j = 1
    pair_dict = {}
    while j < i:
        pair(j, i, path_dict, passengers, pair_dict)
        j +=1
    #Recebe os passageiros que ainda não começaram uma viagem
    #Aqui temos q fazer o refinamento dos caminhos para imprimi-los
    finished = []
    while pair_dict != {}: #enquanto o dicionário tiver algum par, nós verificamos se ele pode ser impresso
        s = min(pair_dict.items(), key=lambda x: x[1][1]) #Pegamos o par com menor incompatibilidade
        if s[0][0] and s[0][1] not in finished: #Se nenhum dos dois passageiros desse par tiverem sido alocados em um uber, podemos alocar eles agora
            print("passageiros:", s[0][0], s[0][1], "percurso:", s[1][0][0], s[1][0][1], s[1][0][2], s[1][0][3]) #imprime os passageiros e sua rota
            finished.append(s[0][0]) #Colocamos os 2 passageiros na lista de quem já pegou o uber
            finished.append(s[0][1])
            pair_dict.pop((s[0][0],s[0][1])) #tiramos a tupla do dicionario
        else:
            pair_dict.pop((s[0][0],s[0][1])) #Caso eles não possam ser alocados, tiramos a tupla do dicionario mesmo assim

    #Agora temos que imprimir os passageiros que sobraram sozinhos
    k = 1
    while k <= i:
        if k not in finished:
            if passengers [k-1][3] == -1 and (passengers[k-1][1], passengers[k-1][2]) in path_dict: #Se o passageiro ainda não tiver em viagem
                print("passageiro:", k, "percurso:", passengers[k-1][1], passengers[k-1][2])
            elif passengers [k-1][3] != -1 and (passengers[k-1][3], passengers[k-1][2]) in path_dict: #Se o passageiro já tiver em viagem
                print("passageiro:", k, "percurso:", passengers[k-1][3], passengers[k-1][2])
            else: #Caso, por algum motivo, não tem como o uber chegar no destino final (e o passageiro falou o destino sem saber disso), podemos imprimir que não há a possibilidade de chegar nesse destino
                print("Passageiro", k, "não pode chegar até o destino final")
        k += 1
    #imprimir os que não dividem o uber

#Aqui verificamos as possíveis duplas de passageiros (desconsideramos o caso onde 2 passageiros estão já em viagem, crendo q seja inviável a pessoa descer de um uber onde o outro está enquanto o outro tem q esperar a chegada desse uber no local atual dele) 
def pair(j, i, graph, passengers, pair_dict):
    for h in passengers:
        if h[0] == j :
            for sec in passengers:
                if(sec>h):
                    if (h[3] == -1 and sec[3] == -1): #Caso os dois sejam viagens não iniciadas
                        trip1(h, sec, graph, pair_dict)
                        trip1(sec, h, graph, pair_dict)
                    elif (sec[3] != -1 and h[3] == -1): #Caso o segundo seja uma viagem em andamento
                        trip2(sec, h, graph, pair_dict)
                    elif (h[3] != -1 and sec[3] == -1): #Caso o primeiro seja uma viagem em andamento
                        trip2(h, sec, graph, pair_dict) 


#Aqui iremos verificar se os caminhos compartilhados existem, se existirem colocamos em alguma estrutura seus valores para depois calcular a inconveniencia
def trip1(first, second, graph, pair_dict):
    ##Se existe um caminho de E1 a E2 e de E2 a F1 e de F1 a F2 podemos usar ele
    if (first[1], second[1]) in graph and (second[1], first[2]) in graph and (first[2], second[2]) in graph:
        incp1 = (graph[(first[1], second[1])][1] + graph[(second[1], first[2])][1])/graph[(first[1], first[2])][1]
        incp2 = (graph[(second[1], first[2])][1] + graph[(first[2], second[2])][1])/graph[(second[1], second[2])][1]
        x = [first[1], second[1], first[2], second[2]]
        if incp1 <= 1.4 and incp2 <= 1.4: #se a incompatibilidade maxima for menor que 1.4, poderemos adicionar
            m = max(incp1, incp2)
            z = (x,m)
            pair_dict[(first[0], second[0])] = (x,m)

    ##Se existe um caminho de E1 a E2 e de E2 a F2 e de F2 a F1 podemos usar ele 
    if (first[1], second[1]) in graph and (second[1], second[2]) in graph and (second[2], first[2]) in graph:
        incp3 = (graph[(first[1], second[1])][1] + graph[(second[1], second[2])][1] + graph[(second[2], first[2])][1])/graph[(first[1], first[2])][1]
        incp4 = graph[(second[1], second[2])][1]/graph[(second[1], second[2])][1]
        x = [first[1], second[1], second[2], first[2]]
        if incp3 <= 1.4 and incp4 <= 1.4: #se a incompatibilidade maxima for menor que 1.4, podemos adicionar
            m2 = max(incp3, incp4)
            a = (x,m2)
            pair_dict[(first[0], second[0])] = (x,m2)
    
    ## Se os dois casos podem ocorrer, vamos ter q optar pelo de menor incompatibilidade maxima
    if (first[1], second[1]) in graph and (second[1], first[2]) in graph and (first[2], second[2]) in graph and (second[1], second[2]) in graph and (second[2], first[2]) in graph:
        if incp1 <= 1.4 and incp2 <= 1.4 and incp3 <= 1.4 and incp4 <= 1.4:
            if m < m2:
                pair_dict[(first[0], second[0])] = z
            else:
                pair_dict[(first[0], second[0])] = a    
## Verificar incompatibilidade no trip 2 (Depois)
#Aqui verificamos se o passageiro em viagem (primeiro) pode chegar ate o segundo e fazer o caminho total
def trip2(first, second, graph, pair_dict):
    ##Se existe um caminho de M1 a E2 e de E2 a F1 e de F1 a F2 podemos usar ele
    if (first[3], second[1]) in graph and (second[1], first[2]) in graph and (first[2], second[2]) in graph:
        incp1 = (graph[(first[1], second[1])][1] + graph[(second[1], first[2])][1])/graph[(first[1], first[2])][1]
        incp2 = (graph[(second[1], first[2])][1] + graph[(first[2], second[2])][1])/graph[(second[1], second[2])][1]
        x = [first[3], second[1], first[2], second[2]] 
        if incp1 <= 1.4 and incp2 <= 1.4: #se a incompatibilidade maxima for menor que 1.4, podemos adicionar
            m = max(incp1, incp2)
            z = (x,m)
            pair_dict[(first[0], second[0])] = (x,m)
   
    ##Se existe um caminho de M1 a E2 e de E2 a F2 e de F2 a F1 podemos usar ele
    if (first[3], second[1]) in graph and (second[1], second[2]) in graph and (second[2], first[2]) in graph:
        incp3 = (graph[(first[1], second[1])][1] + graph[(second[1], second[2])][1] + graph[(second[2], first[2])][1])/graph[(first[1], first[2])][1]
        incp4 = graph[(second[1], second[2])][1]/graph[(second[1], second[2])][1]
        x = [first[3], second[1], second[2], first[2]]
        if incp3 <= 1.4 and incp4 <= 1.4: #se a incompatibilidade maxima for menor que 1.4, podemos adicionar
            m2 = max(incp3, incp4)
            a = (x,m2)
            pair_dict[(first[0], second[0])] = (x,m2)

    ## Se os dois casos podem ocorrer, vamos ter q optar pelo de menor incompatibilidade maxima
    if (first[3], second[1]) in graph and (second[1], first[2]) in graph and (first[2], second[2]) in graph and (second[1], second[2]) in graph and (second[2], first[2]) in graph:
        if incp1 <= 1.4 and incp2 <= 1.4 and incp3 <= 1.4 and incp4 <= 1.4:
            if m < m2:
                pair_dict[(first[0], second[0])] = z
            else:
                pair_dict[(first[0], second[0])] = a

main()
