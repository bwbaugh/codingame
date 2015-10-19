def main():
    (
        num_nodes,
        num_edges,
        num_exit_gateways,
    ) = [int(i) for i in raw_input().split()]
    edge_set = set(
        tuple(int(x) for x in raw_input().split())
        for __ in xrange(num_edges)
    )
    gateway_nodes = set(int(raw_input()) for __ in xrange(num_exit_gateways))
    gateway_edge_map = _make_gateway_edge_map(
        edge_set=edge_set,
        gateway_nodes=gateway_nodes,
    )

    while True:
        # The index of the node on which the Skynet agent is positioned.
        agent_index = int(raw_input())
        edge = pick_edge(
            edge_set=edge_set,
            agent_index=agent_index,
            gateway_nodes=gateway_nodes,
            gateway_edge_map=gateway_edge_map,
        )
        edge_set.remove(edge)
        update_gateway_edge_map(
            gateway_edge_map=gateway_edge_map,
            removed_edge=edge,
        )
        print ' '.join(str(x) for x in edge)


def _make_gateway_edge_map(edge_set, gateway_nodes):
    """Mapping of gateway index to the edges that connect to it.

    Useful to keep track of when a gateway can no longer be connected.
    """
    gateway_edge_map = {}
    for index in gateway_nodes:
        for edge in edge_set:
            if index not in edge:
                continue
            gateway_edge_map.setdefault(index, set()).add(edge)
    return gateway_edge_map


def pick_edge(edge_set, agent_index, gateway_nodes, gateway_edge_map):
    # Heuristic: Sever gateway edge if agent on it.
    gateway_agent_edge = get_gateway_agent_edge(
        edge_set=edge_set,
        agent_index=agent_index,
        gateway_nodes=gateway_nodes,
    )
    if gateway_agent_edge:
        return gateway_agent_edge

    node_set = _make_node_set(edge_set=edge_set)
    shortest_path = dijkstra(
        edge_set=edge_set,
        node_set=node_set,
        source=agent_index,
    )

    # Heuristic: Remove edge from closest neighbor to a gateway with a
    #   gateway-degree of greater than one.
    gateway_degree_map = count_gateway_degree(
        gateway_edge_map=gateway_edge_map
    )
    gateway_degree_map = {
        node: degree
        for node, degree in gateway_degree_map.iteritems()
        if degree > 1
    }
    for index in sorted(
        shortest_path['distance_map'],
        key=shortest_path['distance_map'].get,
    ):
        if index not in gateway_degree_map:
            continue
        for target_gateway, path_node in (
            shortest_path['previous_node_map'].iteritems()
        ):
            if target_gateway not in gateway_nodes:
                continue
            if path_node != index:
                continue
            return _find_edge(u=index, v=target_gateway, edge_set=edge_set)

    # Heuristic: Remove arbitrary edge from gateway with shortest path.
    # Filter so that only gateway nodes remain.
    for index in node_set.difference(gateway_nodes):
        del shortest_path['distance_map'][index]
    # The index of the gateway node with the shortest distance to the agent.
    target_gateway = min(
        shortest_path['distance_map'],
        key=shortest_path['distance_map'].get,
    )
    target_neighbor = shortest_path['previous_node_map'][target_gateway]
    return _find_edge(u=target_gateway, v=target_neighbor, edge_set=edge_set)


def get_gateway_agent_edge(gateway_nodes, edge_set, agent_index):
    for index in gateway_nodes:
        for edge in edge_set:
            if index in edge and agent_index in edge:
                return edge
    return None


def _make_node_set(edge_set):
    return set(index for edge in edge_set for index in edge)


def dijkstra(edge_set, node_set, source):
    """Based on <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>."""
    unvisited_nodes = node_set.copy()
    distance_map = {}
    # Previous node in optimal path from source.
    previous_node_map = {}
    for node in node_set:
        distance_map[node] = float('inf')
        previous_node_map[node] = None
    distance_map[source] = 0

    while unvisited_nodes:
        node = min(unvisited_nodes, key=distance_map.get)
        unvisited_nodes.remove(node)
        for neighbor in _find_neighbors(node=node, edge_set=edge_set):
            if neighbor not in unvisited_nodes:
                continue
            # Using a constant of `1` where `1` is `weight(node, neighbor)`.
            alternative_distance = distance_map[node] + 1
            if alternative_distance < distance_map[neighbor]:
                distance_map[neighbor] = alternative_distance
                previous_node_map[neighbor] = node

    return {
        'distance_map': distance_map,
        'previous_node_map': previous_node_map,
    }


def _find_neighbors(node, edge_set):
    neighbor_set = set()
    for edge in edge_set:
        if node not in edge:
            continue
        neighbor = _other_node_in_edge(edge=edge, node=node)
        if neighbor in neighbor_set:
            # XXX: Don't know if this is needed or not. Playing it safe.
            continue
        yield neighbor
        neighbor_set.add(neighbor)


def _other_node_in_edge(edge, node):
    """Given an edge, return the other node other than ``node``."""
    edge = list(edge)
    edge.remove(node)
    return edge[0]


def count_gateway_degree(gateway_edge_map):
    """Counts of how many gateways a node is connected to."""
    gateway_degree_map = {}
    for gateway, edge_set in gateway_edge_map.iteritems():
        for edge in edge_set:
            node = _other_node_in_edge(edge=edge, node=gateway)
            gateway_degree_map[node] = gateway_degree_map.get(node, 0) + 1
    return gateway_degree_map


def update_gateway_edge_map(gateway_edge_map, removed_edge):
    """Removes the edge from the edge map, removing gateways with no edges."""
    gateways_to_remove = set()
    for index, edge_set in gateway_edge_map.iteritems():
        if removed_edge not in edge_set:
            continue
        edge_set.remove(removed_edge)
        if not edge_set:
            gateways_to_remove.add(index)
    for index in gateways_to_remove:
        del gateway_edge_map[index]


def _find_edge(u, v, edge_set):
    for edge in edge_set:
        if u not in edge:
            continue
        if v not in edge:
            continue
        return edge


if __name__ == '__main__':
    main()
