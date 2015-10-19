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

    while True:
        # The index of the node on which the Skynet agent is positioned.
        agent_index = int(raw_input())
        edge = pick_edge(
            edge_set=edge_set,
            agent_index=agent_index,
            gateway_nodes=gateway_nodes,
        )
        edge_set.remove(edge)
        print ' '.join(str(x) for x in edge)


def pick_edge(edge_set, agent_index, gateway_nodes):
    # Ideas:
    #   Find shortest path to closest node.
    #   Find how to partition the graph.

    # Heuristic: Sever gateway edge if agent on it.
    for index in gateway_nodes:
        for edge in edge_set:
            if index in edge and agent_index in edge:
                return edge

    # Naive heuristic: sever edges or each gateway node,
    #   ignoring where the agent is for now.
    for index in gateway_nodes:
        for edge in edge_set:
            if index in edge:
                return edge


if __name__ == '__main__':
    main()
