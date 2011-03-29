# Concepts

* _thunk_

    A procedure that takes no arguments.

* _cc-cps-thunk_

    A thunk in closure-converted continuation-passing style. Whenever such a procedure gets to a random choice, it returns the current continuation, which is in hashable form due to cc.

* _return-thunk_

    A cc-cps-thunk that has also undergone app conversion: All applications return to the top-level and pass function, continuation, and arguments back (all of which are hashable due to cc).

* _graph_

    An acyclic graph for a probabilistic program, with one node for each random choice (as identified by its continuation and support). Corresponds to a system of linear equations.

* _polygraph_

    A graph for a probabilistic program that makes subproblems explicit. For each subproblem, there is a parentless node, and references to the marginal probabilities of a subproblem are possible. Cyclic dependencies are only introduced via such references. Corresponds to a system of polynomial equations. 

* _polymap_

    A summary graph of the dependency structure of a polygraph. Contains a node for each subproblem (root node). Whenever a subproblem A references another subproblem B, there is a link from A to B in the polymap. In general, this graph is not acyclic.

* _components_

    Clustering the strongly connected components of a polymap results in the acyclic components graph. Each component corresponds to a (linear or polynomial) problem that can be solved independently given the referenced parent parameters. By solving the components in topological order, the marginal distribution of the overall inference problem can be computed.
