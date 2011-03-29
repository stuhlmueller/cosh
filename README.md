# Cosh

Cosh is an experimental Church implementation that uses dynamic programming based on continuation hashing.

## Installation

This installation assumes that you have [git](http://git-scm.com/) and a R6RS Scheme installed. The [instructions on the Church wiki](http://projects.csail.mit.edu/church/wiki/Installation) describe how to install the development version of [Ikarus Scheme](http://ikarus-scheme.org/) or [Vicare Scheme](https://github.com/marcomaggi/vicare) with foreign function interface enabled.

1. Install [scheme-tools](https://github.com/stuhlmueller/scheme-tools):
  
    1. Clone the scheme-tools repository using <code>git clone git://github.com/stuhlmueller/scheme-tools.git</code>.
  
    2. Add the scheme-tools directory to your <code>$IKARUS_LIBRARY_PATH</code>. 

        To add a directory to your <code>$IKARUS_LIBRARY_PATH</code>, change into the directory and type <code>echo -e "\nexport IKARUS_LIBRARY_PATH=\`pwd\`:\$IKARUS_LIBRARY_PATH" >> ~/.bashrc</code>. Replace <code>~/.bashrc</code> with the location of your shell config file.

    3. Add the scheme-tools/bin directory to your <code>$PATH</code>, e.g. by changing into the directory and typing <code>echo -e "\nexport PATH=\`pwd\`:\$PATH" >> ~/.bashrc</code>.

2. Install [scheme-transforms](https://github.com/stuhlmueller/scheme-transforms):

    1. Clone the repository using <code>git clone git://github.com/stuhlmueller/scheme-transforms.git</code>.
  
    2. Add the scheme-transforms directory to your <code>$IKARUS_LIBRARY_PATH</code> (see above).

3. Install [cosh](https://github.com/stuhlmueller/cosh):

    1. Clone the repository using <code>git clone git://github.com/stuhlmueller/cosh.git</code>.
  
    2. Add the cosh directory to your <code>$IKARUS_LIBRARY_PATH</code> (see above).

    3. Add the cosh/bin directory to your <code>$PATH</code> (see above).

## Usage

Create a file called myprogram.church with the following content:

    (rejection-query
     (define x (flip))
     (define y (flip))
     (list x y)
     (or x y))

Then, on the command line, type:

    cosh myprogram.ss

You should see the following output:

    ((#f #t) . 0.3333333333333333)
    ((#t #f) . 0.3333333333333333)
    ((#t #t) . 0.3333333333333333)

## Concepts

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
