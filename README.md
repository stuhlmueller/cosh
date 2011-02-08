# Cosh

Cosh is an experimental Church implementation that uses dynamic programming based on continuation hashing.

## Installation instructions

0. Install [git](http://git-scm.com/).

1. Follow the [instructions on the Church wiki](http://projects.csail.mit.edu/church/wiki/Installation) to install the development version of [Ikarus](http://ikarus-scheme.org/) or [Vicare](https://github.com/marcomaggi/vicare) with foreign function interface enabled.

2. Install [scheme-tools](https://github.com/stuhlmueller/scheme-tools):

    1. To use the linear solver:

        1. Install the [GNU linear programming kit](http://www.gnu.org/software/glpk/), e.g. using <code>sudo apt-get install glpk</code> (Ubuntu/Debian) or <code>sudo port install glpk</code> (MacPorts).
  
        2. Download and install [PyMathProg](http://sourceforge.net/projects/pymprog/), e.g. by downloading the code from sourceforge and then executing <code>sudo python setup.py install</code> in the extracted directory.

    2. To use the polynomial solver:

        1. Install [Mathematica](http://www.wolfram.com/mathematica/).

        2. Download and install [pexpect](http://sourceforge.net/projects/pexpect/files/) e.g. via sourceforge and <code>sudo python setup.py install</code>.
  
    3. Clone the scheme-tools repository using <code>git clone git://github.com/stuhlmueller/scheme-tools.git</code>.
  
    4. Add the scheme-tools directory to your <code>$IKARUS_LIBRARY_PATH</code>. In general, you can add a directory to <code>$IKARUS_LIBRARY_PATH</code> by changing into the directory and typing <code>echo -e "\nexport IKARUS_LIBRARY_PATH=\`pwd\`:\$IKARUS_LIBRARY_PATH" >> ~/.bashrc</code>. Replace <code>~/.bashrc</code> with the location of your shell config file.

    5. Add the scheme-tools/bin directory to your <code>$PATH</code>, e.g. by changing into the directory and typing <code>echo -e "\nexport PATH=\`pwd\`:\$PATH" >> ~/.bashrc</code>.

4. Install [scheme-transforms](https://github.com/stuhlmueller/scheme-transforms):

    1. Clone the repository using <code>git clone git://github.com/stuhlmueller/scheme-transforms.git</code>.
  
    2. Add the scheme-transforms directory to your <code>$IKARUS_LIBRARY_PATH</code> (see above).

5. Install [cosh](https://github.com/stuhlmueller/cosh):

    1. Clone the repository using <code>git clone git://github.com/stuhlmueller/cosh.git</code>.
  
    2. Add the cosh directory to your <code>$IKARUS_LIBRARY_PATH</code> (see above).

    3. Add the cosh/bin directory to your <code>$PATH</code> (see above).

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

    Clustering together the strongly connected components of a polymap results in the acyclic components graph. Each component corresponds to a (linear or polynomial) problem that can be solved independently given the referenced parent parameters. By solving the components in topological order, the marginal distribution of the overall inference problem can be computed.
