# Cosh

Cosh is an experimental Church implementation that uses dynamic programming based on hashing factored continuations.

## Installation

This installation assumes that you have [git](http://git-scm.com/) and a R6RS Scheme installed. The [instructions on the Church wiki](http://projects.csail.mit.edu/church/wiki/Installation) describe how to install the development version of [Ikarus Scheme](http://ikarus-scheme.org/) or [Vicare Scheme](https://github.com/marcomaggi/vicare) with foreign function interface enabled.

1. Install [scheme-tools](https://github.com/stuhlmueller/scheme-tools):
  
    1. Clone the scheme-tools repository using <code>git clone git://github.com/stuhlmueller/scheme-tools.git</code>.
  
    2. Add the scheme-tools directory to your <code>$IKARUS_LIBRARY_PATH</code>. 

        To add a directory to your <code>$IKARUS_LIBRARY_PATH</code>, change into the directory and type <code>echo -e "\nexport IKARUS_LIBRARY_PATH=\`pwd\`:\$IKARUS_LIBRARY_PATH" >> ~/.bashrc</code>. Replace <code>~/.bashrc</code> with the location of your shell config file.

    3. Add the scheme-tools/bin directory to your <code>$PATH</code>.

        To add a directory to your <code>$PATH</code>, <code>cd</code> into the directory and type <code>echo -e "\nexport PATH=\`pwd\`:\$PATH" >> ~/.bashrc</code>.

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

    cosh myprogram.church

You should see the following output:

    ((#f #t) . 0.3333333333333333)
    ((#t #f) . 0.3333333333333333)
    ((#t #t) . 0.3333333333333333)

## Options

    $ cosh --help
    Usage: cosh <file> [options]
    
    Options:
      -h, --help     show this help message and exit
      -n, --nosub    don't factor out subproblems
      -v, --verbose  display all executed commands
      -o, --nostate  don't merge based on the full computation state
      -k, --keep     do not delete compiled file
      -l, --limit    restrict graph size
      -t, --time     record the time it takes for the compiled file to run
      -d, --debug    run all scheme commands in debug mode
