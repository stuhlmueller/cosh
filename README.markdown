# Cosh

Cosh is a Church implementation that uses dynamic programming based on continuation hashing.

## Installation instructions:

0. Install [git](http://git-scm.com/) if you don't have it.

1. Follow the [instructions on the Church wiki](http://projects.csail.mit.edu/church/wiki/Installation) to install the development version of [ikarus scheme](http://bazaar.canonical.com/en/) with foreign function interface enabled and [MIT-Church](https://github.com/stuhlmueller/mit-church) (for srfis and math-env).

2. Install [scheme-tools](https://github.com/stuhlmueller/scheme-tools):

    1. Install the [GNU linear programming kit](http://www.gnu.org/software/glpk/), e.g. using <code>sudo apt-get install glpk</code> (Ubuntu/Debian) or <code>sudo port install glpk</code> (MacPorts).
  
    2. Download and install [PyMathProg](http://sourceforge.net/projects/pymprog/), e.g. by downloading the code from sourceforge and then executing <code>sudo python setup.py install</code> in the extracted directory.
  
    3. Clone the scheme-tools repository using <code>git clone git://github.com/stuhlmueller/scheme-tools.git</code>.
  
    4. Add the scheme-tools directory to your <code>$IKARUS_LIBRARY_PATH</code>. In general, you can add a directory to <code>$IKARUS_LIBRARY_PATH</code> by changing into the directory and typing <code>echo -e "\nexport IKARUS_LIBRARY_PATH=\`pwd\`:\$IKARUS_LIBRARY_PATH" >> ~/.bashrc</code>. Replace <code>~/.bashrc</code> with the location of your shell config file.

4. Install [scheme-transforms](https://github.com/stuhlmueller/scheme-transforms):

    1. Clone the repository using <code>git clone git://github.com/stuhlmueller/scheme-transforms.git</code>.
  
    2. Add the scheme-transforms directory to your <code>$IKARUS_LIBRARY_PATH</code> (see above).

5. Install [cosh](https://github.com/stuhlmueller/cosh):

    1. Clone the repository using <code>git clone git://github.com/stuhlmueller/cosh.git</code>.
  
    2. Add the cosh directory to your <code>$IKARUS_LIBRARY_PATH</code> (see above).