let
  jupyterLibPath = /home/pi/wip/haskell/data-haskell/tweag/jupyterWith;
  jupyter = import jupyterLibPath {};

#  jupyter = import (builtins.fetchGit {
#    url = https://github.com/tweag/jupyterWith;
#    rev = "";
#  }) {};

# dispanser-ml-notes = pkgs.callCabal2nix "dispanser-ml-notes" /home/pi/wip/haskell/data-haskell/isl/. {};

  iPython = jupyter.kernels.iPythonWith {
    name = "python";
    packages = p: with p; [ numpy ];
  };

  iHaskell = jupyter.kernels.iHaskellWith {
    name = "haskell";
    packages = p: with p; [ hvega formatting csv ];
  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iPython iHaskell ];
    };
in
  jupyterEnvironment.env
