# clock-fltkhs
This is a meant to demonstrate building a native clock app with the `fltkhs` and `fltkhs-themes` packages. This uses the `fltkhs-light-theme-skeleton` app as a starting point. 

To build: 

> git clone https://github.com/typeclasses/typeclasses-projects.git
> cd clock-fltkhs
> stack build --flag fltkhs:bundled 

To use in REPL:

> stack ghci --no-nix
> replMain

If you have nix enabled in your global Stack configuration, be sure to pass the `--no-nix` flag to the `stack build`, `stack exec`, or `stack ghci` commands.