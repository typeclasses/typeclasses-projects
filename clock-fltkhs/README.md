# clock-fltkhs

https://typeclasses.com/timepieces/lesson-1

This is a meant to demonstrate building a native clock app with the `fltkhs` and `fltkhs-themes` packages. This uses the `fltkhs-light-theme-skeleton` app as a starting point. 

To build: 

`> git clone https://github.com/typeclasses/typeclasses-projects.git`
`> cd typeclasses-projects/clock-fltkhs`
`> stack build --flag fltkhs:bundled`

To use in REPL:

`> stack ghci --flag fltkhs:bundled --no-nix`   
`> replMain`

If you have nix enabled in your global Stack configuration, you may need to pass the `--no-nix` flag to the `stack build`, `stack exec`, or `stack ghci` commands. You *shouldn't* have to pass it to `stack build` but a few times I have had to. God knows why.
