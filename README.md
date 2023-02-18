# Notty experiments

## Installation

[Install](https://opam.ocaml.org/doc/Install.html) `opam`, the OCaml package manager.
E.g. on Arch Linux:
```bash
pacman -S opam
opam init
```

Install OCaml 4.14.1:
```bash
opam switch install 4.14.1
```

Then clone this repository and install dependencies:
```bash
git clone https://github.com/rand00/notty_experiments
cd notty_experiments
opam install dune lwt lwt_react notty gg containers
dune exec bin/main.exe
```
