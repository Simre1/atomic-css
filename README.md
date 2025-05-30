Atomic CSS
============

[![Hackage](https://img.shields.io/hackage/v/atomic-css.svg)][hackage]

Type-safe, composable CSS utility functions. Inspired by Tailwindcss and Elm-UI


### Write Haskell instead of CSS

Style your html with composable CSS utility functions:

```haskell
el ~ bold . pad 8 $ "Hello World"
```

This renders as the following HTML with embedded CSS utility classes:

```html
<style type='text/css'>
.bold { font-weight:bold }
.p-8 { padding:0.500rem }
</style>

<div class='bold p-8'>Hello World</div>
```

Instead of relying on the fickle cascade, factor and compose styles with the full power of Haskell functions!

```haskell
header = bold
h1 = header . fontSize 32
h2 = header . fontSize 24
page = flexCol . gap 10 . pad 10

example = el ~ page $ do
  el ~ h1 $ "My Page"
  el ~ h2 $ "Introduction"
  el "lorem ipsum..."
```

This approach is inspired by Tailwindcss' [Utility Classes](https://tailwindcss.com/docs/styling-with-utility-classes)


### Intuitive Flexbox Layouts

Create complex layouts with `row`, `col`, `grow`, and `space`

```haskell
holygrail = do
  col ~ grow $ do
    row "Top Bar"
    row ~ grow $ do
      col "Left Sidebar"
      col ~ grow $ "Main Content"
      col "Right Sidebar"
    row "Bottom Bar"
```

### Stateful Styles

We can apply utilities when certain states apply. For example, to change the background on hover:

```haskell
button ~ bg Primary . hover (bg PrimaryLight) $ "Hover Me"
```

Media states allow us to create responsive designs

```haskell
el ~ width 100 . media (MinWidth 800) (width 400) $ do
  "Big if window > 800"
```


### Embedded CSS

Only the utilities used in a given html fragment are rendered:

    >>> renderText $ el ~ bold $ "Hello"
    
    <style type='text/css'>.bold { font-weight:bold }</style>
    <div class='bold'>Hello</div>


### Try Example Project with Nix

If you want to get a feel for atomic-css without cloning the project run `nix run github:seanhess/atomic-css` to run the example webserver locally

Import Flake
------------

You can import this flake's overlay to add `atomic-css` to `overriddenHaskellPackages` and which provides a ghc966 and ghc982 package set that satisfy `atomic-css`'s dependencies.

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    atomic-css.url = "github:seanhess/atomic-css"; # or "path:/path/to/cloned/atomic-css";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, atomic-css, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ atomic-css.overlays.default ];
        };
        haskellPackagesOverride = pkgs.overriddenHaskellPackages.ghc966.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (hfinal: hprev: {
            # your overrides here
          });
        });
      in
      {
        devShells.default = haskellPackagesOverride.shellFor {
          packages = p: [ p.atomic-css ];
        };
      }
    );
}
```

Local Development
-----------------

### Recommended ghcid command

If you want to work on both the atomic-css library and example code, this `ghcid` command will run and reload the examples server as you change any non-testing code.

```
ghcid --command="cabal repl exe:example lib:atomic-css" --run=Main.main --warnings --reload=./embed/preflight.css
```

If you want to work on the test suite, this will run the tests each time any library code is changed.

```
ghcid --command="cabal repl test lib:atomic-css" --run=Main.main --warnings --reload=./embed/preflight.css
```

### Nix

- `nix flake check` will build the library, example executable and devShell with ghc-9.8.2 and ghc-9.6.6
    - This is what the CI on GitHub runs
- `nix run` or `nix run .#ghc982-example` to start the example project with GHC 9.8.2
    - `nix run .#ghc966-example` to start the example project with GHC 9.6.6
- `nix develop` or `nix develop .#ghc982-shell` to get a shell with all dependencies installed for GHC 9.8.2. 
    - `nix develop .#ghc966-shell` to get a shell with all dependencies installed for GHC 9.6.6. 
- `nix build`, `nix build .#ghc982-atomic-css` and `nix build .#ghc966-atomic-css` builds the library with the `overriddenHaskellPackages`
    - If you want to import this flake, use the overlay
- `nix flake update nixpkgs` will update the Haskell package sets and development tools

### Common Nix Issues

#### Not Allowed to Refer to GHC

If you get an error like:

```
error: output '/nix/store/64k8iw0ryz76qpijsnl9v87fb26v28z8-my-haskell-package-1.0.0.0' is not allowed to refer to the following paths:
         /nix/store/5q5s4a07gaz50h04zpfbda8xjs8wrnhg-ghc-9.6.3
```

Follow these [instructions](https://nixos.org/manual/nixpkgs/unstable/#haskell-packaging-helpers)

#### Dependencies Incorrect

You will need to update the overlay, look for where it says `"${packageName}" = hfinal.callCabal2nix packageName src { };` and add a line like `Diff = hfinal.callHackage "Diff" "0.5" { };` with the package and version you need.

#### Missing Files

Check the `include` inside the `nix-filter.lib` to see if all files needed by cabal are there.

Learn More
----------

View Documentation on [Hackage][hackage]
* https://hackage.haskell.org/package/atomic-css

View on Github
* https://github.com/seanhess/atomic-css

View [Examples](https://github.com/seanhess/atomic-css/blob/latest/example/app/Main.hs)


[hackage]: https://hackage.haskell.org/package/atomic-css


Contributors
------------

* [Sean Hess](https://github.com/seanhess)
* [Kamil Figiela](https://github.com/kfigiela)
* [Pfalzgraf Martin](https://github.com/Skyfold)

