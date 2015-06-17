# haskell-datasource
I was wondering whether one could generate Haskell code from web form inputs. Turns out you can. You'll need the `haskell-autogen` package to make this compile.
How to load it during runtime is another question entirely that I'd also like to investigate. Packages that seemed interesting:

 * [plugins](http://hackage.haskell.org/package/plugins): library for developing Haskell plugins that can be enabled during runtime.
 * [hotswap](http://hackage.haskell.org/package/hotswap): convienience library for `plugins`.
 
Anything else seemed far too old or unmaintained.
