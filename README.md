# Cordial

Cordial is an Discord bot framework built similar to how XMonad works,
where your configuration file becomes your bot executable.

(This is not yet implemented:)
`cordial` itself provides a few basic types such as the `Cordial` monad which,
like XMonad, is a `ReaderT`, `StateT` transformer over IO.

More documentation to come, but for now check out the example(s) in `src/exe/`,
the module implementations in `src/Cordial/Module/`, and the
[haddock docs](https://relrod.github.io/cordial/) for more.
