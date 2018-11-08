# Replacing the Prelude module using Backpack

https://typeclasses.com/no-prelude

This project demonstrates one of the many ways to avoid using the default `Prelude` module. We use Cabal's `mixins` feature (also known as Backpack) to specify that we want to:

1. Ignore the `Prelude` module from the `base` package; and
2. Instead use a different module from another package (in this example, that's the `Foundation` module from the `foundation` package) aliased as `Prelude`.
