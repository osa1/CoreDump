# CoreDump

This is a GHC plugin for dumping GHC's internal Core data
structures(`CoreProgram`, `CoreSyn` etc.) in a readable yet completely exposed
way.

When studying GHC's internals most of the time I want to see generated Core for
an expression, or how a particular type/data constructor etc. is used
internally. GHC provides no easy for this. `Outputable.pprTrace` and other
functions in `Outputable` module help, but they don't solve the problem because
1) they use `Outputable` instances, which hide lots of details 2) we have to
modify GHC and recompile it to print things we're interested in, which takes a
lot of time.

This library derives orphan `Show` instances for GHC internals as much as
possible. When not possible(because of a hidden constructor), it tries hard to
print something as similar to the original type as possible.

Currently these types are only printed as `"<TypeName>"`:

- `Class.Class`
- `CoreSyn.RuleFun` (which is a function type)
- `UniqFM.UniqFM`
- `Coercion.Coercion`

`Show` instances of these types are same as `Outputable` instances:

- `Name.Name`
- `OccName.OccName`

Other than these, `Show` instances for pretty much every Core-level data
structure are implemented.

## Usage

After installing as usual:

```
$ ghc --make -fplugin=CoreDump -fplugin-opt=CoreDump:both Hello.hs
```

`-fplugin-opt=CoreDump:both` part is for telling the plugin when to dump Core.
Valid arguments are: (to be used in place of `both`)

- `before`: Print before Core-to-Core passes.
- `after`: Print after Core-to-Core passes.
- `both`: Print before and after Core-to-Core passes.

If you want to follow some other plugin's transformations, make sure to pass
other plugin _after_ passing `CoreDump`, in command line. Example:

```
$ ghc --make -fplugin=CoreDump -fplugin-opt=CoreDump:both -fplugin=Test Hello.hs
```

This will dump the Core, run the plugin named `Test`, and then dump the Core
again.

If you move `-fplugin=Test` part before `-fplugin=CoreDump`, then GHC installs
`Test` _after_ installing `CoreDump`, so depending on how `Test` plugin installs
itself, you may not be able to dump Core output before or after `Test` runs.
