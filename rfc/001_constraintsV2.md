## Rationale

This is something I've been thinking about for a while. As a
maintainer it\'s hard to get an overview of the status of your
packages: Are they constrained? tests disabled? You essentially have to
search through all of build-constraints.yaml for every package to
figure this out.

I know this is coming out of the blue, don't worry if you don't want
us to proceed in this direction! I just wanted to map things out, and
now seems like a good time to request feedback before I spend more time
on it.


## Changes

Current (V1) structure is:

```yaml
packages:
  <maintainer>
    - package1 (optional constraint)
  <other section>
    - package1 (optional constraint)
expected-test-failures:
  - package1
etc
etc
etc
```

I propose that we change this to centralize information about a package (V2), e.g. for me this could look like this:

```yaml
packages:
  "Adam Bergmark <adam@bergmark.nl> @bergmark":
    - name: aeson
      tests: skip # QuickCheck-2.11.3, base-orphans-0.7, hashable-time
    - name: HUnit
      range: < 1.6.2.0 # https://github.com/commercialhaskell/stackage/issues/5834
    - attoparsec-iso8601
    - name: fay
      range: < 0
      flags:
        test: true
    - name: fay-base
      hide: true
      range: < 0
    - name: fay-dom
      range: < 0
    - feed
    - time-compat
      - tests: skip # base-compat 0.11
    - through-text
    # Not my packages
    - HStringTemplate
    - name: language-ecmascript
      range: < 0 # via Diff-0.4.0
      tests: skip # testing-feat 1.1.0.0
    - spoon
    - tagshare
```

## Downsides

From a maintainer perspective I only see upsides for this.

For curators it will be more work when we need to upper bound lots of
packages as we can't add everything in one place. But some of us seem
to already prefer this flow as we are using a mix of the "Stackage
upper bounds" section and inlining this under the maintainers. Notably
GHC upgrades would be more time consuming as we always have a large
chunk of packages that need to be disabled. We could still allow this,
but I would prefer if this was seen as a temporary measure that we
clean up after the upgrade.

You won't as easily be able to sort the packages by name. Hopefully
I'm the only one who feels the urge to sort them.

## Implementation

This is actually very straight forward as curator's internal
representation maps closely to this new format (the old format
requires jumping through a lot more hoops to do the conversion).  If
we decide to support a mix of V1 and V2 style then it will of course
be more complex, but not that hard since we already have the V1
implementation to steal from.

## Migration

1. We can implement a function that converts V1 to V2 to transform all
of build-constraints.yaml.
1. We could do this (semi?) manually by supporting the V1 & V2 mix and
moving things a little bit at a time.
1. A mix of the two above.

## Additional Features


### Supporting mass operations
We could still support the old way of having a group of upper bounds
in one place, to speed up the work needed on GHC upgrades and package
relaeses with big impact. This will make the format more complex than
what V1 is, but apart from the initial work needed I don't think
that's much of an issue. I would prefer to not have to do this... but
I think it's necessary to keep people sane.

### Range shorthand
We can still allow `- package < range` for bounds as well as `range:`
if no other properties are set. I'd lean towards not allowing it as
it's more syntax. As is, removing the range doesn't require you to
remove the `name:` prefix, and you can't remove the `name:` prefix if
other properties are present (e.g. disabled tests)

### Maintainer configuration
We could add another level of nesting under the maintainer name to
allow new maintainer-level features, one thing that has been requested
is to be able to do something like what's below. We can add this later
without breaking the format (if there's a list => only packages, if
object => there should be a packages property with that list).

```yaml
packages:
  "Bob":
    dont-notify-when-a-reverse-dependency-has-bounds-issues-with-my-packages: true
    packages:
      - package1
      - package2
```

### Stricter validation

V1 has the feature/issue that multiple maintainers can list the same
package. I'm not a fan of this as a `< 0 ` may be added in only one
place and a maintainer may think the package is included when it's
not. I propose we error out of this occurs (exception: Don't do this
if packages are listed in the V1/ghc upgrade sections)

### Minor things

These are not introduced in V2, and I think doing them later would be fine.

* `range: < 0` is a hack, it might be clearer to have `disabled:
  true`? OTOH `range: < 0` sticks out more visually.
* `tell-me-when-its-released` is rarely used, but it would make more
  sense if its format was a range condition for triggering it rather
  than "notify me when a newer version than this is available".
* Require a github handle for users. Now and then we run across the
  issue that we don't notify because there is no github handle. I'm
  guessing the missing handles are just accidental. I think it would
  make sense to require this as we usually don't end up sending
  e-mails. We could make the parsing of the maintainer string stricter
  or add it under the proposed "Maintainer configuration". I'd lean
  towards stricter parsing to not have to add that level of
  "indentation" to every maintainer.
* Along that line, should we remove the e-mail addresses as we seldom
  make use of them? Might still be good to have on occasion.

### Implementation Status

Current implementation probably kind of works.

* The haskell side of things is structurally close to what I want, but the YAML
  format is not aligned with my example above (need to strip prefixes,
  add more custom parsing, and more)
* Not tested at all!
* V1->V2 migration support is missing
