## Accessors Transform

The transform generates getters and setters for designated member variables
in struct/union/class and replaces every occurrence of the variable with
relevant accessor. Sample transform syntax:

    ---
    Transforms:
      Accessors:
        - Foo::z
        - Foo::m_([xy]+): \1

In the example above two versions of variable selection is present:
explicit (Foo::z) and pattern-based. Explicit selection is legacy form, it
requires single fully qualified member variable name and generates
accessors using exact member name: `z` accesses will be replaced with
`getZ()` and `setZ()`.

Pattern-based form is a mapping from pattern of fully qualified name to
name has to be used in accessors. This approach allows to target multiple
member at once and support limited renaming, e.g. in the example accesses
fields `m_x` and `m_y` (if present) will be replaced with `set/getX()` and
`set/getY()` despite of original name.

### Notes

Requirement to provide fully qualified names (or patterns for them)
introduces some tricky cases, i.e. transform writer should consider
namespaces, nested scopes (including anonymous) etc. This requirement will
be relaxed in future though.

The tranfsorm will be parameterized with accessor form. For now it is only
java-style camel-case `get/setX`, but it will be perfectly fine to request
first-capital-letter, snake-case or omit get/set prefix. The feature is
there, it just requires config.
