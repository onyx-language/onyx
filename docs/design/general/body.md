# Bodies

There are two different types of bodies you can have in Onyx, one being the standard block, which is defined using `{` and `}`. Then you can define a body as a single expression using `=>` then putting the expression after the arrow.

## Conventions

### Block

Blocks are defined using `{` and `}`. They can be used in any place where a body is expected. They can be used in functions, classes, and even in the top level of a file. Here is an example of a block being used in a function declaration:
```onyx
function main() -> void {
    ...
}
```

### Single Expression

The single expression block is defined with a `=>` where the `{` would usually go. Unlike the block body, it cannot be used anywhere. For example, it can not be used in a class declaration:
```onyx
// This is invalid code, obviously.
class MyClass => ...
```