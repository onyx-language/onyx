# Unsafe Context

In Onyx, we have a concept called unsafe context, which is not a unique idea whatsoever. An unsafe context is defined using the `unsafe` keyword, and can be place almost anyways (examples below).

## What is unsafe and what isn't?

Onyx current only have two features that require an unsafe context, those being raw and weak pointers.

<!--
Onyx currently does not have many features are require an unsafe context, here are the ones that do:
* 
--> 

## Why?

When designing Onyx, one of our main goals are to make the language as safe as possible, and to put it simply, all features that require an unsafe context are not safe to use because of [memory safety](./memory-safety.md)  and [null safety](./null-safety.md).

## Examples

> todo
