# Null safety

In Onyx, we achive null-safety by not allowing types to be null by default. The only way to allow a value to be null is to explicitly declare it as [optional](./types.md), or if the type is a raw pointer, which would be defined as [unsafe](./unsafe.md) behaviour.
