# Types

> Note:
> Onyx is still in a very early beta, meaning that anything in this or any other design document can change at any point in time.

## Integer Types

In Onyx, we have both signed and unsigned integer types. The signed integer types are `i8`, `i16`, `i32`, and `i64`. The unsigned integer types are `u8`, `u16`, `u32`, and `u64`.
The difference between signed and unsigned integers are that signed integers can be negative while unsigned ones can't. For example, the range of an 8-bit signed integer is -128 to 127 while the range of an 8-bit unsigned integer is 0 to 255. This also means that 8-bit unsigned integers can be used instead of the `char` type.
Here is a few examples on how to use the integer types:
```onyx
var a: i8 = -15;
var b: u16 = 1234;

// This also works!
var c: u8 = 'a';
```

## Floating-point Types

We have 32-bit and 64-bit floating-point literals, which are defined with `f32` and `f64`. Here is how you can use floating-point types:
```onyx
var x: f32 = 1.23;
var y: f64 = 123.456;
```

## String Types

Onyx does not currently have a built-in string type. String are currently defined using the `String` class from the built-in libraries. This can be imported using `import String = "builtin:String";`, and can be instansiated using `new String("My String")`.
You can also use a constant raw character pointer (defined as `const raw char`) or a character array (defined as `[char]`) to store a string literal.
Here is how you can define a string:
```onyx
var first_way: String = new String("hello");
var second_way: const raw char = "hello world";
var third_way: const [char] = "this works too!";
```

## Boolean Types

A boolean type is defined using the `bool` keyword and can store either `true` or `false`. Here is an example on how to use booleans:
```onyx
var a_boolean: bool = true;
```

## Character Types

A character type is defined using the `char` keyword and stores a single character. Here is an example of how to use `char`:
```onyx
var my_important_char: char = 'a';
```

## Arrays

Onyx has both static and dynamic arrays, difference being that a static array has a assigned size at compile time, while the dynamic arrays size can change during the programs execution. The static and dynamic arrays are defined as follows, where T is the specified type and S is the size of the array: `[T; S]`, `[T]`. Here are a few example on how you can define arrays:
```onyx
var arr: [i32; 3] = [1, 2, 3];
var arr2: [char] = "Hello, world!";
```

## Pointers

In Onyx, there are two types of pointers you can define, raw and weak pointers. Both of them are treated like unsafe code in Onyx, meaing that you cannot use them outside of an [unsafe context](./unsafe.md).

### Raw Pointers

A raw pointer is defined using the `raw` keyword and it stores the address of an object in memory. This is also the only type that is [nullable](./null-safety.md) by default, meaning that is can be assigned as null. Here is how raw pointers can be used in your code:
```onyx
var null_ex: raw char = null; // Only valid because of the raw pointer
```

### Weak Pointers

> todo

## Optional Types

Declaring a type as optional means that the type is now [nullable](./null-safety.md). This means that for example, in a variable declaration, the value can be assigned as null. A type can be defined as optional by putting a `?` after the type, here is an example:
```
var my_optional: i64? = null;
```

## Constants

Defining a type as constant (using the `const` keyword) means that the value assign to the type is immutable (meaning that it cannot change). For example, declaring the type of a variable declaration means that the value of that variable is immutable. Here is an example of how to use `const`:
```onyx
var foo: const raw char = "Hello";
```
