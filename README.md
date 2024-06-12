# rlox

A Rust tree-walking implementation of [Lox](https://craftinginterpreters.com/) by Bob Nystrom.

# Syntax

## Data Types

```kt
10; // Number (float)
"Hello" // String
true || false; // Boolean
nil; // Nil (null)
```

## Arithmetic

```kt
// Basic arithmetic operators implemented on the Number type
0 + 1; // 1.0
2 - 1; // 1.0
1 * 5; // 5.0
5 / 2; // 2.5
-5 + 2; // -3

// Addition between strings is supported
var temp = "Hello " + "World!";
```

## Comparison & Equality

```kt
1 < 2; // false
2 <= 2; // true
3 > 2; // true
3 >= 4; // false

"Hi!" == "Hi!"; // true
"123" == 123; // false;

true and false; // false
true or false; // true

!true; // false
!false; // true
```

## Statements

```kt
print "Hello there!"; // "Hello There"
print(64/2); // 32
```

## Variables

```kt
var iAmVariable = "value";
var iAmNil; // Value is implicitly converted to Nil

print iAmVariable; // "value"
print iAmNil; // Nil

// Variables are always mutable
iAmVariable = "different value";
print iAmVariable; // "different value"
```

## Control Flow

```kt
// If statements
if (condition) {
    ...
}
else if (condition) {
    ...
} else {
    ...
}

// While loop
var a = 1;
while (a < 10) {
  print a;
  a = a + 1;
}

// For loop
for(var x = 0; x < 10; x = x + 1) {
    ...
}
```
