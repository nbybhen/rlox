# rlox

A Rust tree-walking implementation of [Lox](https://craftinginterpreters.com/) by Bob Nystrom.

## Usage
```bash
# Starts the language REPL
cargo run

# Parses and runs the text as code from within the file
cargo run [file] 
```

## Syntax

### Data Types

```kt
10; // Number (float)
"Hello" // String
true || false; // Boolean
nil; // Nil (null)
```

### Arithmetic

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

### Comparison & Equality

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

### Statements

```kt
print "Hello there!"; // "Hello There"
print(64/2); // 32
```

### Variables

```kt
var iAmVariable = "value";
var iAmNil; // Value is implicitly converted to Nil

print iAmVariable; // "value"
print iAmNil; // Nil

// Variables are always mutable
iAmVariable = "different value";
print iAmVariable; // "different value"
```

### Control Flow

```kt
// If statements
if (condition) {
    ..
}
else if (condition) {
    ..
} else {
    ..
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

### Functions & Closures

```kt
// Functions
fun returnSum(a, b) {
  return a + b;
}

// If no return value is found, the function implicitly returns Nil
fun printSum(a, b) {
  print a + b;
}

// You can declare functions within other functions.
fun outerFunction() {
  fun localFunction() {
    print "I'm local!";
  }

  localFunction();
}

// Closures
fun addPair(a, b) {
  return a + b;
}

fun identity(a) {
  return a;
}

print identity(addPair)(1, 2); // Prints "3".
```

### Classes & Inheritance

```kt
// The body of classes contain a list of methods
class Breakfast {
  cook() {
    print "Eggs a-fryin'!";
  }

  serve(who) {
    print "Enjoy your breakfast, " + who + ".";
  }
}

// Store it in variables.
var someVariable = Breakfast;

// Pass it to functions.
someFunction(Breakfast);

var breakfast = Breakfast();
print breakfast; // "Breakfast instance".

// You can add properties onto objects, creating them if it doesn't already exist.
breakfast.meat = "sausage";
breakfast.bread = "sourdough";

// this keyword allows for access of properties within methods of the class
class Breakfast {
  serve(who) {
    print "Enjoy your " + this.meat + " and " +
        this.bread + ", " + who + ".";
  }

  // ...
}

// init() allows for initialization of object with proper values.
class Breakfast {
  init(meat, bread) {
    this.meat = meat;
    this.bread = bread;
  }

  // ...
}

var baconAndToast = Breakfast("bacon", "toast");
baconAndToast.serve("Dear Reader");
// "Enjoy your bacon and toast, Dear Reader."

// Supports single-inheritance using the '<' operator.
class Brunch < Breakfast {
  drink() {
    print "How about a Bloody Mary?";
  }
}

class Brunch < Breakfast {
  init(meat, bread, drink) {
    super.init(meat, bread);
    this.drink = drink;
  }
}

```
