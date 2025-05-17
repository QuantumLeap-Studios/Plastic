# ![PlasticLogo](https://raw.githubusercontent.com/QuantumLeap-Studios/Plastic/master/Copy%20of%20Copy%20of%20gdfgdf.png)


| Feature               | Plastic                 | Rust                                |
| --------------------- | ----------------------- | ----------------------------------- |
| Keywords              | Basic                   | Extensive                           |
| Data Types            | Basic (`i32`, `string`) | Rich (`u8`, `char`, `()`, `&str`)   |
| Ownership & Borrowing | ❌ Not Present           | ✅ Core Feature                      |
| Functions             | Basic syntax            | Advanced (generics, lifetimes)      |
| Variables             | Basic (`let`)           | `let`, `mut`, patterns, annotations |
| Operators             | Limited                 | Full set                            |
| Comments              | `//` only               | `//`, `/* */`, doc comments         |
| Structs/Traits        | ❌                       | ✅ (`struct`, `trait`, `enum`)       |

---

### Explanation of Plastic

#### **Keywords**
Plastic includes only the essential keywords needed for basic programming, such as `fn`, `let`, `if`, `while`, `for`, and `return`. Its concise syntax helps keep the language simple and easy to learn.

#### **Data Types**
Plastic’s type system is straightforward, offering a few numeric types like `i32` and a basic `string` type. This makes it ideal for simple scripting and learning programming basics without the complexity of a rich type system.

#### **Ownership & Borrowing**
Unlike languages such as Rust, Plastic does not implement ownership or borrowing. Memory management is kept simple, and these advanced concepts are not part of the language, which further contributes to its minimalistic design.

#### **Functions**
Functions in Plastic are defined with a simple syntax. Each function specifies its name, an optional parameter list (with types for each parameter), and a fixed return type. This consistent syntax ensures clarity in function declarations and calls.

#### **Variables**
Variables in Plastic are introduced using `let`. There is no explicit mechanism for mutable variables or variable destructuring. The language focuses on straightforward, immutable variable binding.

#### **Operators**
Plastic provides basic operators for arithmetic and string concatenation. The limited operator set is sufficient for scripting and educational purposes, avoiding the complexity found in languages with a full operator suite.

#### **Comments**
Plastic supports single-line comments using `//`. This keeps the language simple and is often enough for small scripts and tutorials, without the added complexity of block comments or documentation-specific syntax.

#### **Structs / Traits**
Currently, Plastic does not support user-defined data structures such as `structs`, `enums`, or `traits`. Its design is streamlined, focusing on core scripting capabilities without the overhead of advanced data modeling features.

> **Note:** The Rust column in the comparison table is provided for reference only, to offer context about features found in more advanced languages. The primary focus here is on explaining Plastic and its design choices.

---

🔗 **Syntax Highlighting for Plastic**  
[Plastic Syntax in Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=QuantumLeapStudios.plastic-language)
