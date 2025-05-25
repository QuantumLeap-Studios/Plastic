# ![PlasticLogo](https://raw.githubusercontent.com/QuantumLeap-Studios/Plastic/master/Copy%20of%20Copy%20of%20gdfgdf.png)

| Feature               | Plastic (Current)        | Rust                                |
| --------------------- | ----------------------- | ----------------------------------- |
| Keywords              | Basic + Error Handling  | Extensive                           |
| Data Types            | Basic + List/Map/Array  | Rich (`u8`, `char`, `()`, `&str`)   |
| Ownership & Borrowing | 🚧 Partial (WIP)        | ✅ Core Feature                      |
| Functions             | Basic, DLL Import, Closures | Advanced (generics, lifetimes)  |
| Variables             | `let`, `mut`, `pub`     | `let`, `mut`, patterns, annotations |
| Operators             | Arithmetic, Logical, Custom | Full set                        |
| Comments              | `//` only               | `//`, `/* */`, doc comments         |
| Structs/Traits/Enums  | ✅ Supported            | ✅ (`struct`, `trait`, `enum`)       |
| Error Handling        | `try`, `catch`, `throw`, `finally` | Advanced (`Result`, `panic!`) |
| Imports/Plugins       | `import`, `reference`, `using`, DLL | Advanced modules, crates         |
| File/Process I/O      | ✅ Built-in             | Via stdlib                           |
| HTTP/Localhost        | ✅ Built-in server      | Via crates                           |
| Concurrency           | Channels, Threads       | Advanced (async, threads, channels)  |

---

## Plastic Language Overview

### **Keywords & Syntax**
Plastic now supports a broader set of keywords, including:
- Control flow: `fn`, `let`, `mut`, `pub`, `if`, `else`, `while`, `for`, `return`, `match`
- Data modeling: `struct`, `enum`, `trait`, `impl`, `interface`
- Error handling: `try`, `catch`, `throw`, `finally`
- Imports: `import`, `reference`, `using`, `dllimport`
- Others: `self`, `as`, `in`, `true`, `false`

### **Data Types**
- **Primitives:** `i32`, `i64`, `f32`, `f64`, `bool`, `string`, `void`
- **Collections:** `array`, `list`, `map`, `List<string>`
- **User-defined:** `struct`, `enum`, `trait`
- **Function types:** `fn(...) -> ...`
- **Special:** `any`

### **Ownership & Borrowing**
- Plastic has experimental support for ownership and borrowing checks in the type checker (WIP).
- Mutable and reference types: `mut`, `&`, `&mut` in function parameters.

### **Functions**
- Standard function declarations.
- Support for closures and function types.
- DLL import: Call native functions via `dllimport`.

### **Variables**
- `let` for variable binding.
- `mut` for mutability.
- `pub` for public visibility.

### **Operators**
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Logical: `&&`, `||`, `&|` (and-or)
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Assignment: `=`
- Custom: e.g., `added` as an alias for `+`

### **Comments**
- Single-line comments: `//`

### **Structs, Enums, Traits**
- User-defined types: `struct`, `enum`, `trait`, `impl`
- Traits and trait implementations for polymorphism.

### **Error Handling**
- `try`, `catch`, `throw`, `finally` blocks for robust error management.

### **Imports & Plugins**
- `import`, `reference`, `using` for code and plugin loading.
- `dllimport` for native interop.

### **File and Process I/O**
- Built-in functions: `readFile`, `writeFile`, `appendFile`, `deleteFile`, `exists`, `mkdir`, `rmdir`, `listDir`, `copyFile`, `moveFile`, `renameFile`
- Process control: `runProcess`, `runProcessAndWait`, `openFile`, `command`

### **HTTP/Localhost Support**
- Start a local HTTP server: `startHttpServer(port)`
- Access HTTP request data: `GetHttpRequestData()`, `GetHttpRequestMethod()`, `GetHttpRequestPath()`
- Respond to HTTP requests: `SetHttpResponse(response)`
- HTTP client: `curl(url, method, data, headers)`

### **Concurrency & Channels**
- Threading: `CreateNewThread(source, input)`
- Channels: `SendToChannel(value)`, `ReceiveFromChannel()`, `GetChannelCount()`

### **Other Built-ins**
- String/number utilities: `len`, `parseInt`, `parseFloat`, `toString`, `toInt`, `toFloat`, `toUpper`, `toLower`, `trim`, `split`, `join`, `contains`, etc.
- Time/date: `now`, `timestamp`, `formatDate`, `time`
- Random: `random`
- UUID: `uuid`, `GenerateUUID`
- Environment: `env`, `GetEnvironmentVariable`, `GetCurrentDirectory`, `GetCurrentProcessId`
- Logging: `log`, `error`, `showErrorBox`, `showConfirmBox`
- JSON: `ParseJSON`, `SerializeJSON`
- Math: `abs`, `floor`, `ceil`, `round`, `sqrt`, `pow`, `max`, `min`, `clamp`
- Binary/Morse/Hash: `ConvertToBinary`, `BinaryToConvert`, `ConvertToMorse`, `MorseToConvert`, `Hash`, `VerifyHash`
- Garbage collection: `CollectGarbage`

---

## Example: Localhost HTTP Server


```
startHttpServer("8080");
while (true) {
    let method: string = GetHttpRequestMethod();
    let path: string = GetHttpRequestPath();
    let data: string = GetHttpRequestData();
    print("Received", method, path, data);
    SetHttpResponse("Hello from Plastic!");
}

```

## Example: File I/O


```
writeFile("test.txt", "Hello, file!");
let content: string = readFile("test.txt");
print(content);

```

## Example: Concurrency


```
SendToChannel("hello");
let msg: string = ReceiveFromChannel();
print(msg);
```

---

> **Note:** Plastic is evolving rapidly. See the [Plastic Syntax in Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=QuantumLeapStudios.plastic-language) for up-to-date syntax highlighting and tooling.

---

**Summary of Recent Additions:**
- Localhost HTTP server and request/response API
- File and directory operations
- Process and command execution
- Channels and threading
- Structs, enums, traits, and implementations
- Error handling with try/catch/finally/throw
- Many new built-in functions for scripting, system, and utility tasks

---

Feel free to further expand or reorganize this as needed for your documentation site or repository!
