API
===

## `Styx`
Compiler communication.

#### :small_orange_diamond: `CurrentBinder : Binder`
Returns the binder binding the currently executing code.

## `Styx.Bind`
Binding utilities.

#### :small_blue_diamond: `Binder : Type`
The type which represents the Styx binder.

#### :small_blue_diamond: `Bound : Type`
The type which represents a bound input, whether it was successful or not.

#### :small_orange_diamond: `Bind : Binder -> [String] -> Bound`
Binds an input consisting of multiple strings of Styx code.

#### :small_orange_diamond: `BindFiles : Binder -> [String] -> Bound`
Binds an input consisting of multiple files containing Styx code.

#### :small_orange_diamond: `DefinePrelude : Binder -> [String] -> ()`
Defines what symbols should be imported in every bound file, by default.

#### :small_orange_diamond: `Import : Binder -> String -> ()`
Imports the function bearing the given fully-qualified name.

#### :small_orange_diamond: `ImportAll : Binder -> String -> ()`
Imports every function defined in the given namespace.

## `Styx.Intrinsics`
Important functions used throughout the standard library.

## `Styx.Parsing`
Parsing utilities.

## `System`
Primitives and important types.

#### :small_blue_diamond: `String : Type`
The type which represents a string. Behind the scenes, it is implemented as a simple pointer along with a length field.

#### :small_blue_diamond: `Type : Type`
The type which represents... a type. It is defined directly by the Virtual Machine.

## `System.Collections`
Static and chained collections of items.

## `System.Console`
Console utilities.

## `System.Control`
Functional concepts and implementations.

## `System.Environment`
Information about the host and target OSes, platforms and backends.

## `System.FileSystem`
Access to the file system.

## `System.Foreign`
Foreign function interface and communication with the different backends.

## `System.IO`
General IO with the system.

#### :small_blue_diamond: `Stream : Type`
The type which represents a stream of bytes.

#### :small_orange_diamond: `Stdin : Stream`
The standard input stream.

#### :small_orange_diamond: `Stdout : Stream`
The standard output stream.

#### :small_orange_diamond: `Stderr : Stream`
The standard error stream.

## `System.Reactive`
Reactive programming utilities.

## `System.Reflection`
Runtime and compile-time reflection utilities.

#### :small_blue_diamond: `FunctionInfo : Type`
Informations about a function.

#### :small_blue_diamond: `ComponentInfo : Type`
Informations about a component.

#### :small_orange_diamond: `GetFunction : String -> FunctionInfo?`
Gets a Styx function, given its full name.

#### :small_orange_diamond: `GetFunctions : String -> [FunctionInfo]`
Gets all Styx functions bearing the given name.

#### :small_orange_diamond: `Components : FunctionInfo -> [ComponentInfo]`
Gets all components of the given function.

#### :small_orange_diamond: `Returned : FunctionInfo -> ComponentInfo`
Gets the return component of the given function.

#### :small_orange_diamond: `Name : FunctionInfo -> String`
Gets the name of the given function.

#### :small_orange_diamond: `Namespace : FunctionInfo -> String`
Gets the namespace of the given function.

## `Zero.Styx`
Styx bootstrapping and self-hosting utilities.

#### :small_orange_diamond: `RenameAll : ()`
Renames every existing declaration to `Zero.[decl-name]`. This helps prevent name conflicts when self-hosting Styx.