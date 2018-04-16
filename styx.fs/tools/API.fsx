
open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Linq
open System.Text.RegularExpressions

/// Returns the path to the 'styx/' directory.
let RootDirectory = Path.GetDirectoryName(__SOURCE_DIRECTORY__)

/// Returns the path to the 'styx/lib/' directory.
let SourceDirectory = Path.Combine(RootDirectory, "lib")

/// Returns the path to the 'styx/docs/' directory.
let DocsDirectory = Path.Combine(RootDirectory, "docs")

/// Returns an enumerator over all Styx filenames.
let SourceFiles = Directory.EnumerateFiles(SourceDirectory, "*.sx", SearchOption.AllDirectories)
                           .Where(fun x -> match Path.GetFileName(x) with
                                           | "Main.sx" -> false
                                           | "Initialize.sx" -> false
                                           | _  -> true)

/// Returns an enumerator over the content of all Styx files.
let Sources() = seq { for file in SourceFiles do
                        yield File.ReadAllText(file) }

/// Returns an enumerator over all lines of all Styx files.
let SourceLines() = seq { for file in SourceFiles do
                            yield! File.ReadAllLines(file) }


/// Represents a Styx function.
type Function = { Documentation: String;
                  Namespace: String;
                  Name: String;
                  Components: String[] }
with
  member this.FullName = sprintf "%s.%s" this.Namespace this.Name

  member this.IsType = this.Components.Last() = "Type"

  override this.ToString() = sprintf "%s : %s" this.FullName (String.Join(" -> ", this.Components))

let NamespaceDocs = Map.empty<String, String>

/// Parses a source file into a list of functions.
type Parser(lines: String[]) =
  let mutable indent = 0
  let mutable pos = 0
  let mutable docs = ""
  let mutable ns = []

  let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success
      then Some(List.tail [ for g in m.Groups -> g.Value ])
      else None

  let updateIndent len =
    ns <- List.filter (fun (_, y) -> y < len) ns
    indent <- len

  member __.Namespace = String.Join(".", List.map (fun (x, _) -> x) ns)

  member this.NamespaceDocs(?ns: String) =
    let ns = defaultArg ns this.Namespace
    NamespaceDocs.TryFind ns

  member this.Next =
    if pos >= lines.Length
      then None
      else match lines.[pos] with
           | Regex @"(\s*)namespace (.+)" [ indent; newNs ] ->
             updateIndent indent.Length
             ns <- (newNs, indent.Length)::ns
             pos <- pos + 1
             this.Next

           | Regex @"(\s*)--\|(.+)" [ indent; doc ] ->
             docs <- sprintf "%s%s%s" docs Environment.NewLine doc
             pos <- pos + 1
             updateIndent indent.Length
             this.Next

           | Regex @"(\s*)--\^(.+)" [ indent; doc ] ->
             let (docs, add) = match NamespaceDocs.TryFind this.Namespace with
                               | Some docs -> sprintf "%s%s%s" docs Environment.NewLine doc, false
                               | None -> doc, true

             if add
              then NamespaceDocs.Add(this.Namespace, docs) |> ignore
              else NamespaceDocs.[this.Namespace] = docs |> ignore

             pos <- pos + 1
             updateIndent indent.Length
             this.Next

           | Regex @"(\s*)([A-Z]\w+|\S+) : (.+)" [ indent; name; comps ] ->
             pos <- pos + 1
             updateIndent indent.Length
             let doc = docs.Trim()
             docs <- ""
             Some({ Documentation = doc; Namespace = this.Namespace; Name = name;
                    Components = comps.Split([| " -> " |], StringSplitOptions.None) })

           | _ -> pos <- pos + 1
                  this.Next

  interface IEnumerable<Function> with
    member this.GetEnumerator() =
      let rec enumerate() = seq { match this.Next with
                                  | Some fn -> yield fn; yield! enumerate()
                                  | None -> () }

      enumerate().GetEnumerator()

  interface IEnumerable with
    member this.GetEnumerator() =
      (this :> IEnumerable<Function>).GetEnumerator() :> IEnumerator


/// Returns all the defined functions.
let AllFunctions() = Parser(SourceLines().ToArray()).ToArray()

/// Emits a Markdown file that contains all declarations.
let EmitMarkdown(writer: TextWriter, functions: Function[]) =
  let namespaces = query { 
                    for fn in functions do
                      groupBy (fn.Namespace) into ns
                      where (ns.Count() > 0)
                      sortBy ns.Key
                      select ns }

  for ns in namespaces do
    writer.WriteLine("## {0}", ns.Key)

    match NamespaceDocs.TryFind ns.Key with
    | Some doc -> writer.WriteLine(doc)
    | None -> ()

    for fn in ns.OrderBy(fun x -> not x.IsType)
                .ThenBy(fun x -> x.Name) do
      let emoji = if fn.IsType
                  then "small_blue_diamond"
                  else "small_orange_diamond"
      let components = String.Join(" -> ", fn.Components)

      writer.WriteLine("#### :{0}: `{1} : {2}`", emoji, fn.Name, components)
      writer.WriteLine(fn.Documentation)
      writer.WriteLine()

/// Emits the Markdown header of the API file.
let EmitMarkdownHeader(writer: TextWriter) =
  writer.Write("
API
===

This document contains a list of all functions implemented in Styx until now.
It was automatically generated by a tool, and may be incomplete.

### Legend
- :small_blue_diamond: Types
- :small_orange_diamond: Functions

")

/// Emits the content of the API file to 'styx/{name}'.
let EmitMarkdownFile(name: String option) =
  let path = Path.Combine(RootDirectory, match name with
                                         | Some n -> n
                                         | None -> "lib/API.md")
  use file = File.Create(path)
  use writer = new StreamWriter(file)

  EmitMarkdownHeader(writer)
  EmitMarkdown(writer, AllFunctions())

/// Emits the content of the API file to a string.
let EmitMarkdownFileToString() =
  use ms = new MemoryStream()
  use writer = new StreamWriter(ms)

  EmitMarkdownHeader(writer)
  EmitMarkdown(writer, AllFunctions())

  writer.Flush()

  System.Text.Encoding.Default.GetString(ms.ToArray())