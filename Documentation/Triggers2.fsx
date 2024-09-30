// USAGE: dotnet fsi Triggers2.fsx <customer-dir>

#r "nuget: FsExcel"
#r "nuget: Fsharp.Data"

open System
open FsExcel
open FSharp.Data


type TriggerDefinition = XmlProvider<Schema="../Triggers.xsd">

let print (message:string) = Console.WriteLine(message)
let printError (message:string) = Console.Error.WriteLine(message)

let fullPath path =
  let p = IO.Path.GetFullPath(path).Replace('\\', '/')
  if p.EndsWith('/') then p.Remove(p.Length - 1) else p

let xmlSubDir (customerDir:string) (xml:string) =
  IO.Path.GetDirectoryName(xml).Replace('\\', '/').Substring(customerDir.Length + 1)

let listFiles dir = seq {
  for file in IO.Directory.EnumerateFiles(dir, "*.xml", IO.SearchOption.AllDirectories) do
    yield fullPath file
}

let fail _ = failwith("USAGE: dotnet fsi Triggers.fsx <triggers-dir>")

if fsi.CommandLineArgs.Length <> 2 || not <| IO.Directory.Exists fsi.CommandLineArgs[1] then
  fail()

let customerDir = fullPath fsi.CommandLineArgs[1]
let customerName = IO.Path.GetFileName(customerDir)

type StoredProcedure = {
  name: string
  doc: string
  source: string
}

type Action =
| Class of string
| Executable of string
| StoredProcedures of StoredProcedure list
  with
  override this.ToString() =
    match this with
    | Class c -> c
    | Executable e -> e
    | StoredProcedures s ->
      let names = s |> List.map(_.name)
      String.Join("\r\n", names)

  member this.docStrings _ =
    match this with
    | Class _ -> ""
    | Executable _ -> ""
    | StoredProcedures s ->
      let docs = s |> List.map(_.doc)
      String.Join("\r\n", docs)

  member this.sources _ =
    match this with
    | Class _ -> None
    | Executable _ -> None
    | StoredProcedures s -> s |> List.map(fun p -> p.name, p.source) |> Some

type Trigger = {
  xml: string
  name: string
  active: bool
  triggerType: string
  folder: string
  action: Action
  criteria: string
}

let readProcSrc (searchDir:string) (procName:string) =
  if IO.File.Exists $"{searchDir}/{procName}.sql" then
    let procSrc = IO.File.ReadAllText $"{searchDir}/{procName}.sql"
    let docRE = Text.RegularExpressions.Regex($"eWaySP_SetProcedureDescription\s*'{procName}'\s*,\s*N?'(.*)'")
    let m = docRE.Match(procSrc)
    let doc = if m.Success then m.Groups[1].Value else "<documentation not found>"
    procSrc, doc
  else
    $"source of {procName} not found>", "<documentation not found>"

let readProcedures searchDir (p:TriggerDefinition.StoredProcedures) =
  p.StoredProcedures
  |> List.ofArray
  |> List.map (fun p ->
    let def, doc = readProcSrc searchDir p.Name
    {
      name = p.Name
      doc = doc
      source = def
    }
  )

let readAction searchDir (a:TriggerDefinition.Action) =
  match a.Type, a.Class, a.Executable, a.StoredProcedures with
  | "Class", Some c, _, _ -> Class $"Class {c.Name} @ {c.Assembly}"
  | "Executable", _, Some e, _ -> Executable $"  Executable {e.Target}"
  | "StoredProcedure", _, _, Some p -> StoredProcedures (readProcedures searchDir p)
  | t, c, e, p ->
    failwith($"action mismatch: type: {t}, class: {c}, executable: {e}, procedures: {p}")

let jobCriterion (c:TriggerDefinition.Criterias) criterionName =
  match c.ActionCriterias |> Array.tryFind (fun c -> c.Name = criterionName) with
  | None -> $"{criterionName}: Not set"
  | Some v -> $"""{criterionName}: {Option.defaultValue "Not set" v.Value}"""

let criterion (c:TriggerDefinition.ActionCriteria) =
  match c.Operator with
  | Some "IsChanged" -> $"{c.Name} IsChanged"
  | Some op -> $"""{c.Name} {op} {Option.defaultValue "<not set>" c.Value}"""
  | None -> $"""{c.Name} Equals {Option.defaultValue "<not set>" c.Value}"""

let readCriteria triggerType (c:TriggerDefinition.Criterias option) =
  match triggerType, c with
  | "ScheduledAtTime", Some c ->
    let jc = jobCriterion c
    $"""{jc "Time"} {jc "Repeat"} {jc "IterationCount"} {jc "Frequency"}"""
  | _, Some c ->
    let operator = Option.defaultValue "AND" c.Operator
    $"""{String.Join($" {operator} ", c.ActionCriterias |> Array.map criterion)}"""
  | _, None -> "<no criteria>"

let readTrigger searchDir xml (t:TriggerDefinition.TriggerDefinition) = {
  xml = xml
  name = Option.defaultValue "<no-name>" t.Name
  active = t.Active
  triggerType = t.When
  folder = Option.defaultValue "<no-folder>" t.Folder
  action = readAction searchDir t.Action
  criteria = readCriteria t.When t.Action.Criterias
}

let gatherData dir =
  listFiles dir
  |> Seq.map (fun xml ->
    let triggerXML = IO.File.ReadAllText xml |> TriggerDefinition.Parse
    match triggerXML.Triggers with
    | None ->
      printError $"no triggers in file {xml}"
      []
    | Some triggers ->
      triggers.TriggerDefinitions
      |> Array.map (readTrigger (IO.Path.GetDirectoryName(xml)) xml)
      |> List.ofArray
  )
  |> Seq.concat
  |> List.ofSeq

let excelName = $"""{customerName}-{DateTime.Now.ToString("yyMMdd-HHmmss")}.xlsx"""
print $"Scanning directory {customerDir}\nResult file: {excelName}"

let data = gatherData customerDir

let fullHeader = ["Subdirectory"; "Definition file"; "Trigger type"; "Folder"; "Action"; "Criteria"; "Documentation"]

let triggerFullRow (t:Trigger) = [
  Cell [String (xmlSubDir customerDir t.xml)]
  Cell [String (IO.Path.GetFileName t.xml)]
  Cell [String t.triggerType]
  Cell [String t.folder]
  Cell [String (t.action.ToString())]
  Cell [String t.criteria]
]

let jobHeader = ["Subdirectory"; "Definition file"; "Folder"; "Action"; "Criteria"]

let triggerJobRow (t:Trigger) = [
  Cell [String (xmlSubDir customerDir t.xml)]
  Cell [String (IO.Path.GetFileName t.xml)]
  Cell [String t.folder]
  Cell [String (t.action.ToString())]
  Cell [String t.criteria]
]

let header headings = [
  for heading in headings do
    yield Cell [
      String heading
      FontEmphasis Bold
    ]
]

[
  Worksheet "All triggers"
  yield! header fullHeader
  Go NewRow
  for t in data do
    yield! triggerFullRow t
    Go NewRow

  AutoFit AllCols

  Worksheet "Triggers"
  yield! header fullHeader
  Go NewRow
  for t in data do
    if t.triggerType = "ScheduledAtTime" then
      yield! triggerJobRow t
      Go NewRow

  AutoFit AllCols

  let procData' = data |> List.choose (_.action.sources()) |> List.concat |> List.distinctBy fst

  let procData =
    ((Map.empty<string, int>, []), procData')
    ||> List.fold (fun (shorts, procedures) (name, src) ->
      if name.Length <= 31 then
        (shorts, procedures @ [(name, name, src)])
      else
        let shortName = name.Substring(0, 28)
        let newShorts =
          shorts
          |> Map.change shortName (fun x ->
            match x with
            | Some c -> Some (c + 1)
            | None -> Some 0
          )
        (newShorts, procedures @ [($"{shortName}~{newShorts[shortName]}", name, src)])
    )
    |> snd

  for wsName, procName, procSrc in procData do
    Worksheet wsName
    Cell [String procName]
    Go NewRow
    Cell [String procSrc]
    AutoFit AllCols

]
|> Render.AsFile excelName
